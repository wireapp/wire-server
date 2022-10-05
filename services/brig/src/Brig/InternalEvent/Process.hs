-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Brig.InternalEvent.Process
  ( onEvent,
  )
where

import qualified Brig.API.User as API
import Brig.App
import qualified Brig.Data.Client as Data
import Brig.Effects.ClientStore (ClientStore)
import Brig.Effects.GalleyAccess (GalleyAccess)
import Brig.Effects.GundeckAccess (GundeckAccess)
import Brig.Effects.UniqueClaimsStore
import Brig.Effects.UserHandleStore
import Brig.Effects.UserKeyStore (UserKeyStore)
import Brig.Effects.UserQuery
import Brig.IO.Intra (rmClient)
import qualified Brig.IO.Intra as Intra
import Brig.InternalEvent.Types
import Brig.Options (defDeleteThrottleMillis, setDefaultUserLocale, setDeleteThrottleMillis)
import qualified Brig.Provider.API as API
import Brig.Types.User.Event
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Conc.Effect.Race
import Polysemy.Conc.Race
import Polysemy.Input
import Polysemy.Time.Data.TimeUnit
import qualified Polysemy.TinyLog as P
import System.Logger.Class (field, msg, val, (~~))
import Wire.API.User.Client
import Wire.Sem.Concurrency
import Wire.Sem.Paging

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent ::
  forall r p.
  Paging p =>
  Members
    '[ ClientStore,
       Concurrency 'Unsafe,
       GalleyAccess,
       GundeckAccess,
       Input (Local ()),
       P.TinyLog,
       Race,
       UniqueClaimsStore,
       UserHandleStore,
       UserKeyStore,
       UserQuery p
     ]
    r =>
  InternalNotification ->
  AppT r ()
onEvent n = do
  locale <- setDefaultUserLocale <$> view settings
  delay <- fromMaybe defDeleteThrottleMillis . setDeleteThrottleMillis <$> view settings
  handleTimeout $ case n of
    DeleteClient client uid mcon -> do
      let cid = clientId client
      mc <- wrapClient $ Data.lookupClient uid cid
      for_ mc $ \c -> do
        wrapHttp $ rmClient uid cid
        wrapClient $ Data.rmClient uid cid
        liftSem $ Intra.onClientEvent uid mcon (ClientRemoved uid c)
    DeleteUser uid -> do
      liftSem . P.info $
        msg (val "Processing user delete event")
          ~~ field "user" (toByteString uid)
      liftSem (API.lookupAccount locale uid) >>= mapM_ API.deleteAccount
      -- As user deletions are expensive resource-wise in the context of
      -- bulk user deletions (e.g. during team deletions),
      -- wait 'delay' ms before processing the next event
      liftSem $ timeoutU (MicroSeconds $ 1000 * fromIntegral delay) $ pure ()
    DeleteService pid sid -> do
      liftSem . P.info $
        msg (val "Processing service delete event")
          ~~ field "provider" (toByteString pid)
          ~~ field "service" (toByteString sid)
      API.finishDeleteService pid sid
  where
    handleTimeout :: AppT r a -> AppT r a
    handleTimeout _act = undefined

-- act
--   >>= ( \case
--           Just x -> pure x
--           Nothing -> throwM (InternalEventTimeout n)
--       )
--     . flip (withAsyncWait (Minutes 1)) await

newtype InternalEventException
  = -- | 'onEvent' has timed out
    InternalEventTimeout InternalNotification
  deriving (Show)

instance Exception InternalEventException
