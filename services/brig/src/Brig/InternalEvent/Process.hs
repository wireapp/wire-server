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
import Brig.InternalEvent.Types
import Brig.Options (defDeleteThrottleMillis, setDefaultUserLocale, setDeleteThrottleMillis)
import qualified Brig.Provider.API as API
import Brig.Sem.UniqueClaimsStore
import Brig.Sem.UserHandleStore
import Brig.Sem.UserQuery
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

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent ::
  forall r.
  Members
    '[ Input (Local ()),
       P.TinyLog,
       Race,
       UniqueClaimsStore,
       UserHandleStore,
       UserQuery
     ]
    r =>
  InternalNotification ->
  AppT r ()
onEvent n = do
  locale <- setDefaultUserLocale <$> view settings
  delay <- fromMaybe defDeleteThrottleMillis . setDeleteThrottleMillis <$> view settings
  handleTimeout $ case n of
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
      wrapHttpClient $ API.finishDeleteService pid sid
  where
    handleTimeout :: AppT r a -> AppT r a
    handleTimeout _act = undefined


newtype InternalEventException
  = -- | 'onEvent' has timed out
    InternalEventTimeout InternalNotification
  deriving (Show)

instance Exception InternalEventException
