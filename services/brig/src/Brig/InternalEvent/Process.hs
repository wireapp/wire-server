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

import Bilge.IO (MonadHttp)
import Bilge.RPC (HasRequestId)
import qualified Brig.API.User as API
import Brig.App
import qualified Brig.Data.Client as Data
import qualified Brig.IO.Intra as Intra
import Brig.InternalEvent.Types
import Brig.Options (defDeleteThrottleMillis, setDeleteThrottleMillis)
import qualified Brig.Provider.API as API
import Brig.Types.User.Event
import Brig.User.Search.Index (MonadIndexIO)
import Cassandra (MonadClient)
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Imports
import System.Logger.Class (field, msg, val, (~~))
import qualified System.Logger.Class as Log
import UnliftIO (timeout)

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent ::
  ( Log.MonadLogger m,
    MonadCatch m,
    MonadIndexIO m,
    MonadReader Env m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadUnliftIO m,
    MonadClient m
  ) =>
  InternalNotification ->
  m ()
onEvent n = handleTimeout $ case n of
  DeleteClient cid uid mcon -> do
    mc <- Data.lookupClient uid cid
    maybe
      (pure ())
      ( \c -> do
          Intra.onClientEvent uid mcon (ClientRemoved uid c)
          Data.rmClient uid cid
      )
      mc
  DeleteUser uid -> do
    Log.info $
      msg (val "Processing user delete event")
        ~~ field "user" (toByteString uid)
    API.lookupAccount uid >>= mapM_ API.deleteAccount
    -- As user deletions are expensive resource-wise in the context of
    -- bulk user deletions (e.g. during team deletions),
    -- wait 'delay' ms before processing the next event
    delay <- fromMaybe defDeleteThrottleMillis . setDeleteThrottleMillis <$> view settings
    liftIO $ threadDelay (1000 * delay)
  DeleteService pid sid -> do
    Log.info $
      msg (val "Processing service delete event")
        ~~ field "provider" (toByteString pid)
        ~~ field "service" (toByteString sid)
    API.finishDeleteService pid sid
  where
    handleTimeout act =
      timeout 60000000 act >>= \case
        Just x -> pure x
        Nothing -> throwM (InternalEventTimeout n)

newtype InternalEventException
  = -- | 'onEvent' has timed out
    InternalEventTimeout InternalNotification
  deriving (Show)

instance Exception InternalEventException
