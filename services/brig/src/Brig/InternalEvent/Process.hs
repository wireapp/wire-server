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

import Brig.API.User qualified as API
import Brig.App
import Brig.Effects.ConnectionStore
import Brig.IO.Intra (rmClient)
import Brig.IO.Intra qualified as Intra
import Brig.InternalEvent.Types
import Brig.Options (defDeleteThrottleMillis, setDeleteThrottleMillis)
import Brig.Provider.API qualified as API
import Brig.Types.User.Event
import Control.Lens (view)
import Control.Monad.Catch
import Data.ByteString.Conversion
import Data.Qualified (Local)
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Polysemy.Conc
import Polysemy.Input (Input)
import Polysemy.Time
import Polysemy.TinyLog as Log
import System.Logger.Class (field, msg, val, (~~))
import Wire.NotificationSubsystem
import Wire.Sem.Delay
import Wire.Sem.Paging.Cassandra (InternalPaging)

-- | Handle an internal event.
--
-- Has a one-minute timeout that should be enough for anything that it does.
onEvent ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member TinyLog r,
    Member Delay r,
    Member Race r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  InternalNotification ->
  Sem r ()
onEvent n = handleTimeout $ case n of
  DeleteClient clientId uid mcon -> do
    rmClient uid clientId
    Intra.onClientEvent uid mcon (ClientRemoved uid clientId)
  DeleteUser uid -> do
    Log.info $
      msg (val "Processing user delete event")
        ~~ field "user" (toByteString uid)
    embed (API.lookupAccount uid) >>= mapM_ API.deleteAccount
    -- As user deletions are expensive resource-wise in the context of
    -- bulk user deletions (e.g. during team deletions),
    -- wait 'delay' ms before processing the next event
    deleteThrottleMillis <- embed $ fromMaybe defDeleteThrottleMillis . setDeleteThrottleMillis <$> view settings
    delay (1000 * deleteThrottleMillis)
  DeleteService pid sid -> do
    Log.info $
      msg (val "Processing service delete event")
        ~~ field "provider" (toByteString pid)
        ~~ field "service" (toByteString sid)
    embed $ API.finishDeleteService pid sid
  where
    handleTimeout act =
      timeout (pure ()) (Seconds 60) act >>= \case
        Right x -> pure x
        Left _ -> embed $ throwM (InternalEventTimeout n)

newtype InternalEventException
  = -- | 'onEvent' has timed out
    InternalEventTimeout InternalNotification
  deriving (Show)

instance Exception InternalEventException
