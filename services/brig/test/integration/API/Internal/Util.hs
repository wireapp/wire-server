-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API.Internal.Util
  ( TestConstraints,
    getAccountConferenceCallingConfigClient,
    putAccountConferenceCallingConfigClient,
    deleteAccountConferenceCallingConfigClient,
  )
where

import Bilge hiding (host, port)
import Control.Lens ((^.))
import Control.Monad.Catch (MonadCatch)
import Data.Id
import Data.Proxy (Proxy (Proxy))
import Imports
import Servant.API ((:>))
import Servant.API.ContentTypes (NoContent)
import Servant.Client qualified as Client
import Util.Options (Endpoint, host, port)
import Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Team.Feature qualified as Public

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

getAccountConferenceCallingConfigClientM :: UserId -> Client.ClientM (Public.WithStatusNoLock Public.ConferenceCallingConfig)
getAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.GetAccountConferenceCallingConfig))

putAccountConferenceCallingConfigClientM :: UserId -> Public.WithStatusNoLock Public.ConferenceCallingConfig -> Client.ClientM NoContent
putAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.PutAccountConferenceCallingConfig))

deleteAccountConferenceCallingConfigClientM :: UserId -> Client.ClientM NoContent
deleteAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.DeleteAccountConferenceCallingConfig))

getAccountConferenceCallingConfigClient :: (HasCallStack, MonadIO m) => Endpoint -> Manager -> UserId -> m (Either Client.ClientError (Public.WithStatusNoLock Public.ConferenceCallingConfig))
getAccountConferenceCallingConfigClient brigep mgr uid = runHereClientM brigep mgr (getAccountConferenceCallingConfigClientM uid)

putAccountConferenceCallingConfigClient :: (HasCallStack, MonadIO m) => Endpoint -> Manager -> UserId -> Public.WithStatusNoLock Public.ConferenceCallingConfig -> m (Either Client.ClientError NoContent)
putAccountConferenceCallingConfigClient brigep mgr uid cfg = runHereClientM brigep mgr (putAccountConferenceCallingConfigClientM uid cfg)

deleteAccountConferenceCallingConfigClient :: (HasCallStack, MonadIO m) => Endpoint -> Manager -> UserId -> m (Either Client.ClientError NoContent)
deleteAccountConferenceCallingConfigClient brigep mgr uid = runHereClientM brigep mgr (deleteAccountConferenceCallingConfigClientM uid)

runHereClientM :: (HasCallStack, MonadIO m) => Endpoint -> Manager -> Client.ClientM a -> m (Either Client.ClientError a)
runHereClientM brigep mgr action = do
  let env = Client.mkClientEnv mgr baseurl
      baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. host) (fromIntegral $ brigep ^. port) ""
  liftIO $ Client.runClientM action env
