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
    MkUsr,
    scaffolding,
    ejpdRequestClient,
    getAccountConferenceCallingConfigClient,
    putAccountConferenceCallingConfigClient,
    deleteAccountConferenceCallingConfigClient,
  )
where

import API.Team.Util (createPopulatedBindingTeamWithNamesAndHandles)
import Bilge hiding (host, port)
import Control.Lens (view, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import Data.ByteString.Base16 qualified as B16
import Data.Handle (Handle)
import Data.Id
import Data.List1 qualified as List1
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Imports
import Servant.API ((:>))
import Servant.API.ContentTypes (NoContent)
import Servant.Client qualified as Client
import System.Random (randomIO)
import Util
import Util.Options (Endpoint, host, port)
import Wire.API.Connection
import Wire.API.Push.V2.Token qualified as PushToken
import Wire.API.Routes.Internal.Brig as IAPI
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Member qualified as Team
import Wire.API.User

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

type MkUsr =
  Maybe (Set (Relation, EJPDResponseItem)) ->
  Maybe (Set EJPDResponseItem, Team.NewListType) ->
  EJPDResponseItem

scaffolding ::
  forall m.
  (TestConstraints m) =>
  Brig ->
  Gundeck ->
  m (Handle, MkUsr, Handle, MkUsr, MkUsr)
scaffolding brig gundeck = do
  (_tid, usr1, [usr3]) <- createPopulatedBindingTeamWithNamesAndHandles brig 1
  (_handle1, usr2) <- createUserWithHandle brig
  connectUsers brig (userId usr1) (List1.singleton $ userId usr2)
  tok1 <- registerPushToken gundeck $ userId usr1
  tok2 <- registerPushToken gundeck $ userId usr2
  tok3 <- registerPushToken gundeck $ userId usr2
  pure
    ( fromJust $ userHandle usr1,
      mkUsr usr1 (Set.fromList [tok1]),
      fromJust $ userHandle usr2,
      mkUsr usr2 (Set.fromList [tok2, tok3]),
      mkUsr usr3 Set.empty
    )
  where
    mkUsr :: User -> Set Text -> MkUsr
    mkUsr usr toks =
      EJPDResponseItem
        (userId usr)
        (userTeam usr)
        (userDisplayName usr)
        (userHandle usr)
        (userEmail usr)
        (userPhone usr)
        toks

    registerPushToken :: Gundeck -> UserId -> m Text
    registerPushToken gd u = do
      t <- randomToken
      rsp <- registerPushTokenRequest gd u t
      responseJsonEither rsp
        & either
          (error . show)
          (pure . PushToken.tokenText . view PushToken.token)

    registerPushTokenRequest :: Gundeck -> UserId -> PushToken.PushToken -> m ResponseLBS
    registerPushTokenRequest gd u t = do
      post
        ( gd
            . path "/push/tokens"
            . contentJson
            . zUser u
            . zConn "random"
            . json t
        )

    randomToken :: m PushToken.PushToken
    randomToken = liftIO $ do
      c <- liftIO $ newClientId <$> (randomIO :: IO Word64)
      tok <- (PushToken.Token . T.decodeUtf8) . B16.encode <$> randomBytes 32
      pure $ PushToken.pushToken PushToken.APNSSandbox (PushToken.AppName "test") tok c

ejpdRequestClientM :: Maybe Bool -> EJPDRequestBody -> Client.ClientM EJPDResponseBody
ejpdRequestClientM = Client.client (Proxy @("i" :> IAPI.EJPDRequest))

getAccountConferenceCallingConfigClientM :: UserId -> Client.ClientM (Public.WithStatusNoLock Public.ConferenceCallingConfig)
getAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.GetAccountConferenceCallingConfig))

putAccountConferenceCallingConfigClientM :: UserId -> Public.WithStatusNoLock Public.ConferenceCallingConfig -> Client.ClientM NoContent
putAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.PutAccountConferenceCallingConfig))

deleteAccountConferenceCallingConfigClientM :: UserId -> Client.ClientM NoContent
deleteAccountConferenceCallingConfigClientM = Client.client (Proxy @("i" :> IAPI.DeleteAccountConferenceCallingConfig))

ejpdRequestClient :: (HasCallStack, MonadThrow m, MonadIO m) => Endpoint -> Manager -> Maybe Bool -> EJPDRequestBody -> m EJPDResponseBody
ejpdRequestClient brigep mgr includeContacts ejpdReqBody = runHereClientM brigep mgr (ejpdRequestClientM includeContacts ejpdReqBody) >>= either throwM pure

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
