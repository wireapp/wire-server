module API.Internal.Util
  ( TestConstraints,
    MkUsr,
    scaffolding,
    ejpdRequestClient,
    getAccountFeatureConfigClient,
    putAccountFeatureConfigClient,
    deleteAccountFeatureConfigClient,
  )
where

import API.Team.Util (createPopulatedBindingTeamWithNamesAndHandles)
import Bilge
import qualified Brig.API.Internal as IAPI
import Brig.Types
import Control.Lens (view, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow, throwM)
import qualified Data.ByteString.Base16 as B16
import Data.Handle (Handle)
import Data.Id
import qualified Data.List1 as List1
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as T
import Imports
import Servant.API ((:<|>) ((:<|>)))
import Servant.API.ContentTypes (NoContent)
import qualified Servant.Client as Client
import System.Random (randomIO)
import Util
import Util.Options (Endpoint, epHost, epPort)
import qualified Wire.API.Push.V2.Token as PushToken
import Wire.API.Routes.Internal.Brig.EJPD as EJPD
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Team

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

    registerPushToken :: TestConstraints m => Gundeck -> UserId -> m Text
    registerPushToken gd u = do
      t <- randomToken
      rsp <- registerPushTokenRequest gd u t
      responseJsonEither rsp
        & either
          (error . show)
          (pure . PushToken.tokenText . view PushToken.token)

    registerPushTokenRequest :: TestConstraints m => Gundeck -> UserId -> PushToken.PushToken -> m ResponseLBS
    registerPushTokenRequest gd u t = do
      post
        ( gd
            . path "/push/tokens"
            . contentJson
            . zUser u
            . zConn "random"
            . json t
        )

    randomToken :: MonadIO m => m PushToken.PushToken
    randomToken = liftIO $ do
      c <- liftIO $ newClientId <$> (randomIO :: IO Word64)
      tok <- PushToken.Token . T.decodeUtf8 <$> B16.encode <$> randomBytes 32
      return $ PushToken.pushToken PushToken.APNSSandbox (PushToken.AppName "test") tok c

ejpdRequestClientM :: Maybe Bool -> EJPDRequestBody -> Client.ClientM EJPDResponseBody
getAccountFeatureConfigClientM :: UserId -> Client.ClientM (Maybe Public.TeamFeatureStatusNoConfig)
putAccountFeatureConfigClientM :: UserId -> Public.TeamFeatureStatusNoConfig -> Client.ClientM NoContent
deleteAccountFeatureConfigClientM :: UserId -> Client.ClientM NoContent
( ejpdRequestClientM
    :<|> getAccountFeatureConfigClientM
    :<|> putAccountFeatureConfigClientM
    :<|> deleteAccountFeatureConfigClientM
  ) = Client.client (Proxy @IAPI.API)

ejpdRequestClient :: (HasCallStack, MonadThrow m, MonadIO m, MonadHttp m) => Endpoint -> Manager -> Maybe Bool -> EJPDRequestBody -> m EJPDResponseBody
ejpdRequestClient brigep mgr includeContacts ejpdReqBody = runHereClientM brigep mgr (ejpdRequestClientM includeContacts ejpdReqBody) >>= either throwM pure

getAccountFeatureConfigClient :: (HasCallStack, MonadIO m, MonadHttp m) => Endpoint -> Manager -> UserId -> m (Either Client.ClientError (Maybe Public.TeamFeatureStatusNoConfig))
getAccountFeatureConfigClient brigep mgr uid = runHereClientM brigep mgr (getAccountFeatureConfigClientM uid)

putAccountFeatureConfigClient :: (HasCallStack, MonadIO m, MonadHttp m) => Endpoint -> Manager -> UserId -> Public.TeamFeatureStatusNoConfig -> m (Either Client.ClientError NoContent)
putAccountFeatureConfigClient brigep mgr uid cfg = runHereClientM brigep mgr (putAccountFeatureConfigClientM uid cfg)

deleteAccountFeatureConfigClient :: (HasCallStack, MonadIO m, MonadHttp m) => Endpoint -> Manager -> UserId -> m (Either Client.ClientError NoContent)
deleteAccountFeatureConfigClient brigep mgr uid = runHereClientM brigep mgr (deleteAccountFeatureConfigClientM uid)

runHereClientM :: (HasCallStack, MonadIO m, MonadHttp m) => Endpoint -> Manager -> Client.ClientM a -> m (Either Client.ClientError a)
runHereClientM brigep mgr action = do
  let env = Client.mkClientEnv mgr baseurl
      baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. epHost) (fromIntegral $ brigep ^. epPort) ""
  liftIO $ Client.runClientM action env
