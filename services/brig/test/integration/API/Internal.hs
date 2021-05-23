module API.Internal
  ( tests,
  )
where

import API.Team.Util (createPopulatedBindingTeamWithNamesAndHandles)
import Bilge
import qualified Brig.API.Internal as IAPI
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.User.EJPD as EJPD
import Control.Lens (view, (^.))
import Control.Monad.Catch (MonadCatch, throwM)
import qualified Data.ByteString.Base16 as B16
import Data.Handle (Handle)
import Data.Id
import qualified Data.List1 as List1
import Data.Proxy (Proxy (Proxy))
import qualified Data.Set as Set
import Data.String.Conversions (cs)
import qualified Data.Text.Encoding as T
import Imports
import qualified Servant.Client as Client
import System.Random (randomIO)
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint, epHost, epPort)
import qualified Wire.API.Connection as Conn
import qualified Wire.API.Push.V2.Token as PushToken
import qualified Wire.API.Team.Member as Team

tests :: Opt.Opts -> Manager -> Brig -> Endpoint -> Gundeck -> IO TestTree
tests _opts mgr brig brigep gundeck = do
  return $
    testGroup "api/internal" $
      [ test mgr "ejpd requests" $ testEJPDRequest mgr brig brigep gundeck
      ]

type TestConstraints m = (MonadFail m, MonadCatch m, MonadIO m, MonadHttp m)

type MkUsr =
  Maybe (Set (Relation_', EJPDResponseItem)) ->
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
ejpdRequestClientM = Client.client (Proxy @IAPI.ServantAPI)

ejpdRequestClient :: TestConstraints m => Endpoint -> Manager -> Maybe Bool -> EJPDRequestBody -> m EJPDResponseBody
ejpdRequestClient brigep mgr includeContacts ejpdReqBody = do
  let env = Client.mkClientEnv mgr baseurl
      baseurl = Client.BaseUrl Client.Http (cs $ brigep ^. epHost) (fromIntegral $ brigep ^. epPort) ""
  liftIO $
    Client.runClientM (ejpdRequestClientM includeContacts ejpdReqBody) env >>= \case
      Left err -> throwM err
      Right val -> pure val

testEJPDRequest :: TestConstraints m => Manager -> Brig -> Endpoint -> Gundeck -> m ()
testEJPDRequest mgr brig brigep gundeck = do
  (handle1, mkUsr1, handle2, mkUsr2, mkUsr3) <- scaffolding brig gundeck

  do
    let req = EJPDRequestBody [handle1]
        want =
          EJPDResponseBody
            [ mkUsr1 Nothing Nothing
            ]
    have <- ejpdRequestClient brigep mgr Nothing req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle1, handle2]
        want =
          EJPDResponseBody
            [ mkUsr1 Nothing Nothing,
              mkUsr2 Nothing Nothing
            ]
    have <- ejpdRequestClient brigep mgr Nothing req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle2]
        want =
          EJPDResponseBody
            [ mkUsr2
                (Just (Set.fromList [(Conn.Accepted_', mkUsr1 Nothing Nothing)]))
                Nothing
            ]
    have <- ejpdRequestClient brigep mgr (Just True) req
    liftIO $ assertEqual "" want have

  do
    let req = EJPDRequestBody [handle1, handle2]
        want =
          EJPDResponseBody
            [ mkUsr1
                (Just (Set.fromList [(Conn.Accepted_', mkUsr2 Nothing Nothing)]))
                (Just (Set.fromList [mkUsr3 Nothing Nothing], Team.NewListComplete)),
              mkUsr2
                (Just (Set.fromList [(Conn.Accepted_', mkUsr1 Nothing Nothing)]))
                Nothing
            ]
    have <- ejpdRequestClient brigep mgr (Just True) req
    liftIO $ assertEqual "" want have
