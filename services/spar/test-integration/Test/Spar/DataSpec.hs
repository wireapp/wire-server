{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.DataSpec where

import Bilge
import Cassandra as Cas
import Control.Concurrent
import Control.Exception
import Control.Monad.Reader
import Control.Monad.Catch
import Data.Aeson (encode)
import Data.Id
import Data.String.Conversions
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Lens.Micro
import Spar.Data as Data
import Spar.API.Test (IntegrationTests)
import Spar.API.Instances ()
import Spar.Options as Options
import Util
import Util.Options

import qualified SAML2.WebSSO as SAML
import qualified Servant as Servant
import qualified Servant.Client as Servant
import qualified Text.XML.Util as SAML


spec :: SpecWith TestEnv
spec = do
  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (_, _, idpid) <- createTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid

      let probe :: IO Bool
          probe = do
            denv :: Data.Env <- Data.mkEnv (env ^. teOpts) <$> getCurrentTime
            runClient (env ^. teCql) (checkAgainstRequest (req ^. SAML.rqID) `runReaderT` denv)

          maxttl :: Int  -- musec
          maxttl = (fromIntegral . fromTTL $ env ^. teOpts . to maxttlAuthreq) * 1000 * 1000

      liftIO $ do
        maxttl `shouldSatisfy` (< 60 * 1000 * 1000)  -- otherwise the test will be really slow.
        probe `shouldReturn` True
        threadDelay ((maxttl `div` 10) * 8)
        probe `shouldReturn` True
        threadDelay  ((maxttl `div` 10) * 4)
        probe `shouldReturn` False


  describe "cql binding" $ do

    -- requests

    let runStoreReq :: TestEnv -> SAML.ID SAML.AuthnRequest -> Http ()
        runStoreReq env authreqid = do
          endoflife <- getTime (fromIntegral ttlReqs)
          runServantClient env $ clientPostRequest authreqid endoflife

        runCheckReq :: TestEnv -> SAML.ID SAML.AuthnRequest -> Http Bool
        runCheckReq env authreqid = do
          runServantClient env $ clientGetRequest authreqid

        getID :: MonadIO m => m (SAML.ID a)
        getID = SAML.ID . UUID.toText <$> liftIO UUID.nextRandom

        getTime :: MonadIO m => NominalDiffTime -> m SAML.Time
        getTime secs = (SAML.addTime secs) . SAML.Time <$> liftIO getCurrentTime

        ttlReqs :: Int
        ttlReqs = 3

    describe "storeRequest" $ do
      it "responds with 2xx" $ do
        env <- ask
        reqid <- getID
        runStoreReq env reqid `shouldRespondWith` (== ())

    describe "checkAgainstRequest" $ do
      context "request exists and is alive" $ do
        it "returns True" $ do
          env <- ask
          reqid <- getID
          runStoreReq env reqid `shouldRespondWith` (== ())
          runCheckReq env reqid `shouldRespondWith` (== True)

      context "request does not exist" $ do
        it "returns False" $ do
          env <- ask
          reqid <- getID
          runCheckReq env reqid `shouldRespondWith` (== False)

      context "request exists, but is outdated" $ do
        it "returns False" $ do
          env <- ask
          reqid <- getID
          runStoreReq env reqid `shouldRespondWith` (== ())
          liftIO $ threadDelay (ttlReqs * 1000 * 1000)
          runCheckReq env reqid `shouldRespondWith` (== False)


    -- assertions

    let runStoreAssertion :: TestEnv -> SAML.ID SAML.Assertion -> NominalDiffTime -> Http Bool
        runStoreAssertion env assid ttl = do
          endoflife <- getTime ttl
          runServantClient env $ clientPostAssertion assid endoflife

    describe "storeAssertion" $ do
      context "assertion does not exist" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)

      context "assertion exists, but is outdated" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)
          liftIO $ threadDelay (2 * 1000 * 1000)
          runStoreAssertion env uuid 1 `shouldRespondWith` (== True)

      context "assertion exists and is alive" $ do
        it "returns False" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 2 `shouldRespondWith` (== True)
          runStoreAssertion env uuid 2 `shouldRespondWith` (== False)


    -- users

    let runInsertUser :: TestEnv -> SAML.UserRef -> UserId -> Http ResponseLBS  -- TODO: servant-client-ify
        runInsertUser env uref uid = do
          post $ (env ^. teSpar) . path ("/i/integration-tests/user/" <> cs (show uid)) . json uref

        runGetUser :: TestEnv -> SAML.UserRef -> Http ResponseLBS
        runGetUser env uref = do
          get $ (env ^. teSpar) . path "/i/integration-tests/user" . json uref

        nextUserRef :: MonadIO m => m SAML.UserRef
        nextUserRef = liftIO $ do
          (UUID.toText -> tenant) <- UUID.nextRandom
          (UUID.toText -> subject) <- UUID.nextRandom
          pure $ SAML.UserRef
            (SAML.Issuer $ SAML.unsafeParseURI ("http://" <> tenant))
            (SAML.opaqueNameID subject)

        getIs :: Maybe UserId -> ResponseLBS -> Bool
        getIs muid resp = statusCode resp == 200 && responseBody resp == (Just . cs . encode $ muid)

    describe "insertUser, getUser" $ do
      context "user is new" $ do
        it "getUser returns Nothing" $ do
          env  <- ask
          uref <- nextUserRef
          runGetUser env uref `shouldRespondWith` getIs Nothing

        it "inserts new user and responds with 201 / returns new user" $ do
          env  <- ask
          uref <- nextUserRef
          uid  <- Id <$> liftIO UUID.nextRandom
          runInsertUser env uref uid `shouldRespondWith` (\resp -> statusCode resp == 201)
          runGetUser env uref `shouldRespondWith` getIs (Just uid)

      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          env  <- ask
          uref <- nextUserRef
          uid  <- Id <$> liftIO UUID.nextRandom
          uid' <- Id <$> liftIO UUID.nextRandom
          runInsertUser env uref uid `shouldRespondWith` (\resp -> statusCode resp == 201)
          runInsertUser env uref uid' `shouldRespondWith` (\resp -> statusCode resp == 201)
          runGetUser env uref `shouldRespondWith` getIs (Just uid')


runServantClient :: TestEnv -> Servant.ClientM a -> Http a
runServantClient tenv = hoist . (`Servant.runClientM` cenv)
  where
    hoist :: IO (Either Servant.ServantError a) -> Http a
    hoist = HttpT . ReaderT . const . (>>= either (throwM . ErrorCall . show) pure)

    cenv :: Servant.ClientEnv
    cenv = Servant.ClientEnv (tenv ^. teMgr) (tenv ^. teTstOpts . to cfgSpar . to endpointToUrl)
      where
        endpointToUrl :: Endpoint -> Servant.BaseUrl
        endpointToUrl (Endpoint sparhost sparport) =
          Servant.BaseUrl Servant.Http (cs sparhost) (fromIntegral sparport) "/i/integration-tests"

clientPostRequest   :: SAML.ID SAML.AuthnRequest -> SAML.Time -> Servant.ClientM ()
clientGetRequest    :: SAML.ID SAML.AuthnRequest -> Servant.ClientM Bool
clientPostAssertion :: SAML.ID SAML.Assertion -> SAML.Time -> Servant.ClientM Bool
clientPostUser      :: SAML.UserRef -> UserId -> Servant.ClientM ()
clientGetUser       :: SAML.UserRef -> Servant.ClientM (Maybe UserId)

clientPostRequest     Servant.:<|>
  clientGetRequest    Servant.:<|>
  clientPostAssertion Servant.:<|>
  clientPostUser      Servant.:<|>
  clientGetUser
  = Servant.client (Servant.Proxy @IntegrationTests)
