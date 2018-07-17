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
import Control.Monad.Reader
import Data.Aeson (encode)
import Data.Id
import Data.String.Conversions
import Data.Time
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Lens.Micro
import Spar.Data as Data
import Spar.Options as Options
import Util

import qualified SAML2.WebSSO as SAML
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

    let runStoreReq :: TestEnv -> UUID -> Http ResponseLBS
        runStoreReq env uuid = do
          now <- liftIO $ gettime (fromIntegral ttlReqs)
          post $ (env ^. teSpar) . path ("/i/integration-tests/store-req/" <> cs (UUID.toText uuid) <> "/" <> now)

        runCheckReq :: TestEnv -> UUID -> Http ResponseLBS
        runCheckReq env uuid = do
          get $ (env ^. teSpar) . path ("/i/integration-tests/check-req/" <> cs (UUID.toText uuid))

        gettime :: MonadIO m => NominalDiffTime -> m SBS
        gettime secs = cs . SAML.renderTime . (SAML.addTime secs) . SAML.Time <$> liftIO getCurrentTime

        ttlReqs :: Int
        ttlReqs = 3

    describe "storeRequest" $ do
      it "responds with 2xx" $ do
        env <- ask
        uuid <- liftIO UUID.nextRandom
        runStoreReq env uuid `shouldRespondWith` ((== 201) . statusCode)

    describe "checkAgainstRequest" $ do
      context "request exists and is alive" $ do
        it "returns True" $ do
          env <- ask
          uuid <- liftIO UUID.nextRandom
          runStoreReq env uuid `shouldRespondWith` ((== 201) . statusCode)
          runCheckReq env uuid `shouldRespondWith` (\resp -> statusCode resp == 200 && responseBody resp == Just "true")

      context "request does not exist" $ do
        it "returns False" $ do
          env <- ask
          uuid <- liftIO UUID.nextRandom
          runCheckReq env uuid `shouldRespondWith` (\resp -> statusCode resp == 200 && responseBody resp == Just "false")

      context "request exists, but is outdated" $ do
        it "returns False" $ do
          env <- ask
          uuid <- liftIO UUID.nextRandom
          runStoreReq env uuid `shouldRespondWith` ((== 201) . statusCode)
          liftIO $ threadDelay (ttlReqs * 1000 * 1000)
          runCheckReq env uuid `shouldRespondWith` (\resp -> statusCode resp == 200 && responseBody resp == Just "false")


    -- assertions

    let runStoreAssertion :: TestEnv -> SAML.ID SAML.Assertion -> NominalDiffTime -> Http ResponseLBS
        runStoreAssertion env assid ttl = do
          let assidtxt = cs $ SAML.renderID assid
          ttltxt <- gettime ttl
          post $ (env ^. teSpar) . path ("/i/integration-tests/store-ass/" <> assidtxt <> "/" <> ttltxt)

    describe "storeAssertion" $ do
      context "assertion does not exist" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` ((== Just "true") . responseBody)

      context "assertion exists, but is outdated" $ do
        it "returns True" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 1 `shouldRespondWith` ((== Just "true") . responseBody)
          liftIO $ threadDelay (2 * 1000 * 1000)
          runStoreAssertion env uuid 1 `shouldRespondWith` ((== Just "true") . responseBody)

      context "assertion exists and is alive" $ do
        it "returns False" $ do
          env  <- ask
          uuid <- SAML.ID . UUID.toText <$> liftIO UUID.nextRandom
          runStoreAssertion env uuid 2 `shouldRespondWith` ((== Just "true") . responseBody)
          runStoreAssertion env uuid 2 `shouldRespondWith` ((== Just "false") . responseBody)


    -- users

    let runInsertUser :: TestEnv -> SAML.UserRef -> UserId -> Http ResponseLBS
        runInsertUser env uref uid = do
          post $ (env ^. teSpar) . path ("/i/integration-tests/insert-user/" <> cs (show uid)) . json uref

        runGetUser :: TestEnv -> SAML.UserRef -> Http ResponseLBS
        runGetUser env uref = do
          get $ (env ^. teSpar) . path "/i/integration-tests/get-user" . json uref

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
