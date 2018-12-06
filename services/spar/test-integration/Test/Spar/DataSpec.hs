{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.DataSpec (spec) where

import Imports
import Cassandra
import Control.Lens
import Control.Monad.Except
import Data.Typeable
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import SAML2.WebSSO as SAML
import Spar.Data as Data
import Spar.Types
import URI.ByteString.QQ (uri)
import Util.Core
import Util.Types


spec :: SpecWith TestEnv
spec = do
  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (_, _, (^. SAML.idpId) -> idpid) <- createTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid

      let probe :: (MonadIO m, MonadReader TestEnv m) => m Bool
          probe = runSparCass $ isAliveAReqID (req ^. SAML.rqID)

          maxttl :: Int  -- musec
          maxttl = (fromIntegral . fromTTL $ env ^. teOpts . to maxttlAuthreq) * 1000 * 1000

      liftIO $ maxttl `shouldSatisfy` (< 60 * 1000 * 1000)  -- otherwise the test will be really slow.
      p1 <- probe
      liftIO $ p1 `shouldBe` True
      liftIO $ threadDelay (maxttl `div` 2)
      p2 <- probe
      liftIO $ p2 `shouldBe` True  -- 0.5 lifetimes after birth
      liftIO $ threadDelay  maxttl
      p3 <- probe
      liftIO $ p3 `shouldBe` False  -- 1.5 lifetimes after birth


  describe "cql binding" $ do
    describe "AuthnRequest" $ do
      testSPStoreID storeAReqID unStoreAReqID isAliveAReqID

    describe "Assertion" $ do
      testSPStoreID storeAssID unStoreAssID isAliveAssID


    describe "VerdictFormat" $ do
      context "insert and get are \"inverses\"" $ do
        let check vf = it (show vf) $ do
              vid <- nextSAMLID
              ()  <- runSparCass $ storeVerdictFormat 1 vid vf
              mvf <- runSparCass $ getVerdictFormat vid
              liftIO $ mvf `shouldBe` Just vf

        check `mapM_`
          [ VerdictFormatWeb
          , VerdictFormatMobile [uri|https://fw/ooph|] [uri|https://lu/gn|]
          ]

      context "has timed out" $ do
        it "getVerdictFormat returns Nothing" $ do
          vid <- nextSAMLID
          ()  <- runSparCass $ storeVerdictFormat 1 vid VerdictFormatWeb
          liftIO $ threadDelay 2000000
          mvf <- runSparCass $ getVerdictFormat vid
          liftIO $ mvf `shouldBe` Nothing

      context "does not exist" $ do
        it "getVerdictFormat returns Nothing" $ do
          vid <- nextSAMLID
          mvf <- runSparCass $ getVerdictFormat vid
          liftIO $ mvf `shouldBe` Nothing


    describe "User" $ do
      context "user is new" $ do
        it "getUser returns Nothing" $ do
          uref <- nextUserRef
          muid <- runSparCass $ getUser uref
          liftIO $ muid `shouldBe` Nothing

        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid  <- nextWireId
          ()   <- runSparCass $ insertUser uref uid
          muid <- runSparCass $ getUser uref
          liftIO $ muid `shouldBe` Just uid

      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid  <- nextWireId
          uid' <- nextWireId
          ()   <- runSparCass $ insertUser uref uid
          ()   <- runSparCass $ insertUser uref uid'
          muid <- runSparCass $ getUser uref
          liftIO $ muid `shouldBe` Just uid'


    describe "BindCookie" $ do
      let mkcky :: TestSpar SetBindCookie
          mkcky = runSimpleSP . SAML.toggleCookie "/" . Just . (, 1) . UUID.toText =<< liftIO UUID.nextRandom

      it "insert and get are \"inverses\"" $ do
        uid  <- nextWireId
        cky  <- mkcky
        ()   <- runSparCassWithEnv $ insertBindCookie cky uid 1
        muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
        liftIO $ muid `shouldBe` Just uid

      context "has timed out" $ do
        it "lookupBindCookie returns Nothing" $ do
          uid  <- nextWireId
          cky  <- mkcky
          ()   <- runSparCassWithEnv $ insertBindCookie cky uid 1
          liftIO $ threadDelay 2000000
          muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing

      context "does not exist" $ do
        it "lookupBindCookie returns Nothing" $ do
          cky  <- mkcky
          muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing


    describe "IdPConfig" $ do
      it "storeIdPConfig, getIdPConfig are \"inverses\"" $ do
        idp <- IdPConfig <$> (IdPId <$> liftIO UUID.nextRandom) <*> makeTestIdPMetadata <*> nextWireId
        () <- runSparCass $ Data.storeIdPConfig idp
        midp <- runSparCass $ Data.getIdPConfig (idp ^. idpId)
        liftIO $ midp `shouldBe` Just idp

      it "getIdPConfigByIssuer works" $ do
        idp <- IdPConfig <$> (IdPId <$> liftIO UUID.nextRandom) <*> makeTestIdPMetadata <*> nextWireId
        () <- runSparCass $ Data.storeIdPConfig idp
        midp <- runSparCass $ Data.getIdPConfigByIssuer (idp ^. idpMetadata . edIssuer)
        liftIO $ midp `shouldBe` Just idp

      it "getIdPIdByIssuer works" $ do
        idp <- IdPConfig <$> (IdPId <$> liftIO UUID.nextRandom) <*> makeTestIdPMetadata <*> nextWireId
        () <- runSparCass $ Data.storeIdPConfig idp
        midp <- runSparCass $ Data.getIdPIdByIssuer (idp ^. idpMetadata . edIssuer)
        liftIO $ midp `shouldBe` Just (idp ^. idpId)

      it "getIdPConfigsByTeam works" $ do
        teamid <- nextWireId
        idp <- IdPConfig <$> (IdPId <$> liftIO UUID.nextRandom) <*> makeTestIdPMetadata <*> pure teamid
        () <- runSparCass $ Data.storeIdPConfig idp
        idps <- runSparCass $ Data.getIdPConfigsByTeam teamid
        liftIO $ idps `shouldBe` [idp]

      it "deleteIdPConfig works" $ do
        teamid <- nextWireId
        idp <- IdPConfig <$> (IdPId <$> liftIO UUID.nextRandom) <*> makeTestIdPMetadata <*> pure teamid
        () <- runSparCass $ Data.storeIdPConfig idp
        do
          midp <- runSparCass $ Data.getIdPConfig (idp ^. idpId)
          liftIO $ midp `shouldBe` Just idp

        () <- runSparCass $ Data.deleteIdPConfig (idp ^. idpId) (idp ^. idpMetadata . edIssuer) teamid
        do
          midp <- runSparCass $ Data.getIdPConfig (idp ^. idpId)
          liftIO $ midp `shouldBe` Nothing
        do
          midp <- runSparCass $ Data.getIdPConfigByIssuer (idp ^. idpMetadata . edIssuer)
          liftIO $ midp `shouldBe` Nothing
        do
          midp <- runSparCass $ Data.getIdPIdByIssuer (idp ^. idpMetadata . edIssuer)
          liftIO $ midp `shouldBe` Nothing
        do
          idps <- runSparCass $ Data.getIdPConfigsByTeam teamid
          liftIO $ idps `shouldBe` []


testSPStoreID
  :: forall m a. (m ~ ReaderT Data.Env (ExceptT TTLError Client), Typeable a)
  => (SAML.ID a -> SAML.Time -> m ())
  -> (SAML.ID a -> m ())
  -> (SAML.ID a -> m Bool)
  -> SpecWith TestEnv
testSPStoreID store unstore isalive = do
  describe ("SPStoreID @" <> show (typeOf (undefined :: a))) $ do
    context "within TTL" $ do
      it "isAliveID is True" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time      <- addTime 5 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` True

    context "after TTL" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time      <- addTime 2 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        liftIO $ threadDelay 3000000
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` False

    context "after call to unstore" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time      <- addTime 5 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        () <- runSparCassWithEnv $ unstore xid
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` False
