-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

{-# LANGUAGE RecordWildCards #-}

module Test.Spar.DataSpec
  ( spec,
  )
where

import Cassandra
import Control.Lens
import Control.Monad.Except
import Data.Kind (Type)
import Data.Typeable
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import SAML2.WebSSO as SAML
import Spar.Data as Data
import Spar.Intra.Brig (fromUserSSOId)
import Spar.Types
import URI.ByteString.QQ (uri)
import Util.Core
import Util.Scim
import Util.Types
import Web.Scim.Schema.Common as Scim.Common
import Web.Scim.Schema.Meta as Scim.Meta

spec :: SpecWith TestEnv
spec = do
  describe "Cql rountrip" $ do
    let check ::
          forall a.
          (Cql a, Typeable a, Show a, Eq a) =>
          a ->
          SpecWith TestEnv
        check x = it (show . typeRep $ (Proxy @a)) . liftIO $ do
          (fromCql . toCql) x `shouldBe` Right x
    check (mkXmlText "<>%&'\"")
  -- FUTUREWORK: collect all Cql instance, make them Arbitrary instances, and do this right.

  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (_, _, (^. SAML.idpId) -> idpid) <- registerTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid
      let probe :: (MonadIO m, MonadReader TestEnv m) => m Bool
          probe = runSparCass $ isAliveAReqID (req ^. SAML.rqID)
          maxttl :: Int -- musec
          maxttl = (fromIntegral . fromTTL $ env ^. teOpts . to maxttlAuthreq) * 1000 * 1000
      liftIO $ maxttl `shouldSatisfy` (< 60 * 1000 * 1000) -- otherwise the test will be really slow.
      p1 <- probe
      liftIO $ p1 `shouldBe` True
      liftIO $ threadDelay (maxttl `div` 2)
      p2 <- probe
      liftIO $ p2 `shouldBe` True -- 0.5 lifetimes after birth
      liftIO $ threadDelay maxttl
      p3 <- probe
      liftIO $ p3 `shouldBe` False -- 1.5 lifetimes after birth
  describe "cql binding" $ do
    describe "AuthnRequest" $ do
      testSPStoreID storeAReqID unStoreAReqID isAliveAReqID
    describe "Assertion" $ do
      testSPStoreID storeAssID unStoreAssID isAliveAssID
    describe "VerdictFormat" $ do
      context "insert and get are \"inverses\"" $ do
        let check vf = it (show vf) $ do
              vid <- nextSAMLID
              () <- runSparCass $ storeVerdictFormat 1 vid vf
              mvf <- runSparCass $ getVerdictFormat vid
              liftIO $ mvf `shouldBe` Just vf
        check
          `mapM_` [ VerdictFormatWeb,
                    VerdictFormatMobile [uri|https://fw/ooph|] [uri|https://lu/gn|]
                  ]
      context "has timed out" $ do
        it "getVerdictFormat returns Nothing" $ do
          vid <- nextSAMLID
          () <- runSparCass $ storeVerdictFormat 1 vid VerdictFormatWeb
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
          muid <- runSparCass $ Data.getSAMLUser uref
          liftIO $ muid `shouldBe` Nothing
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          () <- runSparCass $ Data.insertSAMLUser uref uid
          muid <- runSparCass $ Data.getSAMLUser uref
          liftIO $ muid `shouldBe` Just uid
      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          uid' <- nextWireId
          () <- runSparCass $ Data.insertSAMLUser uref uid
          () <- runSparCass $ Data.insertSAMLUser uref uid'
          muid <- runSparCass $ Data.getSAMLUser uref
          liftIO $ muid `shouldBe` Just uid'
      describe "DELETE" $ do
        it "works" $ do
          uref <- nextUserRef
          uid <- nextWireId
          do
            () <- runSparCass $ Data.insertSAMLUser uref uid
            muid <- runSparCass (Data.getSAMLUser uref)
            liftIO $ muid `shouldBe` Just uid
          do
            () <- runSparCass $ Data.deleteSAMLUser uref
            muid <- runSparCass (Data.getSAMLUser uref) `aFewTimes` isNothing
            liftIO $ muid `shouldBe` Nothing
    describe "BindCookie" $ do
      let mkcky :: TestSpar SetBindCookie
          mkcky = runSimpleSP . SAML.toggleCookie "/" . Just . (,1) . UUID.toText =<< liftIO UUID.nextRandom
      it "insert and get are \"inverses\"" $ do
        uid <- nextWireId
        cky <- mkcky
        () <- runSparCassWithEnv $ insertBindCookie cky uid 1
        muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
        liftIO $ muid `shouldBe` Just uid
      context "has timed out" $ do
        it "lookupBindCookie returns Nothing" $ do
          uid <- nextWireId
          cky <- mkcky
          () <- runSparCassWithEnv $ insertBindCookie cky uid 1
          liftIO $ threadDelay 2000000
          muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing
      context "does not exist" $ do
        it "lookupBindCookie returns Nothing" $ do
          cky <- mkcky
          muid <- runSparCass $ lookupBindCookie (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing
    describe "Team" $ do
      testDeleteTeam
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

testSPStoreID ::
  forall m (a :: Type).
  (m ~ ReaderT Data.Env (ExceptT TTLError Client), Typeable a) =>
  (SAML.ID a -> SAML.Time -> m ()) ->
  (SAML.ID a -> m ()) ->
  (SAML.ID a -> m Bool) ->
  SpecWith TestEnv
testSPStoreID store unstore isalive = do
  describe ("SPStoreID @" <> show (typeOf (undefined :: a))) $ do
    context "within TTL" $ do
      it "isAliveID is True" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 5 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` True
    context "after TTL" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 2 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        liftIO $ threadDelay 3000000
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` False
    context "after call to unstore" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 5 <$> runSimpleSP getNow
        () <- runSparCassWithEnv $ store xid eol
        () <- runSparCassWithEnv $ unstore xid
        isit <- runSparCassWithEnv $ isalive xid
        liftIO $ isit `shouldBe` False

-- | Test that when a team is deleted, all relevant data is pruned from the
-- Spar database.
testDeleteTeam :: SpecWith TestEnv
testDeleteTeam = it "cleans up all the right tables after deletion" $ do
  -- Create a team with two users and a SCIM token
  (tok, (_uid, tid, idp)) <- registerIdPAndScimToken
  user1 <- randomScimUser
  user2 <- randomScimUser
  storedUser1 <- createUser tok user1
  storedUser2 <- createUser tok user2
  -- Resolve the users' SSO ids
  let getUid = Scim.Common.id . Scim.Meta.thing
  ssoid1 <- getSsoidViaSelf (getUid storedUser1)
  ssoid2 <- getSsoidViaSelf (getUid storedUser2)
  -- Delete the team
  runSparCass $ Data.deleteTeam tid
  -- See that everything got cleaned up.
  --
  -- The token from 'team_provisioning_by_token':
  do
    tokenInfo <- runSparCass $ Data.lookupScimToken tok
    liftIO $ tokenInfo `shouldBe` Nothing
  -- The team from 'team_provisioning_by_team':
  do
    tokens <- runSparCass $ Data.getScimTokens tid
    liftIO $ tokens `shouldBe` []
  -- The users from 'user':
  do
    let Right uref1 = fromUserSSOId ssoid1
    mbUser1 <- runSparCass $ Data.getSAMLUser uref1
    liftIO $ mbUser1 `shouldBe` Nothing
  do
    let Right uref2 = fromUserSSOId ssoid2
    mbUser2 <- runSparCass $ Data.getSAMLUser uref2
    liftIO $ mbUser2 `shouldBe` Nothing
  -- The config from 'idp':
  do
    mbIdp <- runSparCass $ Data.getIdPConfig (idp ^. SAML.idpId)
    liftIO $ mbIdp `shouldBe` Nothing
  -- The config from 'issuer_idp':
  do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    mbIdp <- runSparCass $ Data.getIdPIdByIssuer issuer
    liftIO $ mbIdp `shouldBe` Nothing
  -- The config from 'team_idp':
  do
    idps <- runSparCass $ Data.getIdPConfigsByTeam tid
    liftIO $ idps `shouldBe` []
