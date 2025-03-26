{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Spar.DataSpec
  ( spec,
  )
where

import Cassandra
import Control.Lens
import Imports
import SAML2.WebSSO as SAML
import Spar.App as App
import Spar.Error (IdpDbError (IdpNotFound), SparCustomError (IdpDbError))
import Spar.Intra.BrigApp (veidFromUserSSOId)
import Spar.Options
import qualified Spar.Sem.AReqIDStore as AReqIDStore
import qualified Spar.Sem.AssIDStore as AssIDStore
import qualified Spar.Sem.IdPConfigStore as IdPEffect
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import qualified Spar.Sem.VerdictFormatStore as VerdictFormatStore
import Type.Reflection (typeRep)
import URI.ByteString.QQ (uri)
import Util.Core
import Util.Scim
import Util.Types
import Web.Scim.Schema.Common as Scim.Common
import Web.Scim.Schema.Meta as Scim.Meta
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml

spec :: SpecWith TestEnv
spec = do
  describe "Cql rountrip" $ do
    let check ::
          forall a.
          (Cql a, Typeable a, Show a, Eq a) =>
          a ->
          SpecWith TestEnv
        check x = it (show (typeRep @a)) . liftIO $ do
          (fromCql . toCql) x `shouldBe` Right x
    check (mkXmlText "<>%&'\"")
  -- FUTUREWORK: collect all Cql instance, make them Arbitrary instances, and do this right.

  describe "TTL" $ do
    it "works in seconds" $ do
      env <- ask
      (owner, _teamid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
      ((^. SAML.idpId) -> idpid) <- registerTestIdP owner
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid
      let probe :: (MonadIO m, MonadReader TestEnv m) => m Bool
          probe = runSpar $ isJust <$> AReqIDStore.getIdpIssuer (req ^. SAML.rqID)
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
    describe "SPStoreRequest" $ do
      let idpiss = Issuer [uri|https://idp.io/|]
      context "within TTL" $ do
        it "isAliveID is True" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 5 <$> runSimpleSP getNow
          () <- runSpar $ AReqIDStore.store xid idpiss eol
          isit <- runSpar $ isJust <$> AReqIDStore.getIdpIssuer xid
          liftIO $ isit `shouldBe` True
      context "after TTL" $ do
        it "isAliveID returns False" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 2 <$> runSimpleSP getNow
          () <- runSpar $ AReqIDStore.store xid idpiss eol
          liftIO $ threadDelay 3000000
          isit <- runSpar $ isJust <$> AReqIDStore.getIdpIssuer xid
          liftIO $ isit `shouldBe` False
      context "after call to unstore" $ do
        it "isAliveID returns False" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 5 <$> runSimpleSP getNow
          () <- runSpar $ AReqIDStore.store xid idpiss eol
          () <- runSpar $ AReqIDStore.unStore xid
          isit <- runSpar $ isJust <$> AReqIDStore.getIdpIssuer xid
          liftIO $ isit `shouldBe` False
    describe "SPStoreAssertion" $ do
      context "within TTL" $ do
        it "isAlive is True" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 5 <$> runSimpleSP getNow
          () <- runSpar $ AssIDStore.store xid eol
          isit <- runSpar $ AssIDStore.isAlive xid
          liftIO $ isit `shouldBe` True
      context "after TTL" $ do
        it "isAlive returns False" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 2 <$> runSimpleSP getNow
          () <- runSpar $ AssIDStore.store xid eol
          liftIO $ threadDelay 3000000
          isit <- runSpar $ AssIDStore.isAlive xid
          liftIO $ isit `shouldBe` False
      context "after call to unstore" $ do
        it "isAlive returns False" $ do
          xid :: SAML.ID a <- nextSAMLID
          eol :: Time <- addTime 5 <$> runSimpleSP getNow
          () <- runSpar $ AssIDStore.store xid eol
          () <- runSpar $ AssIDStore.unStore xid
          isit <- runSpar $ AssIDStore.isAlive xid
          liftIO $ isit `shouldBe` False
    describe "VerdictFormat" $ do
      context "insert and get are \"inverses\"" $ do
        let check vf = it (show vf) $ do
              vid <- nextSAMLID
              () <- runSpar $ VerdictFormatStore.store 1 vid vf
              mvf <- runSpar $ VerdictFormatStore.get vid
              liftIO $ mvf `shouldBe` Just vf
        check
          `mapM_` [ VerdictFormatWeb,
                    VerdictFormatMobile [uri|https://fw/ooph|] [uri|https://lu/gn|]
                  ]
      context "has timed out" $ do
        it "VerdictFormatStore.get returns Nothing" $ do
          vid <- nextSAMLID
          () <- runSpar $ VerdictFormatStore.store 1 vid VerdictFormatWeb
          liftIO $ threadDelay 2000000
          mvf <- runSpar $ VerdictFormatStore.get vid
          liftIO $ mvf `shouldBe` Nothing
      context "does not exist" $ do
        it "VerdictFormatStore.get returns Nothing" $ do
          vid <- nextSAMLID
          mvf <- runSpar $ VerdictFormatStore.get vid
          liftIO $ mvf `shouldBe` Nothing
    describe "User" $ do
      context "user is new" $ do
        it "getUser returns Nothing" $ do
          uref <- nextUserRef
          muid <- runSpar $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Nothing
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          () <- runSpar $ SAMLUserStore.insert uref uid
          muid <- runSpar $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Just uid
      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          uid' <- nextWireId
          () <- runSpar $ SAMLUserStore.insert uref uid
          () <- runSpar $ SAMLUserStore.insert uref uid'
          muid <- runSpar $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Just uid'
      describe "DELETE" $ do
        it "works" $ do
          uref <- nextUserRef
          uid <- nextWireId
          do
            () <- runSpar $ SAMLUserStore.insert uref uid
            muid <- runSpar $ SAMLUserStore.get uref
            liftIO $ muid `shouldBe` Just uid
          do
            () <- runSpar $ SAMLUserStore.delete uid uref
            muid <- runSpar (SAMLUserStore.get uref) `aFewTimes` isNothing
            liftIO $ muid `shouldBe` Nothing
    describe "Team" $ do
      testDeleteTeam
    describe "IdPConfig" $ do
      it "insertIdPConfig, getIdPConfig are \"inverses\"" $ do
        idp <- makeTestIdP
        () <- runSpar $ IdPEffect.insertConfig idp
        midp <- runSpar $ IdPEffect.getConfig (idp ^. idpId)
        liftIO $ midp `shouldBe` idp
      it "getIdPByIssuer works" $ do
        idp <- makeTestIdP
        () <- runSpar $ IdPEffect.insertConfig idp
        midp <- getIdPByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . team)
        liftIO $ midp `shouldBe` Just idp
      it "getIdPConfigsByTeam works" $ do
        skipIdPAPIVersions [WireIdPAPIV1]
        teamid <- nextWireId
        idp <- makeTestIdP <&> idpExtraInfo .~ WireIdP teamid Nothing [] Nothing (IdPHandle "IdP 1")
        () <- runSpar $ IdPEffect.insertConfig idp
        idps <- runSpar $ IdPEffect.getConfigsByTeam teamid
        liftIO $ idps `shouldBe` [idp]
      it "deleteIdPConfig works" $ do
        teamid <- nextWireId
        idpApiVersion <- asks (^. teWireIdPAPIVersion)
        idp <- makeTestIdP <&> idpExtraInfo .~ WireIdP teamid (Just idpApiVersion) [] Nothing (IdPHandle "IdP 1")
        () <- runSpar $ IdPEffect.insertConfig idp
        do
          midp <- runSpar $ IdPEffect.getConfig (idp ^. idpId)
          liftIO $ midp `shouldBe` idp
        () <- runSpar $ IdPEffect.deleteConfig idp
        do
          idpOrError <- runSparE $ IdPEffect.getConfig (idp ^. idpId)
          liftIO $ idpOrError `shouldBe` Left (SAML.CustomError $ IdpDbError IdpNotFound)
        do
          midp <- getIdPByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . team)
          liftIO $ midp `shouldBe` Nothing
        do
          midp <- getIdPByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . team)
          liftIO $ midp `shouldBe` Nothing
        do
          idps <- runSpar $ IdPEffect.getConfigsByTeam teamid
          liftIO $ idps `shouldBe` []

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
  runSpar $ App.deleteTeam tid
  -- See that everything got cleaned up.
  --
  -- The token from 'team_provisioning_by_token':
  do
    tokenInfo <- runSpar $ ScimTokenStore.lookup tok
    liftIO $ tokenInfo `shouldBe` Nothing
  -- The team from 'team_provisioning_by_team':
  do
    tokens <- runSpar $ ScimTokenStore.lookupByTeam tid
    liftIO $ tokens `shouldBe` []
  -- The users from 'user':
  do
    mbUser1 <- case veidFromUserSSOId ssoid1 Nothing of
      Right veid ->
        runSpar $
          runValidScimIdEither
            SAMLUserStore.get
            undefined -- could be @Data.lookupScimExternalId@, but we don't hit that path.
            veid
      Left _email -> undefined -- runSparCass . Data.lookupScimExternalId . fromEmail $ _email
    liftIO $ mbUser1 `shouldBe` Nothing
  do
    mbUser2 <- case veidFromUserSSOId ssoid2 Nothing of
      Right veid ->
        runSpar $
          runValidScimIdEither
            SAMLUserStore.get
            undefined
            veid
      Left _email -> undefined
    liftIO $ mbUser2 `shouldBe` Nothing
  -- The config from 'idp':
  do
    idpOrError <- runSparE $ IdPEffect.getConfig (idp ^. SAML.idpId)
    liftIO $ idpOrError `shouldBe` Left (SAML.CustomError $ IdpDbError IdpNotFound)
  -- The config from 'issuer_idp':
  do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    mbIdp <- getIdPByIssuer issuer (idp ^. SAML.idpExtraInfo . team)
    liftIO $ mbIdp `shouldBe` Nothing
  -- The config from 'team_idp':
  do
    idps <- runSpar $ IdPEffect.getConfigsByTeam tid
    liftIO $ idps `shouldBe` []
