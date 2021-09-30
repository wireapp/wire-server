{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Spar.DataSpec
  ( spec,
  )
where

import Cassandra
import Control.Lens
import Control.Monad.Except
import Data.Kind (Type)
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import Polysemy
import SAML2.WebSSO as SAML
import Spar.App as App
import Spar.Data as Data
import Spar.Intra.BrigApp (veidFromUserSSOId)
import qualified Spar.Sem.AReqIDStore as AReqIDStore
import qualified Spar.Sem.AssIDStore as AssIDStore
import qualified Spar.Sem.BindCookieStore as BindCookieStore
import qualified Spar.Sem.IdP as IdPEffect
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import qualified Spar.Sem.ScimTokenStore as ScimTokenStore
import Type.Reflection (typeRep)
import URI.ByteString.QQ (uri)
import Util.Core
import Util.Scim
import Util.Types
import Web.Scim.Schema.Common as Scim.Common
import Web.Scim.Schema.Meta as Scim.Meta
import Wire.API.Cookie
import Wire.API.User.IdentityProvider
import Wire.API.User.Saml
import Wire.API.User.Scim

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
      (_, _, (^. SAML.idpId) -> idpid) <- registerTestIdP
      (_, req) <- call $ callAuthnReq (env ^. teSpar) idpid
      let probe :: (MonadIO m, MonadReader TestEnv m) => m Bool
          probe = runSpar $ liftSem $ AReqIDStore.isAlive (req ^. SAML.rqID)
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
      testSPStoreID AReqIDStore.store AReqIDStore.unStore AReqIDStore.isAlive
    describe "Assertion" $ do
      testSPStoreID AssIDStore.store AssIDStore.unStore AssIDStore.isAlive
    describe "VerdictFormat" $ do
      context "insert and get are \"inverses\"" $ do
        let check vf = it (show vf) $ do
              vid <- nextSAMLID
              () <- runSpar $ liftSem $ AReqIDStore.storeVerdictFormat 1 vid vf
              mvf <- runSpar $ liftSem $ AReqIDStore.getVerdictFormat vid
              liftIO $ mvf `shouldBe` Just vf
        check
          `mapM_` [ VerdictFormatWeb,
                    VerdictFormatMobile [uri|https://fw/ooph|] [uri|https://lu/gn|]
                  ]
      context "has timed out" $ do
        it "AReqIDStore.getVerdictFormat returns Nothing" $ do
          vid <- nextSAMLID
          () <- runSpar $ liftSem $ AReqIDStore.storeVerdictFormat 1 vid VerdictFormatWeb
          liftIO $ threadDelay 2000000
          mvf <- runSpar $ liftSem $ AReqIDStore.getVerdictFormat vid
          liftIO $ mvf `shouldBe` Nothing
      context "does not exist" $ do
        it "AReqIDStore.getVerdictFormat returns Nothing" $ do
          vid <- nextSAMLID
          mvf <- runSpar $ liftSem $ AReqIDStore.getVerdictFormat vid
          liftIO $ mvf `shouldBe` Nothing
    describe "User" $ do
      context "user is new" $ do
        it "getUser returns Nothing" $ do
          uref <- nextUserRef
          muid <- runSpar $ liftSem $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Nothing
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          () <- runSpar $ liftSem $ SAMLUserStore.insert uref uid
          muid <- runSpar $ liftSem $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Just uid
      context "user already exists (idempotency)" $ do
        it "inserts new user and responds with 201 / returns new user" $ do
          uref <- nextUserRef
          uid <- nextWireId
          uid' <- nextWireId
          () <- runSpar $ liftSem $ SAMLUserStore.insert uref uid
          () <- runSpar $ liftSem $ SAMLUserStore.insert uref uid'
          muid <- runSpar $ liftSem $ SAMLUserStore.get uref
          liftIO $ muid `shouldBe` Just uid'
      describe "DELETE" $ do
        it "works" $ do
          uref <- nextUserRef
          uid <- nextWireId
          do
            () <- runSpar $ liftSem $ SAMLUserStore.insert uref uid
            muid <- runSpar $ liftSem (SAMLUserStore.get uref)
            liftIO $ muid `shouldBe` Just uid
          do
            () <- runSpar $ liftSem $ SAMLUserStore.delete uid uref
            muid <- runSpar (liftSem $ SAMLUserStore.get uref) `aFewTimes` isNothing
            liftIO $ muid `shouldBe` Nothing
    describe "BindCookie" $ do
      let mkcky :: TestSpar SetBindCookie
          mkcky = fmap SetBindCookie . runSimpleSP . SAML.toggleCookie "/" . Just . (,1) . UUID.toText =<< liftIO UUID.nextRandom
      it "insert and get are \"inverses\"" $ do
        uid <- nextWireId
        cky <- mkcky
        () <- runSpar $ liftSem $ BindCookieStore.insert cky uid 1
        muid <- runSpar $ liftSem $ BindCookieStore.lookup (setBindCookieValue cky)
        liftIO $ muid `shouldBe` Just uid
      context "has timed out" $ do
        it "BindCookieStore.lookup returns Nothing" $ do
          uid <- nextWireId
          cky <- mkcky
          () <- runSpar $ liftSem $ BindCookieStore.insert cky uid 1
          liftIO $ threadDelay 2000000
          muid <- runSpar $ liftSem $ BindCookieStore.lookup (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing
      context "does not exist" $ do
        it "BindCookieStore.lookup returns Nothing" $ do
          cky <- mkcky
          muid <- runSpar $ liftSem $ BindCookieStore.lookup (setBindCookieValue cky)
          liftIO $ muid `shouldBe` Nothing
    describe "Team" $ do
      testDeleteTeam
    describe "IdPConfig" $ do
      it "storeIdPConfig, getIdPConfig are \"inverses\"" $ do
        idp <- makeTestIdP
        () <- runSpar $ liftSem $ IdPEffect.storeConfig idp
        midp <- runSpar $ liftSem $ IdPEffect.getConfig (idp ^. idpId)
        liftIO $ midp `shouldBe` Just idp
      it "getIdPConfigByIssuer works" $ do
        idp <- makeTestIdP
        () <- runSpar $ liftSem $ IdPEffect.storeConfig idp
        midp <- runSpar $ App.getIdPConfigByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . wiTeam)
        liftIO $ midp `shouldBe` GetIdPFound idp
      it "getIdPIdByIssuer works" $ do
        idp <- makeTestIdP
        () <- runSpar $ liftSem $ IdPEffect.storeConfig idp
        midp <- runSpar $ App.getIdPIdByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . wiTeam)
        liftIO $ midp `shouldBe` GetIdPFound (idp ^. idpId)
      it "getIdPConfigsByTeam works" $ do
        skipIdPAPIVersions [WireIdPAPIV1]
        teamid <- nextWireId
        idp <- makeTestIdP <&> idpExtraInfo .~ (WireIdP teamid Nothing [] Nothing)
        () <- runSpar $ liftSem $ IdPEffect.storeConfig idp
        idps <- runSpar $ liftSem $ IdPEffect.getConfigsByTeam teamid
        liftIO $ idps `shouldBe` [idp]
      it "deleteIdPConfig works" $ do
        teamid <- nextWireId
        idpApiVersion <- asks (^. teWireIdPAPIVersion)
        idp <- makeTestIdP <&> idpExtraInfo .~ (WireIdP teamid (Just idpApiVersion) [] Nothing)
        () <- runSpar $ liftSem $ IdPEffect.storeConfig idp
        do
          midp <- runSpar $ liftSem $ IdPEffect.getConfig (idp ^. idpId)
          liftIO $ midp `shouldBe` Just idp
        () <- runSpar $ liftSem $ IdPEffect.deleteConfig (idp ^. idpId) (idp ^. idpMetadata . edIssuer) teamid
        do
          midp <- runSpar $ liftSem $ IdPEffect.getConfig (idp ^. idpId)
          liftIO $ midp `shouldBe` Nothing
        do
          midp <- runSpar $ App.getIdPConfigByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . wiTeam)
          liftIO $ midp `shouldBe` GetIdPNotFound
        do
          midp <- runSpar $ App.getIdPIdByIssuer (idp ^. idpMetadata . edIssuer) (idp ^. SAML.idpExtraInfo . wiTeam)
          liftIO $ midp `shouldBe` GetIdPNotFound
        do
          idps <- runSpar $ liftSem $ IdPEffect.getConfigsByTeam teamid
          liftIO $ idps `shouldBe` []
      describe "{set,clear}ReplacedBy" $ do
        it "handle non-existent idps gradefully" $ do
          pendingWith "this requires a cql{,-io} upgrade.  https://gitlab.com/twittner/cql-io/-/issues/7"
          idp1 <- makeTestIdP
          idp2 <- makeTestIdP
          runSpar $ liftSem $ IdPEffect.setReplacedBy (Data.Replaced (idp1 ^. idpId)) (Data.Replacing (idp2 ^. idpId))
          idp1' <- runSpar $ liftSem (IdPEffect.getConfig (idp1 ^. idpId))
          liftIO $ idp1' `shouldBe` Nothing
          runSpar $ liftSem $ IdPEffect.clearReplacedBy (Data.Replaced (idp1 ^. idpId))
          idp2' <- runSpar $ liftSem (IdPEffect.getConfig (idp1 ^. idpId))
          liftIO $ idp2' `shouldBe` Nothing

-- TODO(sandy): This function should be more polymorphic over it's polysemy
-- constraints than using 'RealInterpretation' in full anger.
testSPStoreID ::
  forall (a :: Type).
  (Typeable a) =>
  (SAML.ID a -> SAML.Time -> Sem CanonicalEffs ()) ->
  (SAML.ID a -> Sem CanonicalEffs ()) ->
  (SAML.ID a -> Sem CanonicalEffs Bool) ->
  SpecWith TestEnv
testSPStoreID store unstore isalive = do
  describe ("SPStoreID @" <> show (typeRep @a)) $ do
    context "within TTL" $ do
      it "isAliveID is True" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 5 <$> runSimpleSP getNow
        () <- runSpar $ liftSem $ store xid eol
        isit <- runSpar $ liftSem $ isalive xid
        liftIO $ isit `shouldBe` True
    context "after TTL" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 2 <$> runSimpleSP getNow
        () <- runSpar $ liftSem $ store xid eol
        liftIO $ threadDelay 3000000
        isit <- runSpar $ liftSem $ isalive xid
        liftIO $ isit `shouldBe` False
    context "after call to unstore" $ do
      it "isAliveID returns False" $ do
        xid :: SAML.ID a <- nextSAMLID
        eol :: Time <- addTime 5 <$> runSimpleSP getNow
        () <- runSpar $ liftSem $ store xid eol
        () <- runSpar $ liftSem $ unstore xid
        isit <- runSpar $ liftSem $ isalive xid
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
  runSpar $ App.deleteTeam tid
  -- See that everything got cleaned up.
  --
  -- The token from 'team_provisioning_by_token':
  do
    tokenInfo <- runSpar $ liftSem $ ScimTokenStore.lookup tok
    liftIO $ tokenInfo `shouldBe` Nothing
  -- The team from 'team_provisioning_by_team':
  do
    tokens <- runSpar $ liftSem $ ScimTokenStore.getByTeam tid
    liftIO $ tokens `shouldBe` []
  -- The users from 'user':
  do
    mbUser1 <- case veidFromUserSSOId ssoid1 of
      Right veid ->
        runSpar $
          liftSem $
            runValidExternalId
              SAMLUserStore.get
              undefined -- could be @Data.lookupScimExternalId@, but we don't hit that path.
              veid
      Left _email -> undefined -- runSparCass . Data.lookupScimExternalId . fromEmail $ _email
    liftIO $ mbUser1 `shouldBe` Nothing
  do
    mbUser2 <- case veidFromUserSSOId ssoid2 of
      Right veid ->
        runSpar $
          liftSem $
            runValidExternalId
              SAMLUserStore.get
              undefined
              veid
      Left _email -> undefined
    liftIO $ mbUser2 `shouldBe` Nothing
  -- The config from 'idp':
  do
    mbIdp <- runSpar $ liftSem $ IdPEffect.getConfig (idp ^. SAML.idpId)
    liftIO $ mbIdp `shouldBe` Nothing
  -- The config from 'issuer_idp':
  do
    let issuer = idp ^. SAML.idpMetadata . SAML.edIssuer
    mbIdp <- runSpar $ App.getIdPIdByIssuer issuer (idp ^. SAML.idpExtraInfo . wiTeam)
    liftIO $ mbIdp `shouldBe` GetIdPNotFound
  -- The config from 'team_idp':
  do
    idps <- runSpar $ liftSem $ IdPEffect.getConfigsByTeam tid
    liftIO $ idps `shouldBe` []
