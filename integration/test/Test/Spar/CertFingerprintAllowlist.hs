module Test.Spar.CertFingerprintAllowlist where

import API.GalleyInternal (setTeamFeatureStatus)
import API.Spar (createIdpWithZHostV2, updateIdp)
import Control.Lens ((.~), (^.))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.X509 (SignedCertificate)
import qualified Data.X509.Extended as X509E
import qualified SAML2.WebSSO.Test.Util as SAMLTest
import qualified SAML2.WebSSO.Types as SAMLTypes
import SetupHelpers
import Testlib.Prelude
import qualified Text.XML.DSig as XMLDSig

testAcceptsWhenAllowlistEmpty :: (HasCallStack) => App ()
testAcceptsWhenAllowlistEmpty = do
  SAMLTest.SampleIdP idpmeta _ _ _ <- SAMLTest.makeSampleIdPMetadata
  SAMLTest.SampleIdP idpmeta2 _ _ _ <- SAMLTest.makeSampleIdPMetadata
  withModifiedBackend
    def {sparCfg = withAllowlist []}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      idpId <-
        createIdpWithZHostV2 owner Nothing idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      updateIdp owner idpId idpmeta2 >>= assertStatus 200

testCreateRejectsWhenFingerprintNotListed :: (HasCallStack) => App ()
testCreateRejectsWhenFingerprintNotListed = do
  SAMLTest.SampleIdP idpmeta _ _ _ <- SAMLTest.makeSampleIdPMetadata
  withModifiedBackend
    def {sparCfg = withAllowlist [bogusFingerprint]}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      createIdpWithZHostV2 owner Nothing idpmeta >>= assertLabel 403 "idp-cert-not-allowed"

testAcceptsWhenFingerprintListed :: (HasCallStack) => App ()
testAcceptsWhenFingerprintListed = do
  SAMLTest.SampleIdP meta1 _ _ _ <- SAMLTest.makeSampleIdPMetadata
  SAMLTest.SampleIdP meta2 _ _ _ <- SAMLTest.makeSampleIdPMetadata
  let fpr1 = firstCertFingerprint meta1
      fpr2 = firstCertFingerprint meta2
  withModifiedBackend
    def {sparCfg = withAllowlist [fpr1, fpr2]}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      idpId <-
        createIdpWithZHostV2 owner Nothing meta1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      updateIdp owner idpId meta2 >>= assertStatus 200

testAcceptsMultiCertWhenAllListed :: (HasCallStack) => App ()
testAcceptsMultiCertWhenAllListed = do
  (metaCreate, certsCreate) <- makeIdPMetaWithCerts 2
  (metaUpdate, certsUpdate) <- makeIdPMetaWithCerts 3
  let allCerts = NE.toList certsCreate <> NE.toList certsUpdate
  withModifiedBackend
    def {sparCfg = withAllowlist (fingerprintHex <$> allCerts)}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      idpId <-
        createIdpWithZHostV2 owner Nothing metaCreate `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      updateIdp owner idpId metaUpdate >>= assertStatus 200

testRejectsMultiCertWhenAnyMissing :: (HasCallStack) => App ()
testRejectsMultiCertWhenAnyMissing = do
  (multiMeta, _) <- makeIdPMetaWithCerts 2
  SAMLTest.SampleIdP singleMeta _ _ _ <- SAMLTest.makeSampleIdPMetadata
  withModifiedBackend
    -- Allowlist only the first cert of multiMeta and singleMeta's cert.
    -- The second cert in multiMeta is intentionally not listed.
    def {sparCfg = withAllowlist (fingerprintHex <$> [firstCert multiMeta, firstCert singleMeta])}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      createIdpWithZHostV2 owner Nothing multiMeta >>= assertLabel 403 "idp-cert-not-allowed"
      idpId <-
        createIdpWithZHostV2 owner Nothing singleMeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      updateIdp owner idpId multiMeta >>= assertLabel 403 "idp-cert-not-allowed"

testUpdateRejectsWhenFingerprintNotListed :: (HasCallStack) => App ()
testUpdateRejectsWhenFingerprintNotListed = do
  SAMLTest.SampleIdP meta1 _ _ _ <- SAMLTest.makeSampleIdPMetadata
  SAMLTest.SampleIdP meta2 _ _ _ <- SAMLTest.makeSampleIdPMetadata
  let fpr1 = firstCertFingerprint meta1
      fpr2 = firstCertFingerprint meta2
  fpr1 `shouldNotMatch` fpr2
  withModifiedBackend
    def {sparCfg = withAllowlist [fpr1]}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      idpId <-
        createIdpWithZHostV2 owner Nothing meta1 `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      updateIdp owner idpId meta2 >>= assertLabel 403 "idp-cert-not-allowed"

-- | SAML finalize-login succeeds when the IdP cert is on the allowlist.
-- The negative case (cert removed from allowlist after IdP creation) is
-- covered by unit tests in Test.Spar.Saml.IdPSpec: dynamic backends use
-- isolated Cassandra keyspaces so the two configs cannot share IdP state.
testFinalizeLoginSucceedsWhenCertAllowlisted :: (HasCallStack) => App ()
testFinalizeLoginSucceedsWhenCertAllowlisted = do
  SAMLTest.SampleIdP idpmeta privCreds _ _ <- SAMLTest.makeSampleIdPMetadata
  let fpr = firstCertFingerprint idpmeta
  withModifiedBackend
    def {sparCfg = withAllowlist [fpr]}
    $ \domain -> do
      (owner, tid, _) <- createTeam domain 1
      void $ setTeamFeatureStatus owner tid "sso" "enabled"
      idpId <-
        createIdpWithZHostV2 owner Nothing idpmeta `bindResponse` \resp -> do
          resp.status `shouldMatchInt` 201
          resp.json %. "id" >>= asString
      subject <- nextSubject
      (mUid, _) <- loginWithSamlWithZHost Nothing domain True tid subject (idpId, (idpmeta, privCreds))
      void $ assertJust "expected user id in SAML login response" mUid

-- | 20 zero bytes — valid hex, no real cert matches.
bogusFingerprint :: String
bogusFingerprint = "0000000000000000000000000000000000000000"

-- | First cert in the descriptor's @AuthnResponse@ cert list.
firstCert :: SAMLTypes.IdPMetadata -> SignedCertificate
firstCert meta = NE.head $ meta ^. SAMLTypes.edCertAuthnResponse

-- | Cert's SHA-1 fingerprint in canonical @AA:BB:..@ hex form.
fingerprintHex :: SignedCertificate -> String
fingerprintHex = T.unpack . X509E.renderFingerprintHex . X509E.certSha1Fingerprint

-- | First cert's SHA-1, canonical @AA:BB:..@ form.
firstCertFingerprint :: SAMLTypes.IdPMetadata -> String
firstCertFingerprint = fingerprintHex . firstCert

-- | Sample IdP metadata with @n@ distinct certs in its @AuthnResponse@ cert
-- list (@n >= 1@). Returns the metadata and the certs (in the same order as
-- they appear in the metadata).
makeIdPMetaWithCerts :: (HasCallStack) => Int -> App (SAMLTypes.IdPMetadata, NonEmpty SignedCertificate)
makeIdPMetaWithCerts n = do
  SAMLTest.SampleIdP meta _ _ _ <- SAMLTest.makeSampleIdPMetadata
  extra <- liftIO $ replicateM (n - 1) genCert
  let certs = NE.head (meta ^. SAMLTypes.edCertAuthnResponse) :| extra
  pure (meta & SAMLTypes.edCertAuthnResponse .~ certs, certs)
  where
    genCert = (\(_, _, c) -> c) <$> XMLDSig.mkSignCredsWithCert Nothing 96

-- | Patch sparCfg to set the allowlist to the given hex strings.
withAllowlist :: [String] -> Value -> App Value
withAllowlist xs = setField "idpCertFingerprintAllowlist" (toJSON xs)
