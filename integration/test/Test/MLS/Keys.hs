module Test.MLS.Keys where

import API.Galley
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8
import SetupHelpers
import Testlib.Prelude

testPublicKeys :: (HasCallStack) => App ()
testPublicKeys = do
  u <- randomUserId OwnDomain
  keys <- getMLSPublicKeys u >>= getJSON 200

  do
    keys %. "removal.ed25519.crv" `shouldMatch` "Ed25519"
    keys %. "removal.ed25519.kty" `shouldMatch` "OKP"
    pubkeyS <- asString $ keys %. "removal.ed25519.x"
    pubkey <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyS
    B8.length pubkey `shouldMatchInt` 32

  do
    keys %. "removal.ecdsa_secp256r1_sha256.crv" `shouldMatch` "P-256"
    keys %. "removal.ecdsa_secp256r1_sha256.kty" `shouldMatch` "EC"
    pubkeyXS <- asString $ keys %. "removal.ecdsa_secp256r1_sha256.x"
    pubkeyX <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyXS
    B8.length pubkeyX `shouldMatchInt` 32
    pubkeyYS <- asString $ keys %. "removal.ecdsa_secp256r1_sha256.y"
    pubkeyY <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyYS
    B8.length pubkeyY `shouldMatchInt` 32

  do
    keys %. "removal.ecdsa_secp384r1_sha384.crv" `shouldMatch` "P-384"
    keys %. "removal.ecdsa_secp384r1_sha384.kty" `shouldMatch` "EC"
    pubkeyXS <- asString $ keys %. "removal.ecdsa_secp384r1_sha384.x"
    pubkeyX <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyXS
    B8.length pubkeyX `shouldMatchInt` 48
    pubkeyYS <- asString $ keys %. "removal.ecdsa_secp384r1_sha384.y"
    pubkeyY <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyYS
    B8.length pubkeyY `shouldMatchInt` 48

  do
    keys %. "removal.ecdsa_secp521r1_sha512.crv" `shouldMatch` "P-521"
    keys %. "removal.ecdsa_secp521r1_sha512.kty" `shouldMatch` "EC"
    pubkeyXS <- asString $ keys %. "removal.ecdsa_secp521r1_sha512.x"
    pubkeyX <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyXS
    B8.length pubkeyX `shouldMatchInt` 66
    pubkeyYS <- asString $ keys %. "removal.ecdsa_secp521r1_sha512.y"
    pubkeyY <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyYS
    B8.length pubkeyY `shouldMatchInt` 66

testPublicKeysMLSNotEnabled :: (HasCallStack) => App ()
testPublicKeysMLSNotEnabled = withModifiedBackend
  def
    { galleyCfg = removeField "settings.mlsPrivateKeyPaths"
    }
  $ \domain -> do
    alice <- randomUserId domain
    bindResponse (getMLSPublicKeys alice) $ \resp -> do
      resp.status `shouldMatchInt` 400
      resp.json %. "label" `shouldMatch` "mls-not-enabled"
