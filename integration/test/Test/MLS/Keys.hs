module Test.MLS.Keys where

import API.Galley
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as B8
import SetupHelpers
import Testlib.Prelude

testPublicKeys :: App ()
testPublicKeys = do
  u <- randomUserId OwnDomain
  keys <- getMLSPublicKeys u >>= getJSON 200

  keys %. "removal.keys.0.crv" `shouldMatch` "Ed25519"
  keys %. "removal.keys.0.kty" `shouldMatch` "OKP"
  pubkeyS <- asString $ keys %. "removal.keys.0.x"
  pubkey <- assertOne . toList . B64U.decodeUnpadded $ B8.pack pubkeyS
  B8.length pubkey `shouldMatchInt` 32
