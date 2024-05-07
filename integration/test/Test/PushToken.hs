module Test.PushToken where

import API.Common
import API.Gundeck
import qualified Data.ByteString.Base16 as Base16
import Data.Text.Encoding (decodeUtf8)
import SetupHelpers
import Testlib.Prelude

testRegisterPushToken :: App ()
testRegisterPushToken = do
  alice <- randomUser OwnDomain def
  aliceC1 <- randomClientId
  -- aliceC2 <- randomClientId

  apnsC1Token <- decodeUtf8 . Base16.encode <$> randomBytes 32
  let apnsC1 =
        object
          [ "transport" .= "APNS_SANDBOX",
            "app" .= "test",
            "token" .= apnsC1Token,
            "client" .= aliceC1
          ]
  bindResponse (postPushToken alice apnsC1) \resp -> do
    resp.status `shouldMatchInt` 201
