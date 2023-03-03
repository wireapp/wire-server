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

import Control.Monad.Trans.Except
import Data.Jwt.Tools
import Data.String.Conversions (cs)
import Imports
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "generateDpopToken FFI when passing valid inputs" $ do
    it "should return an access token" $ do
      actual <- callFFIWithValidValues
      isRight actual `shouldBe` True
  describe "generateDpopToken FFI when passing nonsense values" $ do
    it "should return an error" $ do
      actual <- callFFIWithNonsenseValues
      isRight actual `shouldBe` False
  describe "toResult" $ do
    it "should convert to correct error" $ do
      toResult Nothing (Just token) `shouldBe` Right (cs token)
      toResult (Just 1) Nothing `shouldBe` Left UnknownError
      toResult (Just 1) (Just token) `shouldBe` Left UnknownError
      toResult (Just 2) Nothing `shouldBe` Left FfiError
      toResult (Just 2) (Just token) `shouldBe` Left FfiError
      toResult (Just 3) Nothing `shouldBe` Left ImplementationError
      toResult (Just 3) (Just token) `shouldBe` Left ImplementationError
      toResult (Just 4) Nothing `shouldBe` Left DpopSyntaxError
      toResult (Just 4) (Just token) `shouldBe` Left DpopSyntaxError
      toResult (Just 5) Nothing `shouldBe` Left DpopTypError
      toResult (Just 5) (Just token) `shouldBe` Left DpopTypError
      toResult (Just 6) Nothing `shouldBe` Left DpopUnsupportedAlgorithmError
      toResult (Just 6) (Just token) `shouldBe` Left DpopUnsupportedAlgorithmError
      toResult (Just 7) Nothing `shouldBe` Left DpopInvalidSignatureError
      toResult (Just 7) (Just token) `shouldBe` Left DpopInvalidSignatureError
      toResult (Just 8) Nothing `shouldBe` Left ClientIdMismatchError
      toResult (Just 8) (Just token) `shouldBe` Left ClientIdMismatchError
      toResult (Just 9) Nothing `shouldBe` Left BackendNonceMismatchError
      toResult (Just 9) (Just token) `shouldBe` Left BackendNonceMismatchError
      toResult (Just 10) Nothing `shouldBe` Left HtuMismatchError
      toResult (Just 10) (Just token) `shouldBe` Left HtuMismatchError
      toResult (Just 11) Nothing `shouldBe` Left HtmMismatchError
      toResult (Just 11) (Just token) `shouldBe` Left HtmMismatchError
      toResult (Just 12) Nothing `shouldBe` Left MissingJtiError
      toResult (Just 12) (Just token) `shouldBe` Left MissingJtiError
      toResult (Just 13) Nothing `shouldBe` Left MissingChallengeError
      toResult (Just 13) (Just token) `shouldBe` Left MissingChallengeError
      toResult (Just 14) Nothing `shouldBe` Left MissingIatError
      toResult (Just 14) (Just token) `shouldBe` Left MissingIatError
      toResult (Just 15) Nothing `shouldBe` Left IatError
      toResult (Just 15) (Just token) `shouldBe` Left IatError
      toResult (Just 16) Nothing `shouldBe` Left MissingExpError
      toResult (Just 16) (Just token) `shouldBe` Left MissingExpError
      toResult (Just 17) Nothing `shouldBe` Left ExpMismatchError
      toResult (Just 17) (Just token) `shouldBe` Left ExpMismatchError
      toResult (Just 18) Nothing `shouldBe` Left ExpError
      toResult (Just 18) (Just token) `shouldBe` Left ExpError
      toResult Nothing Nothing `shouldBe` Left UnknownError
  where
    token :: String
    token = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

callFFIWithNonsenseValues :: IO (Either DPoPTokenGenerationError ByteString)
callFFIWithNonsenseValues =
  runExceptT $ generateDpopToken proof uid cid domain nonce uri method maxSkewSecs expires now pem
  where
    proof = Proof "xxxx.yyyy.zzzz"
    uid = UserId "8a6e8a6e-8a6e-8a6e-8a6e-8a6e8a6e8a6e"
    cid = ClientId 8899
    domain = Domain "example.com"
    nonce = Nonce "123"
    uri = Uri "/foo"
    method = POST
    maxSkewSecs = MaxSkewSecs 1
    now = NowEpoch 5435234232
    expires = ExpiryEpoch $ 5435234232 + 360
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIFANnxZLNE4p+GDzWzR3wm/v8x/0bxZYkCyke1aTRucX\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEACPvhIdimF20tOPjbb+fXJrwS2RKDp7686T90AZ0+Th8=\n\
        \-----END PUBLIC KEY-----\n"

callFFIWithValidValues :: IO (Either DPoPTokenGenerationError ByteString)
callFFIWithValidValues =
  runExceptT $ generateDpopToken proof uid cid domain nonce uri method maxSkewSecs expires now pem
  where
    proof = Proof "eyJhbGciOiJFZERTQSIsInR5cCI6ImRwb3Arand0IiwiandrIjp7Imt0eSI6Ik9LUCIsImNydiI6IkVkMjU1MTkiLCJ4IjoiZzQwakI3V3pmb2ZCdkxCNVlybmlZM2ZPZU1WVGtfNlpfVnNZM0tBbnpOUSJ9fQ.eyJpYXQiOjE2Nzc2NzAwODEsImV4cCI6MTY3Nzc1NjQ4MSwibmJmIjoxNjc3NjcwMDgxLCJzdWIiOiJpbXBwOndpcmVhcHA9WldKa01qY3labUk0TW1aa05ETXlZamczTm1NM1lXSmtZVFUwWkdSaU56VS8xODllNDhjNmNhODZiNWQ0QGV4YW1wbGUub3JnIiwianRpIjoiZDE5ZWExYmItNWI0Ny00ZGJiLWE1MTktNjU0ZWRmMjU0MTQ0Iiwibm9uY2UiOiJZMkZVTjJaTlExUnZSV0l6Ympsa2RGRjFjWGhHZDJKbWFXUlRiamhXZVdRIiwiaHRtIjoiUE9TVCIsImh0dSI6Imh0dHA6Ly9sb2NhbGhvc3Q6NjQwNTQvIiwiY2hhbCI6IkJpMkpkUGk1eWVTTVdhZjA5TnJEZTVUQXFjZ0FnQmE3In0._PrwHUTS7EoAflXyNDlPNqGMbjKu-JuSXwkNPyryBQdg2gDIb20amsH05Ocih78Josz9h7lAB6FvAWsXKQB1Dw"
    uid = UserId "ebd272fb-82fd-432b-876c-7abda54ddb75"
    cid = ClientId 1773935321869104596
    domain = Domain "example.org"
    nonce = Nonce "Y2FUN2ZNQ1RvRWIzbjlkdFF1cXhGd2JmaWRTbjhWeWQ"
    uri = Uri "http://localhost:64054/"
    method = POST
    maxSkewSecs = MaxSkewSecs 2
    now = NowEpoch 5435234232
    expires = ExpiryEpoch $ 2082008461
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIKW3jzXCsRVgnclmiTu53Pu1/r6AUmnKDoghOOVMjozQ\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEA7t9veqi02mPhllm44JXWga8m/l4JxUeQm3qPyMlerxY=\n\
        \-----END PUBLIC KEY-----\n"
