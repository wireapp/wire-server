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
      actual <- runExceptT $ generateDpopToken proof uid cid domain nonce uri method maxSkewSecs expires now pem
      isRight actual `shouldBe` True
  describe "generateDpopToken FFI when passing a wrong nonce value" $ do
    it "should return BackendNonceMismatchError" $ do
      actual <- runExceptT $ generateDpopToken proof uid cid domain (Nonce "foobar") uri method maxSkewSecs expires now pem
      actual `shouldBe` Left BackendNonceMismatchError
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
    token = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
    proof = Proof "eyJhbGciOiJFZERTQSIsInR5cCI6ImRwb3Arand0IiwiandrIjp7Imt0eSI6Ik9LUCIsImNydiI6IkVkMjU1MTkiLCJ4IjoiZ0tYSHpIV3QtRUh1N2ZQbmlWMXFXWGV2Rmk1eFNKd3RNcHJlSjBjdTZ3SSJ9fQ.eyJpYXQiOjE2NzgxMDcwMDksImV4cCI6MjA4ODA3NTAwOSwibmJmIjoxNjc4MTA3MDA5LCJzdWIiOiJpbXBwOndpcmVhcHA9WXpWbE1qRTVNelpqTTJKak5EQXdOMkpsWTJJd1lXTm1OVGszTW1FMVlqTS9lYWZhMDI1NzMwM2Q0MDYwQHdpcmUuY29tIiwianRpIjoiMmQzNzAzYTItNTc4Yi00MmRjLWE2MGUtYmM0NzA3OWVkODk5Iiwibm9uY2UiOiJRV1J4T1VaUVpYVnNTMlJZYjBGS05sWkhXbGgwYUV4amJUUmpTM2M1U2xnIiwiaHRtIjoiUE9TVCIsImh0dSI6Imh0dHBzOi8vd2lyZS5leGFtcGxlLmNvbS9jbGllbnRzLzE2OTMxODQ4MzIyNTQ3NTMxODcyL2FjY2Vzcy10b2tlbiIsImNoYWwiOiJZVE5HTkRSNlRqZHFabGRRZUVGYWVrMTZWMmhqYXpCVmJ6UlFWVXRWUlZJIn0.0J2sx5y0ubZ4NwmQhbKXDj6i5UWTx3cvuTPKbeXXOJFDamr-iFtE6sOnAQT90kfTx1cEoIyDfoUkj3h5GEanAA"
    uid = UserId "c5e21936-c3bc-4007-becb-0acf5972a5b3"
    cid = ClientId 16931848322547531872
    domain = Domain "wire.com"
    nonce = Nonce "QWRxOUZQZXVsS2RYb0FKNlZHWlh0aExjbTRjS3c5Slg"
    uri = Uri "https://wire.example.com/clients/16931848322547531872/access-token"
    method = POST
    maxSkewSecs = MaxSkewSecs 5
    now = NowEpoch 5435234232
    expires = ExpiryEpoch $ 2136351646
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIMROyHqEinw8EvFSNXp0X0suu6gMQvd9i/l9v9R9UnhH\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEA5pDR/Yo4pkKUIxIody2fEQ56eIOW7UqeDeF7FG7WudA=\n\
        \-----END PUBLIC KEY-----\n"
