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
      print actual
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
    token = ""
    proof = Proof "eyJhbGciOiJFZERTQSIsInR5cCI6ImRwb3Arand0IiwiandrIjp7Imt0eSI6Ik9LUCIsImNydiI6IkVkMjU1MTkiLCJ4IjoidUhNR0paWllUbU9zOEdiaTdaRUJLT255TnJYYnJzNTI1dE1QQUZoYjBzbyJ9fQ.eyJpYXQiOjE2Nzg4MDUyNTgsImV4cCI6MjA4ODc3MzI1OCwibmJmIjoxNjc4ODA1MjU4LCJzdWIiOiJpbTp3aXJlYXBwPVpHSmlNRGRsT1RRM1pESTVOREU0TUdFM09UQmhOVGN6WkdWbU16VmtaRFUvN2M2MzExYTFjNDNjMmJhNkB3aXJlLmNvbSIsImp0aSI6ImQyOWFkYTQ2LTBjMzYtNGNiMS05OTVlLWFlMWNiYTY5M2IzNCIsIm5vbmNlIjoiYzB0RWNtOUNUME00TXpKU04zRjRkMEZIV0V4TGIxUm5aMDQ1U3psSFduTSIsImh0bSI6IlBPU1QiLCJodHUiOiJodHRwczovL3dpcmUuZXhhbXBsZS5jb20vY2xpZW50cy84OTYzMDI3MDY5ODc3MTAzNTI2L2FjY2Vzcy10b2tlbiIsImNoYWwiOiJaa3hVV25GWU1HbHFUVVpVU1hnNFdHdHBOa3h1WWpWU09XRnlVRU5hVGxnIn0.8p0lvdOPjJ8ogjjLP6QtOo216qD9ujP7y9vSOhdYb-O8ikmW09N00gjCf0iGT-ZkxBT-LfDE3eQx27tWQ3JPBQ"
    uid = UserId "dbb07e94-7d29-4180-a790-a573def35dd5"
    cid = ClientId 8963027069877103526
    domain = Domain "wire.com"
    nonce = Nonce "c0tEcm9CT0M4MzJSN3F4d0FHWExLb1RnZ045SzlHWnM"
    uri = Uri "https://wire.example.com/clients/8963027069877103526/access-token"
    method = POST
    maxSkewSecs = MaxSkewSecs 5
    now = NowEpoch 5435234232
    expires = ExpiryEpoch $ 2136351646
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIMkvahkqR9sHJSmFeCl3B7aJjsQGgwy++cccWTbuDyy+\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEAdYI38UdxksC0K4Qx6E9JK9YfGm+ehnY18oKmHL2YsZk=\n\
        \-----END PUBLIC KEY-----\n"
