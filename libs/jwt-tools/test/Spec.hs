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
import Data.ByteString.Char8 (split)
import Data.Jwt.Tools
import Imports
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "generateDpopToken FFI when passing valid inputs" $ do
    it "should return an access token with the correct header" $ do
      actual <- runExceptT $ generateDpopToken proof uid cid handle tid domain nonce uri method maxSkewSecs expires now pem
      -- The actual payload of the DPoP token is not deterministic as it depends on the current time.
      -- We therefore only check the header, because if the header is correct, it means the token creation was successful.s
      let expectedHeader = "eyJhbGciOiJFZERTQSIsInR5cCI6ImF0K2p3dCIsImp3ayI6eyJrdHkiOiJPS1AiLCJjcnYiOiJFZDI1NTE5IiwieCI6ImRZSTM4VWR4a3NDMEs0UXg2RTlKSzlZZkdtLWVoblkxOG9LbUhMMllzWmsifX0"
      let actualHeader = either (const "") (head . split '.') actual
      actualHeader `shouldBe` expectedHeader
  describe "generateDpopToken FFI when passing a wrong nonce value" $ do
    it "should return BackendNonceMismatchError" $ do
      actual <- runExceptT $ generateDpopToken proof uid cid handle tid domain (Nonce "foobar") uri method maxSkewSecs expires now pem
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
      toResult (Just 18) Nothing `shouldBe` Left Expired
      toResult (Just 18) (Just token) `shouldBe` Left Expired
      toResult Nothing Nothing `shouldBe` Left UnknownError
  where
    token = ""
    proof = Proof "eyJhbGciOiJFZERTQSIsImp3ayI6eyJjcnYiOiJFZDI1NTE5Iiwia3R5IjoiT0tQIiwieCI6Im5MSkdOLU9hNkpzcTNLY2xaZ2dMbDdVdkFWZG1CMFE2QzNONUJDZ3BoSHcifSwidHlwIjoiZHBvcCtqd3QifQ.eyJjaGFsIjoid2EyVnJrQ3RXMXNhdUoyRDN1S1k4cmM3eTRrbDR1c0giLCJleHAiOjE4MzExMjYxNjMsImhhbmRsZSI6IndpcmVhcHA6Ly8lNDBwaHVoaGliZGhxYnF4cnpibnNhZndAZXhhbXBsZS5jb20iLCJodG0iOiJQT1NUIiwiaHR1IjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9jbGllbnRzL2NjNmU2NDBlMjk2ZThiYmEvYWNjZXNzLXRva2VuIiwiaWF0IjoxNzA0OTgyMTYzLCJqdGkiOiI2ZmM1OWU3Zi1iNjY2LTRmZmMtYjczOC00ZjQ3NjBjODg0Y2EiLCJuYmYiOjE3MDQ5ODIxNjMsIm5vbmNlIjoiVnZHYnc2ZVZUTkdTUWJLNVNlaVNiQSIsInN1YiI6IndpcmVhcHA6Ly9zZ3VNZUxKdFE2U3ZKUGNxUExiMkJnIWNjNmU2NDBlMjk2ZThiYmFAZXhhbXBsZS5jb20iLCJ0ZWFtIjoiNDAyNTE2ODAtMzVlMS00Mzc0LWIzYWEtNzU2MDBkZTc5ZTMzIn0.JgVXD2_E4j4sLcvD284Fj4z_6xmwA0czcP8wzHZmqPpel60HUqDVKDx5GmiWbFWix-E7ZXvYfvZ7NmxlDrgmAg"
    uid = UserId "b20b8c78-b26d-43a4-af24-f72a3cb6f606"
    cid = ClientId 14730821443162901434
    domain = Domain "example.com"
    nonce = Nonce "VvGbw6eVTNGSQbK5SeiSbA"
    uri = Uri "https://example.com/clients/cc6e640e296e8bba/access-token"
    method = POST
    maxSkewSecs = MaxSkewSecs 1
    now = NowEpoch 1704982162
    expires = ExpiryEpoch 1831212562
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIMkvahkqR9sHJSmFeCl3B7aJjsQGgwy++cccWTbuDyy+\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEAdYI38UdxksC0K4Qx6E9JK9YfGm+ehnY18oKmHL2YsZk=\n\
        \-----END PUBLIC KEY-----\n"
    handle = Handle "phuhhibdhqbqxrzbnsafw"
    tid = TeamId "40251680-35e1-4374-b3aa-75600de79e33"
