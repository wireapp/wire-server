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
      actual <- runExceptT $ generateDpopToken proof uid cid handle displayName tid domain nonce uri method maxSkewSecs expires now pem
      -- The actual payload of the DPoP token is not deterministic as it depends on the current time.
      -- We therefore only check the header, because if the header is correct, it means the token creation was successful.s
      let expectedHeader = "eyJhbGciOiJFZERTQSIsInR5cCI6ImF0K2p3dCIsImp3ayI6eyJrdHkiOiJPS1AiLCJjcnYiOiJFZDI1NTE5IiwieCI6ImRZSTM4VWR4a3NDMEs0UXg2RTlKSzlZZkdtLWVoblkxOG9LbUhMMllzWmsifX0"
      let actualHeader = either (const "") (head . split '.') actual
      actualHeader `shouldBe` expectedHeader
  describe "generateDpopToken FFI when passing a wrong nonce value" $ do
    it "should return BackendNonceMismatchError" $ do
      actual <- runExceptT $ generateDpopToken proof uid cid handle displayName tid domain (Nonce "foobar") uri method maxSkewSecs expires now pem
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
    proof = Proof "eyJhbGciOiJFZERTQSIsImp3ayI6eyJjcnYiOiJFZDI1NTE5Iiwia3R5IjoiT0tQIiwieCI6Im5MSkdOLU9hNkpzcTNLY2xaZ2dMbDdVdkFWZG1CMFE2QzNONUJDZ3BoSHcifSwidHlwIjoiZHBvcCtqd3QifQ.eyJhdWQiOiJodHRwczovL3dpcmUuY29tL2FjbWUvY2hhbGxlbmdlL2FiY2QiLCJjaGFsIjoid2EyVnJrQ3RXMXNhdUoyRDN1S1k4cmM3eTRrbDR1c0giLCJleHAiOjE4MzE3MzcyNzEsImhhbmRsZSI6IndpcmVhcHA6Ly8lNDB2bHVwZHlwbml4dm1vdnZzeW1ndHdAZXhhbXBsZS5jb20iLCJodG0iOiJQT1NUIiwiaHR1IjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9jbGllbnRzL2NjNmU2NDBlMjk2ZThiYmEvYWNjZXNzLXRva2VuIiwiaWF0IjoxNzA1NTkzMjcxLCJqdGkiOiI2ZmM1OWU3Zi1iNjY2LTRmZmMtYjczOC00ZjQ3NjBjODg0Y2EiLCJuYmYiOjE3MDU1OTMyNzEsIm5vbmNlIjoibVJDdjNKQS1TNDI0dUJyLVk2QzFndyIsInN1YiI6IndpcmVhcHA6Ly9WNVc3ZnRNeVRJNlBNYlE0Y3ZkazRnIWNjNmU2NDBlMjk2ZThiYmFAZXhhbXBsZS5jb20iLCJ0ZWFtIjoiZmZhODY1ZmEtYjI0YS00Njk3LWFhMDUtMWZjM2YzNjU0ZGI5In0.BVdawX_84Mpmvzbs3v52t3GtCgSKzxgnFDkwf4QK6AusoyfsjhK6grs9GLEe2Lfb1eDrBUJgo-nobeIWmRumBQ"
    uid = UserId "5795bb7e-d332-4c8e-8f31-b43872f764e2"
    nonce = Nonce "mRCv3JA-S424uBr-Y6C1gw"
    expires = ExpiryEpoch 1831823671
    handle = Handle "vlupdypnixvmovvsymgtw"
    displayName = DisplayName ""
    tid = TeamId "ffa865fa-b24a-4697-aa05-1fc3f3654db9"

    now = NowEpoch 1704982162
    cid = ClientId 14730821443162901434
    domain = Domain "example.com"
    uri = Uri "https://example.com/clients/cc6e640e296e8bba/access-token"
    method = POST
    maxSkewSecs = MaxSkewSecs 1
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIMkvahkqR9sHJSmFeCl3B7aJjsQGgwy++cccWTbuDyy+\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEAdYI38UdxksC0K4Qx6E9JK9YfGm+ehnY18oKmHL2YsZk=\n\
        \-----END PUBLIC KEY-----\n"
