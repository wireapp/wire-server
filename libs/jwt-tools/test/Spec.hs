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
  describe "generateDpopToken FFI" $ do
    it "should return a value" $ do
      actual <- callFFIWithConstValues
      let expected = Right $ cs token
      actual `shouldBe` expected
  describe "toResult" $ do
    it "should convert to correct error" $ do
      -- the only valid case is when the error=0 (meaning no error) and the token is not null
      toResult Nothing (Just token) `shouldBe` Right (cs token)

      -- error=1 corresponds to an unknown error on FFI side
      toResult (Just 1) Nothing `shouldBe` Left FfiError
      toResult (Just 1) (Just token) `shouldBe` Left FfiError
      -- error=2 corresponds to 'FfiError' on FFI side
      toResult (Just 2) Nothing `shouldBe` Left FfiError
      toResult (Just 2) (Just token) `shouldBe` Left FfiError
      -- error=3 corresponds to 'ImplementationError' on FFI side
      toResult (Just 3) Nothing `shouldBe` Left FfiError
      toResult (Just 3) (Just token) `shouldBe` Left FfiError
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
      -- this should also not happen, but apparently something went wrong
      toResult Nothing Nothing `shouldBe` Left FfiError

token :: String
token = "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"

callFFIWithConstValues :: IO (Either DPoPTokenGenerationError ByteString)
callFFIWithConstValues = do
  let proof = Proof "xxxx.yyyy.zzzz"
  let uid = UserId "8a6e8a6e-8a6e-8a6e-8a6e-8a6e8a6e8a6e"
  let cid = ClientId 8899
  let domain = Domain "example.com"
  let nonce = Nonce "123"
  let uri = Uri "/foo"
  let method = POST
  let maxSkewSecs = MaxSkewSecs 1
  let now = NowEpoch 5435234232
  let expires = ExpiryEpoch $ 5435234232 + 360
  let pem =
        PemBundle $
          "-----BEGIN PRIVATE KEY-----\n\
          \MC4CAQAwBQYDK2VwBCIEIFANnxZLNE4p+GDzWzR3wm/v8x/0bxZYkCyke1aTRucX\n\
          \-----END PRIVATE KEY-----\n\
          \-----BEGIN PUBLIC KEY-----\n\
          \MCowBQYDK2VwAyEACPvhIdimF20tOPjbb+fXJrwS2RKDp7686T90AZ0+Th8=\n\
          \-----END PUBLIC KEY-----\n"
  runExceptT $ generateDpopToken proof uid cid domain nonce uri method maxSkewSecs expires now pem
