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
import Imports
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "generateDpopToken FFI when passing valid inputs" $ do
    it "should return an access token" $ do
      -- FUTUREWORK(leif): fix this test, we need new valid test data,
      -- this test exists mainly for debugging purposes
      -- a functionality test is also coverd in the integration tests in services/brig/test/integration/API/User/Client.hs (`testCreateAccessToken`)
      pending
      actual <- runExceptT $ generateDpopToken proof uid cid domain nonce uri method maxSkewSecs expires now pem
      print actual
      isRight actual `shouldBe` True
  describe "generateDpopToken FFI when passing a wrong nonce value" $ do
    it "should return BackendNonceMismatchError" $ do
      -- FUTUREWORK(leif): fix this test, we need new valid test data,
      -- this test exists mainly for debugging purposes
      -- a functionality test is also coverd in the integration tests in services/brig/test/integration/API/User/Client.hs (`testCreateAccessToken`)
      pending
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
      toResult (Just 18) Nothing `shouldBe` Left Expired
      toResult (Just 18) (Just token) `shouldBe` Left Expired
      toResult Nothing Nothing `shouldBe` Left UnknownError
  where
    token = ""
    proof = Proof "eyJhbGciOiJFZERTQSIsInR5cCI6ImRwb3Arand0IiwiandrIjp7Imt0eSI6Ik9LUCIsImNydiI6IkVkMjU1MTkiLCJ4IjoidXE2c1hXcDdUM1E3YlNtUFd3eFNlRHJoUHFid1RfcTd4SFBQeGpGT0g5VSJ9fQ.eyJpYXQiOjE2OTQxMTc0MjgsImV4cCI6MTY5NDcyMjIyOCwibmJmIjoxNjk0MTE3NDIzLCJzdWIiOiJpbTp3aXJlYXBwPUlHOVl2enVXUUlLVWFSazEyRjVDSVEvOGUxODk2MjZlYWUwMTExZEBlbG5hLndpcmUubGluayIsImp0aSI6ImM0OGZmOTAyLTc5OGEtNDNjYi04YTk2LTE3NzM0NTgxNjIyMCIsIm5vbmNlIjoiR0FxNG5SajlSWVNzUnhoOVh1MWFtQSIsImh0bSI6IlBPU1QiLCJodHUiOiJodHRwczovL2VsbmEud2lyZS5saW5rL2NsaWVudHMvOGUxODk2MjZlYWUwMTExZC9hY2Nlc3MtdG9rZW4iLCJjaGFsIjoiMkxLbEFWMjR2VGtIMHlaaFdacEZrT01mSEE1d3lGQkgifQ.FW5i40CvndSSo3wQdA1DMUkGRmxk86cORAllwC2PCejVuk7TsdZuIKuJZFVa1VTJKWwNCPqPZ05Gsxxeh1DiDA"
    uid = UserId "206f58bf-3b96-4082-9469-1935d85e4221"
    cid = ClientId 10239098846720299293
    domain = Domain "wire.com"
    nonce = Nonce "GAq4nRj9RYSsRxh9Xu1amA"
    uri = Uri "https://elna.wire.link/clients/10239098846720299293/access-token"
    method = POST
    maxSkewSecs = MaxSkewSecs 5
    now = NowEpoch 360
    expires = ExpiryEpoch 2136351646
    pem =
      PemBundle $
        "-----BEGIN PRIVATE KEY-----\n\
        \MC4CAQAwBQYDK2VwBCIEIMkvahkqR9sHJSmFeCl3B7aJjsQGgwy++cccWTbuDyy+\n\
        \-----END PRIVATE KEY-----\n\
        \-----BEGIN PUBLIC KEY-----\n\
        \MCowBQYDK2VwAyEAdYI38UdxksC0K4Qx6E9JK9YfGm+ehnY18oKmHL2YsZk=\n\
        \-----END PUBLIC KEY-----\n"
