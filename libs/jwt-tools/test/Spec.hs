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

import Data.Jwt.Tools
import Data.String.Conversions
import Imports
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "toResult" $ do
    it "should convert to correct error" $ do
      toResult Nothing (Just emptyToken) `shouldBe` Right (cs emptyToken)
      toResult (Just 1) Nothing `shouldBe` Left UnknownError
      toResult (Just 1) (Just emptyToken) `shouldBe` Left UnknownError
      toResult (Just 2) Nothing `shouldBe` Left FfiError
      toResult (Just 2) (Just emptyToken) `shouldBe` Left FfiError
      toResult (Just 3) Nothing `shouldBe` Left ImplementationError
      toResult (Just 3) (Just emptyToken) `shouldBe` Left ImplementationError
      toResult (Just 4) Nothing `shouldBe` Left DpopSyntaxError
      toResult (Just 4) (Just emptyToken) `shouldBe` Left DpopSyntaxError
      toResult (Just 5) Nothing `shouldBe` Left DpopTypError
      toResult (Just 5) (Just emptyToken) `shouldBe` Left DpopTypError
      toResult (Just 6) Nothing `shouldBe` Left DpopUnsupportedAlgorithmError
      toResult (Just 6) (Just emptyToken) `shouldBe` Left DpopUnsupportedAlgorithmError
      toResult (Just 7) Nothing `shouldBe` Left DpopInvalidSignatureError
      toResult (Just 7) (Just emptyToken) `shouldBe` Left DpopInvalidSignatureError
      toResult (Just 8) Nothing `shouldBe` Left ClientIdMismatchError
      toResult (Just 8) (Just emptyToken) `shouldBe` Left ClientIdMismatchError
      toResult (Just 9) Nothing `shouldBe` Left BackendNonceMismatchError
      toResult (Just 9) (Just emptyToken) `shouldBe` Left BackendNonceMismatchError
      toResult (Just 10) Nothing `shouldBe` Left HtuMismatchError
      toResult (Just 10) (Just emptyToken) `shouldBe` Left HtuMismatchError
      toResult (Just 11) Nothing `shouldBe` Left HtmMismatchError
      toResult (Just 11) (Just emptyToken) `shouldBe` Left HtmMismatchError
      toResult (Just 12) Nothing `shouldBe` Left MissingJtiError
      toResult (Just 12) (Just emptyToken) `shouldBe` Left MissingJtiError
      toResult (Just 13) Nothing `shouldBe` Left MissingChallengeError
      toResult (Just 13) (Just emptyToken) `shouldBe` Left MissingChallengeError
      toResult (Just 14) Nothing `shouldBe` Left MissingIatError
      toResult (Just 14) (Just emptyToken) `shouldBe` Left MissingIatError
      toResult (Just 15) Nothing `shouldBe` Left IatError
      toResult (Just 15) (Just emptyToken) `shouldBe` Left IatError
      toResult (Just 16) Nothing `shouldBe` Left MissingExpError
      toResult (Just 16) (Just emptyToken) `shouldBe` Left MissingExpError
      toResult (Just 17) Nothing `shouldBe` Left ExpMismatchError
      toResult (Just 17) (Just emptyToken) `shouldBe` Left ExpMismatchError
      toResult (Just 18) Nothing `shouldBe` Left Expired
      toResult (Just 18) (Just emptyToken) `shouldBe` Left Expired
      toResult (Just 19) (Just emptyToken) `shouldBe` Left InvalidUserId
      toResult (Just 20) (Just emptyToken) `shouldBe` Left NotYetValid
      toResult (Just 21) (Just emptyToken) `shouldBe` Left JwtSimpleError
      toResult (Just 22) (Just emptyToken) `shouldBe` Left RandError
      toResult (Just 23) (Just emptyToken) `shouldBe` Left Sec1Error
      toResult (Just 24) (Just emptyToken) `shouldBe` Left UrlParseError
      toResult (Just 25) (Just emptyToken) `shouldBe` Left UuidError
      toResult (Just 26) (Just emptyToken) `shouldBe` Left Utf8Error
      toResult (Just 27) (Just emptyToken) `shouldBe` Left Base64DecodeError
      toResult (Just 28) (Just emptyToken) `shouldBe` Left JsonError
      toResult (Just 29) (Just emptyToken) `shouldBe` Left InvalidJsonPath
      toResult (Just 30) (Just emptyToken) `shouldBe` Left JsonPathError
      toResult (Just 31) (Just emptyToken) `shouldBe` Left InvalidJwkThumbprint
      toResult (Just 32) (Just emptyToken) `shouldBe` Left MissingDpopHeader
      toResult (Just 33) (Just emptyToken) `shouldBe` Left MissingIssuer
      toResult (Just 34) (Just emptyToken) `shouldBe` Left DpopChallengeMismatch
      toResult (Just 35) (Just emptyToken) `shouldBe` Left DpopHtuMismatch
      toResult (Just 36) (Just emptyToken) `shouldBe` Left DpopHtmMismatch
      toResult (Just 37) (Just emptyToken) `shouldBe` Left InvalidBackendKeys
      toResult (Just 38) (Just emptyToken) `shouldBe` Left InvalidClientId
      toResult (Just 39) (Just emptyToken) `shouldBe` Left UnsupportedApiVersion
      toResult (Just 40) (Just emptyToken) `shouldBe` Left UnsupportedScope
      toResult (Just 41) (Just emptyToken) `shouldBe` Left DpopHandleMismatch
      toResult (Just 42) (Just emptyToken) `shouldBe` Left DpopTeamMismatch
      toResult (Just 43) (Just emptyToken) `shouldBe` Left DpopDisplayNameMismatch
      toResult Nothing Nothing `shouldBe` Left UnknownError
  where
    emptyToken = ""
