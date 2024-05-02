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
import Imports
import Test.Hspec

main :: IO ()
main = hspec $ do
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
      toResult (Just 19) (Just token) `shouldBe` Left InvalidUserId
      toResult (Just 20) (Just token) `shouldBe` Left NotYetValid
      toResult (Just 21) (Just token) `shouldBe` Left JwtSimpleError
      toResult (Just 22) (Just token) `shouldBe` Left RandError
      toResult (Just 23) (Just token) `shouldBe` Left Sec1Error
      toResult (Just 24) (Just token) `shouldBe` Left UrlParseError
      toResult (Just 25) (Just token) `shouldBe` Left UuidError
      toResult (Just 26) (Just token) `shouldBe` Left Utf8Error
      toResult (Just 27) (Just token) `shouldBe` Left Base64DecodeError
      toResult (Just 28) (Just token) `shouldBe` Left JsonError
      toResult (Just 29) (Just token) `shouldBe` Left InvalidJsonPath
      toResult (Just 30) (Just token) `shouldBe` Left JsonPathError
      toResult (Just 31) (Just token) `shouldBe` Left InvalidJwkThumbprint
      toResult (Just 32) (Just token) `shouldBe` Left MissingDpopHeader
      toResult (Just 33) (Just token) `shouldBe` Left MissingIssuer
      toResult (Just 34) (Just token) `shouldBe` Left DpopChallengeMismatch
      toResult (Just 35) (Just token) `shouldBe` Left DpopHtuMismatch
      toResult (Just 36) (Just token) `shouldBe` Left DpopHtmMismatch
      toResult (Just 37) (Just token) `shouldBe` Left InvalidBackendKeys
      toResult (Just 38) (Just token) `shouldBe` Left InvalidClientId
      toResult (Just 39) (Just token) `shouldBe` Left UnsupportedApiVersion
      toResult (Just 40) (Just token) `shouldBe` Left UnsupportedScope
      toResult (Just 41) (Just token) `shouldBe` Left DpopHandleMismatch
      toResult (Just 42) (Just token) `shouldBe` Left DpopTeamMismatch
      toResult (Just 43) (Just token) `shouldBe` Left DpopDisplayNameMismatch
      toResult Nothing Nothing `shouldBe` Left UnknownError
  where
    token = ""

-- tid = TeamId "1c92362d-620c-4ec3-b240-664d37037ea1"
-- proof = Proof "eyJhbGciOiJFUzI1NiIsImp3ayI6eyJhbGciOiJFUzI1NiIsImNydiI6IlAtMjU2Iiwia3R5IjoiRUMiLCJ4IjoiaGNZamxvTm9keUNMRl9yUWRfSElzelNwYTJKLXZ6cmdudG5lQUpXNXBBOCIsInkiOiI2TVh4bkhxMUZtQVdDYzZBN1lWYWx4dmVraWNCdjUzQVJUUU8zNW1SS0o4In0sInR5cCI6ImRwb3Arand0In0.eyJhdWQiOiJodHRwczovL3dpcmUuY29tL2FjbWUvY2hhbGxlbmdlL2FiY2QiLCJjaGFsIjoid2EyVnJrQ3RXMXNhdUoyRDN1S1k4cmM3eTRrbDR1c0giLCJleHAiOjE3MTQ2NTgxMTIsImhhbmRsZSI6IndpcmVhcHA6Ly8lNDBndWhrdHhncHlpcnhncHZlY2pxeHhAZXhhbXBsZS5jb20iLCJodG0iOiJQT1NUIiwiaHR1IjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9jbGllbnRzL2NjNmU2NDBlMjk2ZThiYmEvYWNjZXNzLXRva2VuIiwiaWF0IjoxNzE0NjU4MTAyLCJqdGkiOiI2ZmM1OWU3Zi1iNjY2LTRmZmMtYjczOC00ZjQ3NjBjODg0Y2EiLCJuYW1lIjoi54SQ6Jy967aH6Lic6rSb7IyE5K-s5qmp6KSy7Luc4bKe5oKd5bWU5Yq5776s4Zuq6pe15a-05LeF45aY7Yim1KDrgbDrsrDgoIDlmKrlhYHrkbrkvqDhvI3ompnro6bjgJDth4Xkt7DtjprsqaLkv6fnvYPZpuy2nOy3le-xt-KKhe2Cj-yOueq2g-eVu-y2p-WMpOGenuq-nOe5qO-wsuOWouOrkuyrreqQsuycvOqqruyjneymsOG_vOi9iuWrnuOaou2JpeOlnOO2oOKYqueAq-aTrOWEteuXjeyFj-qejeyjseKeruyPpOqdnuGYqOGxjuSOieKPrSIsIm5iZiI6MTcxNDY1ODEwMiwibm9uY2UiOiJwd2g2TWtPMFN0V1VJUWw1UllmT1BnIiwic3ViIjoid2lyZWFwcDovL1hhQ1NPbUlyUkN1QXVFTEtTMF9rNVEhY2M2ZTY0MGUyOTZlOGJiYUBleGFtcGxlLmNvbSIsInRlYW0iOiIxYzkyMzYyZC02MjBjLTRlYzMtYjI0MC02NjRkMzcwMzdlYTEifQ.souod7zzskm35erMb5hup3LRE2I2-N2RPyWYVKyYTS-3T4tiFO-4QHe-QMfSrGHc_TXlZK72mDN-sXfEvsDuqg"
-- uid = UserId "5da0923a-622b-442b-80b8-42ca4b4fe4e5"
-- nonce = Nonce "pwh6MkO0StWUIQl5RYfOPg"
-- expires = ExpiryEpoch 1714658401
-- handle = Handle "guhktxgpyirxgpvecjqxx"
-- displayName = DisplayName "\231\132\144\232\156\189\235\182\135\232\184\156\234\180\155\236\140\132\228\175\172\230\169\169\232\164\178\236\187\156\225\178\158\230\130\157\229\181\148\229\138\185\239\190\172\225\155\170\234\151\181\229\175\180\228\183\133\227\150\152\237\136\166\212\160\235\129\176\235\178\176\224\160\128\229\152\170\229\133\129\235\145\186\228\190\160\225\188\141\232\154\153\235\163\166\227\128\144\237\135\133\228\183\176\237\142\154\236\169\162\228\191\167\231\189\131\217\166\236\182\156\236\183\149\239\177\183\226\138\133\237\130\143\236\142\185\234\182\131\231\149\187\236\182\167\229\140\164\225\158\158\234\190\156\231\185\168\239\176\178\227\150\162\227\171\146\236\171\173\234\144\178\236\156\188\234\170\174\236\163\157\236\166\176\225\191\188\232\189\138\229\171\158\227\154\162\237\137\165\227\165\156\227\182\160\226\152\170\231\128\171\230\147\172\229\132\181\235\151\141\236\133\143\234\158\141\236\163\177\226\158\174\236\143\164\234\157\158\225\152\168\225\177\142\228\142\137\226\143\173"

-- now = NowEpoch 1704982162
-- cid = ClientId 14730821443162901434
-- domain = Domain "example.com"
-- uri = Uri "https://example.com/clients/cc6e640e296e8bba/access-token"
-- method = POST
-- maxSkewSecs = MaxSkewSecs 1
-- pem =
--   PemBundle $
--     "-----BEGIN PRIVATE KEY-----\n\
--     \MC4CAQAwBQYDK2VwBCIEIMkvahkqR9sHJSmFeCl3B7aJjsQGgwy++cccWTbuDyy+\n\
--     \-----END PRIVATE KEY-----\n\
--     \-----BEGIN PUBLIC KEY-----\n\
--     \MCowBQYDK2VwAyEAdYI38UdxksC0K4Qx6E9JK9YfGm+ehnY18oKmHL2YsZk=\n\
--     \-----END PUBLIC KEY-----\n"
