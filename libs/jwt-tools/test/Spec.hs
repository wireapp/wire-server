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
    proof = Proof "eyJhbGciOiJFZERTQSIsImp3ayI6eyJjcnYiOiJFZDI1NTE5Iiwia3R5IjoiT0tQIiwieCI6Im5MSkdOLU9hNkpzcTNLY2xaZ2dMbDdVdkFWZG1CMFE2QzNONUJDZ3BoSHcifSwidHlwIjoiZHBvcCtqd3QifQ.eyJhdWQiOiJodHRwczovL3dpcmUuY29tL2FjbWUvY2hhbGxlbmdlL2FiY2QiLCJjaGFsIjoid2EyVnJrQ3RXMXNhdUoyRDN1S1k4cmM3eTRrbDR1c0giLCJleHAiOjE3Mzk4ODA2NzQsImhhbmRsZSI6IndpcmVhcHA6Ly8lNDB5d2Z5ZG5pZ2Jud2h1b3pldGphZ3FAZXhhbXBsZS5jb20iLCJodG0iOiJQT1NUIiwiaHR1IjoiaHR0cHM6Ly9leGFtcGxlLmNvbS9jbGllbnRzL2NjNmU2NDBlMjk2ZThiYmEvYWNjZXNzLXRva2VuIiwiaWF0IjoxNzA4MzQ0Njc0LCJqdGkiOiI2ZmM1OWU3Zi1iNjY2LTRmZmMtYjczOC00ZjQ3NjBjODg0Y2EiLCJuYW1lIjoi5reB4qqu5KSq5rK255Kh4bKV6re14Y2q6omE6Jy16Iu17ICV54Kb66-v56qp5KqW766M6bGw6oOy6b6m57m15pWJ4LqH54et6rOj54KHIiwibmJmIjoxNzA4MzQ0Njc0LCJub25jZSI6IllWZ2dHdWlTUTZlamhQNTNFX0tPS3ciLCJzdWIiOiJ3aXJlYXBwOi8vSWZ0VzBLeFVSb2F1QWVockRremJiQSFjYzZlNjQwZTI5NmU4YmJhQGV4YW1wbGUuY29tIiwidGVhbSI6ImMxNTE5NzVlLWIxOTMtNDAwOS1hM2QyLTc0N2M5NjFmMjMzMyJ9.SHxpMzOe2yC3y6DP7lEH0l7_eOKrUZZI0OjgtnCKjO4OBD0XqKOi0y_z07-7FWc-KtThlsaZatnBNTB67GhQBw"
    uid = UserId "21fb56d0-ac54-4686-ae01-e86b0e4cdb6c"
    nonce = Nonce "YVggGuiSQ6ejhP53E_KOKw"
    expires = ExpiryEpoch 1739967074
    handle = Handle "ywfydnigbnwhuozetjagq"
    displayName = DisplayName "\230\183\129\226\170\174\228\164\170\230\178\182\231\146\161\225\178\149\234\183\181\225\141\170\234\137\132\232\156\181\232\139\181\236\128\149\231\130\155\235\175\175\231\170\169\228\170\150\239\174\140\233\177\176\234\131\178\233\190\166\231\185\181\230\149\137\224\186\135\231\135\173\234\179\163\231\130\135"
    tid = TeamId "c151975e-b193-4009-a3d2-747c961f2333"

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
