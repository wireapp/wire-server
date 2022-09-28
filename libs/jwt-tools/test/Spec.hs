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
  describe "generateDpopToken FFI" $ do
    it "should return a value" $ do
      actual <- callFFIWithConstValues
      let expected = Right $ "eyJ0eXAiOiJKV1QiLA0KICJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJqb2UiLA0KICJleiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFtcGxlLmNvbS9pc19yb290Ijp0cnVlfQ.dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"
      actual `shouldBe` expected

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
