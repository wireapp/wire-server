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
import Data.ByteString.Conversion
import Imports
import Jwt.Tools
import Network.HTTP.Types (StdMethod (..))
import Test.Hspec
import Test.QuickCheck (Arbitrary (arbitrary), generate)
import Wire.API.MLS.Credential (ClientIdentity (..))
import Wire.API.MLS.Epoch
import Wire.API.User.Client.DPoPAccessToken

main :: IO ()
main = hspec $ do
  describe "generateDpopToken FFI" $ do
    it "should return a value" $ do
      response <- callFFIWithRandomValues
      response `shouldSatisfy` (\r -> isRight r || isLeft r)

callFFIWithRandomValues :: IO (Either DPoPTokenGenerationError DPoPAccessToken)
callFFIWithRandomValues = do
  cid <- ClientIdentity <$> generate arbitrary <*> generate arbitrary <*> generate arbitrary
  now <- generate arbitrary
  let expires = now & addToEpoch (360 :: Word32)
  nonce <- generate arbitrary
  uri <- generate arbitrary
  runExceptT $
    generateDpopToken
      (Proof "xxxx.yyyy.zzzz")
      cid
      nonce
      uri
      POST
      16
      expires
      now
      (fromMaybe undefined $ fromByteString pem)
  where
    pem :: ByteString
    pem =
      "-----BEGIN PRIVATE KEY-----\n"
        <> "MC4CAQAwBQYDK2VwBCIEIFANnxZLNE4p+GDzWzR3wm/v8x/0bxZYkCyke1aTRucX\n"
        <> "-----END PRIVATE KEY-----\n"
        <> "-----BEGIN PUBLIC KEY-----\n"
        <> "MCowBQYDK2VwAyEACPvhIdimF20tOPjbb+fXJrwS2RKDp7686T90AZ0+Th8=\n"
        <> "-----END PUBLIC KEY-----\n"
