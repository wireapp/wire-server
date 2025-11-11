-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserSearch.TypesSpec where

import Control.Error (hush)
import Data.Aeson as Aeson
import Data.Fixed
import Data.Handle
import Data.Id
import Data.Json.Util
import Data.Time
import Data.Time.Clock.POSIX
import Imports
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Team.Role
import Wire.API.User
import Wire.UserSearch.Types

spec :: Spec
spec = describe "UserDoc" $ do
  describe "JSON" $ do
    prop "roundrip to/fromJSON" $ \(userDoc :: UserDoc) ->
      fromJSON (toJSON userDoc) === Aeson.Success userDoc

    it "should be backwards comptibile" $ do
      eitherDecode (userDoc1ByteString) `shouldBe` Right userDoc1

mkTime :: Int -> UTCTime
mkTime = posixSecondsToUTCTime . secondsToNominalDiffTime . MkFixed . (* 1000000000) . fromIntegral

userDoc1 :: UserDoc
userDoc1 =
  UserDoc
    { udId = fromJust . hush . parseIdFromText $ "0a96b396-57d6-11ea-a04b-7b93d1a5c19c",
      udTeam = hush . parseIdFromText $ "17c59b18-57d6-11ea-9220-8bbf5eee961a",
      udName = Just . Name $ "Carl Phoomp",
      udNormalized = Just $ "carl phoomp",
      udHandle = Just . fromJust . parseHandle $ "phoompy",
      udEmail = Just $ unsafeEmailAddress "phoompy" "example.com",
      udColourId = Just . ColourId $ 32,
      udAccountStatus = Just Active,
      udSAMLIdP = Just "https://issuer.net/214234",
      udManagedBy = Just ManagedByScim,
      udCreatedAt = Just (toUTCTimeMillis (mkTime 1598737800000)),
      udRole = Just RoleAdmin,
      udSearchVisibilityInbound = Nothing,
      udScimExternalId = Nothing,
      udSso = Nothing,
      udEmailUnvalidated = Nothing,
      udSearchable = Nothing
    }

-- Dont touch this. This represents serialized legacy data.
userDoc1ByteString :: LByteString
userDoc1ByteString = "{\"email\":\"phoompy@example.com\",\"account_status\":\"active\",\"handle\":\"phoompy\",\"managed_by\":\"scim\",\"role\":\"admin\",\"accent_id\":32,\"name\":\"Carl Phoomp\",\"created_at\":\"2020-08-29T21:50:00.000Z\",\"team\":\"17c59b18-57d6-11ea-9220-8bbf5eee961a\",\"id\":\"0a96b396-57d6-11ea-a04b-7b93d1a5c19c\",\"normalized\":\"carl phoomp\",\"saml_idp\":\"https://issuer.net/214234\"}"
