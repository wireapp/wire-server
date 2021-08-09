{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Schema.MetaSchemaSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Text (Text)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.URI.Static (uri)
import Test.Hspec
import Web.Scim.Capabilities.MetaSchema
import Web.Scim.Schema.AuthenticationScheme
import Web.Scim.Schema.Common (ScimBool (ScimBool), URI (..))
import Web.Scim.Schema.Schema (Schema (..))
import Prelude hiding (filter)

prop_roundtrip :: (ToJSON a, FromJSON a, Show a, Eq a) => Gen a -> Property
prop_roundtrip gen = property $ do
  config <- forAll gen
  tripping config toJSON fromJSON

spec :: Spec
spec = do
  describe "MetaSchema" $ do
    it "`Supported ()` roundtrips" $ do
      require (prop_roundtrip (genSupported (pure ())))
    it "`BulkConfig` roundtrips" $ do
      require (prop_roundtrip genBulkConfig)
    it "`FilterConfig` roundtrips" $ do
      require (prop_roundtrip genFilterConfig)
    it "`AuthenticationSchemeEncoding` roundtrips" $ do
      require (prop_roundtrip genAuthenticationSchemeEncoding)
    it "`Configuration` roundtrips" $ do
      require (prop_roundtrip genConfiguration)

genConfiguration :: Gen Configuration
genConfiguration = do
  documentationUri <- Gen.maybe genUri
  schemas <- pure [User20]
  patch <- genSupported (pure ())
  bulk <- genSupported genBulkConfig
  filter <- genSupported genFilterConfig
  changePassword <- genSupported (pure ())
  sort <- genSupported (pure ())
  etag <- genSupported (pure ())
  authenticationSchemes <- Gen.list (Range.linear 0 100) genAuthenticationSchemeEncoding
  pure Configuration {..}

genBulkConfig :: Gen BulkConfig
genBulkConfig = do
  maxOperations <- Gen.int (Range.linear 0 100)
  maxPayloadSize <- Gen.int (Range.linear 0 100)
  pure BulkConfig {..}

genFilterConfig :: Gen FilterConfig
genFilterConfig = do
  maxResults <- Gen.int (Range.linear 0 100)
  pure FilterConfig {..}

genAuthenticationSchemeEncoding :: Gen AuthenticationSchemeEncoding
genAuthenticationSchemeEncoding = do
  typ <- genSimpleText
  name <- genSimpleText
  description <- genSimpleText
  specUri <- Gen.maybe genUri
  documentationUri <- Gen.maybe genUri
  pure AuthenticationSchemeEncoding {..}

genSupported :: forall a. Gen a -> Gen (Supported a)
genSupported gen = do
  supported :: ScimBool <- ScimBool <$> Gen.bool
  subConfig :: a <- gen
  pure Supported {..}

genUri :: Gen URI
genUri = Gen.element [URI [uri|https://example.com|], URI [uri|gopher://glab.io|], URI [uri|ssh://nothing/blorg|]]

genSimpleText :: Gen Text
genSimpleText = Gen.element ["one", "green", "sharp"]
