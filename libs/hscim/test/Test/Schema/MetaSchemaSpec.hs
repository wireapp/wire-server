{-# LANGUAGE TypeApplications #-}

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

module Test.Schema.MetaSchemaSpec
  ( spec,
  )
where

import Data.Aeson
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Schema.Util (genSimpleText, genUri, mk_prop_caseInsensitive)
import Web.Scim.Capabilities.MetaSchema
import Web.Scim.Schema.AuthenticationScheme
import Web.Scim.Schema.Common (ScimBool (ScimBool))
import Web.Scim.Schema.Schema (Schema (..))
import Prelude hiding (filter)

prop_roundtrip :: (ToJSON a, FromJSON a, Show a, Eq a) => Gen a -> Property
prop_roundtrip gen = property $ do
  config <- forAll gen
  tripping config toJSON fromJSON

spec :: Spec
spec = do
  describe "MetaSchema" $ do
    -- the extra 'decode' in the golden tests is to make attribute order not count for Eq.
    it "`Supported ()` golden test" $ do
      decode @Value (encode (Supported (ScimBool True) ())) `shouldBe` decode @Value "{\"supported\":true}"
    it "`Supported a` golden test" $ do
      decode @Value (encode (Supported (ScimBool True) (FilterConfig 3))) `shouldBe` decode @Value "{\"supported\":true,\"maxResults\":3}"
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
    it "`Configuration` satisfies the insane json-case-insensitivity rule." $ do
      require $ mk_prop_caseInsensitive genConfiguration

genConfiguration :: Gen Configuration
genConfiguration = do
  Configuration
    <$> Gen.maybe genUri
    <*> pure [User20]
    <*> genSupported (pure ())
    <*> genSupported genBulkConfig
    <*> genSupported genFilterConfig
    <*> genSupported (pure ())
    <*> genSupported (pure ())
    <*> genSupported (pure ())
    <*> Gen.list (Range.linear 0 100) genAuthenticationSchemeEncoding

genBulkConfig :: Gen BulkConfig
genBulkConfig = do
  BulkConfig
    <$> Gen.int (Range.linear 0 100)
    <*> Gen.int (Range.linear 0 100)

genFilterConfig :: Gen FilterConfig
genFilterConfig = do
  FilterConfig <$> Gen.int (Range.linear 0 100)

genAuthenticationSchemeEncoding :: Gen AuthenticationSchemeEncoding
genAuthenticationSchemeEncoding = do
  AuthenticationSchemeEncoding
    <$> genSimpleText
    <*> genSimpleText
    <*> genSimpleText
    <*> Gen.maybe genUri
    <*> Gen.maybe genUri

genSupported :: forall a. Gen a -> Gen (Supported a)
genSupported gen = do
  Supported
    <$> (ScimBool <$> Gen.bool)
    <*> gen
