{-# LANGUAGE QuasiQuotes #-}

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

module Test.Schema.Util
  ( mk_prop_caseInsensitive,
    genUri,
    genSimpleText,
  )
where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, toLower, toUpper)
import Hedgehog
import Hedgehog.Gen as Gen
import Network.URI.Static
import Web.Scim.Schema.Common (URI (..))

genUri :: Gen URI
genUri = Gen.element [URI [uri|https://example.com|], URI [uri|gopher://glab.io|], URI [uri|ssh://nothing/blorg|]]

genSimpleText :: Gen Text
genSimpleText = Gen.element ["one", "green", "sharp"]

mk_prop_caseInsensitive :: forall a. (ToJSON a, FromJSON a, Show a, Eq a) => Gen a -> Property
mk_prop_caseInsensitive gen = property $ do
  val <- forAll gen
  fromJSON (withCasing toUpper $ toJSON val) === Success val
  fromJSON (withCasing toLower $ toJSON val) === Success val
  where
    withCasing :: (Text -> Text) -> Value -> Value
    withCasing toCasing = \case
      Object obj -> Object $ HM.foldlWithKey' (\u k v -> HM.insert (toCasing k) (withCasing toCasing v) u) HM.empty obj
      Array arr -> Array $ withCasing toCasing <$> arr
      same@(Number _) -> same
      same@(String _) -> same
      same@(Bool _) -> same
      same@Null -> same
