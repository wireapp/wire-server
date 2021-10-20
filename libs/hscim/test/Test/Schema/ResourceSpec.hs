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

module Test.Schema.ResourceSpec
  ( spec,
  )
where

import Data.Aeson
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Test.Hspec
import Test.Schema.Util (genUri, mk_prop_caseInsensitive)
import Web.Scim.Schema.ResourceType
import qualified Web.Scim.Schema.Schema as Schema

prop_roundtrip :: Property
prop_roundtrip = property $ do
  user <- forAll genResource
  tripping user toJSON fromJSON

spec :: Spec
spec = do
  it "roundtrip" $ do
    require prop_roundtrip
  it "case-insensitive" $ do
    require $ mk_prop_caseInsensitive genResource

genResource :: Gen Resource
genResource =
  Resource
    <$> Gen.element ["name1", "name2", "name3"]
    <*> genUri
    <*> genSchema

genSchema :: Gen Schema.Schema
genSchema =
  Gen.element
    [ Schema.User20,
      Schema.ServiceProviderConfig20,
      Schema.Group20,
      Schema.Schema20,
      Schema.ResourceType20,
      Schema.ListResponse20,
      Schema.Error20,
      Schema.PatchOp20,
      Schema.CustomSchema "custom1",
      Schema.CustomSchema "custom2",
      Schema.CustomSchema "custom3"
    ]
