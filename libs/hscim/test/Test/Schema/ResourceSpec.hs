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

module Test.Schema.ResourceSpec
  ( spec,
  )
where

import Data.Aeson
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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

  it "omits schemaExtensions when there are none" $ do
    toJSON usersResource
      `shouldBe` object
        [ "endpoint" .= String "/Users",
          "name" .= String "User",
          "schema" .= String "urn:ietf:params:scim:schemas:core:2.0:User"
        ]

  it "serialises a schema extension in RFC 7643 shape" $ do
    toJSON (SchemaExtension (Schema.CustomSchema "urn:example:X") True)
      `shouldBe` object
        [ "schema" .= String "urn:example:X",
          "required" .= True
        ]

  it "user schema with extension also works" $ do
    toJSON (usersResource {schemaExtensions = [SchemaExtension (Schema.CustomSchema "urn:example:X") True]})
      `shouldBe` object
        [ "endpoint" .= String "/Users",
          "name" .= String "User",
          "schema" .= String "urn:ietf:params:scim:schemas:core:2.0:User",
          "schemaExtensions" .= [object ["schema" .= String "urn:example:X", "required" .= True]]
        ]

genResource :: Gen Resource
genResource =
  Resource
    <$> Gen.element ["name1", "name2", "name3"]
    <*> genUri
    <*> genSchema
    <*> Gen.list (Range.linear 0 3) genSchemaExtension

genSchemaExtension :: Gen SchemaExtension
genSchemaExtension =
  SchemaExtension <$> genSchema <*> Gen.bool

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
