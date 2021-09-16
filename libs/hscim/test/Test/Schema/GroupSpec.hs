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

module Test.Schema.GroupSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Text (Text)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec
import Test.Schema.Util (mk_prop_caseInsensitive)
import qualified Web.Scim.Class.Group as GroupClass

prop_roundtrip :: Property
prop_roundtrip = property $ do
  user <- forAll genGroup
  tripping user toJSON fromJSON

spec :: Spec
spec = do
  it "roundtrip" $ do
    require prop_roundtrip
  it "case-insensitive" $ do
    require $ mk_prop_caseInsensitive genGroup
    require $ mk_prop_caseInsensitive genMember

genMember :: Gen GroupClass.Member
genMember =
  GroupClass.Member
    <$> (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> (Gen.text (Range.constant 0 20) Gen.unicode)

genGroup :: Gen GroupClass.Group
genGroup =
  GroupClass.Group
    <$> Gen.list (Range.linear 0 10) genSchema
    <*> (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.list (Range.linear 0 10) genMember

genSchema :: Gen Text
genSchema = Gen.element ["schema1", "schema2", "schema3"]
