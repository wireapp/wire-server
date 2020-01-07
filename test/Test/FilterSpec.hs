{-# LANGUAGE QuasiQuotes #-}

module Test.FilterSpec where

import           Web.Scim.Filter
import           Web.Scim.AttrName
import           Web.Scim.Schema.Schema (Schema(..))

import           Test.Hspec
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.Text (cons)


prop_roundtrip :: Property
prop_roundtrip = property $ do
  x <- forAll genFilter
  tripping x renderFilter parseFilter

spec :: Spec
spec = do
  describe "Filter" $ do
    it "parse . render === id" $ require $ prop_roundtrip

----------------------------------------------------------------------------
-- Generators

genValuePath :: Gen ValuePath
genValuePath  = ValuePath <$> genAttrPath <*> genFilter

genCompValue :: Gen CompValue
genCompValue = Gen.choice
  [ pure ValNull
  , ValBool <$> Gen.bool
  , ValNumber <$> Gen.choice
      [ Gen.realFrac_ (Range.constantFrom 0 (-100) 100)
      , fromInteger <$> Gen.integral (Range.constantFrom 0 (-100) 100)
      ]
  , ValString <$> Gen.text (Range.constant 0 1000) Gen.unicode
  ]

genCompareOp :: Gen CompareOp
genCompareOp = Gen.enumBounded

genSubAttr :: Gen SubAttr
genSubAttr = SubAttr <$> genAttrName

-- | FUTUREWORK: no support for custom schemas.
--
-- FUTUREWORK: we also may want to factor a bounded enum type out of the 'Schema' type for
-- this: @data Schema = Buitin BuitinSchema | Custom Text; data BuiltinSchema = ... deriving
-- (Bounded, Enum, ...)@
genSchema :: Gen Schema
genSchema = Gen.element [ServiceProviderConfig20, Group20, Schema20, ResourceType20, ListResponse20, Error20, PatchOp20]

genAttrPath :: Gen AttrPath
genAttrPath = AttrPath <$> Gen.maybe genSchema <*> genAttrName <*> Gen.maybe genSubAttr

genAttrName :: Gen AttrName
genAttrName = AttrName <$> (cons <$> Gen.alpha <*> Gen.text (Range.constant 0 50) (Gen.choice [Gen.alphaNum, Gen.constant '-', Gen.constant '_']))

genFilter :: Gen Filter
genFilter = Gen.choice
  [ FilterAttrCompare <$> genAttrPath <*> genCompareOp <*> genCompValue
  ]
