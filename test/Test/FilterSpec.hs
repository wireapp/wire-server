{-# LANGUAGE QuasiQuotes #-}

module Test.FilterSpec (spec) where

import           Web.Scim.Filter

import           Test.Hspec
import           HaskellWorks.Hspec.Hedgehog
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

spec :: Spec
spec = do
  context "parsing:" $ do
    it "parse . render === id" $ require $ property $ do
      filter_ <- forAll genFilter
      parseFilter (renderFilter filter_) === Right filter_

----------------------------------------------------------------------------
-- Generators

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
genCompareOp = Gen.element
  [ OpEq, OpNe, OpCo, OpSw, OpEw, OpGt, OpGe, OpLt, OpLe ]

genAttribute :: Gen Attribute
genAttribute = Gen.element
  [ AttrUserName ]

genFilter :: Gen Filter
genFilter = Gen.choice
  [ FilterAttrCompare <$> genAttribute <*> genCompareOp <*> genCompValue
  ]
