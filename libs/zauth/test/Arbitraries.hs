{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitraries where

import Control.Lens ((.~))
import Data.UUID hiding (fromString)
import Data.ZAuth.Token
import Imports
import Sodium.Crypto.Sign
import Test.Tasty.QuickCheck

instance Arbitrary (Token Access) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ A) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token User) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ U) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token Bot) where
  arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token Provider) where
  arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token LegalHoldAccess) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ LA) <$> arbitrary) <*> arbitrary

instance Arbitrary (Token LegalHoldUser) where
  arbitrary = mkToken <$> arbitrary <*> ((typ .~ LU) <$> arbitrary) <*> arbitrary

instance Arbitrary Header where
  arbitrary =
    mkHeader
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

instance Arbitrary Access where
  arbitrary = mkAccess <$> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = mkUser <$> arbitrary <*> arbitrary

instance Arbitrary Bot where
  arbitrary = mkBot <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Provider where
  arbitrary = mkProvider <$> arbitrary

instance Arbitrary LegalHoldAccess where
  arbitrary = mkLegalHoldAccess <$> arbitrary <*> arbitrary

instance Arbitrary LegalHoldUser where
  arbitrary = mkLegalHoldUser <$> arbitrary <*> arbitrary

instance Arbitrary ByteString where
  arbitrary = fromString <$> arbitrary `suchThat` (not . any (== '.'))

instance Arbitrary Signature where
  arbitrary = Signature <$> arbitrary

instance Arbitrary Type where
  arbitrary = elements [A, U, LA, LU]

instance Arbitrary Tag where
  arbitrary = return S

instance Bounded UUID where
  minBound = nil
  maxBound = fromJust $ fromASCIIBytes "ffffffff-ffff-4fff-ffff-ffffffffffff"

instance Arbitrary UUID where
  arbitrary = arbitraryBoundedRandom
