{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Arbitraries where

import Data.ByteString (ByteString)
import Data.String
import Data.Maybe
import Data.UUID hiding (fromString)
import Data.ZAuth.Token
import Sodium.Crypto.Sign
import Test.Tasty.QuickCheck

instance Arbitrary (Token Access) where
    arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token User) where
    arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token Bot) where
    arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Token Provider) where
    arbitrary = mkToken <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Header where
    arbitrary = mkHeader
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

instance Arbitrary ByteString where
    arbitrary = fromString <$> arbitrary `suchThat` (not . any (== '.'))

instance Arbitrary Signature where
    arbitrary = Signature <$> arbitrary

instance Arbitrary Type where
    arbitrary = elements [A, U]

instance Arbitrary Tag where
    arbitrary = return S

instance Bounded UUID where
    minBound = nil
    maxBound = fromJust $ fromASCIIBytes "ffffffff-ffff-4fff-ffff-ffffffffffff"

instance Arbitrary UUID where
    arbitrary = arbitraryBoundedRandom
