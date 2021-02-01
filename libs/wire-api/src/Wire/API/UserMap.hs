{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.API.UserMap where

import Data.Aeson (FromJSON, ToJSON)
import Data.Domain (Domain)
import Data.Id (UserId)
import Data.Swagger (ToSchema)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Wire.API.Arbitrary (mapOf')

newtype UserMap a = UserMap {userMap :: Map UserId a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, ToSchema)

instance Arbitrary a => Arbitrary (UserMap a) where
  arbitrary = UserMap <$> mapOf' arbitrary arbitrary

newtype QualifiedUserMap a = QualifiedUserMap
  { qualifiedUserMap :: Map Domain (UserMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, ToSchema)

instance Arbitrary a => Arbitrary (QualifiedUserMap a) where
  arbitrary = QualifiedUserMap <$> mapOf' arbitrary arbitrary
