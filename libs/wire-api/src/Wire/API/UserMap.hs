{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.API.UserMap where

import Control.Lens ((?~))
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Domain (Domain)
import Data.Id (UserId)
import Data.Proxy (Proxy (..))
import Data.Swagger (HasDescription (description), HasExample (example), NamedSchema (..), ToSchema (..), declareSchema)
import qualified Data.Text as Text
import Data.Typeable (typeRep)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (Gen (MkGen))
import Test.QuickCheck.Random
import Wire.API.Arbitrary (generateExample, mapOf')

newtype UserMap a = UserMap {userMap :: Map UserId a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (UserMap a) where
  arbitrary = UserMap <$> mapOf' arbitrary arbitrary

newtype QualifiedUserMap a = QualifiedUserMap
  { qualifiedUserMap :: Map Domain (UserMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Arbitrary a => Arbitrary (QualifiedUserMap a) where
  arbitrary = QualifiedUserMap <$> mapOf' arbitrary arbitrary

instance (ToSchema a, ToJSON a, Arbitrary a, Typeable a) => ToSchema (UserMap a) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map UserId a))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    return $
      NamedSchema (Just $ "UserMap (" <> valueTypeName <> ")") $
        mapSch
          & description ?~ "Map of UserId to " <> valueTypeName
          & example ?~ toJSON (generateExample @(UserMap a))

instance (ToSchema a, Typeable a, ToJSON a, Arbitrary a) => ToSchema (QualifiedUserMap a) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map Domain (UserMap a)))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    return $
      NamedSchema (Just $ "QaulifiedUserMap (" <> valueTypeName <> ")") $
        mapSch
          & description ?~ "Map of Domain to (UserMap (" <> valueTypeName <> "))."
          & example ?~ toJSON (generateExample @(QualifiedUserMap a))
