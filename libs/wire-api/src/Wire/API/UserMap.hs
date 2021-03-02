{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wire.API.UserMap where

import Control.Lens ((?~), (^.))
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Domain (Domain)
import Data.Id (UserId)
import qualified Data.Map as Map
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Swagger (HasDescription (description), HasExample (example), NamedSchema (..), ToSchema (..), declareSchema, toSchema)
import qualified Data.Text as Text
import Data.Typeable (typeRep)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Wire.API.Arbitrary (generateExample, mapOf')

newtype UserMap a = UserMap {userMap :: Map UserId a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, Functor)

instance Arbitrary a => Arbitrary (UserMap a) where
  arbitrary = UserMap <$> mapOf' arbitrary arbitrary

newtype QualifiedUserMap a = QualifiedUserMap
  { qualifiedUserMap :: Map Domain (UserMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Functor QualifiedUserMap where
  fmap f (QualifiedUserMap qMap) =
    QualifiedUserMap $ f <$$> qMap

instance Arbitrary a => Arbitrary (QualifiedUserMap a) where
  arbitrary = QualifiedUserMap <$> mapOf' arbitrary arbitrary

instance (Typeable a, ToSchema a, ToJSON a, Arbitrary a) => ToSchema (UserMap (Set a)) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map UserId (Set a)))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    return $
      NamedSchema (Just $ "UserMap (Set " <> valueTypeName <> ")") $
        mapSch
          & description ?~ "Map of UserId to (Set " <> valueTypeName <> ")"
          & example ?~ toJSON (Map.singleton (generateExample @UserId) (Set.singleton (generateExample @a)))

instance (Typeable a, ToSchema (UserMap a)) => ToSchema (QualifiedUserMap a) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map Domain (UserMap a)))
    let userMapSchema = toSchema (Proxy @(UserMap a))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    return $
      NamedSchema (Just $ "QualifiedUserMap (" <> valueTypeName <> ")") $
        mapSch
          & description ?~ "Map of Domain to (UserMap (" <> valueTypeName <> "))."
          & example
            ?~ toJSON
              (Map.singleton ("domain1.example.com" :: Text) (userMapSchema ^. example))
