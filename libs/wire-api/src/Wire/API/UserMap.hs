{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.UserMap where

import Control.Lens ((?~), (^.))
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Domain (Domain)
import Data.Id (UserId)
import Data.Map qualified as Map
import Data.OpenApi (HasDescription (description), HasExample (example), NamedSchema (..), ToSchema (..), declareSchema, toSchema)
import Data.Proxy (Proxy (..))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Typeable (typeRep)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Wire.API.Wrapped (Wrapped)
import Wire.Arbitrary (generateExample, mapOf')

newtype UserMap a = UserMap {userMap :: Map UserId a}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON, Functor)

instance (Arbitrary a) => Arbitrary (UserMap a) where
  arbitrary = UserMap <$> mapOf' arbitrary arbitrary

type WrappedQualifiedUserMap a = Wrapped "qualified_user_map" (QualifiedUserMap a)

newtype QualifiedUserMap a = QualifiedUserMap
  { qualifiedUserMap :: Map Domain (UserMap a)
  }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid, ToJSON, FromJSON)

instance Functor QualifiedUserMap where
  fmap f (QualifiedUserMap qMap) =
    QualifiedUserMap $ f <$$> qMap

instance (Arbitrary a) => Arbitrary (QualifiedUserMap a) where
  arbitrary = QualifiedUserMap <$> mapOf' arbitrary arbitrary

instance (ToSchema a, ToJSON a, Arbitrary a) => ToSchema (UserMap (Set a)) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map UserId (Set a)))
    let valueTypeName = Text.pack $ show $ typeRep $ Proxy @a
    pure $
      NamedSchema (Just $ "UserMap_Set_" <> valueTypeName) $
        mapSch
          & description ?~ "Map of UserId to (Set " <> valueTypeName <> ")"
          & example ?~ toJSON (Map.singleton (generateExample @UserId) (Set.singleton (generateExample @a)))

instance (Typeable a, ToSchema (UserMap a)) => ToSchema (QualifiedUserMap a) where
  declareNamedSchema _ = do
    mapSch <- declareSchema (Proxy @(Map Domain (UserMap a)))
    let userMapSchema = toSchema (Proxy @(UserMap a))
    let valueTypeName = Text.replace " " "_" . Text.pack $ show $ typeRep $ Proxy @a
    pure $
      NamedSchema (Just $ "QualifiedUserMap_" <> valueTypeName) $
        mapSch
          & description ?~ "Map of Domain to (UserMap (" <> valueTypeName <> "))."
          & example
            ?~ toJSON
              (Map.singleton ("domain1.example.com" :: Text) (userMapSchema ^. example))
