{-# LANGUAGE StrictData #-}

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

module Data.IdMapping where

import Data.Id
import Data.Qualified
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), oneof)

data MappedOrLocalId a
  = Mapped (IdMapping a)
  | Local (Id a)
  deriving (Show)

opaqueIdFromMappedOrLocal :: MappedOrLocalId a -> Id (Opaque a)
opaqueIdFromMappedOrLocal = \case
  Local localId -> makeIdOpaque localId
  Mapped IdMapping {idMappingLocal} -> makeMappedIdOpaque idMappingLocal

partitionMappedOrLocalIds :: Foldable f => f (MappedOrLocalId a) -> ([Id a], [IdMapping a])
partitionMappedOrLocalIds = foldMap $ \case
  Mapped mapping -> (mempty, [mapping])
  Local localId -> ([localId], mempty)

data IdMapping a
  = IdMapping
      { idMappingLocal :: Id (Mapped a),
        idMappingGlobal :: Qualified (Id a)
      }
  deriving (Show)

----------------------------------------------------------------------
-- ARBITRARY

instance Arbitrary a => Arbitrary (MappedOrLocalId a) where
  arbitrary = oneof [Mapped <$> arbitrary, Local <$> arbitrary]

instance Arbitrary a => Arbitrary (IdMapping a) where
  arbitrary = IdMapping <$> arbitrary <*> arbitrary
