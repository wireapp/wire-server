{-# LANGUAGE StrictData #-}

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
