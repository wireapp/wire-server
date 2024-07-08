{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Data.List1 where

import Cassandra
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as N
import Data.OpenApi qualified as Swagger
import Data.Schema as S
import Imports
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()

newtype List1 a = List1
  { toNonEmpty :: NonEmpty a
  }
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad, Semigroup, Arbitrary)
  deriving (FromJSON, ToJSON, Swagger.ToSchema) via S.Schema (List1 a)

infixr 5 <|

singleton :: a -> List1 a
singleton a = List1 $ (N.:|) a []
{-# INLINE singleton #-}

list1 :: a -> [a] -> List1 a
list1 a = List1 . (N.:|) a
{-# INLINE list1 #-}

maybeList1 :: [a] -> Maybe (List1 a)
maybeList1 = fmap List1 . N.nonEmpty
{-# INLINE maybeList1 #-}

(<|) :: a -> List1 a -> List1 a
(<|) a = List1 . (N.<|) a . toNonEmpty
{-# INLINE (<|) #-}

cons :: a -> List1 a -> List1 a
cons = (<|)
{-# INLINE cons #-}

head :: List1 a -> a
head = N.head . toNonEmpty
{-# INLINE head #-}

instance (ToSchema a) => ToSchema (List1 a) where
  schema =
    named "List1" $
      toNonEmpty S..= fmap List1 (nonEmptyArray S.schema)

instance Swagger.ToParamSchema (List1 a) where
  toParamSchema _ =
    mempty
      { Swagger._schemaType = Just Swagger.OpenApiArray,
        Swagger._schemaMinLength = Just 1
      }

instance (Cql a) => Cql (List1 a) where
  ctype = Tagged (ListColumn (untag (ctype :: Tagged a ColumnType)))

  toCql = CqlList . map toCql . toList

  fromCql (CqlList []) = Left "At least 1 element in list required."
  fromCql (CqlList l) = List1 . N.fromList <$> mapM fromCql l
  fromCql _ = Left "Expected CqlList."
