{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

module Data.List1 where

import Cassandra
import Data.Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified Data.Vector as V
import Imports
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()

newtype List1 a = List1
  { toNonEmpty :: NonEmpty a
  }
  deriving stock (Eq, Ord, Read, Show, Functor, Foldable, Traversable)
  deriving newtype (Applicative, Monad, Semigroup, Arbitrary)

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

instance ToJSON a => ToJSON (List1 a) where
  toJSON = toJSON . toList
  toEncoding = toEncoding . toList

instance FromJSON a => FromJSON (List1 a) where
  parseJSON a@(Array v)
    | V.length v >= 1 = List1 . N.fromList <$> parseJSON a
    | otherwise = fail "At least 1 element in list required."
  parseJSON _ = mzero

instance (Cql a) => Cql (List1 a) where
  ctype = Tagged (ListColumn (untag (ctype :: Tagged a ColumnType)))

  toCql = CqlList . map toCql . toList

  fromCql (CqlList []) = Left "At least 1 element in list required."
  fromCql (CqlList l) = List1 . N.fromList <$> mapM fromCql l
  fromCql _ = Left "Expected CqlList."

-----------------------------------------------------------------------------
-- A type for non-empty lists that tracks origin

-- | This data type is useful in conversation member adding and removal (one
-- list for local users and the other one for remote users). It ensures there
-- will be at least one element to work with.
data List1WithOrigin a b
  = OnlyFirstList (List1 a)
  | OnlySecondList (List1 b)
  | BothLists (List1 a) (List1 b)

mkList1WithOrigin :: [a] -> [b] -> Maybe (List1WithOrigin a b)
mkList1WithOrigin [] [] = Nothing
mkList1WithOrigin [] (h : t) = Just $ OnlySecondList (list1 h t)
mkList1WithOrigin (h : t) [] = Just $ OnlyFirstList (list1 h t)
mkList1WithOrigin (hl : tl) (hr : tr) =
  Just $
    BothLists (list1 hl tl) (list1 hr tr)

splitList1WithOrigin :: List1WithOrigin a b -> ([a], [b])
splitList1WithOrigin = \case
  OnlyFirstList ls -> (toList ls, [])
  OnlySecondList rs -> ([], toList rs)
  BothLists ls rs -> (toList ls, toList rs)

fromList1WithOrigin ::
  (a -> c) ->
  (b -> c) ->
  List1WithOrigin a b ->
  List1 c
fromList1WithOrigin f g = \case
  OnlyFirstList ls -> fmap f ls
  OnlySecondList rs -> fmap g rs
  BothLists ls rs -> fmap f ls <> fmap g rs
