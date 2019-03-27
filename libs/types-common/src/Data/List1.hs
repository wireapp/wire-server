{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data.List1 where

import Imports
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson
#ifdef WITH_CQL
import Cassandra (Cql, Tagged(Tagged), ColumnType(ListColumn), Value(CqlList), ctype, toCql, fromCql, untag)
#endif

import qualified Data.List.NonEmpty as N
import qualified Data.Vector        as V

newtype List1 a = List1
    { toNonEmpty :: NonEmpty a
    } deriving ( Monad
               , Functor
               , Applicative
               , Foldable
               , Traversable
               , Eq
               , Ord
               , Read
               , Show
               , Semigroup )

infixr 5 <|

singleton :: a -> List1 a
singleton a = List1 $ (N.:|) a []
{-# INLINE singleton #-}

list1 :: a -> [a] -> List1 a
list1 a = List1 . (N.:|) a
{-# INLINE list1 #-}

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
        | otherwise       = fail "At least 1 element in list required."
    parseJSON _           = mzero

#ifdef WITH_CQL
instance (Cql a) => Cql (List1 a) where
    ctype = Tagged (ListColumn (untag (ctype :: Tagged a ColumnType)))

    toCql = CqlList . map toCql . toList

    fromCql (CqlList []) = fail "At least 1 element in list required."
    fromCql (CqlList  l) = List1 . N.fromList <$> mapM fromCql l
    fromCql _            = Left "Expected CqlList."
#endif
