{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-- Disabling due to the use of LTE and other type level checks
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module Data.Range
  ( Range,
    toRange,
    Within,
    Bounds (..),
    checked,
    checkedEither,
    checkedEitherMsg,
    rangedChunks,
    errorMsg,
    unsafeRange,
    fromRange,
    rangedSchema,
    untypedRangedSchema,
    rcast,
    rnil,
    rcons,
    (<|),
    rinc,
    rappend,
    rsingleton,

    -- * 'Arbitrary' generators
    Ranged (..),
    genRangeList,
    genRangeSet,
    genRangeText,
    genRangeAsciiText,
    genRange,
    genIntegral,
  )
where

import Cassandra (ColumnType, Cql (..), Tagged, retag)
import Control.Lens ((%~), (?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Bifunctor qualified as Bifunctor
import Data.ByteString qualified as B
import Data.ByteString.Conversion
  ( FromByteString (..),
    List (fromList),
    ToByteString (..),
  )
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as N
import Data.List1 (List1, toNonEmpty)
import Data.Map qualified as Map
import Data.OpenApi (Schema, ToParamSchema (..))
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema hiding (Schema)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Ascii (AsciiChar, AsciiChars, AsciiText, fromAsciiChars)
import Data.Text.Ascii qualified as Ascii
import Data.Text.Lazy qualified as TL
import Data.Type.Ord
import GHC.TypeNats
import Imports
import Servant (FromHttpApiData (..))
import System.Random (Random)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen)
import Test.QuickCheck qualified as QC

-----------------------------------------------------------------------------

newtype Range (n :: Nat) (m :: Nat) a = Range
  { fromRange :: a
  }
  deriving (Eq, Ord, Show, Functor)

toRange :: (n <= x, x <= m, KnownNat x, Num a) => Proxy x -> Range n m a
toRange = Range . fromIntegral . natVal

instance (Show a, Num a, Within a n m, KnownNat n, KnownNat m) => Bounded (Range n m a) where
  minBound = unsafeRange (fromKnownNat (Proxy @n) :: a)
  maxBound = unsafeRange (fromKnownNat (Proxy @m) :: a)

instance NFData (Range n m a) where rnf (Range a) = seq a ()

instance ToJSON a => ToJSON (Range n m a) where
  toJSON = toJSON . fromRange

instance forall a n m. (KnownNat n, KnownNat m, Within a n m, FromJSON a) => FromJSON (Range n m a) where
  parseJSON v = parseJSON v >>= maybe msg pure . checked
    where
      msg = fail (errorMsg (natVal (Proxy @n)) (natVal (Proxy @m)) "")

rangedSchema ::
  forall n m d v w a b.
  (KnownNat n, KnownNat m, Within a n m, HasRangedSchemaDocModifier d b) =>
  SchemaP d v w a b ->
  SchemaP d v w a (Range n m b)
rangedSchema sch =
  Range <$> untypedRangedSchema (toInteger (natVal (Proxy @n))) (toInteger (natVal (Proxy @m))) sch

untypedRangedSchema ::
  forall d v w a b.
  (HasRangedSchemaDocModifier d b) =>
  Integer ->
  Integer ->
  SchemaP d v w a b ->
  SchemaP d v w a b
untypedRangedSchema n m sch = (sch `withParser` check) & doc %~ rangedSchemaDocModifier (Proxy @b) n m
  where
    check x =
      x <$ guard (within x n m)
        <|> fail (errorMsg n m "")

class Bounds a => HasRangedSchemaDocModifier d a where
  rangedSchemaDocModifier :: Proxy a -> Integer -> Integer -> d -> d

listRangedSchemaDocModifier :: S.HasSchema d S.Schema => Integer -> Integer -> d -> d
listRangedSchemaDocModifier n m = S.schema %~ ((S.minItems ?~ n) . (S.maxItems ?~ m))

stringRangedSchemaDocModifier :: S.HasSchema d S.Schema => Integer -> Integer -> d -> d
stringRangedSchemaDocModifier n m = S.schema %~ ((S.minLength ?~ n) . (S.maxLength ?~ m))

numRangedSchemaDocModifier :: S.HasSchema d S.Schema => Integer -> Integer -> d -> d
numRangedSchemaDocModifier n m = S.schema %~ ((S.minimum_ ?~ fromIntegral n) . (S.maximum_ ?~ fromIntegral m))

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d [a] where rangedSchemaDocModifier _ = listRangedSchemaDocModifier

-- Sets are similar to lists, so use that as our defininition
instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d (Set a) where rangedSchemaDocModifier _ = listRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Text where rangedSchemaDocModifier _ = stringRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d String where rangedSchemaDocModifier _ = stringRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d (AsciiText c) where rangedSchemaDocModifier _ = stringRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Int where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Int32 where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Integer where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Word where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Word8 where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Word16 where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Word32 where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance S.HasSchema d S.Schema => HasRangedSchemaDocModifier d Word64 where rangedSchemaDocModifier _ = numRangedSchemaDocModifier

instance (KnownNat n, KnownNat m, Within a n m, ToSchema a, HasRangedSchemaDocModifier NamedSwaggerDoc a) => ToSchema (Range n m a) where
  schema = fromRange .= rangedSchema schema

instance forall a n m. (KnownNat n, KnownNat m, Within a n m, Cql a) => Cql (Range n m a) where
  ctype = retag (ctype :: Tagged a ColumnType)
  toCql = toCql . fromRange
  fromCql c = fromCql c >>= maybe msg pure . checked
    where
      msg = Left (errorMsg (natVal (Proxy @n)) (natVal (Proxy @m)) "")

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Integer) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Int) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Int8) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Int16) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Int32) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Int64) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Natural) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Word) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Word8) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Word16) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Word32) where toParamSchema = rangedNumToParamSchema

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m Word64) where toParamSchema = rangedNumToParamSchema

instance (ToParamSchema a, KnownNat n, KnownNat m) => ToParamSchema (Range n m [a]) where
  toParamSchema _ =
    toParamSchema (Proxy @[a])
      & S.minItems ?~ fromKnownNat (Proxy @n)
      & S.maxItems ?~ fromKnownNat (Proxy @m)

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m String) where
  toParamSchema _ =
    toParamSchema (Proxy @String)
      & S.maxLength ?~ fromKnownNat (Proxy @n)
      & S.minLength ?~ fromKnownNat (Proxy @m)

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m T.Text) where
  toParamSchema _ =
    toParamSchema (Proxy @T.Text)
      & S.maxLength ?~ fromKnownNat (Proxy @n)
      & S.minLength ?~ fromKnownNat (Proxy @m)

instance (KnownNat n, KnownNat m) => ToParamSchema (Range n m TL.Text) where
  toParamSchema _ =
    toParamSchema (Proxy @TL.Text)
      & S.maxLength ?~ fromKnownNat (Proxy @n)
      & S.minLength ?~ fromKnownNat (Proxy @m)

instance (KnownNat n, S.ToSchema a, KnownNat m) => S.ToSchema (Range n m a) where
  declareNamedSchema _ =
    S.declareNamedSchema (Proxy @a)

instance (KnownNat n, KnownNat m, Within a n m, FromHttpApiData a) => FromHttpApiData (Range n m a) where
  parseUrlPiece t = do
    unchecked <- parseUrlPiece t
    Bifunctor.first T.pack $ checkedEither @_ @n @m unchecked

type Within a (n :: Nat) (m :: Nat) = (Bounds a, n <= m)

mk :: Bounds a => a -> Nat -> Nat -> Maybe (Range n m a)
mk a n m =
  if within a (toInteger n) (toInteger m)
    then Just (Range a)
    else Nothing

checked :: forall n m a. (KnownNat n, KnownNat m, Within a n m) => a -> Maybe (Range n m a)
checked x = mk x (natVal (Proxy @n)) (natVal (Proxy @m))

errorMsg :: (Show a, Show b) => a -> b -> ShowS
errorMsg n m =
  showString "outside range ["
    . shows n
    . showString ", "
    . shows m
    . showString "]"

checkedEitherMsg :: forall a n m. (KnownNat n, KnownNat m) => Within a n m => String -> a -> Either String (Range n m a)
checkedEitherMsg msg x = do
  let sn = natVal (Proxy @n)
      sm = natVal (Proxy @m)
  case mk x sn sm of
    Nothing -> Left $ showString msg . showString ": " . errorMsg sn sm $ ""
    Just r -> Right r

checkedEither :: forall a n m. (KnownNat n, KnownNat m) => Within a n m => a -> Either String (Range n m a)
checkedEither x = do
  let sn = natVal (Proxy @n)
      sm = natVal (Proxy @m)
  case mk x sn sm of
    Nothing -> Left (errorMsg sn sm "")
    Just r -> Right r

rangedChunks :: forall a n. (Within [a] 1 n, KnownNat n) => [a] -> [Range 1 n [a]]
rangedChunks xs =
  let (headPart, tailPart) = splitAt (fromIntegral (natVal (Proxy @n))) xs
   in -- Since n >= 1, headPart being empty can only be when 'xs' was empty.
      case headPart of
        [] -> []
        _ -> Range headPart : rangedChunks tailPart

unsafeRange :: forall a n m. (Show a, KnownNat n, KnownNat m, Within a n m) => a -> Range n m a
unsafeRange x = fromMaybe msg (checked x)
  where
    msg =
      error
        . shows x
        . showString " "
        . errorMsg (natVal (Proxy @n)) (natVal (Proxy @m))
        $ ""

rcast :: (n <= m, m <= m', n >= n') => Range n m a -> Range n' m' a
rcast (Range a) = Range a

rnil :: Monoid a => Range 0 0 a
rnil = Range mempty

rcons, (<|) :: n <= m => a -> Range n m [a] -> Range n (m + 1) [a]
rcons a (Range aa) = Range (a : aa)

infixr 5 <|

(<|) = rcons

rinc :: (Integral a, n <= m) => Range n m a -> Range n (m + 1) a
rinc (Range a) = Range (a + 1)

rappend :: (n <= m, n' <= m', Monoid a) => Range n m a -> Range n' m' a -> Range n (m + m') a
rappend (Range a) (Range b) = Range (a <> b)

rsingleton :: a -> Range 1 1 [a]
rsingleton = Range . pure

rangedNumToParamSchema :: forall a n m. (ToParamSchema a, Num a, KnownNat n, KnownNat m) => Proxy (Range n m a) -> Schema
rangedNumToParamSchema _ =
  toParamSchema (Proxy @a)
    & S.minimum_ ?~ fromKnownNat (Proxy @n)
    & S.maximum_ ?~ fromKnownNat (Proxy @m)

-----------------------------------------------------------------------------

class Bounds a where
  within :: a -> Integer -> Integer -> Bool

rangeCheck :: (Integral a, Integral x, Integral y) => a -> x -> y -> Bool
rangeCheck a x y = a >= fromIntegral x && a <= fromIntegral y
{-# INLINE rangeCheck #-}

instance Bounds Integer where within = rangeCheck

instance Bounds Int where within = rangeCheck

instance Bounds Int8 where within = rangeCheck

instance Bounds Int16 where within = rangeCheck

instance Bounds Int32 where within = rangeCheck

instance Bounds Int64 where within = rangeCheck

instance Bounds Natural where within = rangeCheck

instance Bounds Word where within = rangeCheck

instance Bounds Word8 where within = rangeCheck

instance Bounds Word16 where within = rangeCheck

instance Bounds Word32 where within = rangeCheck

instance Bounds Word64 where within = rangeCheck

instance Bounds T.Text where
  within x y z = rangeCheck (T.length (T.take (fromIntegral z + 1) x)) y z

instance Bounds TL.Text where
  within x y z = rangeCheck (TL.length (TL.take (fromIntegral z + 1) x)) y z

instance Bounds B.ByteString where
  within x = rangeCheck (B.length x)

instance Bounds BL.ByteString where
  within x y z = rangeCheck (BL.length (BL.take (fromIntegral z + 1) x)) y z

instance Bounds [a] where
  within x y z = rangeCheck (length (take (fromIntegral z + 1) x)) y z

instance Bounds (NonEmpty a) where
  within x y z = rangeCheck (length (N.take (fromIntegral z + 1) x)) y z

instance Bounds (List a) where
  within x = within (fromList x)

instance Bounds (List1 a) where
  within x = within (toNonEmpty x)

instance Bounds (Set a) where
  within x = rangeCheck (Set.size x)

instance Bounds (Seq a) where
  within x = rangeCheck (Seq.length x)

instance Bounds (Map k a) where
  within x = rangeCheck (Map.size x)

instance Bounds (HashMap k a) where
  within x y z = rangeCheck (length (take (fromIntegral z + 1) (HashMap.toList x))) y z

instance Bounds (HashSet a) where
  within x y z = rangeCheck (length (take (fromIntegral z + 1) (HashSet.toList x))) y z

instance Bounds a => Bounds (Maybe a) where
  within Nothing _ _ = True
  within (Just x) y z = within x y z

instance Bounds (AsciiText r) where
  within x = within (Ascii.toText x)

-----------------------------------------------------------------------------

instance (KnownNat n, KnownNat m, Within a n m, Read a) => Read (Range n m a) where
  readsPrec p s = fromMaybe [] $ foldr f (Just []) (readsPrec p s)
    where
      f :: (a, String) -> Maybe [(Range n m a, String)] -> Maybe [(Range n m a, String)]
      f _ Nothing = Nothing
      f (a, t) (Just acc) = (\a' -> (a', t) : acc) <$> checked a

-----------------------------------------------------------------------------

instance (KnownNat n, KnownNat m, Within a n m, FromByteString a) => FromByteString (Range n m a) where
  parser = parser >>= maybe msg pure . checked
    where
      msg = fail (errorMsg (natVal (Proxy @n)) (natVal (Proxy @m)) "")

instance ToByteString a => ToByteString (Range n m a) where
  builder = builder . fromRange

----------------------------------------------------------------------------
-- Arbitrary generators

-- | Similar to 'Range', but we export the constructor, so it can be used with @DerivingVia@.
newtype Ranged m n a = Ranged {fromRanged :: a}
  deriving stock (Show)

instance Arbitrary (Range m n a) => Arbitrary (Ranged m n a) where
  arbitrary = Ranged . fromRange <$> arbitrary @(Range m n a)

instance
  (KnownNat n, KnownNat m, n <= m, Arbitrary a, Show a) =>
  Arbitrary (Range n m [a])
  where
  arbitrary = genRangeList @n @m @a arbitrary

genRangeList ::
  forall (n :: Nat) (m :: Nat) (a :: Type).
  (Show a, KnownNat n, KnownNat m, n <= m) =>
  Gen a ->
  Gen (Range n m [a])
genRangeList = genRange id

instance
  (KnownNat n, KnownNat m, n <= m, Arbitrary a, Show a, Ord a) =>
  Arbitrary (Range n m (Set a))
  where
  arbitrary = genRangeSet @n @m @a arbitrary

-- | This has a risk of not terminating if the set is requested to be bigger
-- than the number of possible distinct values.
-- However, it will only show up while running tests and might indicate deeper
-- problems, so I'd say that's ok.
genRangeSet ::
  forall (n :: Nat) (m :: Nat) (a :: Type).
  (Show a, KnownNat n, KnownNat m, n <= m, Ord a) =>
  Gen a ->
  Gen (Range n m (Set a))
genRangeSet gc =
  (Set.fromList . fromRange <$> genRangeList @n @m @a gc) `QC.suchThatMap` checked

instance (KnownNat n, KnownNat m, n <= m) => Arbitrary (Range n m Text) where
  arbitrary = genRangeText arbitrary

  -- FUTUREWORK: the shrinking could be more general (like genRange) and offer more options
  shrink (fromRange -> txt) = [unsafeRange @Text @n @m $ T.take (fromKnownNat (Proxy @n)) txt]

genRangeText ::
  forall (n :: Nat) (m :: Nat).
  (KnownNat n, KnownNat m, n <= m) =>
  Gen Char ->
  Gen (Range n m Text)
genRangeText = genRange fromString

instance
  (AsciiChars c, KnownNat n, KnownNat m, n <= m, Arbitrary (AsciiChar c)) =>
  Arbitrary (Range n m (AsciiText c))
  where
  arbitrary = genRangeAsciiText (arbitrary @(AsciiChar c))

genRangeAsciiText ::
  forall (n :: Nat) (m :: Nat) (c :: Type).
  (HasCallStack, KnownNat n, KnownNat m, n <= m, AsciiChars c) =>
  Gen (AsciiChar c) ->
  Gen (Range n m (AsciiText c))
genRangeAsciiText = genRange @n @m fromAsciiChars

genRange ::
  forall (n :: Nat) (m :: Nat) (a :: Type) (b :: Type).
  (Show b, Bounds b, KnownNat n, KnownNat m, n <= m) =>
  ([a] -> b) ->
  Gen a ->
  Gen (Range n m b)
genRange pack_ gc =
  unsafeRange @b @n @m . pack_
    <$> grange
      (fromKnownNat (Proxy @n))
      (fromKnownNat (Proxy @m))
      gc
  where
    grange mi ma gelem = (`replicateM` gelem) =<< QC.chooseInt (mi, ma)

instance (KnownNat n, KnownNat m, n <= m) => Arbitrary (Range n m Integer) where
  arbitrary = genIntegral

instance (KnownNat n, KnownNat m, n <= m) => Arbitrary (Range n m Int32) where
  arbitrary = genIntegral

instance (KnownNat n, KnownNat m, n <= m) => Arbitrary (Range n m Word) where
  arbitrary = genIntegral

genIntegral ::
  forall n m i.
  (KnownNat n, KnownNat m, n <= m, Integral i, Show i, Bounds i, Random i) =>
  Gen (Range n m i)
genIntegral = unsafeRange @i @n @m <$> QC.choose (fromKnownNat (Proxy @n), fromKnownNat (Proxy @m))

fromKnownNat :: forall (k :: Nat) (i :: Type). (Num i, KnownNat k) => Proxy k -> i
fromKnownNat p = fromIntegral $ natVal p
