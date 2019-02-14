{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Range
    ( Range
    , LTE
    , Within
    , Bounds (..)
    , checked
    , checkedEither
    , checkedEitherMsg
    , errorMsg
    , unsafeRange
    , fromRange
    , rcast
    , rnil
    , rcons, (<|)
    , rinc
    , rappend
    , rsingleton
    ) where

import Imports
import Data.Aeson
import Data.Aeson.Types as Aeson
import Data.ByteString.Conversion
import Data.List.NonEmpty (NonEmpty)
import Data.List1 (List1, toNonEmpty)
import Data.Sequence (Seq)
import Data.Singletons.Prelude.Num
import Data.Singletons
import Data.Singletons.Prelude.Ord
import Data.Singletons.TypeLits
import Data.Text.Ascii (AsciiText)
#ifdef WITH_CQL
import Database.CQL.Protocol hiding (Set, Map)
#endif
import Numeric.Natural

import qualified Data.Attoparsec.ByteString as Atto
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.HashSet               as HashSet
import qualified Data.List.NonEmpty         as N
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Ascii            as Ascii
import qualified Data.Text.Lazy             as TL
import qualified Data.Set                   as Set
import qualified Data.Sequence              as Seq

-----------------------------------------------------------------------------

newtype Range (n :: Nat) (m :: Nat) a = Range
    { fromRange :: a
    } deriving (Eq, Ord, Show)

instance NFData (Range n m a) where rnf (Range a) = seq a ()

instance ToJSON a => ToJSON (Range n m a) where
    toJSON = toJSON . fromRange

instance (Within a n m, FromJSON a) => FromJSON (Range n m a) where
    parseJSON v = parseJSON v >>= maybe (msg sing sing) return . checked
      where
        msg :: Bounds a => SNat n -> SNat m -> Aeson.Parser (Range n m a)
        msg sn sm = fail (errorMsg (fromSing sn) (fromSing sm) "")

#ifdef WITH_CQL
instance (Within a n m, Cql a) => Cql (Range n m a) where
    ctype = retag (ctype :: Tagged a ColumnType)
    toCql = toCql . fromRange
    fromCql c = fromCql c >>= maybe (msg sing sing) return . checked
      where
        msg :: Bounds a => SNat n -> SNat m -> Either String (Range n m a)
        msg sn sm = Left (errorMsg (fromSing sn) (fromSing sm) "")
#endif

type LTE (n :: Nat) (m :: Nat)      = (SingI n, SingI m, (n <= m) ~ 'True)
type Within a (n :: Nat) (m :: Nat) = (Bounds a, LTE n m)

mk :: Bounds a => a -> SNat n -> SNat m -> Maybe (Range n m a)
mk a sn sm =
    let n = fromSing sn
        m = fromSing sm
    in if within a (toInteger n) (toInteger m)
           then Just (Range a)
           else Nothing

checked :: Within a n m => a -> Maybe (Range n m a)
checked x = mk x sing sing

errorMsg :: (Show a, Show b) => a -> b -> ShowS
errorMsg n m = showString "outside range ["
             . shows n
             . showString ", "
             . shows m
             . showString "]"

checkedEitherMsg :: forall a n m.  Within a n m => String -> a -> Either String (Range n m a)
checkedEitherMsg msg x = do
    let sn = sing :: SNat n 
        sm = sing :: SNat m
    case mk x sn sm of
        Nothing -> Left $ showString msg . showString ": " . errorMsg (fromSing sn) (fromSing sm) $ ""
        Just  r -> Right r

checkedEither :: forall a n m . Within a n m => a -> Either String (Range n m a)
checkedEither x = do
    let sn = sing :: SNat n
        sm = sing :: SNat m
    case mk x sn sm of
        Nothing -> Left (errorMsg (fromSing sn) (fromSing sm) "")
        Just  r -> Right r

unsafeRange :: (Show a, Within a n m) => a -> Range n m a
unsafeRange x = fromMaybe (msg sing sing) (checked x)
  where
    msg :: SNat n -> SNat m -> Range n m a
    msg sn sm = error
            . shows x
            . showString " "
            . errorMsg (fromSing sn) (fromSing sm)
            $ ""

rcast :: (LTE n m, (m <= m') ~ 'True, (n >= n') ~ 'True) => Range n m a -> Range n' m' a
rcast (Range a) = Range a

rnil :: Monoid a => Range 0 0 a
rnil = Range mempty

rcons, (<|) :: LTE n m => a -> Range n m [a] -> Range n (m + 1) [a]
rcons a (Range aa) = Range (a:aa)

infixr 5 <|
(<|) = rcons

rinc :: (Integral a, LTE n m ) => Range n m a -> Range n (m + 1) a
rinc (Range a) = Range (a + 1)

rappend :: (LTE n m, LTE n' m', Monoid a) => Range n m a -> Range n' m' a -> Range n (m + m') a
rappend (Range a) (Range b) = Range (a <> b)

rsingleton :: a -> Range 1 1 [a]
rsingleton = Range . pure

-----------------------------------------------------------------------------

class Bounds a where
    within :: a -> Integer -> Integer -> Bool

rangeCheck :: (Integral a, Integral x, Integral y) => a -> x -> y -> Bool
rangeCheck a x y = a >= fromIntegral x && a <= fromIntegral y
{-# INLINE rangeCheck #-}

instance Bounds Integer where within = rangeCheck
instance Bounds Int     where within = rangeCheck
instance Bounds Int8    where within = rangeCheck
instance Bounds Int16   where within = rangeCheck
instance Bounds Int32   where within = rangeCheck
instance Bounds Int64   where within = rangeCheck
instance Bounds Natural where within = rangeCheck
instance Bounds Word    where within = rangeCheck
instance Bounds Word8   where within = rangeCheck
instance Bounds Word16  where within = rangeCheck
instance Bounds Word32  where within = rangeCheck
instance Bounds Word64  where within = rangeCheck

instance Bounds T.Text where
    within x y z = rangeCheck (T.length (T.take (fromIntegral z + 1) x)) y z

instance Bounds TL.Text where
    within x y z = rangeCheck (TL.length (TL.take (fromIntegral z + 1) x)) y z

instance Bounds B.ByteString where
    within x = rangeCheck (B.length x)

instance Bounds BL.ByteString where
    within x y z =  rangeCheck (BL.length (BL.take (fromIntegral z + 1) x)) y z

instance Bounds [a] where
    within x y z = rangeCheck (length (take (fromIntegral z + 1) x)) y z

instance Bounds (NonEmpty a) where
    within x y z = rangeCheck (length (N.take (fromIntegral z + 1) x)) y z

instance Bounds (List a) where
    within x = within (fromList x)

instance Bounds (List1 a) where
    within x = within (toNonEmpty x)

instance Bounds (Set a) where
    within x y z = rangeCheck (Set.size x) y z

instance Bounds (Seq a) where
    within x y z = rangeCheck (Seq.length x) y z

instance Bounds (Map k a) where
    within x y z = rangeCheck (Map.size x) y z

instance Bounds (HashMap k a) where
    within x y z = rangeCheck (length (take (fromIntegral z + 1) (HashMap.toList x))) y z

instance Bounds (HashSet a) where
    within x y z = rangeCheck (length (take (fromIntegral z + 1) (HashSet.toList x))) y z

instance Bounds a => Bounds (Maybe a) where
    within Nothing  _ _ = True
    within (Just x) y z = within x y z

instance Bounds (AsciiText r) where
    within x y z = within (Ascii.toText x) y z

-----------------------------------------------------------------------------

instance (Within a n m, Read a) => Read (Range n m a) where
    readsPrec p s = fromMaybe [] $ foldr f (Just []) (readsPrec p s)
      where
        f :: (Within a n m, Read a) => (a, String) -> Maybe [(Range n m a, String)] -> Maybe [(Range n m a, String)]
        f _      Nothing    = Nothing
        f (a, t) (Just acc) = (\a' -> (a',t):acc) <$> checked a

-----------------------------------------------------------------------------

instance (Within a n m, FromByteString a) => FromByteString (Range n m a) where
    parser = parser >>= maybe (msg sing sing) return . checked
      where
        msg :: Bounds a => SNat n -> SNat m -> Atto.Parser (Range n m a)
        msg sn sm = fail (errorMsg (fromSing sn) (fromSing sm) "")

instance ToByteString a => ToByteString (Range n m a) where
    builder = builder . fromRange
