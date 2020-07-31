{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

-- | Text containing (extensible) subsets of the ASCII character set,
-- captured in distinct types.
module Data.Text.Ascii
  ( AsciiText,
    toText,
    fromAsciiChars,
    AsciiChar,
    toChar,
    fromChar,
    AsciiChars (Subset, validate, contains),

    -- * Standard Characters
    Standard (..),
    Ascii,
    validateStandard,

    -- * Printable Characters
    Printable (..),
    AsciiPrintable,
    validatePrintable,

    -- * Base64 Characters
    Base64 (..),
    AsciiBase64,
    validateBase64,
    encodeBase64,
    decodeBase64,

    -- * Url-Safe Base64 Characters
    Base64Url (..),
    AsciiBase64Url,
    validateBase64Url,
    encodeBase64Url,
    decodeBase64Url,

    -- * Base16 (Hex) Characters
    Base16 (..),
    AsciiBase16,
    validateBase16,
    encodeBase16,
    decodeBase16,

    -- * Safe Widening
    widen,
    widenChar,

    -- * Unsafe Construction
    unsafeFromText,
    unsafeFromByteString,
  )
where

import Cassandra hiding (Ascii)
import Data.Aeson
import Data.Attoparsec.ByteString (Parser)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64Url
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import Data.Hashable (Hashable)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeLatin1, decodeUtf8')
import Imports
import Test.QuickCheck (Arbitrary (arbitrary), listOf, suchThatMap)
import Test.QuickCheck.Instances ()

-- | 'AsciiText' is text that is known to contain only the subset
-- of ASCII characters indicated by its character set @c@.
newtype AsciiText c = AsciiText {toText :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid, NFData, ToByteString, FromJSONKey, ToJSONKey, Hashable)

newtype AsciiChar c = AsciiChar {toChar :: Char}
  deriving stock (Eq, Ord, Show)

-- | Class of types representing subsets of ASCII characters.
class AsciiChars c where
  -- | Type-level subset relations between ASCII character sets.
  type Subset c c' :: Bool

  -- | Validate that all characters in a 'Text' are contained in
  -- the character set. Instances should ensure that
  --
  --      @validate ('toText' a) == Right ('widen' a :: 'Ascii')@
  --
  -- holds for any @a :: AsciiText c@.
  validate :: Text -> Either String (AsciiText c)

  -- | Check whether a character is in the character set.
  -- Instances should ensure that
  --
  --      @contains c a ==> contains 'Standard' a@
  --
  -- holds for any @a :: Char@.
  contains :: c -> Char -> Bool

-- | Note: Assumes UTF8 encoding. If the bytestring is known to
-- be in a different encoding, 'validate' the text after decoding it with
-- the correct encoding instead of using this instance.
instance AsciiChars c => FromByteString (AsciiText c) where
  parser = parseBytes validate

-- | Note: 'fromString' is a partial function that will 'error' when given
-- a string containing characters not in the set @c@. It is only intended to be used
-- via the @OverloadedStrings@ extension, i.e. for known ASCII string literals.
instance AsciiChars c => IsString (AsciiText c) where
  fromString = unsafeString validate

instance ToJSON (AsciiText r) where
  toJSON = String . toText

instance AsciiChars c => FromJSON (AsciiText c) where
  parseJSON = withText "ASCII" $ either fail pure . validate

instance AsciiChars c => Cql (AsciiText c) where
  ctype = Tagged AsciiColumn
  toCql = CqlAscii . toText
  fromCql = fmap (unsafeFromText . fromAscii) . fromCql

fromAsciiChars :: AsciiChars c => [AsciiChar c] -> AsciiText c
fromAsciiChars = fromString . map toChar

fromChar :: AsciiChars c => c -> Char -> Maybe (AsciiChar c)
fromChar c char
  | contains c char = Just (AsciiChar char)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Standard

-- | The standard ASCII character set.
data Standard = Standard

type Ascii = AsciiText Standard

-- we could write a generic instance for AsciiChar if we didn't have to pass
-- 'Standard' on the value level here. Could be a 'Proxy' instead.
instance Arbitrary (AsciiChar Standard) where
  arbitrary = arbitrary `suchThatMap` fromChar Standard

instance Arbitrary (AsciiText Standard) where
  arbitrary = fromAsciiChars <$> listOf arbitrary

instance AsciiChars Standard where
  type Subset Standard Standard = 'True
  validate = check "Invalid ASCII characters" (contains Standard)
  contains Standard = isAscii
  {-# INLINE contains #-}

validateStandard :: Text -> Either String Ascii
validateStandard = validate

--------------------------------------------------------------------------------
-- Printable

-- | The character set of all printable ASCII characters.
data Printable = Printable

type AsciiPrintable = AsciiText Printable

instance Arbitrary (AsciiChar Printable) where
  arbitrary = arbitrary `suchThatMap` fromChar Printable

instance Arbitrary (AsciiText Printable) where
  arbitrary = fromAsciiChars <$> listOf arbitrary

instance AsciiChars Printable where
  type Subset Printable Printable = 'True
  type Subset Printable Standard = 'True
  validate = check "Invalid printable ASCII characters" (contains Printable)
  contains Printable c = isAscii c && isPrint c
  {-# INLINE contains #-}

validatePrintable :: Text -> Either String AsciiPrintable
validatePrintable = validate

--------------------------------------------------------------------------------
-- Base64

-- | The character set used in base-64 encoding.
--
-- Note: That a text contains only characters of the base-64 character set
-- does not imply that it is a valid base-64 /encoding/, i.e. it might
-- have intermittent padding characters or might not be a multiple of
-- 4 bytes in length.
data Base64 = Base64

type AsciiBase64 = AsciiText Base64

instance Arbitrary (AsciiChar Base64) where
  arbitrary = arbitrary `suchThatMap` fromChar Base64

instance Arbitrary (AsciiText Base64) where
  arbitrary = encodeBase64 <$> arbitrary

instance AsciiChars Base64 where
  type Subset Base64 Standard = 'True
  type Subset Base64 Printable = 'True
  type Subset Base64 Base64 = 'True
  validate = check "Invalid base-64 characters" (contains Base64)
  contains Base64 c =
    isAsciiLower c
      || isAsciiUpper c
      || isDigit c
      || c == '+'
      || c == '/'
      || c == '='
  {-# INLINE contains #-}

validateBase64 :: Text -> Either String AsciiBase64
validateBase64 = validate

-- | Encode a bytestring into a text containing only base-64 characters.
-- The resulting text is always a valid encoding and a multiple of 4 bytes
-- in length.
encodeBase64 :: ByteString -> AsciiBase64
encodeBase64 = unsafeFromByteString . B64.encode

-- | Decode a text containing only base-64 characters.
-- Decoding only succeeds if the text is a valid encoding and
-- a multiple of 4 bytes in length.
decodeBase64 :: AsciiBase64 -> Maybe ByteString
decodeBase64 = either (const Nothing) Just . B64.decode . toByteString'

--------------------------------------------------------------------------------
-- Base64Url

-- | The character set used in url-safe base64-encoding.
--
-- Note: That a text contains only characters of the url-safe base-64 character
-- set does not imply that it is a valid url-safe base-64 /encoding/, i.e.
-- it might have intermittent padding characters or might not be a multiple of
-- 4 bytes in length.
data Base64Url = Base64Url

type AsciiBase64Url = AsciiText Base64Url

instance Arbitrary (AsciiChar Base64Url) where
  arbitrary = arbitrary `suchThatMap` fromChar Base64Url

instance Arbitrary (AsciiText Base64Url) where
  arbitrary = encodeBase64Url <$> arbitrary

instance AsciiChars Base64Url where
  type Subset Base64Url Standard = 'True
  type Subset Base64Url Printable = 'True
  type Subset Base64Url Base64Url = 'True
  validate = check "Invalid url-safe base-64 characters" (contains Base64Url)
  contains Base64Url c =
    isAsciiLower c
      || isAsciiUpper c
      || isDigit c
      || c == '-'
      || c == '_'
      || c == '='
  {-# INLINE contains #-}

validateBase64Url :: Text -> Either String AsciiBase64Url
validateBase64Url = validate

-- | Encode a bytestring into a text containing only url-safe
-- base-64 characters. The resulting text is always a valid
-- encoding and a multiple of 4 bytes in length.
encodeBase64Url :: ByteString -> AsciiBase64Url
encodeBase64Url = unsafeFromByteString . B64Url.encode

-- | Decode a text containing only url-safe base-64 characters.
-- Decoding only succeeds if the text is a valid encoding and
-- a multiple of 4 bytes in length.
decodeBase64Url :: AsciiBase64Url -> Maybe ByteString
decodeBase64Url = either (const Nothing) Just . B64Url.decode . toByteString'

--------------------------------------------------------------------------------
-- Base16

-- | The character set used in base16 (aka hex) encoding.
data Base16 = Base16

type AsciiBase16 = AsciiText Base16

instance Arbitrary (AsciiChar Base16) where
  arbitrary = arbitrary `suchThatMap` fromChar Base16

instance Arbitrary (AsciiText Base16) where
  arbitrary = encodeBase16 <$> arbitrary

instance AsciiChars Base16 where
  type Subset Base16 Standard = 'True
  type Subset Base16 Printable = 'True
  type Subset Base16 Base64 = 'True
  type Subset Base16 Base64Url = 'True
  type Subset Base16 Base16 = 'True
  validate = check "Invalid base-16 (hex) characters" (contains Base16)
  contains Base16 = isHexDigit
  {-# INLINE contains #-}

validateBase16 :: Text -> Either String AsciiBase16
validateBase16 = validate

-- | Encode a bytestring into a text containing only hex characters.
-- The resulting text is always a multiple of 2 bytes in length.
encodeBase16 :: ByteString -> AsciiBase16
encodeBase16 = unsafeFromByteString . B16.encode

-- | Decode a text containing only hex characters.
-- Decoding only succeeds if the text is a multiple of 2 bytes in length.
decodeBase16 :: AsciiBase16 -> Maybe ByteString
decodeBase16 t = case B16.decode (toByteString' t) of
  (b, r) | r == mempty -> Just b
  (_, _) -> Nothing

--------------------------------------------------------------------------------
-- Safe Widening

-- | Safely widen an ASCII text into another ASCII text with a larger
-- character set.
widen :: (Subset c c' ~ 'True) => AsciiText c -> AsciiText c'
widen (AsciiText t) = AsciiText t

-- | Safely widen an ASCII character into another ASCII character with a larger
-- character set.
widenChar :: (Subset c c' ~ 'True) => AsciiChar c -> AsciiChar c'
widenChar (AsciiChar t) = AsciiChar t

--------------------------------------------------------------------------------
-- Unsafe Construction

-- | Construct 'AsciiText' from a known ASCII 'Text'.
-- This is a total function but unsafe because the text is not checked
-- for non-ASCII characters.
unsafeFromText :: AsciiChars c => Text -> AsciiText c
unsafeFromText = AsciiText

-- | Construct 'AsciiText' from a known ASCII 'ByteString'.
-- This is a total function but unsafe because the bytestring is not checked
-- for non-ASCII characters.
unsafeFromByteString :: AsciiChars c => ByteString -> AsciiText c
unsafeFromByteString = AsciiText . decodeLatin1

--------------------------------------------------------------------------------
-- Internal

check :: String -> (Char -> Bool) -> Text -> Either String (AsciiText c)
check m f t
  | Text.all f t = Right (AsciiText t)
  | otherwise = Left m

parseBytes :: (Text -> Either String a) -> Parser a
parseBytes f =
  parser >>= \bs ->
    case decodeUtf8' bs of
      Left _ -> fail $ "Invalid ASCII characters in: " ++ C8.unpack bs
      Right t -> case f t of
        Left e -> fail $ e ++ ": " ++ Text.unpack t
        Right a -> pure a

unsafeString :: (Text -> Either String a) -> String -> a
unsafeString f s = case f (Text.pack s) of
  Right a -> a
  Left e -> error $ "Data.Text.Ascii.fromString: " ++ e ++ ": " ++ s
