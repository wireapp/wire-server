{-# LANGUAGE BinaryLiterals #-}
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

module Wire.API.MLS.Serialisation
  ( ParseMLS (..),
    SerialiseMLS (..),
    VarInt (..),
    parseMLSStream,
    parseMLSVector,
    serialiseMLSVector,
    parseMLSBytes,
    serialiseMLSBytes,
    serialiseMLSBytesLazy,
    parseMLSOptional,
    serialiseMLSOptional,
    parseMLSEnum,
    serialiseMLSEnum,
    MLSEnumError (..),
    fromMLSEnum,
    toMLSEnum',
    toMLSEnum,
    encodeMLS,
    encodeMLS',
    decodeMLS,
    decodeMLS',
    decodeMLSWith,
    decodeMLSWith',
    RawMLS (..),
    rawMLSSchema,
    mlsSwagger,
    parseRawMLS,
    mkRawMLS,
    traceMLS,
  )
where

import Control.Applicative
import Control.Comonad
import Control.Lens ((?~))
import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.Bifunctor
import Data.Binary
import Data.Binary.Builder (toLazyByteString)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Json.Util
import Data.Kind
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Text qualified as Text
import Debug.Trace
import Imports
import Test.QuickCheck (Arbitrary (..), chooseInt)

-- | Parse a value encoded using the "TLS presentation" format.
class ParseMLS a where
  parseMLS :: Get a

-- | Convert a value to "TLS presentation" format.
class SerialiseMLS a where
  serialiseMLS :: a -> Put

-- | An integer value serialised with a variable-size encoding.
--
-- The underlying Word32 must be strictly less than 2^30.
newtype VarInt = VarInt {unVarInt :: Word32}
  deriving newtype (Eq, Ord, Num, Enum, Integral, Real, Show)

instance Arbitrary VarInt where
  arbitrary = fromIntegral <$> chooseInt (0, 1073741823)

-- From the MLS spec:
--
-- Prefix | Length | Usable Bits | Min | Max
-- -------+--------+-------------+-----+---------
-- 00       1        6             0     63
-- 01       2        14            64    16383
-- 10       4        30            16384 1073741823
-- 11       invalid  -             -     -
--
instance Binary VarInt where
  put :: VarInt -> Put
  put (VarInt w)
    | w < 64 = putWord8 (fromIntegral w)
    | w < 16384 = putWord16be (0x4000 .|. fromIntegral w)
    | w < 1073741824 = putWord32be (0x80000000 .|. w)
    | otherwise = error "invalid VarInt"

  get :: Get VarInt
  get = do
    w <- lookAhead getWord8
    case shiftR (w .&. 0xc0) 6 of
      0b00 -> VarInt . fromIntegral <$> getWord8
      0b01 -> VarInt . (.&. 0x3fff) . fromIntegral <$> getWord16be
      0b10 -> VarInt . (.&. 0x3fffffff) . fromIntegral <$> getWord32be
      _ -> fail "invalid VarInt prefix"

instance SerialiseMLS VarInt where serialiseMLS = put

instance ParseMLS VarInt where parseMLS = get

parseMLSStream :: Get a -> Get [a]
parseMLSStream p = do
  e <- isEmpty
  if e
    then pure []
    else (:) <$> p <*> parseMLSStream p

parseMLSVector :: forall w a. (Binary w, Integral w) => Get a -> Get [a]
parseMLSVector getItem = do
  len <- get @w
  if len == 0
    then pure []
    else isolate (fromIntegral len) $ go (fromIntegral len)
  where
    go :: Int64 -> Get [a]
    go endPos = do
      x <- getItem
      pos <- bytesRead
      (:) x <$> (if pos < endPos then go endPos else pure [])

serialiseMLSVector ::
  forall w a.
  (Binary w, Integral w) =>
  (a -> Put) ->
  [a] ->
  Put
serialiseMLSVector p =
  serialiseMLSBytesLazy @w . toLazyByteString . execPut . traverse_ p

parseMLSBytes :: forall w. (Binary w, Integral w) => Get ByteString
parseMLSBytes = do
  len <- fromIntegral <$> get @w
  getByteString len

serialiseMLSBytes :: forall w. (Binary w, Integral w) => ByteString -> Put
serialiseMLSBytes x = do
  put @w (fromIntegral (BS.length x))
  putByteString x

serialiseMLSBytesLazy :: forall w. (Binary w, Integral w) => LBS.ByteString -> Put
serialiseMLSBytesLazy x = do
  put @w (fromIntegral (LBS.length x))
  putLazyByteString x

parseMLSOptional :: Get a -> Get (Maybe a)
parseMLSOptional g = do
  b <- getWord8
  sequenceA $ guard (b /= 0) $> g

serialiseMLSOptional :: (a -> Put) -> Maybe a -> Put
serialiseMLSOptional _p Nothing = putWord8 0
serialiseMLSOptional p (Just x) = do
  putWord8 1
  p x

-- | Parse a positive tag for an enumeration. The value 0 is considered
-- "reserved", and all other values are shifted down by 1 to get the
-- corresponding enumeration index. This makes it possible to parse enumeration
-- types that don't contain an explicit constructor for a "reserved" value.
parseMLSEnum ::
  forall (w :: Type) a.
  (Bounded a, Enum a, Integral w, Binary w) =>
  String ->
  Get a
parseMLSEnum name = toMLSEnum name =<< get @w

serialiseMLSEnum ::
  forall w a.
  (Enum a, Integral w, Binary w) =>
  a ->
  Put
serialiseMLSEnum = put . fromMLSEnum @w

data MLSEnumError = MLSEnumUnknown Int | MLSEnumInvalid

toMLSEnum' :: forall a w. (Bounded a, Enum a, Integral w) => w -> Either MLSEnumError a
toMLSEnum' w = case fromIntegral w - 1 of
  n
    | n < 0 -> Left MLSEnumInvalid
    | n < fromEnum @a minBound || n > fromEnum @a maxBound -> Left (MLSEnumUnknown n)
    | otherwise -> pure (toEnum n)

toMLSEnum :: forall a w f. (Bounded a, Enum a, MonadFail f, Integral w) => String -> w -> f a
toMLSEnum name = either err pure . toMLSEnum'
  where
    err (MLSEnumUnknown value) = fail $ "Unknown " <> name <> ": " <> show value
    err MLSEnumInvalid = fail $ "Invalid " <> name

fromMLSEnum :: (Integral w, Enum a) => a -> w
fromMLSEnum = fromIntegral . succ . fromEnum

instance ParseMLS Word8 where parseMLS = get

instance ParseMLS Word16 where parseMLS = get

instance ParseMLS Word32 where parseMLS = get

instance ParseMLS Word64 where parseMLS = get

instance SerialiseMLS Word8 where serialiseMLS = put

instance SerialiseMLS Word16 where serialiseMLS = put

instance SerialiseMLS Word32 where serialiseMLS = put

instance SerialiseMLS Word64 where serialiseMLS = put

-- | Encode an MLS value to a lazy bytestring.
encodeMLS :: (SerialiseMLS a) => a -> LByteString
encodeMLS = runPut . serialiseMLS

encodeMLS' :: (SerialiseMLS a) => a -> ByteString
encodeMLS' = LBS.toStrict . encodeMLS

-- | Decode an MLS value from a lazy bytestring. Return an error message in case of failure.
decodeMLS :: (ParseMLS a) => LByteString -> Either Text a
decodeMLS = decodeMLSWith parseMLS

decodeMLS' :: (ParseMLS a) => ByteString -> Either Text a
decodeMLS' = decodeMLS . LBS.fromStrict

-- | Decode an MLS value from a lazy bytestring given a custom parser.
-- Return an error message in case of failure.
decodeMLSWith :: Get a -> LByteString -> Either Text a
decodeMLSWith p b = case runGetOrFail p b of
  Left (_, _, msg) -> Left (Text.pack msg)
  Right (remainder, pos, x)
    | LBS.null remainder -> Right x
    | otherwise -> Left $ "Trailing data at position " <> Text.pack (show pos)

decodeMLSWith' :: Get a -> ByteString -> Either Text a
decodeMLSWith' p = decodeMLSWith p . LBS.fromStrict

-- | An MLS value together with its serialisation.
--
-- This can be used whenever we need to parse an object, but at the same time
-- retain the original serialised bytes (e.g. for signature verification, or to
-- forward them verbatim).
data RawMLS a = RawMLS
  { raw :: ByteString,
    value :: a
  }
  deriving stock (Eq, Show, Foldable)

instance (Arbitrary a, SerialiseMLS a) => Arbitrary (RawMLS a) where
  arbitrary = mkRawMLS <$> arbitrary

-- | A schema for a raw MLS object.
--
-- This can be used for embedding MLS objects into JSON. It expresses the
-- object as a base64-encoded string containing the raw bytes of its native MLS
-- serialisation.
--
-- Note that a 'ValueSchema' for the underlying type @a@ is /not/ required.
rawMLSSchema :: Text -> (ByteString -> Either Text a) -> ValueSchema NamedSwaggerDoc (RawMLS a)
rawMLSSchema name p =
  (toBase64Text . raw)
    .= parsedText name (rawMLSFromText p)

mlsSwagger :: Text -> S.NamedSchema
mlsSwagger name =
  S.NamedSchema (Just name) $
    mempty
      & S.description
        ?~ "This object can only be parsed in TLS format. \
           \Please refer to the MLS specification for details."

rawMLSFromText :: (ByteString -> Either Text a) -> Text -> Either String (RawMLS a)
rawMLSFromText p txt = do
  mlsData <- fromBase64Text txt
  value <- first Text.unpack (p mlsData)
  pure $ RawMLS mlsData value

instance (S.ToSchema a) => S.ToSchema (RawMLS a) where
  declareNamedSchema _ = S.declareNamedSchema (Proxy @a)

instance (ParseMLS a) => FromJSON (RawMLS a) where
  parseJSON =
    Aeson.withText "Base64 MLS object" $
      either fail pure . rawMLSFromText decodeMLS'

-- | Parse an MLS object, but keep the raw bytes as well.
parseRawMLS :: Get a -> Get (RawMLS a)
parseRawMLS p = do
  -- mark the starting position
  begin <- bytesRead
  -- read value, but don't consume input, and mark final position
  (x, end) <- lookAhead $ (,) <$> p <*> bytesRead
  -- now just get the input data
  raw <- getByteString (fromIntegral (end - begin))
  -- construct RawMLS value
  pure $ RawMLS raw x

instance (ParseMLS a) => ParseMLS (RawMLS a) where
  parseMLS = parseRawMLS parseMLS

instance SerialiseMLS (RawMLS a) where
  serialiseMLS = putByteString . raw

mkRawMLS :: (SerialiseMLS a) => a -> RawMLS a
mkRawMLS x = RawMLS (LBS.toStrict (runPut (serialiseMLS x))) x

traceMLS :: (Show a) => String -> Get a -> Get a
traceMLS l g = do
  begin <- bytesRead
  r <- g
  end <- bytesRead
  traceM $ l <> " " <> show begin <> ":" <> show end <> " " <> show r
  pure r
