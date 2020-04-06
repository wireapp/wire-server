{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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

module Data.Misc
  ( -- * IpAddr / Port
    IpAddr (..),
    Port (..),

    -- * Location
    Location,
    location,
    latitude,
    longitude,
    Latitude (..),
    Longitude (..),

    -- * Time
    Milliseconds (..),

    -- * HttpsUrl
    HttpsUrl (..),
    mkHttpsUrl,

    -- * Fingerprint
    Fingerprint (..),
    Rsa,

    -- * PlainTextPassword
    PlainTextPassword (..),

    -- * Functor infix ops
    (<$$>),
    (<$$$>),
  )
where

import Cassandra
import Control.Lens ((^.), makeLenses)
import Data.Aeson
import qualified Data.Aeson.Types as Json
import qualified Data.Attoparsec.ByteString.Char8 as Chars
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.IP (IP)
import Data.Int (Int64)
import Data.Range
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Imports
import Test.QuickCheck (Arbitrary (..))
import Text.Read (Read (..))
import URI.ByteString hiding (Port)

--------------------------------------------------------------------------------
-- IpAddr / Port

newtype IpAddr = IpAddr {ipAddr :: IP} deriving (Eq, Ord, Show, Generic)

instance FromByteString IpAddr where
  parser = do
    s <- Chars.takeWhile1 (not . isSpace)
    case readMaybe (unpack s) of
      Nothing -> fail "Failed parsing bytestring as IpAddr."
      Just ip -> return (IpAddr ip)

instance ToByteString IpAddr where
  builder = string8 . show . ipAddr

instance Read IpAddr where
  readPrec = IpAddr <$> readPrec

instance NFData IpAddr where rnf (IpAddr a) = seq a ()

newtype Port
  = Port
      { portNumber :: Word16
      }
  deriving (Eq, Ord, Show, Real, Enum, Num, Integral, NFData, Generic)

instance Read Port where
  readsPrec n = map (\x -> (Port (fst x), snd x)) . readsPrec n

instance ToJSON IpAddr where
  toJSON (IpAddr ip) = String (Text.pack $ show ip)

instance FromJSON IpAddr where
  parseJSON = withText "IpAddr" $ \txt ->
    case readMaybe (Text.unpack txt) of
      Nothing -> fail "Failed parsing IP address."
      Just ip -> return (IpAddr ip)

instance ToJSON Port where
  toJSON (Port p) = toJSON p

instance FromJSON Port where
  parseJSON = fmap Port . parseJSON

--------------------------------------------------------------------------------
-- Location

data Location
  = Location
      { _latitude :: !Double,
        _longitude :: !Double
      }
  deriving (Eq, Ord, Generic)

instance Show Location where
  show p =
    showString "{latitude="
      . shows (_latitude p)
      . showString ", longitude="
      . shows (_longitude p)
      $ "}"

instance NFData Location

makeLenses ''Location

newtype Latitude = Latitude Double deriving (NFData, Generic)

newtype Longitude = Longitude Double deriving (NFData, Generic)

location :: Latitude -> Longitude -> Location
location (Latitude lat) (Longitude lon) =
  Location {_latitude = lat, _longitude = lon}

instance ToJSON Location where
  toJSON p = object ["lat" .= (p ^. latitude), "lon" .= (p ^. longitude)]

instance FromJSON Location where
  parseJSON = withObject "Location" $ \o ->
    location <$> (Latitude <$> o .: "lat")
      <*> (Longitude <$> o .: "lon")

instance Cql Latitude where
  ctype = Tagged DoubleColumn

  toCql (Latitude x) = CqlDouble x

  fromCql (CqlDouble x) = return (Latitude x)
  fromCql _ = fail "Latitude: Expected CqlDouble."

instance Cql Longitude where
  ctype = Tagged DoubleColumn

  toCql (Longitude x) = CqlDouble x

  fromCql (CqlDouble x) = return (Longitude x)
  fromCql _ = fail "Longitude: Expected CqlDouble."

--------------------------------------------------------------------------------
-- Time

newtype Milliseconds
  = Ms
      { ms :: Word64
      }
  deriving (Eq, Ord, Show, Num, Generic)

-- | Convert milliseconds to 'Int64', with clipping if it doesn't fit.
msToInt64 :: Milliseconds -> Int64
msToInt64 = fromIntegral . min (fromIntegral (maxBound @Int64)) . ms

-- | Convert 'Int64' to milliseconds, rounding negative values to 0.
int64ToMs :: Int64 -> Milliseconds
int64ToMs = Ms . fromIntegral . max 0

instance ToJSON Milliseconds where
  toJSON = toJSON . msToInt64

instance FromJSON Milliseconds where
  parseJSON = fmap int64ToMs . parseJSON

instance Cql Milliseconds where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . msToInt64
  fromCql = \case
    CqlBigInt i -> pure $ int64ToMs i
    _ -> fail "Milliseconds: expected CqlBigInt"

--------------------------------------------------------------------------------
-- HttpsUrl

newtype HttpsUrl
  = HttpsUrl
      { httpsUrl :: URIRef Absolute
      }
  deriving (Eq, Generic)

mkHttpsUrl :: URIRef Absolute -> Either String HttpsUrl
mkHttpsUrl uri =
  if uri ^. uriSchemeL . schemeBSL == "https"
    then Right $ HttpsUrl uri
    else Left $ "Non-HTTPS URL: " ++ show uri

instance Show HttpsUrl where
  showsPrec i = showsPrec i . httpsUrl

instance ToByteString HttpsUrl where
  builder = serializeURIRef . httpsUrl

instance FromByteString HttpsUrl where
  parser = either fail pure . mkHttpsUrl =<< uriParser strictURIParserOptions

instance FromJSON HttpsUrl where
  parseJSON =
    withText "HttpsUrl" $
      either fail return . runParser parser . encodeUtf8

instance ToJSON HttpsUrl where
  toJSON = toJSON . decodeUtf8 . toByteString'

instance Cql HttpsUrl where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString

  fromCql (CqlBlob t) = runParser parser (toStrict t)
  fromCql _ = fail "HttpsUrl: Expected CqlBlob"

--------------------------------------------------------------------------------
-- Fingerprint

-- Tag for Rsa encoded fingerprints
data Rsa

newtype Fingerprint a
  = Fingerprint
      { fingerprintBytes :: ByteString
      }
  deriving (Eq, Show, FromByteString, ToByteString, NFData, Generic)

instance FromJSON (Fingerprint Rsa) where
  parseJSON =
    withText "Fingerprint" $
      either fail (pure . Fingerprint) . B64.decode . encodeUtf8

instance ToJSON (Fingerprint Rsa) where
  toJSON = String . decodeUtf8 . B64.encode . fingerprintBytes

instance Cql (Fingerprint a) where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . toByteString

  fromCql (CqlBlob b) = return (Fingerprint (toStrict b))
  fromCql _ = fail "Fingerprint: Expected CqlBlob"

--------------------------------------------------------------------------------
-- Password

newtype PlainTextPassword
  = PlainTextPassword
      {fromPlainTextPassword :: Text}
  deriving (Eq, ToJSON, Generic)

instance Show PlainTextPassword where
  show _ = "PlainTextPassword <hidden>"

instance FromJSON PlainTextPassword where
  parseJSON x =
    PlainTextPassword . fromRange
      <$> (parseJSON x :: Json.Parser (Range 6 1024 Text))

instance Arbitrary PlainTextPassword where
  -- TODO: why 6..1024? For tests we might want invalid passwords as well, e.g. 3 chars
  arbitrary = PlainTextPassword . fromRange <$> genRangeText @6 @1024 arbitrary

----------------------------------------------------------------------
-- Functor

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infix 4 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infix 4 <$$$>
