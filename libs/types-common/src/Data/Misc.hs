{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Data.Misc
    ( -- * IpAddr / Port
      IpAddr (..)
    , Port   (..)

      -- * Location
    , Location
    , location
    , latitude
    , longitude
    , Latitude  (..)
    , Longitude (..)

      -- * HttpsUrl
    , HttpsUrl (..)

      -- * Fingerprint
    , Fingerprint (..)
    , Rsa

      -- * PlainTextPassword
    , PlainTextPassword (..)

      -- * Functor infix ops
    , (<$$>), (<$$$>)
    ) where

import Control.DeepSeq (NFData (..))
import Control.Lens ((^.), makeLenses)
import Control.Monad (when)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Conversion
import Data.Char (isSpace)
import Data.IP (IP)
import Safe (readMay)
import Data.Range
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word
#ifdef WITH_CQL
import Data.ByteString.Lazy (toStrict)
import Database.CQL.Protocol hiding (unpack)
#endif
import GHC.Generics (Generic)
import Text.Read (Read (..))
import URI.ByteString hiding (Port)

import qualified Data.Aeson.Types                 as Json
import qualified Data.Attoparsec.ByteString.Char8 as Chars
import qualified Data.ByteString.Base64           as B64
import qualified Data.Text                        as Text

--------------------------------------------------------------------------------
-- IpAddr / Port

newtype IpAddr = IpAddr { ipAddr :: IP } deriving (Eq, Ord, Show)

instance FromByteString IpAddr where
    parser = do
        s <- Chars.takeWhile1 (not . isSpace)
        case readMay (unpack s) of
            Nothing -> fail "Failed parsing bytestring as IpAddr."
            Just ip -> return (IpAddr ip)

instance ToByteString IpAddr where
    builder = string8 . show . ipAddr

instance Read IpAddr where
    readPrec = IpAddr <$> readPrec

instance NFData IpAddr where rnf (IpAddr a) = seq a ()

newtype Port = Port
    { portNumber :: Word16
    } deriving (Eq, Ord, Show, Real, Enum, Num, Integral, NFData)

instance Read Port where
    readsPrec n = map (\x -> (Port (fst x), snd x)) . readsPrec n

instance ToJSON IpAddr where
    toJSON (IpAddr ip) = String (Text.pack $ show ip)

instance FromJSON IpAddr where
    parseJSON = withText "IpAddr" $ \txt ->
        case readMay (Text.unpack txt) of
            Nothing -> fail "Failed parsing IP address."
            Just ip -> return (IpAddr ip)

instance ToJSON Port where
    toJSON (Port p) = toJSON p

instance FromJSON Port where
    parseJSON = fmap Port . parseJSON

--------------------------------------------------------------------------------
-- Location

data Location = Location
    { _latitude  :: !Double
    , _longitude :: !Double
    } deriving (Eq, Ord, Generic)

instance Show Location where
    show p = showString "{latitude="
           . shows (_latitude p)
           . showString ", longitude="
           . shows (_longitude p)
           $ "}"

instance NFData Location

makeLenses ''Location

newtype Latitude  = Latitude  Double deriving NFData
newtype Longitude = Longitude Double deriving NFData

location :: Latitude -> Longitude -> Location
location (Latitude lat) (Longitude lon) =
    Location { _latitude = lat, _longitude = lon }

instance ToJSON Location where
    toJSON p = object [ "lat" .= (p^.latitude), "lon" .= (p^.longitude) ]

instance FromJSON Location where
    parseJSON = withObject "Location" $ \o ->
        location <$> (Latitude  <$> o .: "lat")
                 <*> (Longitude <$> o .: "lon")

#ifdef WITH_CQL
instance Cql Latitude where
    ctype = Tagged DoubleColumn

    toCql (Latitude x) = CqlDouble x

    fromCql (CqlDouble x) = return (Latitude x)
    fromCql _             = fail "Latitude: Expected CqlDouble."

instance Cql Longitude where
    ctype = Tagged DoubleColumn

    toCql (Longitude x) = CqlDouble x

    fromCql (CqlDouble x) = return (Longitude x)
    fromCql _             = fail "Longitude: Expected CqlDouble."
#endif

--------------------------------------------------------------------------------
-- HttpsUrl

newtype HttpsUrl = HttpsUrl
    { httpsUrl :: URIRef Absolute
    } deriving Eq

instance Show HttpsUrl where
    showsPrec i = showsPrec i . httpsUrl

instance ToByteString HttpsUrl where
    builder = serializeURIRef . httpsUrl

instance FromByteString HttpsUrl where
    parser = do
        u <- uriParser strictURIParserOptions
        when (u^.uriSchemeL.schemeBSL /= "https") $
            fail $ "Non-HTTPS URL: " ++ show u
        return (HttpsUrl u)

instance FromJSON HttpsUrl where
    parseJSON = withText "HttpsUrl" $
        either fail return . runParser parser . encodeUtf8

instance ToJSON HttpsUrl where
    toJSON = toJSON . decodeUtf8 . toByteString'

#ifdef WITH_CQL
instance Cql HttpsUrl where
    ctype = Tagged BlobColumn
    toCql = CqlBlob . toByteString

    fromCql (CqlBlob t) = runParser parser (toStrict t)
    fromCql _           = fail "HttpsUrl: Expected CqlBlob"
#endif

--------------------------------------------------------------------------------
-- Fingerprint

data Rsa

newtype Fingerprint a = Fingerprint
    { fingerprintBytes :: ByteString
    } deriving (Eq, Show, FromByteString, ToByteString, NFData)

instance FromJSON (Fingerprint a) where
    parseJSON = withText "Fingerprint" $
        either fail (pure . Fingerprint) . B64.decode . encodeUtf8

instance ToJSON (Fingerprint a) where
    toJSON = String . decodeUtf8 . B64.encode . fingerprintBytes

#ifdef WITH_CQL
instance Cql (Fingerprint a) where
    ctype = Tagged BlobColumn
    toCql = CqlBlob . toByteString

    fromCql (CqlBlob b) = return (Fingerprint (toStrict b))
    fromCql _           = fail "Fingerprint: Expected CqlBlob"
#endif

--------------------------------------------------------------------------------
-- Password

newtype PlainTextPassword = PlainTextPassword
    { fromPlainTextPassword :: Text }
    deriving (ToJSON)

instance FromJSON PlainTextPassword where
    parseJSON x = PlainTextPassword . fromRange
               <$> (parseJSON x :: Json.Parser (Range 6 1024 Text))

----------------------------------------------------------------------
-- Functor

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infix 4 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infix 4 <$$$>
