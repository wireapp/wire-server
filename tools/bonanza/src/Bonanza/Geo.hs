{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Bonanza.Geo
  ( geolocate,
    mkGeo,
  )
where

import Bonanza.Parser.IP
import Bonanza.Types
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString as B
import Data.Geolocation.GeoIP
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Encoding (decodeLatin1, encodeUtf8)
import Imports

mkGeo :: FilePath -> IO GeoDB
mkGeo path = do
  -- exit orderly if file doesn't exist (c api segfaults)
  withBinaryFile path ReadMode (const $ pure ())
  openGeoDB mmap_cache path

geolocate :: GeoDB -> Text -> LogEvent -> IO LogEvent
geolocate db t evt =
  maybe
    (pure evt)
    (fmap update . geoLocateByIPNum db . toInteger)
    $ ip t evt
  where
    update x =
      evt & logTags
        %~ _Wrapped'
          . at "geoip"
          . non (Object HashMap.empty)
          . _Object
          . at t
          .~ fmap (toJSON . toGeo) x

ip :: Text -> LogEvent -> Maybe IPv4
ip t = join . fmap parse . view (logTags . _Wrapped' . at t)
  where
    parse =
      join
        . fmap (preview _Right . parseOnly ipv4 . encodeUtf8)
        . preview _String

toGeo :: GeoIPRecord -> Geo
toGeo GeoIPRecord {..} =
  Geo
    { geoCountry = ne geoCountryCode,
      geoCity = ne geoCity,
      geoLocation = Coordinate {lat = geoLatitude, lon = geoLongitude}
    }
  where
    ne s
      | B.length s > 0 = Just $ decodeLatin1 s
      | otherwise = Nothing
