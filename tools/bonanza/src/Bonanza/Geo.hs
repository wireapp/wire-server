{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.GeoIP2
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Imports

mkGeo :: FilePath -> IO GeoDB
mkGeo path = do
  -- exit orderly if file doesn't exist (c api segfaults)
  withBinaryFile path ReadMode (const $ pure ())
  openGeoDB path

geolocate :: GeoDB -> Text -> LogEvent -> LogEvent
geolocate db t evt =
  maybe
    evt
    (update . preview _Right . findGeoData db "en" . read . Text.unpack . showIPv4Text)
    $ ip t evt
  where
    update :: Maybe GeoResult -> LogEvent
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

toGeo :: GeoResult -> Geo
toGeo GeoResult {..} =
  Geo
    { geoCountry = geoCountryISO,
      geoCity = geoCity,
      geoLocation = toCoordinate (fmap locationLatitude geoLocation) (fmap locationLongitude geoLocation)
    }
  where
    toCoordinate (Just lat) (Just lon) = Just $ Coordinate lat lon
    toCoordinate _          _          = Nothing
