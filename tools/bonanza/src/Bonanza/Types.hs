{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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

module Bonanza.Types
  ( -- * Exported types
    Coordinate (..),
    Geo (..),
    Host (..),
    LogEvent,
    logTime,
    logOrigin,
    logTags,
    logMessage,
    ToLogEvent (..),
    Tags (..),
    TagValue,

    -- * re-exports
    IPv4 (..),

    -- * Utility functions
    modLabels,
    stripPrefix,
  )
where

import Bonanza.Parser.IP (IPv4 (..))
import Control.Lens
import Data.Aeson hiding (Value)
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Imports hiding (stripPrefix)

-- | Canonical representation of a log record / event
data LogEvent
  = LogEvent
      { _logTime :: Maybe UTCTime,
        _logOrigin :: Maybe Host,
        _logTags :: !Tags,
        _logMessage :: Maybe Text
      }
  deriving (Eq, Show, Generic)

instance ToJSON LogEvent where
  toJSON =
    genericToJSON
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier = stripPrefix "_log",
          Aeson.omitNothingFields = True
        }

instance FromJSON LogEvent where
  parseJSON =
    genericParseJSON
      Aeson.defaultOptions
        { Aeson.fieldLabelModifier = stripPrefix "_log",
          Aeson.omitNothingFields = True
        }

instance Semigroup LogEvent where
  (<>) a b =
    LogEvent
      { _logTime = _logTime b `mplus` _logTime a,
        _logOrigin = _logOrigin b `mplus` _logOrigin a,
        _logTags = _logTags b `mappend` _logTags a,
        _logMessage = _logMessage a `mappend` _logMessage b
      }

instance Monoid LogEvent where
  mempty = LogEvent Nothing Nothing mempty mempty

--------------------------------------------------------------------------------
-- Auxiliary Types

newtype Tags = Tags {fromTags :: HashMap Text TagValue}
  deriving (Eq, Show, Generic)

instance ToJSON Tags where
  toJSON = toJSON . fromTags

instance FromJSON Tags where
  parseJSON (Object o) =
    fmap (Tags . M.fromList)
      . mapM (\(k, v) -> (,) k <$> parseJSON v)
      . M.toList
      $ o
  parseJSON _ = mzero

instance Semigroup Tags where
  (<>) a b = Tags $ fromTags a <> fromTags b

instance Monoid Tags where
  mempty = Tags mempty

type TagValue = Aeson.Value

newtype Host = Host {host :: Text}
  deriving (Eq, Show)

instance ToJSON Host where
  toJSON (Host h) = toJSON h

instance FromJSON Host where
  parseJSON (String t) = pure $ Host t
  parseJSON _ = mzero

data Geo
  = Geo
      { geoCountry :: Maybe Text,
        geoCity :: Maybe Text,
        geoLocation :: !Coordinate
      }
  deriving (Eq, Show, Generic)

instance ToJSON Geo where
  toJSON =
    genericToJSON
      (modLabels (map toLower . stripPrefix "geo"))
        { Aeson.omitNothingFields = True
        }

instance FromJSON Geo where
  parseJSON =
    genericParseJSON
      (modLabels (map toLower . stripPrefix "geo"))
        { Aeson.omitNothingFields = True
        }

data Coordinate
  = Coordinate
      { lat :: !Double,
        lon :: !Double
      }
  deriving (Eq, Show, Generic)

instance ToJSON Coordinate

instance FromJSON Coordinate

-- | Conversion to 'LogEvent'
class ToLogEvent a where
  toLogEvent :: a -> LogEvent

instance ToLogEvent ByteString where toLogEvent = toLogEvent . decodeUtf8

instance ToLogEvent Text where toLogEvent t = mempty {_logMessage = Just t}

instance ToLogEvent LogEvent where toLogEvent = id

--------------------------------------------------------------------------------
-- Helpers

modLabels :: (String -> String) -> Aeson.Options
modLabels f = Aeson.defaultOptions {Aeson.fieldLabelModifier = f}

stripPrefix :: String -> String -> String
stripPrefix pre' = lowerFst . go
  where
    go (L.stripPrefix pre' -> Just suf) = suf
    go x = x
    lowerFst [] = []
    lowerFst (x : xs) = toLower x : xs

--------------------------------------------------------------------------------
-- Lenses

makeLenses ''LogEvent

makeWrapped ''Tags
