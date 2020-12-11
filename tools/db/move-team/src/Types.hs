{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Types where

import Cassandra
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withText)
import Data.Aeson.Types (Value (Null))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.IP (IP (..))
import Data.Id
import qualified Data.Text as T
import Data.Text.Ascii (AsciiText, Base64, decodeBase64, encodeBase64)
import Galley.Data.Instances ()
import Imports
import System.Logger (Logger)

data Env = Env
  { envLogger :: Logger,
    envBrig :: ClientState,
    envGalley :: ClientState,
    envSpar :: ClientState,
    envGundeck :: ClientState,
    envTargetPath :: FilePath,
    envTeamId :: TeamId,
    envPageSize :: Int32
  }

-- | (ideally, we shouldn't need this, since we shouldn't even touch tables that have this in them.)
data AssetIgnoreData = AssetIgnoreData

instance FromJSON AssetIgnoreData where
  parseJSON _ = pure AssetIgnoreData

instance ToJSON AssetIgnoreData where
  toJSON _ = Null

instance Cql AssetIgnoreData where
  ctype =
    Tagged
      ( UdtColumn
          "asset"
          [ ("typ", IntColumn),
            ("key", TextColumn),
            ("size", IntColumn) -- TODO check if this works
          ]
      )
  toCql _ = error "AssetIgnoreData: you should only have nulls of this"
  fromCql _ = pure AssetIgnoreData

instance ToJSON a => ToJSON (Cassandra.Set a) where
  toJSON = toJSON . Cassandra.fromSet

instance FromJSON a => FromJSON (Cassandra.Set a) where
  parseJSON = fmap Cassandra.Set . parseJSON

instance ToJSON Blob where
  toJSON (Blob bstr) = toJSON (encodeBase64 (toStrict bstr))

instance FromJSON Blob where
  parseJSON x = do
    y <- parseJSON @(AsciiText Base64) x
    case decodeBase64 y of
      Nothing -> fail "Blob': not a valid base64 string"
      Just bs -> pure . Blob . fromStrict $ bs

instance ToJSON IP where
  toJSON ip = String (T.pack . show $ ip)

instance FromJSON IP where
  parseJSON = withText "IP" $ \str ->
    case (read . T.unpack) str of
      Nothing -> fail "not a formatted IP address"
      Just ip -> pure ip
