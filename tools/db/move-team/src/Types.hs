{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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

module Types where

import Cassandra
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), withArray, withText)
import Data.Aeson.Types (Value (Array))
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.IP (IP (..))
import Data.Id
import Data.Text qualified as T
import Data.Text.Ascii (AsciiText, Base64, decodeBase64, encodeBase64)
import Data.Vector qualified as V
import Galley.Cassandra.Instances ()
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

instance {-# OVERLAPPING #-} FromJSON [AssetIgnoreData] where
  parseJSON _ = pure []

instance {-# OVERLAPPING #-} ToJSON [AssetIgnoreData] where
  toJSON _ = Array $ V.fromList []

instance Cql AssetIgnoreData where
  ctype =
    Tagged
      ( UdtColumn
          "asset"
          [ ("typ", IntColumn),
            ("key", VarCharColumn),
            ("size", IntColumn) -- FUTUREWORK check if this works
          ]
      )
  toCql _ = error "AssetIgnoreData: you should not have any data of this"
  fromCql _ = pure AssetIgnoreData

instance (ToJSON a) => ToJSON (Cassandra.Set a) where
  toJSON = toJSON . Cassandra.fromSet

instance (FromJSON a) => FromJSON (Cassandra.Set a) where
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

deriving instance ToJSON Ascii

deriving instance FromJSON Ascii

deriving instance ToJSON TimeUuid

deriving instance FromJSON TimeUuid

instance (ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, ToJSON h, ToJSON i, ToJSON j, ToJSON k, ToJSON l, ToJSON m, ToJSON n, ToJSON o, ToJSON p, ToJSON q, ToJSON r, ToJSON s, ToJSON t, ToJSON u) => ToJSON ((,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u) where
  toJSON (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =
    Array $
      V.fromList
        [ toJSON a,
          toJSON b,
          toJSON c,
          toJSON d,
          toJSON e,
          toJSON f,
          toJSON g,
          toJSON h,
          toJSON i,
          toJSON j,
          toJSON k,
          toJSON l,
          toJSON m,
          toJSON n,
          toJSON o,
          toJSON p,
          toJSON q,
          toJSON r,
          toJSON s,
          toJSON t,
          toJSON u
        ]

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, FromJSON h, FromJSON i, FromJSON j, FromJSON k, FromJSON l, FromJSON m, FromJSON n, FromJSON o, FromJSON p, FromJSON q, FromJSON r, FromJSON s, FromJSON t, FromJSON u) => FromJSON ((,,,,,,,,,,,,,,,,,,,,) a b c d e f g h i j k l m n o p q r s t u) where
  parseJSON = withArray "Tuple" $ \case
    (toList -> [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u]) ->
      (,,,,,,,,,,,,,,,,,,,,)
        <$> parseJSON a
        <*> parseJSON b
        <*> parseJSON c
        <*> parseJSON d
        <*> parseJSON e
        <*> parseJSON f
        <*> parseJSON g
        <*> parseJSON h
        <*> parseJSON i
        <*> parseJSON j
        <*> parseJSON k
        <*> parseJSON l
        <*> parseJSON m
        <*> parseJSON n
        <*> parseJSON o
        <*> parseJSON p
        <*> parseJSON q
        <*> parseJSON r
        <*> parseJSON s
        <*> parseJSON t
        <*> parseJSON u
    _ -> fail "Expected array of length 21"
