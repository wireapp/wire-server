{-# LANGUAGE OverloadedStrings #-}

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

module Brig.Types.TURN.Internal where

import Data.Aeson
import qualified Data.ByteString.Conversion as BC
import Data.Misc (IpAddr (..))
import qualified Data.Text.Encoding as TE
import Imports
import Text.Hostname

data TurnHost
  = TurnHostIp IpAddr
  | TurnHostName Text
  deriving (Eq, Show, Generic)

isHostName :: TurnHost -> Bool
isHostName (TurnHostIp _) = False
isHostName (TurnHostName _) = True

parseTurnHost :: Text -> Maybe TurnHost
parseTurnHost h = case BC.fromByteString host of
  Just ip@(IpAddr _) -> Just $ TurnHostIp ip
  Nothing | validHostname host -> Just $ TurnHostName h -- NOTE: IP addresses are also valid hostnames
  _ -> Nothing
  where
    host = TE.encodeUtf8 h

instance BC.FromByteString TurnHost where
  parser = BC.parser >>= maybe (fail "Invalid turn host") return . parseTurnHost

instance BC.ToByteString TurnHost where
  builder (TurnHostIp ip) = BC.builder ip
  builder (TurnHostName n) = BC.builder n

instance ToJSON TurnHost

instance FromJSON TurnHost
