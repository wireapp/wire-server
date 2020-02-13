{-# LANGUAGE OverloadedStrings #-}

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
