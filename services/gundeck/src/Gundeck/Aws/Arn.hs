{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Gundeck.Aws.Arn
  ( ArnEnv (..),
    Region (..),
    Account (..),
    EndpointId (..),
    toText,
    fromText,

    -- * SNS ARN
    SnsArn,
    mkSnsArn,
    snsRegion,
    snsAccount,
    snsTopic,

    -- ** ARNs
    AppArn,
    EndpointArn,
    transportParser,

    -- ** Topics
    AppTopic,
    mkAppTopic,
    appTransport,
    appName,
    EndpointTopic,
    mkEndpointTopic,
    endpointTransport,
    endpointAppName,
    endpointId,
  )
where

import Amazonka (Region (..))
import Amazonka.Data
import Control.Lens
import Data.Attoparsec.Text
import Data.Text qualified as Text
import Gundeck.Types (AppName (..), Transport (..))
import Imports

newtype ArnEnv = ArnEnv {arnEnvText :: Text} deriving (Show, ToText, FromJSON)

newtype Account = Account {fromAccount :: Text} deriving (Eq, Ord, Show, ToText, FromJSON)

newtype EndpointId = EndpointId Text deriving (Eq, Ord, Show, ToText)

data SnsArn a = SnsArn
  { _snsAsText :: !Text,
    _snsRegion :: !Region,
    _snsAccount :: !Account,
    _snsTopic :: !a
  }
  deriving (Eq, Ord, Show)

data AppTopic = AppTopic
  { _appAsText :: !Text,
    _appTransport :: !Transport,
    _appName :: !AppName
  }
  deriving (Eq, Show)

data EndpointTopic = EndpointTopic
  { _endpointAsText :: !Text,
    _endpointTransport :: !Transport,
    _endpointAppName :: !AppName,
    _endpointId :: !EndpointId
  }
  deriving (Eq, Ord, Show)

type AppArn = SnsArn AppTopic

type EndpointArn = SnsArn EndpointTopic

makeLenses ''SnsArn

makeLenses ''AppTopic

makeLenses ''EndpointTopic

instance ToText (SnsArn a) where
  toText = view snsAsText

instance (FromText a, ToText a) => FromText (SnsArn a) where
  fromText = parseOnly snsArnParser

instance ToText AppTopic where
  toText = view appAsText

instance ToText EndpointTopic where
  toText = view endpointAsText

instance FromText EndpointTopic where
  fromText = parseOnly endpointTopicParser

mkSnsArn :: ToText topic => Region -> Account -> topic -> SnsArn topic
mkSnsArn r a t =
  let txt = Text.intercalate ":" ["arn:aws:sns", toText r, toText a, toText t]
   in SnsArn txt r a t

mkAppTopic :: ArnEnv -> Transport -> AppName -> AppTopic
mkAppTopic e t n =
  let name = toText e <> "-" <> appNameText n
      txt = Text.intercalate "/" ["app", arnTransportText t, name]
   in AppTopic txt t n

mkEndpointTopic :: ArnEnv -> Transport -> AppName -> EndpointId -> EndpointTopic
mkEndpointTopic e t n i =
  let name = toText e <> "-" <> appNameText n
      txt = Text.intercalate "/" ["endpoint", arnTransportText t, name, toText i]
   in EndpointTopic txt t n i

arnTransportText :: Transport -> Text
arnTransportText GCM = "GCM"
arnTransportText APNS = "APNS"
arnTransportText APNSSandbox = "APNS_SANDBOX"
arnTransportText APNSVoIP = "APNS_VOIP"
arnTransportText APNSVoIPSandbox = "APNS_VOIP_SANDBOX"

-- Parsers --------------------------------------------------------------------

snsArnParser :: (FromText t, ToText t) => Parser (SnsArn t)
snsArnParser = do
  _ <- string "arn" *> char ':' *> string "aws" *> char ':' *> string "sns"
  r <- char ':' *> takeTill (== ':') >>= either fail pure . fromText
  a <- char ':' *> takeTill (== ':')
  t <- char ':' *> takeText >>= either fail pure . fromText
  pure $ mkSnsArn r (Account a) t

endpointTopicParser :: Parser EndpointTopic
endpointTopicParser = do
  _ <- string "endpoint"
  t <- char '/' *> transportParser
  e <- char '/' *> takeTill (== '-')
  a <- char '-' *> takeTill (== '/')
  i <- char '/' *> takeWhile1 (not . isSpace)
  pure $ mkEndpointTopic (ArnEnv e) t (AppName a) (EndpointId i)

transportParser :: Parser Transport
transportParser =
  string "GCM" $> GCM
    <|> string "APNS_VOIP_SANDBOX" $> APNSVoIPSandbox
    <|> string "APNS_VOIP" $> APNSVoIP
    <|> string "APNS_SANDBOX" $> APNSSandbox
    <|> string "APNS" $> APNS
