{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Gundeck.Aws.Arn
    ( ArnEnv     (..)
    , Region     (..)
    , Account    (..)
    , EndpointId (..)
    , toText
    , fromText

      -- * SNS ARN
    , SnsArn
    , mkSnsArn
    , snsRegion
    , snsAccount
    , snsTopic

      -- ** ARNs
    , AppArn
    , EndpointArn
    , transportParser

      -- ** Topics
    , AppTopic
    , mkAppTopic
    , appTransport
    , appName

    , EndpointTopic
    , mkEndpointTopic
    , endpointTransport
    , endpointAppName
    , endpointId
    ) where

import Control.Applicative
import Control.Lens
import Data.Char
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text (Text, intercalate)
import Data.Yaml (FromJSON)
import Gundeck.Types (AppName (..), Transport (..))
import Network.AWS (Region (..))
import Network.AWS.Data

newtype ArnEnv     = ArnEnv  { arnEnvText  :: Text } deriving (Show, ToText, FromJSON)
newtype Account    = Account { fromAccount :: Text } deriving (Eq, Show, ToText, FromJSON)
newtype EndpointId = EndpointId Text deriving (Eq, Show, ToText)

data SnsArn a = SnsArn
    { _snsAsText  :: !Text
    , _snsRegion  :: !Region
    , _snsAccount :: !Account
    , _snsTopic   :: !a
    } deriving (Eq, Show)

data AppTopic = AppTopic
    { _appAsText    :: !Text
    , _appTransport :: !Transport
    , _appName      :: !AppName
    } deriving (Eq, Show)

data EndpointTopic = EndpointTopic
    { _endpointAsText    :: !Text
    , _endpointTransport :: !Transport
    , _endpointAppName   :: !AppName
    , _endpointId        :: !EndpointId
    } deriving (Eq, Show)

type AppArn      = SnsArn AppTopic
type EndpointArn = SnsArn EndpointTopic

makeLenses ''SnsArn
makeLenses ''AppTopic
makeLenses ''EndpointTopic

instance ToText (SnsArn a) where
    toText = view snsAsText

instance (FromText a, ToText a) => FromText (SnsArn a) where
    parser = snsArnParser

instance ToText AppTopic where
    toText = view appAsText

instance ToText EndpointTopic where
    toText = view endpointAsText

instance FromText EndpointTopic where
    parser = endpointTopicParser

mkSnsArn :: ToText topic => Region -> Account -> topic -> SnsArn topic
mkSnsArn r a t =
    let txt = intercalate ":" ["arn:aws:sns", toText r, toText a, toText t]
    in SnsArn txt r a t

mkAppTopic :: ArnEnv -> Transport -> AppName -> AppTopic
mkAppTopic e t n =
    let name = toText e <> "-" <> appNameText n
        txt  = intercalate "/" ["app", arnTransportText t, name]
    in AppTopic txt t n

mkEndpointTopic :: ArnEnv -> Transport -> AppName -> EndpointId -> EndpointTopic
mkEndpointTopic e t n i =
    let name = toText e <> "-" <> appNameText n
        txt  = intercalate "/" ["endpoint", arnTransportText t, name, toText i]
    in EndpointTopic txt t n i

arnTransportText :: Transport -> Text
arnTransportText GCM             = "GCM"
arnTransportText APNS            = "APNS"
arnTransportText APNSSandbox     = "APNS_SANDBOX"
arnTransportText APNSVoIP        = "APNS_VOIP"
arnTransportText APNSVoIPSandbox = "APNS_VOIP_SANDBOX"

-- Parsers --------------------------------------------------------------------

snsArnParser :: (FromText t, ToText t) => Parser (SnsArn t)
snsArnParser = do
    _ <- string "arn" *> char ':' *> string "aws" *> char ':' *> string "sns"
    r <- char ':' *> takeTill (== ':') >>= either fail return . parseOnly parser
    a <- char ':' *> takeTill (== ':')
    t <- char ':' *> parser
    return $ mkSnsArn r (Account a) t

endpointTopicParser :: Parser EndpointTopic
endpointTopicParser = do
    _ <- string "endpoint"
    t <- char '/' *> transportParser
    e <- char '/' *> takeTill (== '-')
    a <- char '-' *> takeTill (== '/')
    i <- char '/' *> takeWhile1 (not . isSpace)
    return $ mkEndpointTopic (ArnEnv e) t (AppName a) (EndpointId i)

transportParser :: Parser Transport
transportParser =
        string "GCM"               *> pure GCM
    <|> string "APNS_VOIP_SANDBOX" *> pure APNSVoIPSandbox
    <|> string "APNS_VOIP"         *> pure APNSVoIP
    <|> string "APNS_SANDBOX"      *> pure APNSSandbox
    <|> string "APNS"              *> pure APNS
