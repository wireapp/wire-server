module Wire.BackgroundWorker.Options where

import Data.Aeson
import Data.Domain
import Imports
import System.Logger.Extended
import Util.Options
import Data.Misc (HttpsUrl)
import Galley.Types.Teams
import Wire.API.Team.Member
import Data.Range
import Data.Id

data Opts = Opts
  { logLevel :: !Level,
    logFormat :: !(Maybe (Last LogFormat)),
    federatorInternal :: !Endpoint,
    rabbitmq :: !RabbitMqOpts,
    remoteDomains :: [Domain],
    localDomain :: Domain,
    galleyConversationCodeUri :: HttpsUrl,
    legalHoldFlag :: FeatureLegalHold,
    currentFanoutLimit :: Range 1 HardTruncationLimit Int32,
    brig :: !Endpoint,
    spar :: !Endpoint,
    gundeck :: !Endpoint,
    requestId :: RequestId
  }
  deriving (Show, Generic)

instance FromJSON Opts

data RabbitMqOpts = RabbitMqOpts
  { host :: !String,
    port :: !Int,
    vHost :: !Text
  }
  deriving (Show, Generic)

instance FromJSON RabbitMqOpts
