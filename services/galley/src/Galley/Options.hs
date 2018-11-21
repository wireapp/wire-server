{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Galley.Options where

import Imports
import Control.Lens hiding ((.=))
import Data.Aeson.TH (deriveFromJSON)
import Options.Applicative
import Util.Options
import Util.Options.Common
import Data.Misc

data Settings = Settings
    {
    -- | Number of connections for the HTTP client pool
      _setHttpPoolSize          :: !Int
    -- | Max number of members in a team. NOTE: This must be in sync with Brig
    , _setMaxTeamSize           :: !Word16
    -- | Max number of members in a conversation. NOTE: This must be in sync with Brig
    , _setMaxConvSize           :: !Word16
    -- | Whether to call Brig for device listing
    , _setIntraListing          :: !Bool
    -- | URI prefix for conversations with access mode @code@
    , _setConversationCodeURI   :: !HttpsUrl
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings
makeLenses ''Settings

data JournalOpts = JournalOpts
    { _awsQueueName :: !Text         -- ^ SQS queue name to send team events
    , _awsEndpoint  :: !AWSEndpoint  -- ^ AWS endpoint
    } deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''JournalOpts
makeLenses ''JournalOpts

data Opts = Opts
    { _optGalley    :: !Endpoint             -- ^ Host and port to bind to
    , _optCassandra :: !CassandraOpts        -- ^ Cassandra settings
    , _optBrig      :: !Endpoint             -- ^ Brig endpoint
    , _optGundeck   :: !Endpoint             -- ^ Gundeck endpoint
    , _optDiscoUrl  :: !(Maybe Text)         -- ^ Disco URL
    , _optSettings  :: !Settings             -- ^ Other settings
    , _optJournal   :: !(Maybe JournalOpts)  -- ^ Journaling options ('Nothing'
                                             --   disables journaling)
    }

deriveFromJSON toOptionFieldName ''Opts
makeLenses ''Opts

-- Used by the journaler
journalOptsParser :: Parser JournalOpts
journalOptsParser = JournalOpts
    <$> (textOption $
            long "team-events-queue-name"
            <> metavar "STRING"
            <> help "sqs queue name to send team events")
    <*> (option parseAWSEndpoint $
            long "aws-sqs-endpoint"
            <> value (AWSEndpoint "sqs.eu-west-1.amazonaws.com" True 443)
            <> metavar "STRING"
            <> showDefault
            <> help "aws endpoint")
