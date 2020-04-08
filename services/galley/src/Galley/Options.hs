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

module Galley.Options where

import Control.Lens hiding ((.=), Level)
import Data.Aeson.TH (deriveFromJSON)
import Data.Misc
import Galley.Types.Teams (FeatureFlags (..))
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common

data Settings
  = Settings
      { -- | Number of connections for the HTTP client pool
        _setHttpPoolSize :: !Int,
        -- | Max number of members in a team. NOTE: This must be in sync with Brig
        _setMaxTeamSize :: !Word16,
        -- | Max number of members in a conversation. NOTE: This must be in sync with Brig
        _setMaxConvSize :: !Word16,
        -- | Whether to call Brig for device listing
        _setIntraListing :: !Bool,
        -- | URI prefix for conversations with access mode @code@
        _setConversationCodeURI :: !HttpsUrl,
        -- | Throttling: limits to concurrent deletion events
        _setConcurrentDeletionEvents :: !(Maybe Int),
        -- | Throttling: delay between sending events upon team deletion
        _setDeleteConvThrottleMillis :: !(Maybe Int),
        -- | When false, assume there are no other backends and IDs are always local.
        -- This means we don't run any queries on federation-related tables and don't
        -- make any calls to the federator service.
        _setEnableFederation :: !(Maybe Bool),
        _setFeatureFlags :: !FeatureFlags
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

defConcurrentDeletionEvents :: Int
defConcurrentDeletionEvents = 128

defDeleteConvThrottleMillis :: Int
defDeleteConvThrottleMillis = 20

defEnableFederation :: Bool
defEnableFederation = False

data JournalOpts
  = JournalOpts
      { -- | SQS queue name to send team events
        _awsQueueName :: !Text,
        -- | AWS endpoint
        _awsEndpoint :: !AWSEndpoint
      }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''JournalOpts

makeLenses ''JournalOpts

data Opts
  = Opts
      { -- | Host and port to bind to
        _optGalley :: !Endpoint,
        -- | Cassandra settings
        _optCassandra :: !CassandraOpts,
        -- | Brig endpoint
        _optBrig :: !Endpoint,
        -- | Gundeck endpoint
        _optGundeck :: !Endpoint,
        -- | Spar endpoint
        _optSpar :: !Endpoint,
        -- | Disco URL
        _optDiscoUrl :: !(Maybe Text),
        -- | Other settings
        _optSettings :: !Settings,
        -- | Journaling options ('Nothing'
        --   disables journaling)
        -- Logging
        _optJournal :: !(Maybe JournalOpts),
        -- | Log level (Debug, Info, etc)
        _optLogLevel :: !Level,
        -- | Use netstrings encoding
        --  <http://cr.yp.to/proto/netstrings.txt>
        _optLogNetStrings :: !(Maybe (Last Bool)),
        -- | What log format to use
        _optLogFormat :: !(Maybe (Last LogFormat))
      }

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts
