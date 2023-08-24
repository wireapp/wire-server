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

module Galley.Options
  ( Settings,
    httpPoolSize,
    maxTeamSize,
    maxFanoutSize,
    exposeInvitationURLsTeamAllowlist,
    maxConvSize,
    intraListing,
    disabledAPIVersions,
    conversationCodeURI,
    concurrentDeletionEvents,
    deleteConvThrottleMillis,
    federationDomain,
    mlsPrivateKeyPaths,
    featureFlags,
    defConcurrentDeletionEvents,
    defDeleteConvThrottleMillis,
    defFanoutLimit,
    JournalOpts (JournalOpts),
    queueName,
    endpoint,
    Opts,
    galley,
    cassandra,
    brig,
    gundeck,
    spar,
    federator,
    rabbitmq,
    discoUrl,
    settings,
    journal,
    logLevel,
    logNetStrings,
    logFormat,
  )
where

import Control.Lens hiding (Level, (.=))
import Data.Aeson.TH (deriveFromJSON)
import Data.Domain (Domain)
import Data.Id (TeamId)
import Data.Misc
import Data.Range
import Galley.Keys
import Galley.Types.Teams
import Imports
import Network.AMQP.Extended
import System.Logger.Extended (Level, LogFormat)
import Util.Options hiding (endpoint)
import Util.Options.Common
import Wire.API.Routes.Version
import Wire.API.Team.Member

data Settings = Settings
  { -- | Number of connections for the HTTP client pool
    _httpPoolSize :: !Int,
    -- | Max number of members in a team. NOTE: This must be in sync with Brig
    _maxTeamSize :: !Word32,
    -- | Max number of team members users to fanout events to. For teams larger than
    --   this value, team events and user updates will no longer be sent to team users.
    --   This defaults to setMaxTeamSize and cannot be > HardTruncationLimit. Useful
    --   to tune mainly for testing purposes.
    _maxFanoutSize :: !(Maybe (Range 1 HardTruncationLimit Int32)),
    -- | List of teams for which the invitation URL can be added to the list of all
    -- invitations retrievable by team admins.  See also:
    -- 'ExposeInvitationURLsToTeamAdminConfig'.
    _exposeInvitationURLsTeamAllowlist :: !(Maybe [TeamId]),
    -- | Max number of members in a conversation. NOTE: This must be in sync with Brig
    _maxConvSize :: !Word16,
    -- | Whether to call Brig for device listing
    _intraListing :: !Bool,
    -- | URI prefix for conversations with access mode @code@
    _conversationCodeURI :: !HttpsUrl,
    -- | Throttling: limits to concurrent deletion events
    _concurrentDeletionEvents :: !(Maybe Int),
    -- | Throttling: delay between sending events upon team deletion
    _deleteConvThrottleMillis :: !(Maybe Int),
    -- | FederationDomain is required, even when not wanting to federate with other backends
    -- (in that case the 'allowedDomains' can be set to empty in Federator)
    -- Federation domain is used to qualify local IDs and handles,
    -- e.g. 0c4d8944-70fa-480e-a8b7-9d929862d18c@wire.com and somehandle@wire.com.
    -- It should also match the SRV DNS records under which other wire-server installations can find this backend:
    --    _wire-server-federator._tcp.<federationDomain>
    -- Once set, DO NOT change it: if you do, existing users may have a broken experience and/or stop working
    -- Remember to keep it the same in all services.
    -- Example:
    --   allowedDomains:
    --     - wire.com
    --     - example.com
    _federationDomain :: !Domain,
    -- | When true, galley will assume data in `billing_team_member` table is
    -- consistent and use it for billing.
    -- When false, billing information for large teams is not guaranteed to have all
    -- the owners.
    -- Defaults to false.
    _mlsPrivateKeyPaths :: !(Maybe MLSPrivateKeyPaths),
    -- | FUTUREWORK: 'setFeatureFlags' should be renamed to 'setFeatureConfigs' in all types.
    _featureFlags :: !FeatureFlags,
    _disabledAPIVersions :: Maybe (Set Version)
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings

defConcurrentDeletionEvents :: Int
defConcurrentDeletionEvents = 128

defDeleteConvThrottleMillis :: Int
defDeleteConvThrottleMillis = 20

defFanoutLimit :: Range 1 HardTruncationLimit Int32
defFanoutLimit = unsafeRange hardTruncationLimit

data JournalOpts = JournalOpts
  { -- | SQS queue name to send team events
    _queueName :: !Text,
    -- | AWS endpoint
    _endpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''JournalOpts

makeLenses ''JournalOpts

data Opts = Opts
  { -- | Host and port to bind to
    _galley :: !Endpoint,
    -- | Cassandra settings
    _cassandra :: !CassandraOpts,
    -- | Brig endpoint
    _brig :: !Endpoint,
    -- | Gundeck endpoint
    _gundeck :: !Endpoint,
    -- | Spar endpoint
    _spar :: !Endpoint,
    -- | Federator endpoint
    _federator :: !(Maybe Endpoint),
    -- | RabbitMQ settings, required when federation is enabled.
    _rabbitmq :: !(Maybe RabbitMqOpts),
    -- | Disco URL
    _discoUrl :: !(Maybe Text),
    -- | Other settings
    _settings :: !Settings,
    -- | Journaling options ('Nothing'
    --   disables journaling)
    -- Logging
    _journal :: !(Maybe JournalOpts),
    -- | Log level (Debug, Info, etc)
    _logLevel :: !Level,
    -- | Use netstrings encoding
    --  <http://cr.yp.to/proto/netstrings.txt>
    _logNetStrings :: !(Maybe (Last Bool)),
    -- | What log format to use
    _logFormat :: !(Maybe (Last LogFormat))
  }

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts
