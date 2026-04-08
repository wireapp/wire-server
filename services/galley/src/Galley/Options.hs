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
  ( Settings (..),
    httpPoolSize,
    maxTeamSize,
    maxFanoutSize,
    exposeInvitationURLsTeamAllowlist,
    maxConvSize,
    intraListing,
    disabledAPIVersions,
    conversationCodeURI,
    multiIngress,
    concurrentDeletionEvents,
    deleteConvThrottleMillis,
    federationDomain,
    federationProtocols,
    mlsPrivateKeyPaths,
    featureFlags,
    defConcurrentDeletionEvents,
    JournalOpts (JournalOpts),
    queueName,
    endpoint,
    Opts (..),
    galley,
    cassandra,
    postgresql,
    postgresqlPassword,
    postgresqlPool,
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
    guestLinkTTLSeconds,
    passwordHashingOptions,
    passwordHashingRateLimit,
    checkGroupInfo,
    meetings,
    validityPeriod,
    postgresMigration,
    PostgresMigrationOpts (..),
    StorageLocation (..),
  )
where

import Control.Lens hiding (Level, (.=))
import Data.Aeson.TH (deriveFromJSON)
import Data.Domain (Domain)
import Data.Id (TeamId)
import Data.Misc
import Data.Range
import Galley.Keys
import Hasql.Pool.Extended
import Imports
import Network.AMQP.Extended
import System.Logger.Extended (Level, LogFormat)
import Util.Options hiding (endpoint)
import Util.Options.Common
import Wire.API.Conversation.Protocol
import Wire.API.Routes.Version
import Wire.API.Team.FeatureFlags
import Wire.API.Team.Member
import Wire.ConversationSubsystem.Interpreter (GuestLinkTTLSeconds)
import Wire.PostgresMigrationOpts
import Wire.RateLimit.Interpreter (RateLimitConfig)

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
    _conversationCodeURI :: !(Maybe HttpsUrl),
    -- | Map from @Z-Host@ header to URI prefix for conversations with access mode @code@
    --
    -- If setMultiIngress is set then the URI prefix for guest links is looked
    -- up in this config setting using the @Z-Host@ header value as a key. If
    -- the lookup fails then no guest link can be created via the API.
    --
    -- This option is only useful in the context of multi-ingress setups where
    -- one backend / deployment is is reachable under several domains.
    --
    -- multiIngress and conversationCodeURI are mutually exclusive. One of
    -- both options need to be configured.
    _multiIngress :: Maybe (Map Text HttpsUrl),
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
    _federationProtocols :: !(Maybe [ProtocolTag]),
    _mlsPrivateKeyPaths :: !(Maybe MLSPrivateKeyPaths),
    -- | FUTUREWORK: 'setFeatureFlags' should be renamed to 'setFeatureConfigs' in all types.
    _featureFlags :: !FeatureFlags,
    _disabledAPIVersions :: !(Set VersionExp),
    -- | The lifetime of a conversation guest link in seconds with the maximum of 1 year (31536000 seconds).
    -- If not set use the default `defGuestLinkTTLSeconds`
    _guestLinkTTLSeconds :: !(Maybe GuestLinkTTLSeconds),
    _passwordHashingOptions :: !(PasswordHashingOptions),
    -- | Rate limiting options for hashing passwords (used for conversation codes)
    _passwordHashingRateLimit :: RateLimitConfig,
    -- | Check group info
    _checkGroupInfo :: !(Maybe Bool),
    -- | Configuration for meetings
    _meetings :: !(Maybe MeetingsConfig)
  }
  deriving (Show, Generic)

data MeetingsConfig = MeetingsConfig
  { -- | Validity period of a meeting. After this time, the meeting is considered expired.
    _validityPeriod :: !(Maybe Duration)
  }
  deriving (Show, Generic)

deriveFromJSON toOptionFieldName ''MeetingsConfig
deriveFromJSON toOptionFieldName ''Settings

makeLenses ''Settings
makeLenses ''MeetingsConfig

defConcurrentDeletionEvents :: Int
defConcurrentDeletionEvents = 128

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
    -- | Postgresql settings, the key values must be in libpq format.
    -- https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-PARAMKEYWORDS
    _postgresql :: !(Map Text Text),
    _postgresqlPassword :: !(Maybe FilePathSecrets),
    _postgresqlPool :: !PoolConfig,
    -- | Brig endpoint
    _brig :: !Endpoint,
    -- | Gundeck endpoint
    _gundeck :: !Endpoint,
    -- | Spar endpoint
    _spar :: !Endpoint,
    -- | Federator endpoint
    _federator :: !(Maybe Endpoint),
    -- | RabbitMQ settings, required when federation is enabled.
    _rabbitmq :: !(Maybe AmqpEndpoint),
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
    _logFormat :: !(Maybe (Last LogFormat)),
    _postgresMigration :: !PostgresMigrationOpts
  }

deriveFromJSON toOptionFieldName ''Opts

makeLenses ''Opts
