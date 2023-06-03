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
    defConcurrentDeletionEvents,
    defDeleteConvThrottleMillis,
    defFanoutLimit,
    JournalOpts (..),
    Opts (..),
  )
where

import Data.Aeson (FromJSON)
import Data.Domain (Domain)
import Data.Id (TeamId)
import Data.Misc
import Data.Range
import Galley.Keys
import Galley.Types.Teams
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Wire.API.Routes.Version
import Wire.API.Team.Member

data Settings = Settings
  { -- | Number of connections for the HTTP client pool
    httpPoolSize :: !Int,
    -- | Max number of members in a team. NOTE: This must be in sync with Brig
    maxTeamSize :: !Word32,
    -- | Max number of team members users to fanout events to. For teams larger than
    --   this value, team events and user updates will no longer be sent to team users.
    --   This defaults to setMaxTeamSize and cannot be > HardTruncationLimit. Useful
    --   to tune mainly for testing purposes.
    maxFanoutSize :: !(Maybe (Range 1 HardTruncationLimit Int32)),
    -- | List of teams for which the invitation URL can be added to the list of all
    -- invitations retrievable by team admins.  See also:
    -- 'ExposeInvitationURLsToTeamAdminConfig'.
    exposeInvitationURLsTeamAllowlist :: !(Maybe [TeamId]),
    -- | Max number of members in a conversation. NOTE: This must be in sync with Brig
    maxConvSize :: !Word16,
    -- | Whether to call Brig for device listing
    intraListing :: !Bool,
    -- | URI prefix for conversations with access mode @code@
    conversationCodeURI :: !HttpsUrl,
    -- | Throttling: limits to concurrent deletion events
    concurrentDeletionEvents :: !(Maybe Int),
    -- | Throttling: delay between sending events upon team deletion
    deleteConvThrottleMillis :: !(Maybe Int),
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
    federationDomain :: !Domain,
    -- | When true, galley will assume data in `billing_team_member` table is
    -- consistent and use it for billing.
    -- When false, billing information for large teams is not guaranteed to have all
    -- the owners.
    -- Defaults to false.
    enableIndexedBillingTeamMembers :: !(Maybe Bool),
    mlsPrivateKeyPaths :: !(Maybe MLSPrivateKeyPaths),
    -- | FUTUREWORK: 'setFeatureFlags' should be renamed to 'setFeatureConfigs' in all types.
    featureFlags :: !FeatureFlags,
    disabledAPIVersions :: Maybe (Set Version)
  }
  deriving (Show, Generic)

instance FromJSON Settings

defConcurrentDeletionEvents :: Int
defConcurrentDeletionEvents = 128

defDeleteConvThrottleMillis :: Int
defDeleteConvThrottleMillis = 20

defFanoutLimit :: Range 1 HardTruncationLimit Int32
defFanoutLimit = unsafeRange hardTruncationLimit

data JournalOpts = JournalOpts
  { -- | SQS queue name to send team events
    queueName :: !Text,
    -- | AWS endpoint
    endpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)

instance FromJSON JournalOpts

data Opts = Opts
  { -- | Host and port to bind to
    galley :: !Endpoint,
    -- | Cassandra settings
    cassandra :: !CassandraOpts,
    -- | Brig endpoint
    brig :: !Endpoint,
    -- | Gundeck endpoint
    gundeck :: !Endpoint,
    -- | Spar endpoint
    spar :: !Endpoint,
    -- | Federator endpoint
    federator :: !(Maybe Endpoint),
    -- | Disco URL
    discoUrl :: !(Maybe Text),
    -- | Other settings
    settings :: !Settings,
    -- | Journaling options ('Nothing'
    --   disables journaling)
    -- Logging
    journal :: !(Maybe JournalOpts),
    -- | Log level (Debug, Info, etc)
    logLevel :: !Level,
    -- | Use netstrings encoding
    --  <http://cr.yp.to/proto/netstrings.txt>
    logNetStrings :: !(Maybe (Last Bool)),
    -- | What log format to use
    logFormat :: !(Maybe (Last LogFormat))
  }
  deriving (Show, Generic)

instance FromJSON Opts
