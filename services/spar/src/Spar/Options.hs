{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Reading the Spar config.
module Spar.Options
  ( Opts (..),
    Settings (..),
    ZAuthOpts (..),
    EmailSMSOpts (..),
    BrandingOpts (..),
    CustomerExtensions (..),
    DomainsBlockedForRegistration (..),
    ElasticSearchOpts (..),
    getOpts,
    readOptsFile,
    maxttlAuthreqDiffTime,
  )
where

import Control.Exception
import Data.Aeson hiding (fieldLabelModifier)
import Data.Domain (Domain)
import Data.Time
import qualified Data.Yaml as Yaml
import qualified Database.Bloodhound.Types as ES
import Imports
import Network.AMQP.Extended (AmqpEndpoint)
import Options.Applicative
import SAML2.WebSSO
import qualified SAML2.WebSSO as SAML
import System.Logger.Extended (Level, LogFormat)
import URI.ByteString
import Util.Options
import Util.Timeout (Timeout)
import Wire.API.Allowlists (AllowlistEmailDomains)
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfig, FederationStrategy)
import Wire.API.Routes.Version
import Wire.API.User (EmailAddress, EmailVisibilityConfig, Locale)
import Wire.API.User.Orphans ()
import Wire.API.User.Saml
import Wire.AuthenticationSubsystem.Config (ZAuthSettings)
import Wire.AuthenticationSubsystem.Cookie.Limit (CookieThrottle)
import Wire.RateLimit.Interpreter (RateLimitConfig)

data Opts = Opts
  { saml :: !SAML.Config,
    brig :: !Endpoint,
    galley :: !Endpoint,
    cassandra :: !CassandraOpts,
    -- | Federator address
    federatorInternal :: !(Maybe Endpoint),
    -- | RabbitMQ settings, required when federation is enabled.
    rabbitmq :: !(Maybe AmqpEndpoint),
    maxttlAuthreq :: !(TTL "authreq"),
    maxttlAuthresp :: !(TTL "authresp"),
    -- | The maximum number of SCIM tokens that we will allow teams to have.
    maxScimTokens :: !Int,
    -- | The maximum size of rich info. Should be in sync with 'Brig.Types.richInfoLimit'.
    richInfoLimit :: !Int,
    -- | Wire/AWS specific; optional; used to discover Cassandra instance
    -- IPs using describe-instances.
    discoUrl :: !(Maybe Text),
    -- | Log level
    logLevel :: !Level,
    logNetStrings :: !(Maybe (Last Bool)),
    logFormat :: !(Maybe (Last LogFormat)),
    disabledAPIVersions :: !(Set VersionExp),
    scimBaseUri :: URI,
    -- | Runtime settings
    settings :: !Settings
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Options that persist as runtime settings
data Settings = Settings
  { -- | FederationDomain is required, even when not wanting to federate
    federationDomain :: !Domain,
    -- | See https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections
    federationStrategy :: !(Maybe FederationStrategy),
    -- | Federation domain configs
    federationDomainConfigs :: !(Maybe [FederationDomainConfig]),
    -- | How long until a new TURN configuration should be fetched, in seconds
    federationDomainConfigsUpdateFreq :: !(Maybe Int),
    -- | Password hashing options
    passwordHashingOptions :: !PasswordHashingOptions,
    -- | Rate limit config for password hashing
    passwordHashingRateLimit :: !RateLimitConfig,
    -- | STOMP broker credentials
    stomp :: !(Maybe FilePathSecrets),
    -- | The amount of time in milliseconds to wait after reading from an SQS queue
    sqsThrottleMillis :: !(Maybe Int),
    -- | Whether to expose user emails and to whom
    emailVisibility :: !EmailVisibilityConfig,
    -- | Default locale to use for users
    defaultUserLocale :: !(Maybe Locale),
    -- | When true, search only returns users from the same team
    searchSameTeamOnly :: !(Maybe Bool),
    -- | Max. # of members in a team (should be in sync with galley)
    maxTeamSize :: !Word32,
    -- | Activation timeout, in seconds
    activationTimeout :: !Timeout,
    -- | Customer extensions - blocked domains for registration
    customerExtensions :: !(Maybe CustomerExtensions),
    -- | Whitelist of allowed emails/phones
    allowlistEmailDomains :: !(Maybe AllowlistEmailDomains),
    -- | Minimum age of a user cookie before it is renewed during token refresh
    userCookieRenewAge :: !Integer,
    -- | Max. # of cookies per user and cookie type
    userCookieLimit :: !Int,
    -- | Throttling settings for user cookies
    userCookieThrottle :: !CookieThrottle
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Customer extensions for blocked domains
data CustomerExtensions = CustomerExtensions
  { domainsBlockedForRegistration :: !DomainsBlockedForRegistration
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype DomainsBlockedForRegistration = DomainsBlockedForRegistration (HashSet Domain)
  deriving stock (Show)
  deriving newtype (FromJSON)

-- | ZAuth options
data ZAuthOpts = ZAuthOpts
  { -- | Private key file
    privateKeys :: !FilePath,
    -- | Public key file
    publicKeys :: !FilePath,
    -- | Other settings
    authSettings :: !ZAuthSettings
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

-- | Email and SMS settings (simplified for Spar)
data EmailSMSOpts = EmailSMSOpts
  { -- | Template directory
    templateDir :: !FilePath,
    -- | Email sender address
    emailSender :: !EmailAddress,
    -- | Customizable branding text
    templateBranding :: !BrandingOpts
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data BrandingOpts = BrandingOpts
  { brand :: !Text,
    brandUrl :: !Text,
    brandLabelUrl :: !Text,
    brandLogoUrl :: !Text,
    brandService :: !Text,
    copyright :: !Text,
    misuse :: !Text,
    legal :: !Text,
    forgot :: !Text,
    support :: !Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data ElasticSearchOpts = ElasticSearchOpts
  { -- | ElasticSearch URL
    url :: !ES.Server,
    -- | The name of the ElasticSearch user index
    index :: !ES.IndexName,
    -- | Elasticsearch credentials
    credentials :: !(Maybe FilePathSecrets),
    -- | Credentials for additional ES index (maily used for migrations)
    additionalCredentials :: !(Maybe FilePathSecrets),
    insecureSkipVerifyTls :: Bool,
    caCert :: Maybe FilePath
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

maxttlAuthreqDiffTime :: Opts -> NominalDiffTime
maxttlAuthreqDiffTime = ttlToNominalDiffTime . maxttlAuthreq

-- | Throws an exception if no config file is found.
getOpts :: IO Opts
getOpts = do
  let desc = "Spar - SSO Service"
  readOptsFile
    =<< execParser (info (helper <*> cliOptsParser) (header desc <> fullDesc))

-- | This should not leave this module.  It is only for callling 'sparResponseURI' before the 'Spar'
-- monad is fully initialized.
newtype WithConfig a = WithConfig (Reader Opts a)
  deriving newtype (Functor, Applicative, Monad)

instance SAML.HasConfig WithConfig where
  getConfig = WithConfig $ asks saml

-- | Accept config file location as cli option.
--
-- FUTUREWORK: it would be nicer for the Parser to return the contents of the file, and return an
-- error that explains the cli options if it doesn't succeed.
cliOptsParser :: Parser FilePath
cliOptsParser =
  strOption $
    long "config-file"
      <> short 'c'
      <> help "Spar application config to load"
      <> showDefault
      <> value defaultSparPath
  where
    defaultSparPath = "/etc/wire/spar/conf/spar.yaml"

readOptsFile :: FilePath -> IO Opts
readOptsFile path =
  either err1 pure =<< Yaml.decodeFileEither path
  where
    err1 = throwIO . ErrorCall . ("no or bad config file: " <>) . show
