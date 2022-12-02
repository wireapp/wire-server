{-# LANGUAGE DeriveAnyClass #-}
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

module Brig.Options where

import Brig.Queue.Types (Queue (..))
import Brig.SMTP (SMTPConnType (..))
import Brig.Whitelist (Whitelist (..))
import qualified Brig.ZAuth as ZAuth
import Control.Applicative
import qualified Control.Lens as Lens
import Data.Aeson (defaultOptions, fieldLabelModifier, genericParseJSON)
import qualified Data.Aeson as A
import qualified Data.Char as Char
import qualified Data.Code as Code
import Data.CookieThrottle
import Data.Domain (Domain (..))
import Data.EmailVisibility
import Data.Id
import Data.LanguageCodes (ISO639_1 (EN))
import Data.LimitFailedLogins
import Data.Misc (HttpsUrl)
import Data.Nonce
import Data.Range
import Data.Schema
import Data.SuspendInactiveUsers
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock (DiffTime, NominalDiffTime, secondsToDiffTime)
import Data.Yaml (FromJSON (..), ToJSON (..), (.:), (.:?))
import qualified Data.Yaml as Y
import Galley.Types.Teams (unImplicitLockStatus)
import Imports
import qualified Network.DNS as DNS
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Wire.API.Routes.Public.Brig
import qualified Wire.API.Team.Feature as Public
import Wire.API.User
import Wire.API.User.Search (FederatedUserSearchPolicy)
import Wire.Data.Timeout

data ElasticSearchOpts = ElasticSearchOpts
  { -- | ElasticSearch URL
    url :: !Text,
    -- | The name of the ElasticSearch user index
    index :: !Text,
    -- | An additional index to write user data, useful while migrating to a new
    -- index.
    -- There is a bug hidden when using this option. Sometimes a user won't get
    -- deleted from the index. Attempts at reproducing this issue in a simpler
    -- environment have failed. As a workaround, there is a tool in
    -- tools/db/find-undead which can be used to find the undead users right
    -- after the migration, if they exist, we can run the reindexing to get data
    -- in elasticsearch in a consistent state.
    additionalWriteIndex :: !(Maybe Text),
    -- | An additional ES URL to write user data, useful while migrating to a
    -- new instace of ES. It is necessary to provide 'additionalWriteIndex' for
    -- this to be used. If this is 'Nothing' and 'additionalWriteIndex' is
    -- configured, the 'url' field will be used.
    additionalWriteIndexUrl :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data AWSOpts = AWSOpts
  { -- | Event journal queue for user events
    --   (e.g. user deletion)
    userJournalQueue :: !(Maybe Text),
    -- | Dynamo table for storing prekey data
    prekeyTable :: !Text,
    -- | AWS SQS endpoint
    sqsEndpoint :: !AWSEndpoint,
    -- | DynamoDB endpoint
    dynamoDBEndpoint :: !(Maybe AWSEndpoint)
  }
  deriving (Show, Generic)

instance FromJSON AWSOpts

data EmailAWSOpts = EmailAWSOpts
  { -- | Event feedback queue for SES
    --   (e.g. for email bounces and complaints)
    sesQueue :: !Text,
    -- | AWS SES endpoint
    sesEndpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)

instance FromJSON EmailAWSOpts

data EmailSMTPCredentials = EmailSMTPCredentials
  { -- | Username to authenticate
    --   against the SMTP server
    smtpUsername :: !Text,
    -- | File containing password to
    --   authenticate against the SMTP server
    smtpPassword :: !FilePathSecrets
  }
  deriving (Show, Generic)

instance FromJSON EmailSMTPCredentials

data EmailSMTPOpts = EmailSMTPOpts
  { -- | Hostname of the SMTP server to connect to
    smtpEndpoint :: !Endpoint,
    smtpCredentials :: !(Maybe EmailSMTPCredentials),
    -- | Which type of connection to use
    --   against the SMTP server {tls,ssl,plain}
    smtpConnType :: !SMTPConnType
  }
  deriving (Show, Generic)

instance FromJSON EmailSMTPOpts

data StompOpts = StompOpts
  { stompHost :: !Text,
    stompPort :: !Int,
    stompTls :: !Bool
  }
  deriving (Show, Generic)

instance FromJSON StompOpts

data InternalEventsOpts = InternalEventsOpts
  { internalEventsQueue :: !Queue
  }
  deriving (Show)

instance FromJSON InternalEventsOpts where
  parseJSON = Y.withObject "InternalEventsOpts" $ \o ->
    InternalEventsOpts <$> parseJSON (Y.Object o)

data EmailSMSGeneralOpts = EmailSMSGeneralOpts
  { -- | Email, SMS, ... template directory
    templateDir :: !FilePath,
    -- | Email sender address
    emailSender :: !Email,
    -- | Twilio sender identifier (sender phone number in E.104 format)
    --   or twilio messaging sender ID - see
    --   https://www.twilio.com/docs/sms/send-messages#use-an-alphanumeric-sender-id
    smsSender :: !Text,
    -- | Customizable branding text for
    --   emails/sms/calls
    templateBranding :: !BrandingOpts
  }
  deriving (Show, Generic)

instance FromJSON EmailSMSGeneralOpts

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
  deriving (Show, Generic)

instance FromJSON BrandingOpts

data EmailUserOpts = EmailUserOpts
  { -- | Activation URL template
    activationUrl :: !Text,
    -- | SMS activation URL template
    smsActivationUrl :: !Text,
    -- | Password reset URL template
    passwordResetUrl :: !Text,
    -- | Deletion URL template
    deletionUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON EmailUserOpts

-- | Provider settings
data ProviderOpts = ProviderOpts
  { -- | Homepage URL
    homeUrl :: !Text,
    -- | Activation URL template
    providerActivationUrl :: !Text,
    -- | Approval URL template
    approvalUrl :: !Text,
    -- | Approval email recipient
    approvalTo :: !Email,
    -- | Password reset URL template
    providerPwResetUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON ProviderOpts

data TeamOpts = TeamOpts
  { -- | Team Invitation URL template
    tInvitationUrl :: !Text,
    -- | Team Activation URL template
    tActivationUrl :: !Text,
    -- | Team Creator Welcome URL
    tCreatorWelcomeUrl :: !Text,
    -- | Team Member Welcome URL
    tMemberWelcomeUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON TeamOpts

data EmailOpts
  = EmailAWS EmailAWSOpts
  | EmailSMTP EmailSMTPOpts
  deriving (Show, Generic)

instance FromJSON EmailOpts where
  parseJSON o =
    EmailAWS <$> parseJSON o
      <|> EmailSMTP <$> parseJSON o

data EmailSMSOpts = EmailSMSOpts
  { email :: !EmailOpts,
    general :: !EmailSMSGeneralOpts,
    user :: !EmailUserOpts,
    provider :: !ProviderOpts,
    team :: !TeamOpts
  }
  deriving (Show, Generic)

instance FromJSON EmailSMSOpts

-- | ZAuth options
data ZAuthOpts = ZAuthOpts
  { -- | Private key file
    privateKeys :: !FilePath,
    -- | Public key file
    publicKeys :: !FilePath,
    -- | Other settings
    authSettings :: !ZAuth.Settings
  }
  deriving (Show, Generic)

instance FromJSON ZAuthOpts

-- | TURN server options
data TurnOpts = TurnOpts
  { -- | Where to get list of turn servers from
    serversSource :: !TurnServersSource,
    -- | TURN shared secret file path
    secret :: !FilePath,
    -- | For how long TURN credentials should be
    --   valid, in seconds
    tokenTTL :: !Word32,
    -- | How long until a new TURN configuration
    --   should be fetched, in seconds
    configTTL :: !Word32
  }
  deriving (Show)

instance FromJSON TurnOpts where
  parseJSON = A.withObject "TurnOpts" $ \o -> do
    sourceName <- o .: "serversSource"
    source <-
      case sourceName of
        "files" -> TurnSourceFiles <$> A.parseJSON (A.Object o)
        "dns" -> TurnSourceDNS <$> A.parseJSON (A.Object o)
        _ -> fail $ "TurnOpts: Invalid sourceType, expected one of [files, dns] but got: " <> Text.unpack sourceName
    TurnOpts source
      <$> o .: "secret"
      <*> o .: "tokenTTL"
      <*> o .: "configTTL"

data TurnServersSource
  = TurnSourceDNS TurnDnsOpts
  | TurnSourceFiles TurnServersFiles
  deriving (Show)

data TurnServersFiles = TurnServersFiles
  { tsfServers :: !FilePath,
    tsfServersV2 :: !FilePath
  }
  deriving (Show)

instance FromJSON TurnServersFiles where
  parseJSON = A.withObject "TurnServersFiles" $ \o ->
    TurnServersFiles
      <$> o .: "servers"
      <*> o .: "serversV2"

data TurnDnsOpts = TurnDnsOpts
  { tdoBaseDomain :: DNS.Domain,
    tdoDiscoveryIntervalSeconds :: !(Maybe DiffTime)
  }
  deriving (Show)

instance FromJSON TurnDnsOpts where
  parseJSON = A.withObject "TurnDnsOpts" $ \o ->
    TurnDnsOpts
      <$> (asciiOnly =<< o .: "baseDomain")
      <*> o .:? "discoveryIntervalSeconds"

data ListAllSFTServers
  = ListAllSFTServers
  | HideAllSFTServers
  deriving (Show, Eq, Ord)
  deriving (FromJSON) via Schema ListAllSFTServers

instance ToSchema ListAllSFTServers where
  schema =
    enum @Text "ListSFTServers" $
      mconcat
        [ element "enabled" ListAllSFTServers,
          element "disabled" HideAllSFTServers
        ]

data FederationDomainConfig = FederationDomainConfig
  { domain :: Domain,
    cfgSearchPolicy :: FederatedUserSearchPolicy
  }
  deriving (Show, Generic)
  deriving (ToJSON, FromJSON) via Schema FederationDomainConfig

instance ToSchema FederationDomainConfig where
  schema =
    object "FederationDomainConfig" $
      FederationDomainConfig
        <$> domain .= field "domain" schema
        <*> cfgSearchPolicy .= field "search_policy" schema

-- | Options that are consumed on startup
data Opts = Opts
  -- services
  { -- | Host and port to bind to
    brig :: !Endpoint,
    -- | Cargohold address
    cargohold :: !Endpoint,
    -- | Galley address
    galley :: !Endpoint,
    -- | Gundeck address
    gundeck :: !Endpoint,
    -- | Federator address
    federatorInternal :: !(Maybe Endpoint),
    -- external

    -- | Cassandra settings
    cassandra :: !CassandraOpts,
    -- | ElasticSearch settings
    elasticsearch :: !ElasticSearchOpts,
    -- | AWS settings
    aws :: !AWSOpts,
    -- | Enable Random Prekey Strategy
    randomPrekeys :: !(Maybe Bool),
    -- | STOMP broker settings
    stomp :: !(Maybe StompOpts),
    -- Email & SMS

    -- | Email and SMS settings
    emailSMS :: !EmailSMSOpts,
    -- ZAuth

    -- | ZAuth settings
    zauth :: !ZAuthOpts,
    -- Misc.

    -- | Disco URL
    discoUrl :: !(Maybe Text),
    -- | GeoDB file path
    geoDb :: !(Maybe FilePath),
    -- | Event queue for
    --   Brig-generated events (e.g.
    --   user deletion)
    internalEvents :: !InternalEventsOpts,
    -- Logging

    -- | Log level (Debug, Info, etc)
    logLevel :: !Level,
    -- | Use netstrings encoding (see
    --   <http://cr.yp.to/proto/netstrings.txt>)
    logNetStrings :: !(Maybe (Last Bool)),
    -- | Logformat to use
    -- TURN
    logFormat :: !(Maybe (Last LogFormat)),
    -- | TURN server settings
    turn :: !TurnOpts,
    -- | SFT Settings
    sft :: !(Maybe SFTOptions),
    -- | Runtime settings
    optSettings :: !Settings
  }
  deriving (Show, Generic)

-- | Options that persist as runtime settings.
data Settings = Settings
  { -- | Activation timeout, in seconds
    setActivationTimeout :: !Timeout,
    -- | Default verification code timeout, in seconds
    -- use `setVerificationTimeout` as the getter function which always provides a default value
    setVerificationCodeTimeoutInternal :: !(Maybe Code.Timeout),
    -- | Team invitation timeout, in seconds
    setTeamInvitationTimeout :: !Timeout,
    -- | Check for expired users every so often, in seconds
    setExpiredUserCleanupTimeout :: !(Maybe Timeout),
    -- | Twilio credentials
    setTwilio :: !FilePathSecrets,
    -- | Nexmo credentials
    setNexmo :: !FilePathSecrets,
    -- | STOMP broker credentials
    setStomp :: !(Maybe FilePathSecrets),
    -- | Whitelist of allowed emails/phones
    setWhitelist :: !(Maybe Whitelist),
    -- | Max. number of sent/accepted
    --   connections per user
    setUserMaxConnections :: !Int64,
    -- | Max. number of permanent clients per user
    setUserMaxPermClients :: !(Maybe Int),
    -- | Whether to allow plain HTTP transmission
    --   of cookies (for testing purposes only)
    setCookieInsecure :: !Bool,
    -- | Minimum age of a user cookie before
    --   it is renewed during token refresh
    setUserCookieRenewAge :: !Integer,
    -- | Max. # of cookies per user and cookie type
    setUserCookieLimit :: !Int,
    -- | Throttling settings (not to be confused
    -- with 'LoginRetryOpts')
    setUserCookieThrottle :: !CookieThrottle,
    -- | Block user from logging in
    -- for m minutes after n failed
    -- logins
    setLimitFailedLogins :: !(Maybe LimitFailedLogins),
    -- | If last cookie renewal is too long ago,
    -- suspend the user.
    setSuspendInactiveUsers :: !(Maybe SuspendInactiveUsers),
    -- | Max size of rich info (number of chars in
    --   field names and values), should be in sync
    --   with Spar
    setRichInfoLimit :: !Int,
    -- | Default locale to use when selecting templates
    -- use `setDefaultTemplateLocale` as the getter function which always provides a default value
    setDefaultTemplateLocaleInternal :: !(Maybe Locale),
    -- | Default locale to use for users
    -- use `setDefaultUserLocale` as the getter function which always provides a default value
    setDefaultUserLocaleInternal :: !(Maybe Locale),
    -- | Max. # of members in a team.
    --   NOTE: This must be in sync with galley
    setMaxTeamSize :: !Word32,
    -- | Max. # of members in a conversation.
    --   NOTE: This must be in sync with galley
    setMaxConvSize :: !Word16,
    -- | Filter ONLY services with
    --   the given provider id
    setProviderSearchFilter :: !(Maybe ProviderId),
    -- | Whether to expose user emails and to whom
    setEmailVisibility :: !EmailVisibility,
    -- | Max length of user properties keys (in characters)
    setPropertyMaxKeyLen :: !(Maybe Int64),
    -- | Max length of user properties values (in characters)
    setPropertyMaxValueLen :: !(Maybe Int64),
    -- | How long, in milliseconds, to wait
    -- in between processing delete events
    -- from the internal delete queue
    setDeleteThrottleMillis :: !(Maybe Int),
    -- | When true, search only
    -- returns users from the same team
    setSearchSameTeamOnly :: !(Maybe Bool),
    -- | FederationDomain is required, even when not wanting to federate with other backends
    -- (in that case the 'allowedDomains' can be set to empty in Federator)
    -- Federation domain is used to qualify local IDs and handles,
    -- e.g. 0c4d8944-70fa-480e-a8b7-9d929862d18c@wire.com and somehandle@wire.com.
    -- It should also match the SRV DNS records under which other wire-server installations can find this backend:
    --    _wire-server-federator._tcp.<federationDomain>
    -- Once set, DO NOT change it: if you do, existing users may have a broken experience and/or stop working
    -- Remember to keep it the same in Galley.
    -- Example:
    --   allowedDomains:
    --     - wire.com
    --     - example.com
    setFederationDomain :: !Domain,
    setFederationDomainConfigs :: !(Maybe [FederationDomainConfig]),
    -- | The amount of time in milliseconds to wait after reading from an SQS queue
    -- returns no message, before asking for messages from SQS again.
    -- defaults to 'defSqsThrottleMillis'.
    -- When using real SQS from AWS, throttling isn't needed as much, since using
    --   SQS.rmWaitTimeSeconds (Just 20) in Brig.AWS.listen
    -- ensures that there is only one request every 20 seconds.
    -- However, that parameter is not honoured when using fake-sqs
    -- (where throttling can thus make sense)
    setSqsThrottleMillis :: !(Maybe Int),
    -- | Do not allow certain user creation flows.
    -- docs/reference/user/registration.md {#RefRestrictRegistration}.
    setRestrictUserCreation :: !(Maybe Bool),
    -- | The analog to `Galley.Options.setFeatureFlags`.  See 'AccountFeatureConfigs'.
    setFeatureFlags :: !(Maybe AccountFeatureConfigs),
    -- | Customer extensions.  Read 'CustomerExtensions' docs carefully!
    setCustomerExtensions :: !(Maybe CustomerExtensions),
    -- | When set; instead of using SRV lookups to discover SFTs the calls
    -- config will always return this entry. This is useful in Kubernetes
    -- where SFTs are deployed behind a load-balancer.  In the long-run the SRV
    -- fetching logic can go away completely
    setSftStaticUrl :: !(Maybe HttpsUrl),
    -- | When set the /calls/config/v2 endpoint will include all the
    -- loadbalanced servers of `setSftStaticUrl` under the @sft_servers_all@
    -- field. The default setting is to exclude and omit the field from the
    -- response.
    setSftListAllServers :: Maybe ListAllSFTServers,
    setKeyPackageMaximumLifetime :: Maybe NominalDiffTime,
    -- | When set, development API versions are advertised to clients.
    setEnableDevelopmentVersions :: Maybe Bool,
    -- | Minimum delay in seconds between consecutive attempts to generate a new verification code.
    -- use `set2FACodeGenerationDelaySecs` as the getter function which always provides a default value
    set2FACodeGenerationDelaySecsInternal :: !(Maybe Int),
    -- | The time-to-live of a nonce in seconds.
    -- use `setNonceTtlSecs` as the getter function which always provides a default value
    setNonceTtlSecsInternal :: !(Maybe NonceTtlSecs),
    -- | The maximum number of seconds of clock skew the implementation of generate_dpop_access_token in jwt-tools will allow
    -- use `setDpopMaxSkewSecs` as the getter function which always provides a default value
    setDpopMaxSkewSecsInternal :: !(Maybe Word16),
    -- | The expiration time of a JWT DPoP token in seconds.
    -- use `setDpopTokenExpirationTimeSecs` as the getter function which always provides a default value
    setDpopTokenExpirationTimeSecsInternal :: !(Maybe Word64),
    -- | Path to a .pem file containing the server's public key and private key
    -- e.g. to sign JWT tokens
    setPublicKeyBundle :: !(Maybe FilePath)
  }
  deriving (Show, Generic)

defaultTemplateLocale :: Locale
defaultTemplateLocale = Locale (Language EN) Nothing

defaultUserLocale :: Locale
defaultUserLocale = defaultTemplateLocale

setDefaultUserLocale :: Settings -> Locale
setDefaultUserLocale = fromMaybe defaultUserLocale . setDefaultUserLocaleInternal

defVerificationTimeout :: Code.Timeout
defVerificationTimeout = Code.Timeout (60 * 10) -- 10 minutes

setVerificationTimeout :: Settings -> Code.Timeout
setVerificationTimeout = fromMaybe defVerificationTimeout . setVerificationCodeTimeoutInternal

setDefaultTemplateLocale :: Settings -> Locale
setDefaultTemplateLocale = fromMaybe defaultTemplateLocale . setDefaultTemplateLocaleInternal

def2FACodeGenerationDelaySecs :: Int
def2FACodeGenerationDelaySecs = 5 * 60 -- 5 minutes

set2FACodeGenerationDelaySecs :: Settings -> Int
set2FACodeGenerationDelaySecs = fromMaybe def2FACodeGenerationDelaySecs . set2FACodeGenerationDelaySecsInternal

defaultNonceTtlSecs :: NonceTtlSecs
defaultNonceTtlSecs = NonceTtlSecs $ 5 * 60 -- 5 minutes

setNonceTtlSecs :: Settings -> NonceTtlSecs
setNonceTtlSecs = fromMaybe defaultNonceTtlSecs . setNonceTtlSecsInternal

defaultDpopMaxSkewSecs :: Word16
defaultDpopMaxSkewSecs = 1

setDpopMaxSkewSecs :: Settings -> Word16
setDpopMaxSkewSecs = fromMaybe defaultDpopMaxSkewSecs . setDpopMaxSkewSecsInternal

defaultDpopTokenExpirationTimeSecs :: Word64
defaultDpopTokenExpirationTimeSecs = 30

setDpopTokenExpirationTimeSecs :: Settings -> Word64
setDpopTokenExpirationTimeSecs = fromMaybe defaultDpopTokenExpirationTimeSecs . setDpopTokenExpirationTimeSecsInternal

getAfcConferenceCallingDefNewMaybe :: Lens.Getter Settings (Maybe (Public.WithStatus Public.ConferenceCallingConfig))
getAfcConferenceCallingDefNewMaybe = Lens.to (Lens.^? (Lens.to setFeatureFlags . Lens._Just . Lens.to afcConferenceCallingDefNew . unImplicitLockStatus))

getAfcConferenceCallingDefNullMaybe :: Lens.Getter Settings (Maybe (Public.WithStatus Public.ConferenceCallingConfig))
getAfcConferenceCallingDefNullMaybe = Lens.to (Lens.^? (Lens.to setFeatureFlags . Lens._Just . Lens.to afcConferenceCallingDefNull . unImplicitLockStatus))

getAfcConferenceCallingDefNew :: Lens.Getter Settings (Public.WithStatus Public.ConferenceCallingConfig)
getAfcConferenceCallingDefNew = Lens.to (Public._unImplicitLockStatus . afcConferenceCallingDefNew . fromMaybe defAccountFeatureConfigs . setFeatureFlags)

getAfcConferenceCallingDefNull :: Lens.Getter Settings (Public.WithStatus Public.ConferenceCallingConfig)
getAfcConferenceCallingDefNull = Lens.to (Public._unImplicitLockStatus . afcConferenceCallingDefNull . fromMaybe defAccountFeatureConfigs . setFeatureFlags)

defAccountFeatureConfigs :: AccountFeatureConfigs
defAccountFeatureConfigs =
  AccountFeatureConfigs
    { afcConferenceCallingDefNew = Public.ImplicitLockStatus Public.defFeatureStatus,
      afcConferenceCallingDefNull = Public.ImplicitLockStatus Public.defFeatureStatus
    }

-- | Customer extensions naturally are covered by the AGPL like everything else, but use them
-- at your own risk!  If you use the default server config and do not set
-- @customerExtensions@, none of this will have any effect.
--
-- This is code implemented to comply with particular contracts.  It may change or be removed
-- at any point in the future without any further notice.
data CustomerExtensions = CustomerExtensions
  { -- | You cannot create an account (free user without team or user with new team) if your
    -- email address has a domain listed here.  You can only accept an invitation from a team
    -- that has already been created.
    --
    -- This feature is a work-around for issues with our somewhat convoluted onboarding
    -- process.  We are working on a more sustainable solution.  Meanwhile this should not be
    -- used unless absolutely necessary (legal reasons, constracts).  It has numerous
    -- drawbacks:
    --
    -- * Changing it requires changing the configuration of the backend, which usually means
    --   a new release.  This is intentional to keep this feature from contaminating more of
    --   the code base (see below), but it also makes it awkward to use.
    --
    -- * So far, we have been using emails as opaque identifiers and not made any assumptions
    --   about their structure.  Now, email is still securely associated with a user, but can
    --   also be securely associated with another user who owns the first user's domain.  This
    --   new setup is more complex, and complexity is bad for security.  Security is now based
    --   on a much larger number of assumptions, and any one of these assumptions can be
    --   broken by software or usage errors.  Example: is it even legal for the owner of an
    --   email domain to keep users from signing up with wire using that email?  This is
    --   possibly true for domains like @mystartup.com@, but what about
    --   @globalmailhosting.com@?  Other example: next point.
    --
    -- * We could implement something more sophisticated involving 'TXT' DNS records that the
    --   team admin needs to set up containing some secrets obtainable via team settings.
    --   This is a lot more involved to implement, and very easy for coders or users to get
    --   wrong.
    --
    -- Bottom line: you probably want to keep either @customerExtensions = Nothing@ or at
    -- least @domainsBlockedForRegistration = []@.  :)
    domainsBlockedForRegistration :: DomainsBlockedForRegistration
  }
  deriving (Show, FromJSON, Generic)

-- | See also: "Galley.API.CustomBackend", `galley.custom_backend`.
newtype DomainsBlockedForRegistration = DomainsBlockedForRegistration [Domain]
  deriving newtype (Show, FromJSON, Generic)

data SFTOptions = SFTOptions
  { sftBaseDomain :: !DNS.Domain,
    sftSRVServiceName :: !(Maybe ByteString), -- defaults to defSftServiceName if unset
    sftDiscoveryIntervalSeconds :: !(Maybe DiffTime), -- defaults to defSftDiscoveryIntervalSeconds
    sftListLength :: !(Maybe (Range 1 100 Int)) -- defaults to defSftListLength
  }
  deriving (Show, Generic)

instance FromJSON SFTOptions where
  parseJSON = Y.withObject "SFTOptions" $ \o ->
    SFTOptions
      <$> (asciiOnly =<< o .: "sftBaseDomain")
      <*> (mapM asciiOnly =<< o .:? "sftSRVServiceName")
      <*> (secondsToDiffTime <$$> o .:? "sftDiscoveryIntervalSeconds")
      <*> (o .:? "sftListLength")

asciiOnly :: Text -> Y.Parser ByteString
asciiOnly t =
  if Text.all Char.isAscii t
    then pure $ Text.encodeUtf8 t
    else fail $ "Expected ascii string only, found: " <> Text.unpack t

defMaxKeyLen :: Int64
defMaxKeyLen = 1024

defMaxValueLen :: Int64
defMaxValueLen = 524288

defDeleteThrottleMillis :: Int
defDeleteThrottleMillis = 100

defSqsThrottleMillis :: Int
defSqsThrottleMillis = 500

defUserMaxPermClients :: Int
defUserMaxPermClients = 7

defSftServiceName :: ByteString
defSftServiceName = "_sft"

defSrvDiscoveryIntervalSeconds :: DiffTime
defSrvDiscoveryIntervalSeconds = secondsToDiffTime 10

defSftListLength :: Range 1 100 Int
defSftListLength = unsafeRange 5

instance FromJSON Settings where
  parseJSON = genericParseJSON customOptions
    where
      customOptions =
        defaultOptions
          { fieldLabelModifier = \case
              "setDefaultUserLocaleInternal" -> "setDefaultUserLocale"
              "setDefaultTemplateLocaleInternal" -> "setDefaultTemplateLocale"
              "setVerificationCodeTimeoutInternal" -> "setVerificationTimeout"
              "set2FACodeGenerationDelaySecsInternal" -> "set2FACodeGenerationDelaySecs"
              "setNonceTtlSecsInternal" -> "setNonceTtlSecs"
              "setDpopMaxSkewSecsInternal" -> "setDpopMaxSkewSecs"
              "setDpopTokenExpirationTimeSecsInternal" -> "setDpopTokenExpirationTimeSecs"
              other -> other
          }

instance FromJSON Opts

-- TODO: Does it make sense to generate lens'es for all?
Lens.makeLensesFor
  [ ("optSettings", "optionSettings"),
    ("elasticsearch", "elasticsearchL"),
    ("sft", "sftL"),
    ("turn", "turnL")
  ]
  ''Opts

Lens.makeLensesFor
  [ ("setEmailVisibility", "emailVisibility"),
    ("setPropertyMaxKeyLen", "propertyMaxKeyLen"),
    ("setPropertyMaxValueLen", "propertyMaxValueLen"),
    ("setSearchSameTeamOnly", "searchSameTeamOnly"),
    ("setUserMaxPermClients", "userMaxPermClients"),
    ("setFederationDomain", "federationDomain"),
    ("setSqsThrottleMillis", "sqsThrottleMillis"),
    ("setSftStaticUrl", "sftStaticUrl"),
    ("setSftListAllServers", "sftListAllServers"),
    ("setFederationDomainConfigs", "federationDomainConfigs"),
    ("setEnableDevelopmentVersions", "enableDevelopmentVersions"),
    ("setRestrictUserCreation", "restrictUserCreation")
  ]
  ''Settings

Lens.makeLensesFor
  [ ("url", "urlL"),
    ("index", "indexL"),
    ("additionalWriteIndex", "additionalWriteIndexL"),
    ("additionalWriteIndexUrl", "additionalWriteIndexUrlL")
  ]
  ''ElasticSearchOpts

Lens.makeLensesFor [("sftBaseDomain", "sftBaseDomainL")] ''SFTOptions

Lens.makeLensesFor [("serversSource", "serversSourceL")] ''TurnOpts
