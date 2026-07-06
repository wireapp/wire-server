{-# LANGUAGE TemplateHaskell #-}

module Wire.Options where

import Amazonka (Region)
import Amazonka.Types (S3AddressingStyle)
import Control.Lens (makePrisms)
import Data.Aeson (FromJSON (..), Value (..), parseJSON, withObject, (.:), (.:?))
import Data.Aeson.TH (Options (..), defaultOptions, deriveFromJSON, deriveJSON)
import Data.Aeson.Types qualified as A
import Data.Char qualified as Char
import Data.Code qualified as Code
import Data.Domain (Domain (..))
import Data.Id (ProviderId)
import Data.LanguageCodes (ISO639_1 (EN))
import Data.Proxy (Proxy (..))
import Data.Range (Range, toRange)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (DiffTime, secondsToDiffTime)
import Database.Bloodhound.Types qualified as ES
import Hasql.Pool.Extended (PoolConfig)
import Imports
import Network.AMQP.Extended (AmqpEndpoint)
import Network.DNS qualified as DNS
import System.Logger.Extended (Level, LogFormat)
import Util.Options
import Util.Options.Common (toOptionFieldName)
import Util.Timeout (Timeout)
import Wire.API.Allowlists (AllowlistEmailDomains (..))
import Wire.API.Routes.FederationDomainConfig (FederationDomainConfig (..), FederationRestriction (..), FederationStrategy)
import Wire.API.Routes.Version
import Wire.API.Team.FeatureFlags (FeatureFlags)
import Wire.API.User
import Wire.AuthenticationSubsystem.Config (ZAuthSettings)
import Wire.AuthenticationSubsystem.Cookie.Limit (CookieThrottle)
import Wire.EmailSending.SMTP (SMTPConnType (..))
import Wire.EmailSubsystem.Template (TeamOpts)
import Wire.PostgresMigrationOpts (PostgresMigrationOpts)
import Wire.RateLimit.Interpreter

asciiOnly :: Text -> A.Parser ByteString
asciiOnly t =
  if Text.all Char.isAscii t
    then pure $ Text.encodeUtf8 t
    else fail $ "Expected ascii string only, found: " <> Text.unpack t

defaultLocale :: Locale
defaultLocale = Locale (Language EN) Nothing

data WireConfig = WireConfig
  { internalServices :: InternalServices,
    externalServices :: ExternalServices,
    settings :: WireSettings
  }

data InternalServices = InternalServices
  { brig :: !Endpoint,
    cargohold :: !Endpoint,
    galley :: !Endpoint,
    spar :: !Endpoint,
    gundeck :: !Endpoint,
    federatorInternal :: !(Maybe Endpoint),
    backgroundWorker :: !Endpoint,
    wireServerEnterprise :: !(Maybe Endpoint)
  }

data ExternalServices = ExternalServices
  { cassandraBrig :: !CassandraOpts,
    cassandraGalley :: !CassandraOpts,
    cassandraGundeck :: !CassandraOpts,
    cassandraSpar :: !CassandraOpts,
    elasticsearch :: !ElasticSearchOpts,
    redis :: !RedisEndpoint,
    redisAdditionalWrite :: !(Maybe RedisEndpoint),
    -- | Postgresql settings, the key values must be in libpq format.
    -- https://www.postgresql.org/docs/17/libpq-connect.html#LIBPQ-PARAMKEYWORDS
    postgresql :: !(Map Text Text),
    postgresqlPassword :: !(Maybe FilePathSecrets),
    postgresqlPool :: !PoolConfig,
    rabbitmq :: !AmqpEndpoint,
    email :: !EmailOpts,
    prekeySelection :: !PrekeySelectionOpts,
    -- TODO: See if user and team journal can even be configured differently. If
    -- everything is supposed to be used by ibis, we cannot actually configure
    -- them seperately anyway.
    -- userJournal :: !(Maybe SqsOpts),
    -- teamJournal :: !(Maybe SqsOpts),
    sqs :: !SqsOpts,
    assets :: !AssetOpts,
    pushNotifications :: !PushNotifiactionOpts
  }

data RedisConnectionMode
  = Master
  | Cluster
  deriving (Show, Generic)

data RedisEndpoint = RedisEndpoint
  { _host :: !Text,
    _port :: !Word16,
    _connectionMode :: !RedisConnectionMode,
    _enableTls :: !Bool,
    -- | When not specified, use system CA bundle
    _tlsCa :: !(Maybe FilePath),
    -- | When 'True', uses TLS but does not verify hostname or CA or validity of
    -- the cert. Not recommended to set to 'True'.
    _insecureSkipVerifyTls :: !Bool
  }
  deriving (Show, Generic)

data PrekeySelectionOpts
  = RandomPrekeySelection
  | DynamoDBPrekeySelection !DynamoDBPrekeySelectionOpts

data DynamoDBPrekeySelectionOpts = DynamoDBPrekeySelectionOpts
  { dynamoDBEndpoint :: !AWSEndpoint,
    tableName :: !Text
  }

data SqsOpts = SqsOpts
  { sqsEndpoint :: !AWSEndpoint,
    internalEventsQueue :: !Text,
    userJournalQueue :: !(Maybe Text),
    teamJournalQueue :: !(Maybe Text)
  }

data AssetOpts = AssetOpts
  { s3Endpoint :: !AWSEndpoint,
    -- | S3 can either by addressed in path style, i.e.
    -- https://<s3-endpoint>/<bucket-name>/<object>, or vhost style, i.e.
    -- https://<bucket-name>.<s3-endpoint>/<object>. AWS's S3 offering has
    -- deprecated path style addressing for S3 and completely disabled it for
    -- buckets created after 30 Sep 2020:
    -- https://aws.amazon.com/blogs/aws/amazon-s3-path-deprecation-plan-the-rest-of-the-story/
    --
    -- However other object storage providers (specially self-deployed ones like
    -- MinIO) may not support vhost style addressing yet (or ever?). Users of
    -- such buckets should configure this option to "path".
    --
    -- Installations using S3 service provided by AWS, should use "auto", this
    -- option will ensure that vhost style is only used when it is possible to
    -- construct a valid hostname from the bucket name and the bucket name
    -- doesn't contain a '.'. Having a '.' in the bucket name causes TLS
    -- validation to fail, hence it is not used by default.
    --
    -- Using "virtual" as an option is only useful in situations where vhost
    -- style addressing must be used even if it is not possible to construct a
    -- valid hostname from the bucket name or the S3 service provider can ensure
    -- correct certificate is issued for bucket which contain one or more '.'s
    -- in the name.
    --
    -- When this option is unspecified, we default to path style addressing to
    -- ensure smooth transition for older deployments.
    s3AddressingStyle :: !(Maybe OptS3AddressingStyle),
    -- | S3 endpoint for generating download links. Useful if Cargohold is configured to use
    -- an S3 replacement running inside the internal network (in which case internally we
    -- would use one hostname for S3, and when generating an asset link for a client app, we
    -- would use another hostname).
    s3DownloadEndpoint :: !(Maybe AWSEndpoint),
    s3Bucket :: !Text,
    -- | Enable this option for compatibility with specific S3 backends.
    s3Compatibility :: !(Maybe S3Compatibility),
    cloudFront :: !(Maybe CloudFrontOpts),
    -- | @Z-Host@ header to s3 download endpoint `Map`
    --
    -- This logic is: If the @Z-Host@ header is provided and found in this map,
    -- the map's values is taken as s3 download endpoint to redirect to;
    -- otherwise a 404 is retuned. This option is only useful
    -- in the context of multi-ingress setups where one backend / deployment is
    -- reachable under several domains.
    multiIngress :: !(Maybe (Map String AWSEndpoint))
  }

newtype OptS3AddressingStyle = OptS3AddressingStyle
  { unwrapS3AddressingStyle :: S3AddressingStyle
  }

data WireSettings = WireSettings
  { users :: UserSettings,
    search :: SearchSettings,
    teams :: TeamSettings,
    conversations :: ConversationSettings,
    auth :: AuthSettings,
    calling :: CallingSettings,
    notifications :: NotificationSettings,
    federation :: FederationSettings,
    email :: EmailSettings,
    featureFlags :: FeatureFlags,
    assets :: AssetSettings,
    bots :: BotSettings,
    postgresMigration :: PostgresMigrationOpts,
    logs :: LogSettings,
    disabledAPIVersions :: !(Set VersionExp)
  }

data SearchSettings = SearchSettings
  { emailVisibility :: !EmailVisibilityConfig,
    -- | When true, search only
    -- returns users from the same team
    searchSameTeamOnly :: !(Maybe Bool)
  }

data LogSettings = LogSettings
  { logLevel :: !Level,
    logFormat :: !(Maybe LogFormat)
  }

data UserSettings = UserSettings
  { -- \| Activation timeout, in seconds
    activationTimeout :: !Timeout,
    -- | Default verification code timeout, in seconds
    -- use `verificationTimeout` as the getter function which always provides a default value
    verificationCodeTimeoutInternal :: !(Maybe Code.Timeout),
    -- | Check for expired users every so often, in seconds
    expiredUserCleanupTimeout :: !(Maybe Timeout),
    -- | Whitelist of allowed emails/phones
    allowlistEmailDomains :: !(Maybe AllowlistEmailDomains),
    -- | Max. number of sent/accepted
    --   connections per user
    userMaxConnections :: !Int64,
    -- | Max. number of permanent clients per user
    userMaxPermClients :: !(Maybe Int),
    suspendInactiveUsers :: !(Maybe SuspendInactiveUsers),
    -- | Max size of rich info (number of chars in
    --   field names and values), should be in sync
    --   with Spar
    richInfoLimit :: !Int,
    -- | Default locale to use when selecting templates use
    -- `defaultTemplateLocale` as the getter function which always provides a
    -- default value. TODO: Merge this and next.
    defaultTemplateLocale :: !(Maybe Locale),
    -- | Default locale to use for users
    defaultUserLocale :: !(Maybe Locale),
    propertyMaxKeyLen :: !(Maybe Int64),
    propertyMaxValueLen :: !(Maybe Int64),
    -- | How long, in milliseconds, to wait in between processing delete events
    -- from the internal delete queue
    deleteThrottleMillis :: !(Maybe Int),
    -- | The amount of time in milliseconds to wait after reading from an SQS queue
    -- returns no message, before asking for messages from SQS again.
    -- defaults to 'defSqsThrottleMillis'.
    -- When using real SQS from AWS, throttling isn't needed as much, since using
    -- >>> SQS.rmWaitTimeSeconds (Just 20) in Brig.AWS.listen
    -- ensures that there is only one request every 20 seconds.
    -- However, that parameter is not honoured when using fake-sqs
    -- (where throttling can thus make sense)
    sqsThrottleMillis :: !(Maybe Int),
    -- | Do not allow certain user creation flows.
    -- docs/reference/user/registration.md {#RefRestrictRegistration}.
    restrictUserCreation :: !(Maybe Bool),
    domainsBlockedForRegistration :: !(HashSet Domain)
  }

data TeamSettings = TeamSettings
  { -- \| Team invitation timeout, in seconds
    teamInvitationTimeout :: !Timeout,
    -- | Max. # of members in a team.
    maxTeamSize :: !Word32
  }

data ConversationSettings = ConversationSettings
  { -- | Max. # of members in a conversation.
    maxConvSize :: !Word16
  }

data AuthSettings = AuthSettings
  { zauth :: !ZAuthOpts,
    -- | Whether to allow plain HTTP transmission of cookies (for testing
    --   purposes only)
    cookieInsecure :: !Bool,
    -- | Minimum age of a user cookie before it is renewed during token refresh
    userCookieRenewAge :: !Integer,
    -- | Max. # of cookies per user and cookie type
    userCookieLimit :: !Int,
    -- | Throttling tings (not to be confused with 'LoginRetryOpts')
    userCookieThrottle :: !CookieThrottle,
    -- | Block user from logging in for m minutes after n failed logins
    limitFailedLogins :: !(Maybe LimitFailedLogins),
    -- | Rate limit on password hashing
    passwordHashingRateLimit :: RateLimitConfig
  }

data NotificationSettings = NotificationSettings {}

data FederationSettings = FederationSettings
  { -- \| FederationDomain is required, even when not wanting to federate with other backends
    -- (in that case the 'federationStrategy' can be set to `allowNone` below, or to
    -- `allowDynamic` while keeping the list of allowed domains empty, see
    -- https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections)
    -- Federation domain is used to qualify local IDs and handles,
    -- e.g. 0c4d8944-70fa-480e-a8b7-9d929862d18c@wire.com and somehandle@wire.com.
    -- It should also match the SRV DNS records under which other wire-server installations can find this backend:
    -- >>>   _wire-server-federator._tcp.<federationDomain>
    -- Once set, DO NOT change it: if you do, existing users may have a broken experience and/or stop working.
    -- Remember to keep it the same in all services.
    federationDomain :: !Domain,
    -- | See https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections
    -- default: AllowNone
    federationStrategy :: !(Maybe FederationStrategy),
    -- | 'federationDomainConfigs' is introduced in
    -- https://github.com/wireapp/wire-server/pull/3260 for the sole purpose of transitioning
    -- to dynamic federation remote configuration.  See
    -- https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections
    -- for details.
    -- default: []
    federationDomainConfigs :: !(Maybe [ImplicitNoFederationRestriction]),
    -- | In seconds.  Default: 10 seconds.  Values <1 are silently replaced by 1.  See
    -- https://docs.wire.com/understand/federation/backend-communication.html#configuring-remote-connections
    federationDomainConfigsUpdateFreq :: !(Maybe Int)
  }

data AssetSettings = AssetSettings {}

data CallingSettings = CallingSettings
  { turn :: !TurnOpts,
    sft :: !(Maybe SFTOptions),
    multiSFT :: !(Maybe Bool)
  }

data BotSettings = BotSettings
  { -- \| Filter ONLY services with
    --   the given provider id
    providerSearchFilter :: !(Maybe ProviderId)
  }

data S3Compatibility
  = -- | Scality RING, might also work for Zenko CloudServer
    -- <https://www.scality.com/products/ring/>
    S3CompatibilityScalityRing

-- | AWS CloudFront settings.
data CloudFrontOpts = CloudFrontOpts
  { -- | Domain
    domain :: CFDomain,
    -- | Keypair ID
    keyPairId :: CFKeyPairId,
    -- | Path to private key
    privateKey :: FilePath
  }
  deriving (Show, Generic)

-- TODO: This is copied from cargohold, dedupe
newtype CFKeyPairId = CFKeyPairId Text
  deriving (Eq, Show, Generic)

-- TODO: This is copied from cargohold, dedupe
newtype CFDomain = CFDomain Text
  deriving (Eq, Show, Generic)

data PushNotifiactionOpts = PushNotifiactionOpts
  { -- \| AWS account
    _account :: !Text,
    -- | AWS region name
    _region :: !Region,
    -- | Environment name to scope ARNs to. TODO: Add explanation for on-prem operators.
    _arnEnv :: !Text,
    -- | SQS queue name
    _queueName :: !Text,
    _sqsEndpoint :: !AWSEndpoint,
    _snsEndpoint :: !AWSEndpoint
  }

-- | Wraps 'FederationDomainConfig' with a 'FromJSON' instance that defaults
-- 'FederationRestriction' to 'FederationRestrictionAllowAll' when absent.
newtype ImplicitNoFederationRestriction = ImplicitNoFederationRestriction
  {federationDomainConfig :: FederationDomainConfig}
  deriving (Show, Eq, Generic)

instance FromJSON ImplicitNoFederationRestriction where
  parseJSON =
    withObject
      "ImplicitNoFederationRestriction"
      ( \obj -> do
          domain <- obj .: "domain"
          searchPolicy <- obj .: "search_policy"
          pure . ImplicitNoFederationRestriction $
            FederationDomainConfig domain searchPolicy FederationRestrictionAllowAll
      )

-- ---------------------------------------------------------------------------
-- Types moved from Wire.Options
-- ---------------------------------------------------------------------------

data ElasticSearchOpts = ElasticSearchOpts
  { url :: !ES.Server,
    index :: !ES.IndexName,
    additionalWriteIndex :: !(Maybe ES.IndexName),
    additionalWriteIndexUrl :: !(Maybe ES.Server),
    credentials :: !(Maybe FilePathSecrets),
    additionalCredentials :: !(Maybe FilePathSecrets),
    insecureSkipVerifyTls :: Bool,
    caCert :: Maybe FilePath,
    additionalInsecureSkipVerifyTls :: Bool,
    additionalCaCert :: Maybe FilePath
  }
  deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data EmailAWSOpts = EmailAWSOpts
  { -- | Event feedback queue for SES (e.g. for email bounces and complaints)
    sesQueue :: !Text,
    -- | AWS SES endpoint
    sesEndpoint :: !AWSEndpoint
  }
  deriving (Show, Generic)

instance FromJSON EmailAWSOpts

data EmailSMTPCredentials = EmailSMTPCredentials
  { smtpUsername :: !Text,
    smtpPassword :: !FilePathSecrets
  }
  deriving (Show, Generic)

instance FromJSON EmailSMTPCredentials

data EmailSMTPOpts = EmailSMTPOpts
  { smtpEndpoint :: !Endpoint,
    smtpCredentials :: !(Maybe EmailSMTPCredentials),
    smtpConnType :: !SMTPConnType
  }
  deriving (Show, Generic)

instance FromJSON EmailSMTPOpts

data EmailOpts
  = EmailAWS EmailAWSOpts
  | EmailSMTP EmailSMTPOpts
  deriving (Show, Generic)

instance FromJSON EmailOpts where
  parseJSON o =
    EmailAWS
      <$> parseJSON o
        <|> EmailSMTP
      <$> parseJSON o

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

data EmailSMSGeneralOpts = EmailSMSGeneralOpts
  { templateDir :: !FilePath,
    emailSender :: !EmailAddress,
    smsSender :: !Text,
    templateBranding :: !BrandingOpts
  }
  deriving (Show, Generic)

instance FromJSON EmailSMSGeneralOpts

data EmailUserOpts = EmailUserOpts
  { activationUrl :: !Text,
    smsActivationUrl :: !Text,
    passwordResetUrl :: !Text,
    deletionUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON EmailUserOpts

data ProviderOpts = ProviderOpts
  { homeUrl :: !Text,
    providerActivationUrl :: !Text,
    approvalUrl :: !Text,
    approvalTo :: !EmailAddress,
    providerPwResetUrl :: !Text
  }
  deriving (Show, Generic)

instance FromJSON ProviderOpts

data EmailSettings = EmailSettings
  { general :: !EmailSMSGeneralOpts,
    user :: !EmailUserOpts,
    provider :: !ProviderOpts,
    team :: !TeamOpts
  }
  deriving (Show, Generic)

instance FromJSON EmailSettings

data LimitFailedLogins = LimitFailedLogins
  { timeout :: !Timeout,
    retryLimit :: !Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON LimitFailedLogins

data SuspendInactiveUsers = SuspendInactiveUsers
  { suspendTimeout :: !Timeout
  }
  deriving (Eq, Show, Generic)

instance FromJSON SuspendInactiveUsers

data ZAuthOpts = ZAuthOpts
  { privateKeys :: !FilePath,
    publicKeys :: !FilePath,
    authSettings :: !ZAuthSettings
  }
  deriving (Show, Generic)

instance FromJSON ZAuthOpts

data TurnServersFiles = TurnServersFiles
  { tsfServers :: !FilePath,
    tsfServersV2 :: !FilePath
  }
  deriving (Show)

instance FromJSON TurnServersFiles where
  parseJSON = withObject "TurnServersFiles" $ \o ->
    TurnServersFiles
      <$> o .: "servers"
      <*> o .: "serversV2"

data TurnDnsOpts = TurnDnsOpts
  { tdoBaseDomain :: DNS.Domain,
    tdoDiscoveryIntervalSeconds :: !(Maybe DiffTime)
  }
  deriving (Show)

instance FromJSON TurnDnsOpts where
  parseJSON = withObject "TurnDnsOpts" $ \o ->
    TurnDnsOpts
      <$> (asciiOnly =<< o .: "baseDomain")
      <*> o .:? "discoveryIntervalSeconds"

data TurnServersSource
  = TurnSourceDNS TurnDnsOpts
  | TurnSourceFiles TurnServersFiles
  deriving (Show)

data TurnOpts = TurnOpts
  { serversSource :: !TurnServersSource,
    secret :: !FilePath,
    tokenTTL :: !Word32,
    configTTL :: !Word32
  }
  deriving (Show)

instance FromJSON TurnOpts where
  parseJSON = withObject "TurnOpts" $ \o -> do
    sourceName <- o .: "serversSource"
    source <-
      case sourceName of
        "files" -> TurnSourceFiles <$> parseJSON (Object o)
        "dns" -> TurnSourceDNS <$> parseJSON (Object o)
        _ -> fail $ "TurnOpts: Invalid sourceType, expected one of [files, dns] but got: " <> Text.unpack sourceName
    TurnOpts source
      <$> o .: "secret"
      <*> o .: "tokenTTL"
      <*> o .: "configTTL"

data SFTTokenOptions = SFTTokenOptions
  { sttTTL :: !Word32,
    sttSecret :: !FilePath
  }
  deriving (Show, Generic)

instance FromJSON SFTTokenOptions where
  parseJSON = withObject "SFTTokenOptions" $ \o ->
    SFTTokenOptions
      <$> o .: "ttl"
      <*> o .: "secret"

data SFTOptions = SFTOptions
  { sftBaseDomain :: !DNS.Domain,
    sftSRVServiceName :: !(Maybe ByteString),
    sftDiscoveryIntervalSeconds :: !(Maybe DiffTime),
    sftListLength :: !(Maybe (Range 1 100 Int)),
    sftTokenOptions :: !(Maybe SFTTokenOptions)
  }
  deriving (Show, Generic)

instance FromJSON SFTOptions where
  parseJSON = withObject "SFTOptions" $ \o ->
    SFTOptions
      <$> (asciiOnly =<< o .: "sftBaseDomain")
      <*> (mapM asciiOnly =<< o .:? "sftSRVServiceName")
      <*> (fmap . fmap) secondsToDiffTime (o .:? "sftDiscoveryIntervalSeconds")
      <*> o .:? "sftListLength"
      <*> o .:? "sftToken"

-- ---------------------------------------------------------------------------
-- TH splices — must come after all data declarations due to stage restrictions
-- ---------------------------------------------------------------------------

deriveJSON defaultOptions {constructorTagModifier = map toLower} ''RedisConnectionMode

deriveFromJSON toOptionFieldName ''RedisEndpoint

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
defSftListLength = toRange (Proxy @5)

makePrisms ''PrekeySelectionOpts
