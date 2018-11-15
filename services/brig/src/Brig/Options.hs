{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Options where

import Imports
import Brig.Queue.Types (Queue (..))
import Brig.SMTP (SMTPConnType (..))
import Brig.Types
import Brig.User.Auth.Cookie.Limit
import Brig.Whitelist (Whitelist(..))
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Conversion
import Data.Id
import Data.Scientific (toBoundedInteger)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Yaml (FromJSON(..))
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Util.Options
import Util.Options.Common

import qualified Brig.ZAuth  as ZAuth
import qualified Data.Text   as T
import qualified Data.Yaml   as Y

newtype Timeout = Timeout
    { timeoutDiff :: DiffTime
    } deriving (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Read Timeout where
    readsPrec i s =
        case readsPrec i s of
            [(x, s')] -> [(Timeout (secondsToDiffTime x), s')]
            _ -> []

data ElasticSearchOpts = ElasticSearchOpts
    { url   :: !Text
    , index :: !Text
    } deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data AWSOpts = AWSOpts
    { userJournalQueue :: !(Maybe Text)
    , prekeyTable      :: !Text
    , sqsEndpoint      :: !AWSEndpoint
    , dynamoDBEndpoint :: !AWSEndpoint
    } deriving (Show, Generic)

instance FromJSON AWSOpts

data EmailAWSOpts = EmailAWSOpts
    { sesQueue         :: !Text
    , sesEndpoint      :: !AWSEndpoint
    } deriving (Show, Generic)

instance FromJSON EmailAWSOpts

data EmailSMTPCredentials = EmailSMTPCredentials
    { smtpUsername :: !Text
    , smtpPassword :: !FilePathSecrets
    } deriving (Show, Generic)

instance FromJSON EmailSMTPCredentials

data EmailSMTPOpts = EmailSMTPOpts
    { smtpEndpoint    :: !Endpoint
    , smtpCredentials :: !(Maybe EmailSMTPCredentials)
    , smtpConnType    :: !SMTPConnType
    } deriving (Show, Generic)

instance FromJSON EmailSMTPOpts

data StompOpts = StompOpts
    { stompHost :: !Text
    , stompPort :: !Int
    , stompTls  :: !Bool
    } deriving (Show, Generic)

instance FromJSON StompOpts

data InternalEventsOpts = InternalEventsOpts
    { internalEventsQueue :: !Queue
    } deriving (Show)

instance FromJSON InternalEventsOpts where
    parseJSON = Y.withObject "InternalEventsOpts" $ \o ->
        InternalEventsOpts <$> parseJSON (Y.Object o)

data EmailSMSGeneralOpts = EmailSMSGeneralOpts
    { templateDir :: !FilePath
    , emailSender :: !Email
    , smsSender   :: !Text
    } deriving (Show, Generic)

instance FromJSON EmailSMSGeneralOpts

data EmailUserOpts = EmailUserOpts
    { activationUrl     :: !Text
    , smsActivationUrl  :: !Text
    , passwordResetUrl  :: !Text
    , deletionUrl       :: !Text
    } deriving (Show, Generic)

instance FromJSON EmailUserOpts

data ProviderOpts = ProviderOpts
    { homeUrl               :: !Text
    , providerActivationUrl :: !Text
    , approvalUrl           :: !Text
    , approvalTo            :: !Email
    , providerPwResetUrl    :: !Text
    } deriving (Show, Generic)

instance FromJSON ProviderOpts

data TeamOpts = TeamOpts
    { tInvitationUrl     :: !Text
    , tActivationUrl     :: !Text
    , tCreatorWelcomeUrl :: !Text
    , tMemberWelcomeUrl  :: !Text
    } deriving (Show, Generic)

instance FromJSON TeamOpts

data EmailOpts = EmailAWS  EmailAWSOpts
               | EmailSMTP EmailSMTPOpts
               deriving (Show, Generic)

instance FromJSON EmailOpts where
    parseJSON o =  EmailAWS <$> parseJSON o
               <|> EmailSMTP <$> parseJSON o

data EmailSMSOpts = EmailSMSOpts
    { email    :: !EmailOpts
    , general  :: !EmailSMSGeneralOpts
    , user     :: !EmailUserOpts
    , provider :: !ProviderOpts
    , team     :: !TeamOpts
    } deriving (Show, Generic)

instance FromJSON EmailSMSOpts

data ZAuthOpts = ZAuthOpts
    { privateKeys  :: !FilePath
    , publicKeys   :: !FilePath
    , authSettings :: !ZAuth.Settings
    } deriving (Show, Generic)

instance FromJSON ZAuthOpts

data TurnOpts = TurnOpts
    { servers   :: !FilePath
    , serversV2 :: !FilePath
    , secret    :: !FilePath
    , tokenTTL  :: !Word32
    , configTTL :: !Word32
    } deriving (Show, Generic)

instance FromJSON TurnOpts

-- | Options that are consumed on startup
data Opts = Opts
    -- services
    { brig          :: !Endpoint
    , cargohold     :: !Endpoint
    , galley        :: !Endpoint
    , gundeck       :: !Endpoint

    -- external
    , cassandra     :: !CassandraOpts
    , elasticsearch :: !ElasticSearchOpts
    , aws           :: !AWSOpts
    , stomp         :: !(Maybe StompOpts)

    -- Email & SMS
    , emailSMS      :: !EmailSMSOpts

    -- ZAuth
    , zauth         :: !ZAuthOpts

    -- Misc.
    , discoUrl      :: !(Maybe Text)
    , geoDb         :: !(Maybe FilePath)
    , internalEvents :: !InternalEventsOpts

    -- TURN
    , turn          :: !TurnOpts

    -- Runtime settings
    , optSettings :: !Settings
    } deriving (Show, Generic)

-- | Options that persist as runtime settings.
data Settings = Settings
    { setActivationTimeout     :: !Timeout
    , setTeamInvitationTimeout :: !Timeout
    , setTwilio                :: !FilePathSecrets
    , setNexmo                 :: !FilePathSecrets
    , setStomp                 :: !(Maybe FilePathSecrets)
    , setWhitelist             :: !(Maybe Whitelist)
    , setUserMaxConnections    :: !Int64
    , setCookieDomain          :: !Text
    , setCookieInsecure        :: !Bool
    , setUserCookieRenewAge    :: !Integer
    , setUserCookieLimit       :: !Int
    , setUserCookieThrottle    :: !CookieThrottle
    , setDefaultLocale         :: !Locale
    , setMaxTeamSize           :: !Word16 -- NOTE: This must be in sync with galley
    , setMaxConvSize           :: !Word16 -- NOTE: This must be in sync with galley
    , setProviderSearchFilter  :: !(Maybe ProviderId)
    -- ^ Temporary optional provider ID to use for filtering services during search
    } deriving (Show, Generic)

instance FromJSON Timeout where
    parseJSON (Y.Number n) =
        let defaultV = 3600
            bounded = toBoundedInteger n :: Maybe Int64
        in pure $
           Timeout $
           secondsToDiffTime $ maybe defaultV fromIntegral bounded
    parseJSON v = typeMismatch "activationTimeout" v

instance FromJSON Settings

instance FromJSON Opts

parseOptions :: IO Opts
parseOptions = execParser (info (helper <*> optsParser) desc)
  where
    desc = header "Brig - User Service" <> fullDesc

optsParser :: Parser Opts
optsParser =
    Opts <$>
    (Endpoint <$>
     (textOption $
      long "host" <> value "*4" <> showDefault <> metavar "HOSTNAME" <>
      help "Hostname or address to bind to") <*>
     (option auto $
      long "port" <> short 'p' <> metavar "PORT" <> help "Port to listen on")) <*>
    (Endpoint <$>
     (textOption $
      long "cargohold-host" <> metavar "HOSTNAME" <> help "Cargohold hostname") <*>
     (option auto $ long "cargohold-port" <> metavar "PORT" <> help "Cargohold port")) <*>
    (Endpoint <$>
     (textOption $
      long "galley-host" <> metavar "HOSTNAME" <> help "Galley hostname") <*>
     (option auto $ long "galley-port" <> metavar "PORT" <> help "Galley port")) <*>
    (Endpoint <$>
     (textOption $
      long "gundeck-host" <> metavar "HOSTNAME" <> help "Gundeck hostname") <*>
     (option auto $ long "gundeck-port" <> metavar "PORT" <> help "Gundeck port")) <*>
    cassandraParser <*>
    (ElasticSearchOpts <$>
     (textOption $
      long "elasticsearch-url" <> metavar "URL" <> help "Elasticsearch URL") <*>
     (textOption $
      long "elasticsearch-user-index" <> metavar "STRING" <> value "directory" <>
      showDefault <>
      help "The name of the ElasticSearch user index")) <*>
    (AWSOpts <$>
     (optional $ textOption $
      long "aws-user-journal-queue" <> metavar "STRING" <>
      help "Event journal queue for user events (e.g. user deletion)") <*>
     (textOption $
      long "aws-dynamo-prekeys" <> metavar "STRING" <>
      help "Dynamo table for storing prekey data") <*>
     (option parseAWSEndpoint $
      long "aws-sqs-endpoint" <> value (AWSEndpoint "sqs.eu-west-1.amazonaws.com" True 443)
      <> metavar "STRING" <> showDefault <> help "aws SQS endpoint") <*>
     (option parseAWSEndpoint $
      long "aws-dynamodb-endpoint" <> value (AWSEndpoint "dynamodb.eu-west-1.amazonaws.com" True 443)
      <> metavar "STRING" <> showDefault <> help "aws DYNAMODB endpoint")) <*>
    (optional $ StompOpts <$>
     (textOption $
      long "stomp-host" <> metavar "URL" <>
      help "STOMP broker URL (e.g. for RabbitMQ or ActiveMQ)") <*>
     (option auto $
      long "stomp-port" <> metavar "INT" <>
      help "STOMP broker port (usually 61613 or 61614)") <*>
     (switch $
      long "stomp-tls" <>
      help "Connect to the STOMP broker via TLS")) <*>
    (EmailSMSOpts <$>
     emailOptsParser <*>
     (EmailSMSGeneralOpts <$>
      (strOption $
       long "template-dir" <> metavar "FILE" <>
       help "Email/SMS/... template directory") <*>
      (emailOption $
       long "email-sender" <> metavar "STRING" <> help "Email sender address") <*>
      (textOption $
       long "twilio-sender" <> metavar "STRING" <>
       help "Twilio sender identifier (number or messaging service ID")) <*>
     (EmailUserOpts <$>
      (textOption $
       long "activation-url" <> metavar "URL" <> help "Activation URL template") <*>
      (textOption $
       long "sms-activation-url" <> metavar "URL" <>
       help "SMS activation URL template") <*>
      (textOption $
       long "password-reset-url" <> metavar "URL" <>
       help "Password reset URL template") <*>
      (textOption $
       long "deletion-url" <> metavar "URL" <> help "Deletion URL template")) <*>
     (ProviderOpts <$>
      (textOption $
       long "provider-home-url" <> metavar "URL" <> help "Provider Homepage URL") <*>
      (textOption $
       long "provider-activation-url" <> metavar "URL" <>
       help "Provider Activation URL template") <*>
      (textOption $
       long "provider-approval-url" <> metavar "URL" <>
       help "Provider Approval URL template") <*>
      (emailOption $
       long "provider-approval-to" <> metavar "STRING" <>
       help "Provider approval email recipient") <*>
      (textOption $
       long "provider-password-reset-url" <> metavar "URL" <>
       help "Provider Password reset URL template")) <*>
     (TeamOpts <$>
      (textOption $
       long "team-invitation-url" <> metavar "URL" <>
       help "Team Invitation URL template") <*>
      (textOption $
       long "team-activation-url" <> metavar "URL" <>
       help "Team Activation URL template") <*>
      (textOption $
       long "team-creator-welcome-url" <> metavar "URL" <>
       help "Team Creator Welcome URL") <*>
      (textOption $
       long "team-member-welcome-url" <> metavar "URL" <>
       help "Team Member Welcome URL"))) <*>
    (ZAuthOpts <$>
     (strOption $
      long "zauth-private-keys" <> metavar "FILE" <>
      help "zauth private key file" <>
      action "file") <*>
     (strOption $
      long "zauth-public-keys" <> metavar "FILE" <> help "zauth public key file" <>
      action "file") <*>
     (ZAuth.Settings <$>
      (option auto $
       long "zauth-key-index" <> metavar "INT" <> value 1 <> showDefault <>
       help "Secret key index to use for token creation") <*>
      (fmap ZAuth.UserTokenTimeout . option auto $
       long "zauth-user-token-timeout" <> metavar "INT" <>
       help "User token validity timeout") <*>
      (fmap ZAuth.SessionTokenTimeout . option auto $
       long "zauth-session-token-timeout" <> metavar "INT" <>
       help "Session token validity timeout") <*>
      (fmap ZAuth.AccessTokenTimeout . option auto $
       long "zauth-access-token-timeout" <> metavar "INT" <>
       help "Access token validity timeout") <*>
      (fmap ZAuth.ProviderTokenTimeout . option auto $
       long "zauth-provider-token-timeout" <> metavar "INT" <>
       help "Access token validity timeout"))) <*>
    (optional discoUrlParser) <*>
    (optional $
     option auto $ long "geodb" <> metavar "FILE" <> help "GeoDB file path") <*>
    (InternalEventsOpts <$>
     (queueOption $
      long "internal-events-queue" <> metavar "STRING" <>
      help "Queue to use for internal events. Either 'stomp:...' or 'sqs:...'")) <*>
    (TurnOpts <$>
     (strOption $
      long "turn-servers" <> metavar "FILE" <>
      help "Line separated file with IP addresses of the available turn servers, supporting UDP" <>
      action "file") <*>
     (strOption $
      long "turn-servers-v2" <> metavar "FILE" <>
      help "Line separated file with hostnames of all available turn servers with all protocols/transports" <>
      action "file") <*>
     (strOption $
      long "turn-secret" <> metavar "FILE" <>
      help "TURN shared secret file path" <>
      action "file") <*>
     (option auto $
      long "turn-token-lifetime" <> metavar "INT" <> value 21600 <> showDefault <>
      help "Number of seconds TURN credentials should be valid.") <*>
     (option auto $
      long "turn-config-ttl" <> metavar "INT" <> value 3600 <> showDefault <>
      help "Number of seconds until a new TURN configuration should be fetched.")) <*>
    settingsParser

emailOptsParser :: Parser EmailOpts
emailOptsParser = EmailAWS <$> emailAWSOptsParser <|> EmailSMTP <$> emailSMTPOptsParser

emailAWSOptsParser :: Parser EmailAWSOpts
emailAWSOptsParser =
     EmailAWSOpts <$>
      (textOption $
        long "aws-ses-queue" <> metavar "STRING" <>
        help "Event feedback queue for SES (e.g. for email bounces and complaints)") <*>
      (option parseAWSEndpoint $
        long "aws-ses-endpoint" <> value (AWSEndpoint "email.eu-west-1.amazonaws.com" True 443)
        <> metavar "STRING" <> showDefault <> help "aws SES endpoint")

emailSMTPOptsParser :: Parser EmailSMTPOpts
emailSMTPOptsParser =
    EmailSMTPOpts <$>
      (Endpoint <$>
       (textOption $ long "smtp-host" <> metavar "HOSTNAME" <> help "SMTP hostname") <*>
       (option auto $ long "smtp-port" <> metavar "PORT" <> help "SMTP port")) <*>
      (optional smtpCredentialsParser) <*>
      (smtpConnTypeOption $
        long "smtp-conn-type" <> metavar "STRING" <> value "tls" <> showDefault <>
        help "Which type of connection to use against the SMTP server {tls,ssl,plain}")

  where
    smtpCredentialsParser :: Parser EmailSMTPCredentials
    smtpCredentialsParser =
      EmailSMTPCredentials <$>
        (textOption $
          long "smtp-username" <> metavar "STRING" <>
          help "Username to authenticate against the SMTP server") <*>
        (FilePathSecrets <$> (strOption $
          long "smtp-password" <> metavar "FILE" <>
          help "File containing password to authenticate against the SMTP server" <> action "file"))
settingsParser :: Parser Settings
settingsParser =
    Settings <$>
    (option auto $
     long "activation-timeout" <> metavar "SECONDS" <>
     value (Timeout (secondsToDiffTime 3600)) <>
     help "Activation timeout in seconds") <*>
    (option auto $
     long "team-invitation-timeout" <> metavar "SECONDS" <>
     value (Timeout (secondsToDiffTime 3600)) <>
     help "Team invitation timeout in seconds") <*>
    (FilePathSecrets <$> (strOption $
     long "twilio-credentials" <> metavar "FILE" <> help "File containing Twilio credentials" <> action "file")) <*>
    (FilePathSecrets <$> (strOption $
     long "nexmo-credentials" <> metavar "FILE" <> help "File containing Nexmo credentials" <> action "file")) <*>
    (optional $ FilePathSecrets <$> (strOption $
     long "stomp-credentials" <> metavar "FILE" <> help "File containing STOMP broker credentials" <> action "file")) <*>
    (optional $
     Whitelist <$>
     (textOption $
      long "whitelist-url" <>
      help
          "URL of a service providing a whitelist of allowed email addresses and phone numbers.") <*>
     (textOption $
      long "whitelist-user" <> metavar "STRING" <> value "" <>
      help "Username for accessing the whitelist") <*>
     (textOption $
      long "whitelist-pass" <> metavar "STRING" <> value "" <>
      help "Password for accessing the whitelist")) <*>
    (option auto $
     long "user-connection-limit" <> metavar "INT" <>
     help "Max. number of sent/accepted connections per user." <>
     value 1000) <*>
    (textOption $
     long "cookie-domain" <> metavar "STRING" <>
     help "The domain to restrict cookies to.") <*>
    (switch $
     long "cookie-insecure" <>
     help
         "Allow plain HTTP transmission of cookies (for testing purposes only).") <*>
    (option auto $
     long "user-cookie-renew-age" <> metavar "INT" <>
     help
         "Minimum age of a user cookie before it is renewed during token refresh.") <*>
    (option auto $
     long "user-cookie-limit" <> metavar "INT" <> value 32 <> showDefault <>
     help "Max. # of cookies per user and cookie type.") <*>
    (StdDevThrottle <$>
     (fmap StdDev . option auto $
      long "user-cookie-min-deviation" <> metavar "SECONDS" <> value 3000 <>
      showDefault <>
      help "Min. standard deviation cookie creation") <*>
     (fmap RetryAfter . option auto $
      long "user-cookie-retry-after" <> metavar "SECONDS" <> value 86400 <>
      showDefault <>
      help "Wait time when the min deviation is violated")) <*>
    (localeOption $
     long "default-locale" <> metavar "STRING" <> value "en" <> showDefault <>
     help "Default locale to use (e.g. when selecting templates)") <*>
    (option auto $
     long "team-max-size" <> metavar "INT" <>
     help "Max. # of members in a team") <*>
    (option auto $
     long "conv-max-size" <> metavar "INT" <>
     help "Max. # of members in a conversation") <*>
    (optional $ option providerIdOption $
     long "provider-id-search-filter" <> metavar "STRING" <>
     help "Filter _ONLY_ services with the given provider id")

queueOption :: Mod OptionFields String -> Parser Queue
queueOption =
    fmap
        (\s -> let (type_, name_) = break (== ':') s in
               case type_ of
                   "stomp" -> StompQueue (T.pack $ drop 1 name_)
                   "sqs"   -> SqsQueue (T.pack $ drop 1 name_)
                   _       -> error ("Unknown queue type: " <> show type_)
        ) .
    strOption

localeOption :: Mod OptionFields String -> Parser Locale
localeOption =
    fmap
        (fromMaybe (error "Ensure proper default locale is used") .
         parseLocale . T.pack) .
    strOption

emailOption :: Mod OptionFields String -> Parser Email
emailOption =
    fmap
        (fromMaybe (error "Ensure proper email address is used") .
         parseEmail . T.pack) .
    strOption

providerIdOption :: ReadM ProviderId
providerIdOption = readerAsk >>=
    maybe (fail "Failed to parse ") pure . fromByteString . pack

smtpConnTypeOption :: Mod OptionFields String -> Parser SMTPConnType
smtpConnTypeOption =
    fmap
        (either (\e -> error ("Ensure proper STMP conn type is used: " <> show e)) id .
        Y.decodeEither' . pack) .
    strOption
