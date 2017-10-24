{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Brig.Options where

import Brig.Types
import Brig.User.Auth.Cookie.Limit
import Brig.Whitelist (Whitelist(..))
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Conversion
import Data.Int (Int64)
import Data.Maybe
import Data.Misc (HttpsUrl)
import Data.Monoid
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Word (Word32)
import Data.Yaml (FromJSON(..))
import GHC.Generics
import Network.HTTP.Client (Request, parseRequest)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import Util.Options
import Util.Options.Common

import qualified Data.Text             as T
import qualified Data.Yaml             as Y
import qualified Ropes.Aws             as Aws
import qualified Ropes.Nexmo           as Nexmo
import qualified Brig.ZAuth            as ZAuth

newtype ActivationTimeout = ActivationTimeout
    { activationTimeoutDiff :: DiffTime
    } deriving (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Read ActivationTimeout where
    readsPrec i s =
        case readsPrec i s of
            [(x, s')] -> [(ActivationTimeout (secondsToDiffTime x), s')]
            _ -> []

data ElasticSearchOpts = ElasticSearchOpts
    { url   :: !Text
    , index :: !Text
    } deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data AWSOpts = AWSOpts
    { account         :: !Text
    , sesQueue        :: !Text
    , internalQueue   :: !Text
    , blacklistTable  :: !Text
    , prekeyTable     :: !Text
    , awsKeyId        :: !(Maybe Aws.AccessKeyId)
    , awsSecretKey    :: !(Maybe Aws.SecretAccessKey)
    } deriving (Show, Generic)

instance FromJSON AWSOpts

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
    , invitationUrl     :: !Text
    , deletionUrl       :: !Text
    } deriving (Show, Generic)

instance FromJSON EmailUserOpts

data ProviderOpts = ProviderOpts
    { homeUrl               :: !Text
    , providerActivationUrl :: !Text
    , approvalUrl           :: !Text
    , approvalTo            :: !Email
    } deriving (Show, Generic)

instance FromJSON ProviderOpts

data TeamOpts = TeamOpts
    { tInvitationUrl :: !Text
    , tActivationUrl :: !Text
    } deriving (Show, Generic)

instance FromJSON TeamOpts

data EmailSMSOpts = EmailSMSOpts
    { general  :: !EmailSMSGeneralOpts
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
    { servers  :: !FilePath
    , secret   :: !FilePath
    , lifetime :: !Word32
    } deriving (Show, Generic)

instance FromJSON TurnOpts

-- | Options that are consumed on startup
data Opts = Opts
    -- services
    { brig          :: !Endpoint
    , galley        :: !Endpoint
    , gundeck       :: !Endpoint

    -- external
    , cassandra     :: !CassandraOpts
    , elasticsearch :: !ElasticSearchOpts
    , aws           :: !AWSOpts

    -- Email & SMS
    , emailSMS      :: !EmailSMSOpts

    -- ZAuth
    , zauth         :: !ZAuthOpts

    -- Misc.
    , discoUrl      :: !(Maybe Text)
    , geoDb         :: !(Maybe FilePath)

    -- TURN
    , turn          :: !TurnOpts

    -- Runtime settings
    , optSettings :: !Settings
    } deriving (Show, Generic)

-- | Options that persist as runtime settings.
data Settings = Settings
    { setActivationTimeout  :: !ActivationTimeout
    , setTwilioSID          :: !Text
    , setTwilioToken        :: !Text
    , setNexmoKey           :: !Text
    , setNexmoSecret        :: !Text
    , setNexmoEndpoint      :: !Nexmo.ApiEndpoint
    , setWhitelist          :: !(Maybe Whitelist)
    , setUserMaxConnections :: !Int64
    , setCookieDomain       :: !Text
    , setCookieInsecure     :: !Bool
    , setUserCookieRenewAge :: !Integer
    , setUserCookieLimit    :: !Int
    , setUserCookieThrottle :: !CookieThrottle
    , setDefaultLocale      :: !Locale
    } deriving (Show, Generic)

instance FromJSON ActivationTimeout where
    parseJSON (Y.Number n) =
        let defaultV = 3600
            bounded = toBoundedInteger n :: Maybe Int64
        in pure $
           ActivationTimeout $
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
     (textOption $
      long "aws-account-id" <> metavar "STRING" <> help "AWS Account ID") <*>
     (textOption $
      long "aws-ses-queue" <> metavar "STRING" <>
      help
          "Event feedback queue for SES (e.g. for email bounces and complaints)") <*>
     (textOption $
      long "aws-internal-queue" <> metavar "STRING" <>
      help "Event queue for internal brig generated events (e.g. user deletion)") <*>
     (textOption $
      long "aws-dynamo-blacklist" <> metavar "STRING" <>
      help "Dynamo table for storing blacklisted user keys") <*>
     (textOption $
      long "aws-dynamo-prekeys" <> metavar "STRING" <>
      help "Dynamo table for storing prekey data") <*>
     (fmap (Aws.AccessKeyId . encodeUtf8) <$>
      (optional . textOption $
       long "aws-access-key-id" <> metavar "STRING" <> help "AWS Access Key ID")) <*>
     (fmap (Aws.SecretAccessKey . encodeUtf8) <$>
      (optional . textOption $
       long "aws-secret-access-key" <> metavar "STRING" <>
       help "AWS Secret Access Key"))) <*>
    (EmailSMSOpts <$>
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
       long "invitation-url" <> metavar "URL" <> help "Invitation URL template") <*>
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
       help "Provider approval email recipient")) <*>
     (TeamOpts <$>
      (textOption $
       long "team-invitation-url" <> metavar "URL" <>
       help "Team Invitation URL template") <*>
      (textOption $
       long "team-activation-url" <> metavar "URL" <>
       help "Team Activation URL template"))) <*>
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
    (TurnOpts <$>
     (strOption $
      long "turn-servers" <> metavar "FILE" <>
      help "Line separated file with IP addresses of the available turn servers" <>
      action "file") <*>
     (strOption $
      long "turn-secret" <> metavar "FILE" <>
      help "TURN shared secret file path" <>
      action "file") <*>
     (option auto $
      long "turn-token-lifetime" <> metavar "INT" <> value 3600 <> showDefault <>
      help "Number of seconds TURN credentials should be valid.")) <*>
    settingsParser

settingsParser :: Parser Settings
settingsParser =
    Settings <$>
    (option auto $
     long "activation-timeout" <> metavar "SECONDS" <>
     value (ActivationTimeout (secondsToDiffTime 3600)) <>
     help "Activation timeout in seconds") <*>
    (textOption $ long "twilio-sid" <> metavar "STRING" <> help "Twilio SID") <*>
    (textOption $
     long "twilio-token" <> metavar "STRING" <> help "Twilio API token") <*>
    (textOption $ long "nexmo-key" <> metavar "STRING" <> help "Nexmo API key") <*>
    (textOption $
     long "nexmo-secret" <> metavar "STRING" <> help "Nexmo API secret") <*>
    (option toNexmoEndpoint $
     long "nexmo-endpoint" <> value Nexmo.Production <> metavar "STRING" <>
     showDefaultWith (const "production") <>
     help "Nexmo API environment: sandbox | production") <*>
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
     help "Default locale to use (e.g. when selecting templates)")

httpsUrlOption :: Mod OptionFields String -> Parser HttpsUrl
httpsUrlOption =
    fmap (fromMaybe (error "Invalid HTTPS URL") . fromByteString) . bytesOption

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

toNexmoEndpoint :: ReadM Nexmo.ApiEndpoint
toNexmoEndpoint =
    readerAsk >>= \s ->
        case s of
            "production" -> return Nexmo.Production
            "sandbox" -> return Nexmo.Sandbox
            other -> readerError $ "Unsupported Nexmo environment: " <> other

requestUrl :: ReadM Request
requestUrl =
    readerAsk >>= maybe (fail "Invalid request URL") pure . parseRequest
