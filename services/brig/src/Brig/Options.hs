{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Brig.Options where

import Imports
import Brig.Queue.Types (Queue (..))
import Brig.SMTP (SMTPConnType (..))
import Brig.Types
import Brig.User.Auth.Cookie.Limit
import Brig.Whitelist (Whitelist(..))
import Data.Aeson.Types (typeMismatch)
import Data.Barbie
import Data.Id
import Data.Scientific (toBoundedInteger)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Data.Yaml (FromJSON(..), ToJSON(..))
import Util.Options
import System.Logger (Level)

import qualified Brig.ZAuth  as ZAuth
import qualified Data.Yaml   as Y

newtype Timeout = Timeout
    { timeoutDiff :: DiffTime
    } deriving newtype (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Read Timeout where
    readsPrec i s =
        case readsPrec i s of
            [(x, s')] -> [(Timeout (secondsToDiffTime x), s')]
            _ -> []

data ElasticSearchOpts = ElasticSearchOpts
    { url   :: !Text                    -- ^ ElasticSearch URL
    , index :: !Text                    -- ^ The name of the ElasticSearch user index
    } deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data AWSOpts = AWSOpts
    { userJournalQueue :: !(Maybe Text) -- ^ Event journal queue for user events
                                        --   (e.g. user deletion)
    , prekeyTable      :: !Text         -- ^ Dynamo table for storing prekey data
    , sqsEndpoint      :: !AWSEndpoint  -- ^ AWS SQS endpoint
    , dynamoDBEndpoint :: !AWSEndpoint  -- ^ DynamoDB endpoint
    } deriving (Show, Generic)

instance FromJSON AWSOpts

data EmailAWSOpts = EmailAWSOpts
    { sesQueue         :: !Text         -- ^ Event feedback queue for SES
                                        --   (e.g. for email bounces and complaints)
    , sesEndpoint      :: !AWSEndpoint  -- ^ AWS SES endpoint
    } deriving (Show, Generic)

instance FromJSON EmailAWSOpts

data EmailSMTPCredentials = EmailSMTPCredentials
    { smtpUsername :: !Text            -- ^ Username to authenticate
                                       --   against the SMTP server
    , smtpPassword :: !FilePathSecrets -- ^ File containing password to
                                       --   authenticate against the SMTP server
    } deriving (Show, Generic)

instance FromJSON EmailSMTPCredentials

data EmailSMTPOpts = EmailSMTPOpts
    { smtpEndpoint    :: !Endpoint     -- ^ Hostname of the SMTP server to connect to
    , smtpCredentials :: !(Maybe EmailSMTPCredentials)
    , smtpConnType    :: !SMTPConnType -- ^ Which type of connection to use
                                       --   against the SMTP server {tls,ssl,plain}
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
    { templateDir      :: !FilePath     -- ^ Email, SMS, ... template directory
    , emailSender      :: !Email        -- ^ Email sender address
    , smsSender        :: !Text         -- ^ Twilio sender identifier (number or
                                        --   messaging service ID)
    , templateBranding :: !BrandingOpts -- ^ Customizable branding text for
                                        --   emails/sms/calls
    } deriving (Show, Generic)

instance FromJSON EmailSMSGeneralOpts

data BrandingOpts = BrandingOpts
    { brand         :: !Text
    , brandUrl      :: !Text
    , brandLabelUrl :: !Text
    , brandLogoUrl  :: !Text
    , brandService  :: !Text
    , copyright     :: !Text
    , misuse        :: !Text
    , legal         :: !Text
    , forgot        :: !Text
    , support       :: !Text
    } deriving (Show, Generic)

instance FromJSON BrandingOpts

data EmailUserOpts = EmailUserOpts
    { activationUrl     :: !Text  -- ^ Activation URL template
    , smsActivationUrl  :: !Text  -- ^ SMS activation URL template
    , passwordResetUrl  :: !Text  -- ^ Password reset URL template
    , deletionUrl       :: !Text  -- ^ Deletion URL template
    } deriving (Show, Generic)

instance FromJSON EmailUserOpts

-- | Provider settings
data ProviderOpts = ProviderOpts
    { homeUrl               :: !Text   -- ^ Homepage URL
    , providerActivationUrl :: !Text   -- ^ Activation URL template
    , approvalUrl           :: !Text   -- ^ Approval URL template
    , approvalTo            :: !Email  -- ^ Approval email recipient
    , providerPwResetUrl    :: !Text   -- ^ Password reset URL template
    } deriving (Show, Generic)

instance FromJSON ProviderOpts

data TeamOpts = TeamOpts
    { tInvitationUrl     :: !Text  -- ^ Team Invitation URL template
    , tActivationUrl     :: !Text  -- ^ Team Activation URL template
    , tCreatorWelcomeUrl :: !Text  -- ^ Team Creator Welcome URL
    , tMemberWelcomeUrl  :: !Text  -- ^ Team Member Welcome URL
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

-- | ZAuth options
data ZAuthOpts = ZAuthOpts
    { privateKeys  :: !FilePath        -- ^ Private key file
    , publicKeys   :: !FilePath        -- ^ Public key file
    , authSettings :: !ZAuth.Settings  -- ^ Other settings
    } deriving (Show, Generic)

instance FromJSON ZAuthOpts

-- | TURN server options
data TurnOpts = TurnOpts
    { servers   :: !FilePath  -- ^ Line separated file with IP addresses of
                              --   available TURN servers supporting UDP
    , serversV2 :: !FilePath  -- ^ Line separated file with hostnames of all
                              --   available TURN servers with all protocols
                              --   and transports
    , secret    :: !FilePath  -- ^ TURN shared secret file path
    , tokenTTL  :: !Word32    -- ^ For how long TURN credentials should be
                              --   valid, in seconds
    , configTTL :: !Word32    -- ^ How long until a new TURN configuration
                              --   should be fetched, in seconds
    } deriving (Show, Generic)

instance FromJSON TurnOpts

-- | Options that are consumed on startup
data Opts = Opts
    -- services
    { brig          :: !Endpoint               -- ^ Host and port to bind to
    , cargohold     :: !Endpoint               -- ^ Cargohold address
    , galley        :: !Endpoint               -- ^ Galley address
    , gundeck       :: !Endpoint               -- ^ Gundeck address

    -- external
    , cassandra     :: !CassandraOpts          -- ^ Cassandra settings
    , elasticsearch :: !ElasticSearchOpts      -- ^ ElasticSearch settings
    , aws           :: !AWSOpts                -- ^ AWS settings
    , stomp         :: !(Maybe StompOpts)      -- ^ STOMP broker settings

    -- Email & SMS
    , emailSMS      :: !EmailSMSOpts           -- ^ Email and SMS settings

    -- ZAuth
    , zauth         :: !ZAuthOpts              -- ^ ZAuth settings

    -- Misc.
    , discoUrl      :: !(Maybe Text)           -- ^ Disco URL
    , geoDb         :: !(Maybe FilePath)       -- ^ GeoDB file path
    , internalEvents :: !InternalEventsOpts     -- ^ Event queue for
                                                --   Brig-generated events (e.g.
                                                --   user deletion)

    -- Logging
    , logLevel      :: !Level                  -- ^ Log level (Debug, Info, etc)
    , logNetStrings :: !Bool                   -- ^ Use netstrings encoding (see
                                               --   <http://cr.yp.to/proto/netstrings.txt>)

    -- TURN
    , turn          :: !TurnOpts               -- ^ TURN server settings

    -- Runtime settings
    , optSettings :: !Settings                 -- ^ Runtime settings
    , optMutableSettings :: !MutableSettings   -- ^ Mutable runtime settings
    } deriving (Show, Generic)

-- | We use the Higher Kinded Data pattern here because it's very useful for handling
--   partial 'PUT' payloads in the API layer as @MutableSettings Maybe@
data MutableSettings' f = MutableSettings
    { setEmailVisibility :: !(f EmailVisibility)
    } deriving (Generic)
      deriving anyclass (FunctorB, ProductB, TraversableB, ConstraintsB, ProductBC)
type MutableSettings = MutableSettings' Identity
deriving instance AllBF Show f MutableSettings' => Show (MutableSettings' f)
deriving instance AllBF Eq   f MutableSettings' => Eq   (MutableSettings' f)

-- | Options that persist as runtime settings.
data Settings = Settings
    { setActivationTimeout     :: !Timeout          -- ^ Activation timeout, in seconds
    , setTeamInvitationTimeout :: !Timeout          -- ^ Team invitation timeout, in seconds
    , setTwilio                :: !FilePathSecrets  -- ^ Twilio credentials
    , setNexmo                 :: !FilePathSecrets  -- ^ Nexmo credentials
    , setStomp                 :: !(Maybe FilePathSecrets)  -- ^ STOMP broker credentials
    , setWhitelist             :: !(Maybe Whitelist) -- ^ Whitelist of allowed emails/phones
    , setUserMaxConnections    :: !Int64    -- ^ Max. number of sent/accepted
                                            --   connections per user
    , setCookieDomain          :: !Text     -- ^ The domain to restrict cookies to
    , setCookieInsecure        :: !Bool     -- ^ Whether to allow plain HTTP transmission
                                            --   of cookies (for testing purposes only)
    , setUserCookieRenewAge    :: !Integer  -- ^ Minimum age of a user cookie before
                                            --   it is renewed during token refresh
    , setUserCookieLimit       :: !Int      -- ^ Max. # of cookies per user and cookie type
    , setUserCookieThrottle    :: !CookieThrottle -- ^ Throttling settings
    , setRichInfoLimit         :: !Int     -- ^ Max size of rich info (number of chars in
                                           --   field names and values), should be in sync
                                           --   with Spar
    , setDefaultLocale         :: !Locale  -- ^ Default locale to use
                                           --   (e.g. when selecting templates)
    , setMaxTeamSize           :: !Word16  -- ^ Max. # of members in a team.
                                           --   NOTE: This must be in sync with galley
    , setMaxConvSize           :: !Word16  -- ^ Max. # of members in a conversation.
                                           --   NOTE: This must be in sync with galley
    , setProviderSearchFilter  :: !(Maybe ProviderId) -- ^ Filter ONLY services with
                                                      --   the given provider id
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
instance (FromJSON (f EmailVisibility)) => FromJSON (MutableSettings' f)
instance (ToJSON (f EmailVisibility)) => ToJSON (MutableSettings' f)

instance FromJSON Opts
