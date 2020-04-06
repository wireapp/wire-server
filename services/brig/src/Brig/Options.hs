{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Brig.Options where

import Brig.Queue.Types (Queue (..))
import Brig.SMTP (SMTPConnType (..))
import Brig.Types
import Brig.User.Auth.Cookie.Limit
import Brig.Whitelist (Whitelist (..))
import qualified Brig.ZAuth as ZAuth
import qualified Control.Lens as Lens
import Data.Aeson (withText)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Id
import Data.Scientific (toBoundedInteger)
import Data.Time.Clock (NominalDiffTime)
import Data.Yaml (FromJSON (..), ToJSON (..))
import qualified Data.Yaml as Y
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options

newtype Timeout
  = Timeout
      { timeoutDiff :: NominalDiffTime
      }
  deriving newtype (Eq, Enum, Ord, Num, Real, Fractional, RealFrac, Show)

instance Read Timeout where
  readsPrec i s =
    case readsPrec i s of
      [(x :: Int, s')] -> [(Timeout (fromIntegral x), s')]
      _ -> []

data ElasticSearchOpts
  = ElasticSearchOpts
      { -- | ElasticSearch URL
        url :: !Text,
        -- | The name of the ElasticSearch user index
        index :: !Text
      }
  deriving (Show, Generic)

instance FromJSON ElasticSearchOpts

data AWSOpts
  = AWSOpts
      { -- | Event journal queue for user events
        --   (e.g. user deletion)
        userJournalQueue :: !(Maybe Text),
        -- | Dynamo table for storing prekey data
        prekeyTable :: !Text,
        -- | AWS SQS endpoint
        sqsEndpoint :: !AWSEndpoint,
        -- | DynamoDB endpoint
        dynamoDBEndpoint :: !AWSEndpoint
      }
  deriving (Show, Generic)

instance FromJSON AWSOpts

data EmailAWSOpts
  = EmailAWSOpts
      { -- | Event feedback queue for SES
        --   (e.g. for email bounces and complaints)
        sesQueue :: !Text,
        -- | AWS SES endpoint
        sesEndpoint :: !AWSEndpoint
      }
  deriving (Show, Generic)

instance FromJSON EmailAWSOpts

data EmailSMTPCredentials
  = EmailSMTPCredentials
      { -- | Username to authenticate
        --   against the SMTP server
        smtpUsername :: !Text,
        -- | File containing password to
        --   authenticate against the SMTP server
        smtpPassword :: !FilePathSecrets
      }
  deriving (Show, Generic)

instance FromJSON EmailSMTPCredentials

data EmailSMTPOpts
  = EmailSMTPOpts
      { -- | Hostname of the SMTP server to connect to
        smtpEndpoint :: !Endpoint,
        smtpCredentials :: !(Maybe EmailSMTPCredentials),
        -- | Which type of connection to use
        --   against the SMTP server {tls,ssl,plain}
        smtpConnType :: !SMTPConnType
      }
  deriving (Show, Generic)

instance FromJSON EmailSMTPOpts

data StompOpts
  = StompOpts
      { stompHost :: !Text,
        stompPort :: !Int,
        stompTls :: !Bool
      }
  deriving (Show, Generic)

instance FromJSON StompOpts

data InternalEventsOpts
  = InternalEventsOpts
      { internalEventsQueue :: !Queue
      }
  deriving (Show)

instance FromJSON InternalEventsOpts where
  parseJSON = Y.withObject "InternalEventsOpts" $ \o ->
    InternalEventsOpts <$> parseJSON (Y.Object o)

data EmailSMSGeneralOpts
  = EmailSMSGeneralOpts
      { -- | Email, SMS, ... template directory
        templateDir :: !FilePath,
        -- | Email sender address
        emailSender :: !Email,
        -- | Twilio sender identifier (number or
        --   messaging service ID)
        smsSender :: !Text,
        -- | Customizable branding text for
        --   emails/sms/calls
        templateBranding :: !BrandingOpts
      }
  deriving (Show, Generic)

instance FromJSON EmailSMSGeneralOpts

data BrandingOpts
  = BrandingOpts
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

data EmailUserOpts
  = EmailUserOpts
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
data ProviderOpts
  = ProviderOpts
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

data TeamOpts
  = TeamOpts
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

data EmailSMSOpts
  = EmailSMSOpts
      { email :: !EmailOpts,
        general :: !EmailSMSGeneralOpts,
        user :: !EmailUserOpts,
        provider :: !ProviderOpts,
        team :: !TeamOpts
      }
  deriving (Show, Generic)

instance FromJSON EmailSMSOpts

-- | Login retry limit.  In contrast to 'setUserCookieThrottle', this is not about mitigating
-- DOS attacks, but about preventing dictionary attacks.  This introduces the orthogonal risk
-- of an attacker blocking legitimate login attempts of a user by constantly keeping the retry
-- limit for that user exhausted with failed login attempts.
--
-- If in doubt, do not ues retry options and worry about encouraging / enforcing a good
-- password policy.
data LimitFailedLogins
  = LimitFailedLogins
      { -- | Time the user is blocked when retry limit is reached (in
        -- seconds mostly for making it easier to write a fast-ish
        -- integration test.)
        timeout :: !Timeout,
        -- | Maximum number of failed login attempts for one user.
        retryLimit :: !Int
      }
  deriving (Eq, Show, Generic)

instance FromJSON LimitFailedLogins

data SuspendInactiveUsers
  = SuspendInactiveUsers
      { suspendTimeout :: !Timeout
      }
  deriving (Eq, Show, Generic)

instance FromJSON SuspendInactiveUsers

-- | ZAuth options
data ZAuthOpts
  = ZAuthOpts
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
data TurnOpts
  = TurnOpts
      { -- | Line separated file with IP addresses of
        --   available TURN servers supporting UDP
        servers :: !FilePath,
        -- | Line separated file with hostnames of all
        --   available TURN servers with all protocols
        --   and transports
        serversV2 :: !FilePath,
        -- | TURN shared secret file path
        secret :: !FilePath,
        -- | For how long TURN credentials should be
        --   valid, in seconds
        tokenTTL :: !Word32,
        -- | How long until a new TURN configuration
        --   should be fetched, in seconds
        configTTL :: !Word32
      }
  deriving (Show, Generic)

instance FromJSON TurnOpts

-- | Configurations for whether to show a user's email to others.
data EmailVisibility
  = -- | Anyone can see the email of someone who is on ANY team.
    --         This may sound strange; but certain on-premise hosters have many different teams
    --         and still want them to see each-other's emails.
    EmailVisibleIfOnTeam
  | -- | Anyone on your team with at least 'Member' privileges can see your email address.
    EmailVisibleIfOnSameTeam
  | -- | Show your email only to yourself
    EmailVisibleToSelf
  deriving (Eq, Show, Bounded, Enum)

instance FromJSON EmailVisibility where
  parseJSON = withText "EmailVisibility" $ \case
    "visible_if_on_team" -> pure EmailVisibleIfOnTeam
    "visible_if_on_same_team" -> pure EmailVisibleIfOnSameTeam
    "visible_to_self" -> pure EmailVisibleToSelf
    _ ->
      fail $
        "unexpected value for EmailVisibility settings: "
          <> "expected one of "
          <> show (Aeson.encode <$> [(minBound :: EmailVisibility) ..])

instance ToJSON EmailVisibility where
  toJSON EmailVisibleIfOnTeam = "visible_if_on_team"
  toJSON EmailVisibleIfOnSameTeam = "visible_if_on_same_team"
  toJSON EmailVisibleToSelf = "visible_to_self"

-- | Options that are consumed on startup
data Opts
  = Opts
      -- services
      { -- | Host and port to bind to
        brig :: !Endpoint,
        -- | Cargohold address
        cargohold :: !Endpoint,
        -- | Galley address
        galley :: !Endpoint,
        -- | Gundeck address
        gundeck :: !Endpoint,
        -- external

        -- | Cassandra settings
        cassandra :: !CassandraOpts,
        -- | ElasticSearch settings
        elasticsearch :: !ElasticSearchOpts,
        -- | AWS settings
        aws :: !AWSOpts,
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
        -- Runtime settings

        -- | Runtime settings
        optSettings :: !Settings
      }
  deriving (Show, Generic)

-- | Options that persist as runtime settings.
data Settings
  = Settings
      { -- | Activation timeout, in seconds
        setActivationTimeout :: !Timeout,
        -- | Team invitation timeout, in seconds
        setTeamInvitationTimeout :: !Timeout,
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
        -- | The domain to restrict cookies to
        setCookieDomain :: !Text,
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
        -- | Default locale to use
        --   (e.g. when selecting templates)
        setDefaultLocale :: !Locale,
        -- | Max. # of members in a team.
        --   NOTE: This must be in sync with galley
        setMaxTeamSize :: !Word16,
        -- | Max. # of members in a conversation.
        --   NOTE: This must be in sync with galley
        setMaxConvSize :: !Word16,
        -- | Filter ONLY services with
        --   the given provider id
        setProviderSearchFilter :: !(Maybe ProviderId),
        -- | Whether to expose user emails and to whom
        setEmailVisibility :: !EmailVisibility,
        setPropertyMaxKeyLen :: !(Maybe Int64),
        setPropertyMaxValueLen :: !(Maybe Int64),
        -- | How long, in milliseconds, to wait
        -- in between processing delete events
        -- from the internal delete queue
        setDeleteThrottleMillis :: !(Maybe Int),
        -- | When true, search only
        -- returns users from the same team
        setSearchSameTeamOnly :: !(Maybe Bool),
        -- | When false, assume there are no other backends and IDs are always local.
        -- This means we don't run any queries on federation-related tables and don't
        -- make any calls to the federator service.
        setEnableFederation :: !(Maybe Bool)
      }
  deriving (Show, Generic)

defMaxKeyLen :: Int64
defMaxKeyLen = 1024

defMaxValueLen :: Int64
defMaxValueLen = 524288

defDeleteThrottleMillis :: Int
defDeleteThrottleMillis = 100

defUserMaxPermClients :: Int
defUserMaxPermClients = 7

defEnableFederation :: Bool
defEnableFederation = False

instance FromJSON Timeout where
  parseJSON (Y.Number n) =
    let defaultV = 3600
        bounded = toBoundedInteger n :: Maybe Int64
     in pure
          $ Timeout
          $ fromIntegral @Int
          $ maybe defaultV fromIntegral bounded
  parseJSON v = typeMismatch "activationTimeout" v

instance FromJSON Settings

instance FromJSON Opts

-- TODO: Does it make sense to generate lens'es for all?
Lens.makeLensesFor [("optSettings", "optionSettings")] ''Opts

Lens.makeLensesFor [("setEmailVisibility", "emailVisibility")] ''Settings

Lens.makeLensesFor [("setPropertyMaxKeyLen", "propertyMaxKeyLen")] ''Settings

Lens.makeLensesFor [("setPropertyMaxValueLen", "propertyMaxValueLen")] ''Settings

Lens.makeLensesFor [("setSearchSameTeamOnly", "searchSameTeamOnly")] ''Settings

Lens.makeLensesFor [("setUserMaxPermClients", "userMaxPermClients")] ''Settings

Lens.makeLensesFor [("setEnableFederation", "enableFederation")] ''Settings
