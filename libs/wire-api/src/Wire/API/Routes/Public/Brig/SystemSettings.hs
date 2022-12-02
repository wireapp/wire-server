module Wire.API.Routes.Public.Brig.SystemSettings where

import Control.Lens hiding ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Code as Code
import qualified Data.CookieThrottle as CookieThrottle
import Data.Domain
import qualified Data.EmailVisibility as EmailVisibility
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import qualified Data.LimitFailedLogins as LimitFailedLogins
import qualified Data.Nonce as Nonce
import Data.SOP
import Data.Schema as Schema
import qualified Data.SuspendInactiveUsers as SuspendInactiveUsers
import Data.Swagger hiding (Contact, Header, Schema, ToSchema)
import qualified Data.Swagger as S
import Imports hiding (head)
import Servant.Swagger.Internal.Orphans ()
import Test.QuickCheck
import qualified Wire.API.Team.Feature as Public
import Wire.API.User hiding (NoIdentity)
import Wire.Arbitrary
import qualified Wire.Data.Timeout as WireTimeout

-- | Subset of `Brig.Options.Settings` that is safe to be shown in public
--
-- Used to expose settings via the @/system/settings@ endpoint.
data SystemSettings = SystemSettings
  { systemSettingsSetActivationTimeout :: !WireTimeout.Timeout,
    systemSettingsSetVerificationCodeTimeoutInternal :: !(Maybe Code.Timeout),
    systemSettingsSetTeamInvitationTimeout :: !WireTimeout.Timeout,
    systemSettingsSetExpiredUserCleanupTimeout :: !(Maybe WireTimeout.Timeout),
    systemSettingsSetUserMaxConnections :: !Int64,
    systemSettingsSetUserMaxPermClients :: !(Maybe Int),
    systemSettingsSetCookieInsecure :: !Bool,
    systemSettingsSetUserCookieRenewAge :: !Integer,
    systemSettingsSetUserCookieLimit :: !Int,
    systemSettingsSetUserCookieThrottle :: !CookieThrottle.CookieThrottle,
    systemSettingsSetLimitFailedLogins :: !(Maybe LimitFailedLogins.LimitFailedLogins),
    systemSettingsSetSuspendInactiveUsers :: !(Maybe SuspendInactiveUsers.SuspendInactiveUsers),
    systemSettingsSetRichInfoLimit :: !Int,
    systemSettingsSetDefaultTemplateLocaleInternal :: !(Maybe Locale),
    systemSettingsSetDefaultUserLocaleInternal :: !(Maybe Locale),
    systemSettingsSetMaxTeamSize :: !Word32,
    systemSettingsSetMaxConvSize :: !Word16,
    systemSettingsSetEmailVisibility :: !EmailVisibility.EmailVisibility,
    systemSettingsSetPropertyMaxKeyLen :: !(Maybe Int64),
    systemSettingsSetPropertyMaxValueLen :: !(Maybe Int64),
    systemSettingsSetDeleteThrottleMillis :: !(Maybe Int),
    systemSettingsSetSearchSameTeamOnly :: !(Maybe Bool),
    systemSettingsSetFederationDomain :: !Domain,
    systemSettingsSetSqsThrottleMillis :: !(Maybe Int),
    systemSettingsSetRestrictUserCreation :: !(Maybe Bool),
    systemSettingsSetFeatureFlags :: !(Maybe AccountFeatureConfigs),
    systemSettingsSetEnableDevelopmentVersions :: Maybe Bool,
    systemSettingsSet2FACodeGenerationDelaySecsInternal :: !(Maybe Int),
    systemSettingsSetNonceTtlSecsInternal :: !(Maybe Nonce.NonceTtlSecs)
  }
  deriving (Eq, Show, Generic)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via Schema.Schema SystemSettings
  deriving (Arbitrary) via (GenericUniform SystemSettings)

instance Schema.ToSchema SystemSettings where
  schema =
    Schema.object "SystemSettings" $
      SystemSettings
        <$> systemSettingsSetActivationTimeout
        Schema..= Schema.fieldWithDocModifier "setActivationTimeout" (description ?~ "Activation timeout, in seconds") Schema.schema
        <*> systemSettingsSetVerificationCodeTimeoutInternal
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setVerificationCodeTimeoutInternal" (description ?~ "Default verification code timeout, in seconds") Schema.schema)
        <*> systemSettingsSetTeamInvitationTimeout
        Schema..= Schema.fieldWithDocModifier "setTeamInvitationTimeout" (description ?~ "Team invitation timeout, in seconds") Schema.schema
        <*> systemSettingsSetExpiredUserCleanupTimeout
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setExpiredUserCleanupTimeout" (description ?~ "Check for expired users every so often, in seconds") Schema.schema)
        <*> systemSettingsSetUserMaxConnections
        Schema..= Schema.fieldWithDocModifier "setUserMaxConnections" (description ?~ "Max. number of sent/accepted connections per user") Schema.schema
        <*> systemSettingsSetUserMaxPermClients
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setUserMaxPermClients" (description ?~ "Max. number of permanent clients per user") Schema.schema)
        <*> systemSettingsSetCookieInsecure
        Schema..= Schema.fieldWithDocModifier "setCookieInsecure" (description ?~ "Whether to allow plain HTTP transmission of cookies (for testing purposes only)") Schema.schema
        <*> systemSettingsSetUserCookieRenewAge
        Schema..= Schema.fieldWithDocModifier "setUserCookieRenewAge" (description ?~ "Minimum age of a user cookie before it is renewed during token refresh") Schema.schema
        <*> systemSettingsSetUserCookieLimit
        Schema..= Schema.fieldWithDocModifier "setUserCookieLimit" (description ?~ "Max. # of cookies per user and cookie type") Schema.schema
        <*> systemSettingsSetUserCookieThrottle
        Schema..= Schema.fieldWithDocModifier "setUserCookieThrottle" (description ?~ "Throttling settings") Schema.schema
        <*> systemSettingsSetLimitFailedLogins
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setLimitFailedLogins" (description ?~ "Block user from logging in for m minutes after n failed logins") Schema.schema)
        <*> systemSettingsSetSuspendInactiveUsers
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setSuspendInactiveUsers" (description ?~ "If last cookie renewal is too long ago, suspend the user.") Schema.schema)
        <*> systemSettingsSetRichInfoLimit
        Schema..= Schema.fieldWithDocModifier "setRichInfoLimit" (description ?~ "Max size of rich info (number of chars in field names and values)") Schema.schema
        <*> systemSettingsSetDefaultTemplateLocaleInternal
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setDefaultTemplateLocaleInternal" (description ?~ "Default locale to use when selecting templates") Schema.schema)
        <*> systemSettingsSetDefaultUserLocaleInternal
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setDefaultUserLocaleInternal" (description ?~ "Default locale to use for users") Schema.schema)
        <*> systemSettingsSetMaxTeamSize
        Schema..= Schema.fieldWithDocModifier "setMaxTeamSize" (description ?~ "Max. # of members in a team") Schema.schema
        <*> systemSettingsSetMaxConvSize
        Schema..= Schema.fieldWithDocModifier "setMaxConvSize" (description ?~ "Max. # of members in a conversation") Schema.schema
        <*> systemSettingsSetEmailVisibility
        Schema..= Schema.fieldWithDocModifier "setEmailVisibility" (description ?~ "Whether to expose user emails and to whom") Schema.schema
        <*> systemSettingsSetPropertyMaxKeyLen
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setPropertyMaxKeyLen" (description ?~ "Max length of user properties keys (in characters)") Schema.schema)
        <*> systemSettingsSetPropertyMaxValueLen
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setPropertyMaxValueLen" (description ?~ "Max length of user properties values (in characters)") Schema.schema)
        <*> systemSettingsSetDeleteThrottleMillis
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setDeleteThrottleMillis" (description ?~ "How long, in milliseconds, to wait in between processing delete events from the internal delete queue") Schema.schema)
        <*> systemSettingsSetSearchSameTeamOnly
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setSearchSameTeamOnly" (description ?~ "When true, search only returns users from the same team") Schema.schema)
        <*> systemSettingsSetFederationDomain
        Schema..= Schema.fieldWithDocModifier "setFederationDomain" (description ?~ "The system's federation domain; used to e.g. qualify local IDs and handles") Schema.schema
        <*> systemSettingsSetSqsThrottleMillis
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setSqsThrottleMillis" (description ?~ "The amount of time in milliseconds to wait after reading from an SQS queue returns no message, before asking for messages from SQS again") Schema.schema)
        <*> systemSettingsSetRestrictUserCreation
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setRestrictUserCreation" (description ?~ "Do not allow certain user creation flows") Schema.schema)
        <*> systemSettingsSetFeatureFlags
        Schema..= Schema.maybe_ (Schema.optField "setFeatureFlags" Schema.schema)
        <*> systemSettingsSetEnableDevelopmentVersions
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setEnableDevelopmentVersions" (description ?~ "When set, development API versions are advertised to clients") Schema.schema)
        <*> systemSettingsSet2FACodeGenerationDelaySecsInternal
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "set2FACodeGenerationDelaySecsInternal" (description ?~ "Minimum delay in seconds between consecutive attempts to generate a new verification code") Schema.schema)
        <*> systemSettingsSetNonceTtlSecsInternal
        Schema..= Schema.maybe_ (Schema.optFieldWithDocModifier "setNonceTtlSecsInternal" (description ?~ "The time-to-live of a nonce in seconds") Schema.schema)

-- | The analog to `GT.FeatureFlags`.  This type tracks only the things that we need to
-- express our current cloud business logic.
--
-- FUTUREWORK: it would be nice to have a system of feature configs that allows to coherently
-- express arbitrary logic accross personal and team accounts, teams, and instances; including
-- default values for new records, default for records that have a NULL value (eg., because
-- they are grandfathered), and feature-specific extra data (eg., TLL for self-deleting
-- messages).  For now, we have something quick & simple.
data AccountFeatureConfigs = AccountFeatureConfigs
  { afcConferenceCallingDefNew :: !(Public.ImplicitLockStatus Public.ConferenceCallingConfig),
    afcConferenceCallingDefNull :: !(Public.ImplicitLockStatus Public.ConferenceCallingConfig)
  }
  deriving (Show, Eq, Generic)

instance Arbitrary AccountFeatureConfigs where
  arbitrary = AccountFeatureConfigs <$> fmap unlocked arbitrary <*> fmap unlocked arbitrary
    where
      unlocked :: Public.ImplicitLockStatus a -> Public.ImplicitLockStatus a
      unlocked = Public.ImplicitLockStatus . Public.setLockStatus Public.LockStatusUnlocked . Public._unImplicitLockStatus

instance A.FromJSON AccountFeatureConfigs where
  parseJSON =
    A.withObject
      "AccountFeatureConfigs"
      ( \obj -> do
          confCallInit <- obj A..: "conferenceCalling"
          A.withObject
            "conferenceCalling"
            ( \obj' -> do
                AccountFeatureConfigs
                  <$> obj'
                  A..: "defaultForNew"
                  <*> obj'
                  A..: "defaultForNull"
            )
            confCallInit
      )

instance A.ToJSON AccountFeatureConfigs where
  toJSON
    AccountFeatureConfigs
      { afcConferenceCallingDefNew,
        afcConferenceCallingDefNull
      } =
      A.object
        [ "conferenceCalling"
            A..= A.object
              [ "defaultForNew" A..= afcConferenceCallingDefNew,
                "defaultForNull" A..= afcConferenceCallingDefNull
              ]
        ]

instance ToSchema AccountFeatureConfigs where
  schema =
    mkSchema d parse serialize
    where
      d :: NamedSwaggerDoc
      d = swaggerDoc @AccountFeatureConfigs
      parse :: A.Value -> AT.Parser AccountFeatureConfigs
      parse = A.parseJSON
      serialize :: AccountFeatureConfigs -> Maybe A.Value
      serialize = Just . A.toJSON

instance S.ToSchema AccountFeatureConfigs where
  declareNamedSchema _ = do
    fieldSchemaRef <-
      S.Inline . (^. S.schema)
        <$> schemaToSwagger (Proxy :: Proxy Public.ConferenceCallingConfig)
    let conferenceCallingRef =
          S.Inline . (^. S.schema)
            <$> NamedSchema (Just "conferenceCalling")
            $ mempty
              & type_ ?~ SwaggerObject
              & S.description ?~ "Default feature settings for conference calling"
              & S.properties
                .~ InsOrdHashMap.fromList
                  [ ("defaultForNew", fieldSchemaRef),
                    ("defaultForNull", fieldSchemaRef)
                  ]
              & required .~ ["defaultForNew", "defaultForNull"]
    pure $
      NamedSchema (Just "AccountFeatureConfigs") $
        mempty
          & type_ ?~ SwaggerObject
          & S.properties .~ InsOrdHashMap.singleton "conferenceCalling" conferenceCallingRef
          & required .~ ["conferenceCalling"]
