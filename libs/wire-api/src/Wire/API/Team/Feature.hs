{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Wire.API.Team.Feature
  ( FeatureStatus (..),
    featureName,
    featureNameBS,
    LockStatus (..),
    DbFeature (..),
    dbFeatureLockStatus,
    dbFeatureStatus,
    dbFeatureConfig,
    dbFeatureModConfig,
    LockableFeature (..),
    defUnlockedFeature,
    defLockedFeature,
    LockableFeaturePatch (..),
    Feature (..),
    forgetLock,
    withLockStatus,
    withUnlocked,
    FeatureTTL,
    FeatureTTLDays,
    FeatureTTL' (..),
    FeatureTTLUnit (..),
    convertFeatureTTLDaysToSeconds,
    EnforceAppLock (..),
    genericComputeFeature,
    IsFeatureConfig (..),
    FeatureSingleton (..),
    DeprecatedFeatureName,
    LockStatusResponse (..),
    One2OneCalls (..),
    -- Features
    LegalholdConfig (..),
    SSOConfig (..),
    SearchVisibilityAvailableConfig (..),
    SelfDeletingMessagesConfig (..),
    ValidateSAMLEmailsConfig (..),
    DigitalSignaturesConfig (..),
    ConferenceCallingConfig (..),
    GuestLinksConfig (..),
    ExposeInvitationURLsToTeamAdminConfig (..),
    SndFactorPasswordChallengeConfig (..),
    SearchVisibilityInboundConfig (..),
    ClassifiedDomainsConfig (..),
    AppLockConfig (..),
    FileSharingConfig (..),
    MLSConfig (..),
    OutlookCalIntegrationConfig (..),
    MlsE2EIdConfig (..),
    MlsMigrationConfig (..),
    EnforceFileDownloadLocationConfig (..),
    LimitedEventFanoutConfig (..),
    Features,
    AllFeatures,
    NpProject (..),
    npProject,
    NpUpdate (..),
    npUpdate,
    AllTeamFeatures,
  )
where

import Cassandra.CQL qualified as Cass
import Control.Lens ((?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.ByteString qualified as Parser
import Data.ByteString (fromStrict)
import Data.ByteString.Conversion
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Domain (Domain)
import Data.Either.Extra (maybeToEither)
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Misc (HttpsUrl)
import Data.Monoid
import Data.OpenApi qualified as S
import Data.Proxy
import Data.SOP
import Data.Schema
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as TL
import Data.Time
import Deriving.Aeson
import GHC.TypeLits
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (getPrintableString)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite (CipherSuiteTag (MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519))
import Wire.API.Routes.Named
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- FeatureTag

-- | Checklist for adding a new feature
--
-- 1. Add a data type for your feature's "config" part, naming convention:
-- **<NameOfFeature>Config**. If your feature doesn't have a config besides
-- being enabled/disabled, locked/unlocked, then the config should be a unit
-- type, e.g. **data MyFeatureConfig = MyFeatureConfig**. Add a singleton for
-- the new data type. Implement type classes 'RenderableSymbol', 'ToSchema',
-- 'IsFeatureConfig' and 'Arbitrary'.
--
-- 2. Add the config to 'AllTeamFeatures'.
--
-- 3. If your feature is configurable on a per-team basis, add a schema
-- migration in galley and extend 'getFeatureStatus' and similar functions in
-- Galley.Cassandra.TeamFeatures
--
-- 4. Add the feature to the config schema of galley in Galley.Types.Teams.
-- and extend the Arbitrary instance of FeatureConfigs in the unit tests
-- Test.Galley.Types
--
-- 5. Implement 'GetFeatureConfig' and 'SetFeatureConfig' in
-- Galley.API.Teams.Features which defines the main business logic for getting
-- and setting (with side-effects). Note that we don't have to check the
-- lockstatus inside 'setConfigForTeam' because the lockstatus is checked in
-- 'setFeatureStatus' before which is the public API for setting the feature
-- status.
--
-- 6. Add public routes to Wire.API.Routes.Public.Galley.Feature:
-- 'FeatureStatusGet', 'FeatureStatusPut' (optional). Then implement them in
-- Galley.API.Public.Feature.
--
-- 7. Add internal routes in Wire.API.Routes.Internal.Galley and implement them
-- in Galley.API.Internal.
--
-- 8. If the feature should be configurable via Stern add routes to Stern.API.
-- Manually check that the swagger looks okay and works.
--
-- 9. If the feature is configured on a per-user level, see the
-- 'ConferenceCallingConfig' as an example.
-- (https://github.com/wireapp/wire-server/pull/1811,
-- https://github.com/wireapp/wire-server/pull/1818)
--
-- 10. Extend the integration tests with cases.
--
-- 11. If applicable, edit/update the configurations:
--     - optionally add the config for local integration tests to 'galley.integration.yaml'
--     - add a config mapping to 'charts/galley/templates/configmap.yaml'
--     - add the defaults to 'charts/galley/values.yaml'
--     - optionally add config for CI to 'hack/helm_vars/wire-server/values.yaml'
--
-- 12. Add a section to the documentation at an appropriate place
-- (e.g. 'docs/src/developer/reference/config-options.md' (if applicable) or
-- 'docs/src/understand/team-feature-settings.md')
class
  ( Default cfg,
    ToSchema cfg,
    Default (LockableFeature cfg),
    KnownSymbol (FeatureSymbol cfg)
  ) =>
  IsFeatureConfig cfg
  where
  type FeatureSymbol cfg :: Symbol
  featureSingleton :: FeatureSingleton cfg

  objectSchema ::
    -- | Should be "pure MyFeatureConfig" if the feature doesn't have config,
    -- which results in a trivial empty schema and the "config" field being
    -- omitted/ignored in the JSON encoder / parser.
    ObjectSchema SwaggerDoc cfg

data FeatureSingleton cfg where
  FeatureSingletonGuestLinksConfig :: FeatureSingleton GuestLinksConfig
  FeatureSingletonLegalholdConfig :: FeatureSingleton LegalholdConfig
  FeatureSingletonSSOConfig :: FeatureSingleton SSOConfig
  FeatureSingletonSearchVisibilityAvailableConfig :: FeatureSingleton SearchVisibilityAvailableConfig
  FeatureSingletonValidateSAMLEmailsConfig :: FeatureSingleton ValidateSAMLEmailsConfig
  FeatureSingletonDigitalSignaturesConfig :: FeatureSingleton DigitalSignaturesConfig
  FeatureSingletonConferenceCallingConfig :: FeatureSingleton ConferenceCallingConfig
  FeatureSingletonSndFactorPasswordChallengeConfig :: FeatureSingleton SndFactorPasswordChallengeConfig
  FeatureSingletonSearchVisibilityInboundConfig :: FeatureSingleton SearchVisibilityInboundConfig
  FeatureSingletonClassifiedDomainsConfig :: FeatureSingleton ClassifiedDomainsConfig
  FeatureSingletonAppLockConfig :: FeatureSingleton AppLockConfig
  FeatureSingletonSelfDeletingMessagesConfig :: FeatureSingleton SelfDeletingMessagesConfig
  FeatureSingletonFileSharingConfig :: FeatureSingleton FileSharingConfig
  FeatureSingletonMLSConfig :: FeatureSingleton MLSConfig
  FeatureSingletonExposeInvitationURLsToTeamAdminConfig :: FeatureSingleton ExposeInvitationURLsToTeamAdminConfig
  FeatureSingletonOutlookCalIntegrationConfig :: FeatureSingleton OutlookCalIntegrationConfig
  FeatureSingletonMlsE2EIdConfig :: FeatureSingleton MlsE2EIdConfig
  FeatureSingletonMlsMigrationConfig :: FeatureSingleton MlsMigrationConfig
  FeatureSingletonEnforceFileDownloadLocationConfig :: FeatureSingleton EnforceFileDownloadLocationConfig
  FeatureSingletonLimitedEventFanoutConfig :: FeatureSingleton LimitedEventFanoutConfig

type family DeprecatedFeatureName cfg :: Symbol

featureName :: forall cfg. (IsFeatureConfig cfg) => Text
featureName = T.pack $ symbolVal (Proxy @(FeatureSymbol cfg))

featureNameBS :: forall cfg. (IsFeatureConfig cfg) => ByteString
featureNameBS = UTF8.fromString $ symbolVal (Proxy @(FeatureSymbol cfg))

--------------------------------------------------------------------------------
-- DbFeature

-- | Feature data stored in the database, as a function of its default values.
newtype DbFeature cfg = DbFeature
  {applyDbFeature :: LockableFeature cfg -> LockableFeature cfg}
  deriving (Semigroup, Monoid) via Endo (LockableFeature cfg)

dbFeatureLockStatus :: LockStatus -> DbFeature cfg
dbFeatureLockStatus s = DbFeature $ \w -> w {lockStatus = s}

dbFeatureStatus :: FeatureStatus -> DbFeature cfg
dbFeatureStatus s = DbFeature $ \w -> w {status = s}

dbFeatureConfig :: cfg -> DbFeature cfg
dbFeatureConfig c = DbFeature $ \w -> w {config = c}

dbFeatureModConfig :: (cfg -> cfg) -> DbFeature cfg
dbFeatureModConfig f = DbFeature $ \w -> w {config = f w.config}

----------------------------------------------------------------------
-- LockableFeature

-- [Note: unsettable features]
--
-- Some feature flags (e.g. sso) don't have a lock status stored in the
-- database. Instead, they are considered unlocked by default, but behave as if
-- they were locked, since they lack a public PUT endpoint.
--
-- This trick has caused a lot of confusion in the past, and cannot be extended
-- to flags that have non-trivial configuration. For this reason, we are in the
-- process of changing this mechanism to make it work like every other feature.
--
-- That means that such features will afterwards be toggled by setting their
-- lock status instead. And we'll have some logic in place to make the default
-- status when unlocked be enabled. This achieves a similar behaviour but with
-- fewer exceptional code paths.
--
-- See the implementation of 'computeFeature' for 'ConferenceCallingConfig' for
-- an example of this mechanism in practice.

data LockableFeature cfg = LockableFeature
  { status :: FeatureStatus,
    lockStatus :: LockStatus,
    config :: cfg
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema (LockableFeature cfg)

instance (Default (LockableFeature cfg)) => Default (Feature cfg) where
  def = forgetLock def

-- | A feature that is disabled and locked.
defLockedFeature :: (Default cfg) => LockableFeature cfg
defLockedFeature =
  LockableFeature
    { status = FeatureStatusDisabled,
      lockStatus = LockStatusLocked,
      config = def
    }

-- | A feature that is enabled and unlocked.
defUnlockedFeature :: (Default cfg) => LockableFeature cfg
defUnlockedFeature =
  LockableFeature
    { status = FeatureStatusEnabled,
      lockStatus = LockStatusUnlocked,
      config = def
    }

instance (IsFeatureConfig cfg) => ToSchema (LockableFeature cfg) where
  schema =
    object name $
      LockableFeature
        <$> (.status) .= field "status" schema
        <*> (.lockStatus) .= field "lockStatus" schema
        <*> (.config) .= objectSchema @cfg
        <* const FeatureTTLUnlimited
          .= optField
            "ttl"
            (schema :: ValueSchema NamedSwaggerDoc FeatureTTL)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".LockableFeature"

instance (Arbitrary cfg, IsFeatureConfig cfg) => Arbitrary (LockableFeature cfg) where
  arbitrary = LockableFeature <$> arbitrary <*> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- LockableFeaturePatch

data LockableFeaturePatch (cfg :: Type) = LockableFeaturePatch
  { status :: Maybe FeatureStatus,
    lockStatus :: Maybe LockStatus,
    config :: Maybe cfg
  }
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (LockableFeaturePatch cfg))

-- | The ToJSON implementation of `LockableFeaturePatch` will encode the trivial config as `"config": {}`
-- when the value is a `Just`, if it's `Nothing` it will be omitted, which is the important part.
instance (ToSchema cfg) => ToSchema (LockableFeaturePatch cfg) where
  schema =
    object name $
      LockableFeaturePatch
        <$> (.status) .= maybe_ (optField "status" schema)
        <*> (.lockStatus) .= maybe_ (optField "lockStatus" schema)
        <*> (.config) .= maybe_ (optField "config" schema)
        <* const FeatureTTLUnlimited
          .= optField
            "ttl"
            (schema :: ValueSchema NamedSwaggerDoc FeatureTTL)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".LockableFeaturePatch"

instance (Arbitrary cfg, IsFeatureConfig cfg) => Arbitrary (LockableFeaturePatch cfg) where
  arbitrary = LockableFeaturePatch <$> arbitrary <*> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- Feature

data Feature (cfg :: Type) = Feature
  { status :: FeatureStatus,
    config :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable, Functor)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (Feature cfg))

instance (Arbitrary cfg) => Arbitrary (Feature cfg) where
  arbitrary = Feature <$> arbitrary <*> arbitrary

forgetLock :: LockableFeature a -> Feature a
forgetLock ws = Feature ws.status ws.config

withLockStatus :: LockStatus -> Feature a -> LockableFeature a
withLockStatus ls (Feature s c) = LockableFeature s ls c

withUnlocked :: Feature a -> LockableFeature a
withUnlocked = withLockStatus LockStatusUnlocked

instance (ToSchema cfg, IsFeatureConfig cfg) => ToSchema (Feature cfg) where
  schema =
    object name $
      Feature
        <$> (.status) .= field "status" schema
        <*> (.config) .= objectSchema @cfg
        <* const FeatureTTLUnlimited
          .= optField
            "ttl"
            (schema :: ValueSchema NamedSwaggerDoc FeatureTTL)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".Feature"

----------------------------------------------------------------------
-- FeatureTTL

-- Using Word to avoid dealing with negative numbers.
-- Ideally we would also not support zero.
-- Currently a TTL=0 is ignored on the cassandra side.
data FeatureTTL' (u :: FeatureTTLUnit)
  = -- | actually, unit depends on phantom type.
    FeatureTTLSeconds Word
  | FeatureTTLUnlimited
  deriving stock (Eq, Show, Generic)

data FeatureTTLUnit = FeatureTTLUnitSeconds | FeatureTTLUnitDays

type FeatureTTL = FeatureTTL' 'FeatureTTLUnitSeconds

type FeatureTTLDays = FeatureTTL' 'FeatureTTLUnitDays

convertFeatureTTLDaysToSeconds :: FeatureTTLDays -> FeatureTTL
convertFeatureTTLDaysToSeconds FeatureTTLUnlimited = FeatureTTLUnlimited
convertFeatureTTLDaysToSeconds (FeatureTTLSeconds d) = FeatureTTLSeconds (d * (60 * 60 * 24))

instance Arbitrary FeatureTTL where
  arbitrary =
    (nonZero <$> arbitrary)
      `suchThat` ( \case
                     -- A very short TTL (<= 2) can cause race conditions in the integration tests
                     FeatureTTLSeconds n -> n > 2
                     _ -> True
                 )
    where
      nonZero 0 = FeatureTTLUnlimited
      nonZero n = FeatureTTLSeconds n

instance ToSchema FeatureTTL where
  schema = mkSchema ttlDoc toTTL fromTTL
    where
      ttlDoc :: NamedSwaggerDoc
      ttlDoc = swaggerDoc @Word & S.schema . S.example ?~ "unlimited"

      toTTL :: A.Value -> A.Parser FeatureTTL
      toTTL v = parseUnlimited v <|> parseSeconds v

      parseUnlimited :: A.Value -> A.Parser FeatureTTL
      parseUnlimited =
        A.withText "FeatureTTL" $
          \t ->
            if t == "unlimited" || t == "0"
              then pure FeatureTTLUnlimited
              else A.parseFail "Expected ''unlimited' or '0'."

      parseSeconds :: A.Value -> A.Parser FeatureTTL
      parseSeconds = A.withScientific "FeatureTTL" $
        \s -> case toBoundedInteger s of
          Just 0 -> error "impossible (this would have parsed in `parseUnlimited` above)."
          Just i -> pure . FeatureTTLSeconds $ i
          Nothing -> A.parseFail "Expected an positive integer."

      fromTTL :: FeatureTTL -> Maybe A.Value
      fromTTL FeatureTTLUnlimited = Just "unlimited"
      fromTTL (FeatureTTLSeconds 0) = Nothing -- Should be unlimited
      fromTTL (FeatureTTLSeconds s) = Just $ A.toJSON s

instance ToHttpApiData (FeatureTTL' u) where
  toQueryParam = T.decodeUtf8 . toByteString'

instance FromHttpApiData (FeatureTTL' u) where
  parseQueryParam = maybeToEither invalidTTLErrorString . fromByteString . T.encodeUtf8

instance S.ToParamSchema (FeatureTTL' u) where
  toParamSchema _ = S.toParamSchema (Proxy @Text)

instance ToByteString (FeatureTTL' u) where
  builder FeatureTTLUnlimited = "unlimited"
  builder (FeatureTTLSeconds d) = (builder . TL.pack . show) d

instance FromByteString (FeatureTTL' u) where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "unlimited" -> pure FeatureTTLUnlimited
        Right d -> case readEither . T.unpack $ d of
          Left _ -> fail $ T.unpack invalidTTLErrorString
          Right d' -> pure . FeatureTTLSeconds $ d'
        Left _ -> fail $ T.unpack invalidTTLErrorString

instance Cass.Cql FeatureTTL where
  ctype = Cass.Tagged Cass.IntColumn

  -- Passing TTL = 0 to Cassandra removes the TTL.
  -- It does not instantly revert back.
  fromCql (Cass.CqlInt 0) = pure FeatureTTLUnlimited
  fromCql (Cass.CqlInt n) = pure . FeatureTTLSeconds . fromIntegral $ n
  fromCql _ = Left "fromCql: TTLValue: CqlInt expected"

  toCql FeatureTTLUnlimited = Cass.CqlInt 0
  toCql (FeatureTTLSeconds d) = Cass.CqlInt . fromIntegral $ d

invalidTTLErrorString :: Text
invalidTTLErrorString = "Invalid FeatureTTLSeconds: must be a positive integer or 'unlimited.'"

-- LockStatus

data LockStatus = LockStatusLocked | LockStatusUnlocked
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LockStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LockStatus)

instance FromHttpApiData LockStatus where
  parseUrlPiece = maybeToEither "Invalid lock status" . fromByteString . T.encodeUtf8

instance ToSchema LockStatus where
  schema =
    enum @Text "LockStatus" $
      mconcat
        [ element "locked" LockStatusLocked,
          element "unlocked" LockStatusUnlocked
        ]

instance S.ToParamSchema LockStatus where
  toParamSchema _ = mempty & S.type_ ?~ S.OpenApiString & S.enum_ ?~ ["locked", "unlocked"]

instance ToByteString LockStatus where
  builder LockStatusLocked = "locked"
  builder LockStatusUnlocked = "unlocked"

instance FromByteString LockStatus where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "locked" -> pure LockStatusLocked
        Right "unlocked" -> pure LockStatusUnlocked
        Right t -> fail $ "Invalid LockStatus: " <> T.unpack t
        Left e -> fail $ "Invalid LockStatus: " <> show e

instance Cass.Cql LockStatus where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure LockStatusLocked
    1 -> pure LockStatusUnlocked
    _ -> Left "fromCql: Invalid LockStatus"
  fromCql _ = Left "fromCql: LockStatus: CqlInt expected"

  toCql LockStatusLocked = Cass.CqlInt 0
  toCql LockStatusUnlocked = Cass.CqlInt 1

newtype LockStatusResponse = LockStatusResponse {_unlockStatus :: LockStatus}
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LockStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LockStatusResponse)

instance ToSchema LockStatusResponse where
  schema =
    object "LockStatusResponse" $
      LockStatusResponse
        <$> _unlockStatus .= field "lockStatus" schema

-- | Convert a feature coming from the database to its public form. This can be
-- overridden on a feature basis by implementing the `computeFeature` method of
-- the `GetFeatureConfig` class.
genericComputeFeature :: forall cfg. LockableFeature cfg -> DbFeature cfg -> LockableFeature cfg
genericComputeFeature defFeature dbFeature =
  let feat = applyDbFeature dbFeature defFeature
   in case feat.lockStatus of
        LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
        LockStatusUnlocked -> feat

--------------------------------------------------------------------------------
-- GuestLinks feature

data GuestLinksConfig = GuestLinksConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GuestLinksConfig)
  deriving (RenderableSymbol) via (RenderableTypeName GuestLinksConfig)

instance Default GuestLinksConfig where
  def = GuestLinksConfig

instance ToSchema GuestLinksConfig where
  schema = object "GuestLinksConfig" objectSchema

instance Default (LockableFeature GuestLinksConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig GuestLinksConfig where
  type FeatureSymbol GuestLinksConfig = "conversationGuestLinks"
  featureSingleton = FeatureSingletonGuestLinksConfig

  objectSchema = pure GuestLinksConfig

--------------------------------------------------------------------------------
-- Legalhold feature

data LegalholdConfig = LegalholdConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalholdConfig)
  deriving (RenderableSymbol) via (RenderableTypeName LegalholdConfig)

instance Default LegalholdConfig where
  def = LegalholdConfig

instance Default (LockableFeature LegalholdConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig LegalholdConfig where
  type FeatureSymbol LegalholdConfig = "legalhold"
  featureSingleton = FeatureSingletonLegalholdConfig
  objectSchema = pure LegalholdConfig

instance ToSchema LegalholdConfig where
  schema = object "LegalholdConfig" objectSchema

--------------------------------------------------------------------------------
-- SSO feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data SSOConfig = SSOConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SSOConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SSOConfig)

instance Default SSOConfig where
  def = SSOConfig

instance Default (LockableFeature SSOConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig SSOConfig where
  type FeatureSymbol SSOConfig = "sso"
  featureSingleton = FeatureSingletonSSOConfig
  objectSchema = pure SSOConfig

instance ToSchema SSOConfig where
  schema = object "SSOConfig" objectSchema

--------------------------------------------------------------------------------
-- SearchVisibility available feature

-- | Wether a team is allowed to change search visibility
-- See the handle of PUT /teams/:tid/search-visibility
data SearchVisibilityAvailableConfig = SearchVisibilityAvailableConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityAvailableConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SearchVisibilityAvailableConfig)

instance Default SearchVisibilityAvailableConfig where
  def = SearchVisibilityAvailableConfig

instance Default (LockableFeature SearchVisibilityAvailableConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig SearchVisibilityAvailableConfig where
  type FeatureSymbol SearchVisibilityAvailableConfig = "searchVisibility"
  featureSingleton = FeatureSingletonSearchVisibilityAvailableConfig
  objectSchema = pure SearchVisibilityAvailableConfig

instance ToSchema SearchVisibilityAvailableConfig where
  schema = object "SearchVisibilityAvailableConfig" objectSchema

type instance DeprecatedFeatureName SearchVisibilityAvailableConfig = "search-visibility"

--------------------------------------------------------------------------------
-- ValidateSAMLEmails feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data ValidateSAMLEmailsConfig = ValidateSAMLEmailsConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ValidateSAMLEmailsConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ValidateSAMLEmailsConfig)

instance Default ValidateSAMLEmailsConfig where
  def = ValidateSAMLEmailsConfig

instance ToSchema ValidateSAMLEmailsConfig where
  schema = object "ValidateSAMLEmailsConfig" objectSchema

instance Default (LockableFeature ValidateSAMLEmailsConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig ValidateSAMLEmailsConfig where
  type FeatureSymbol ValidateSAMLEmailsConfig = "validateSAMLemails"
  featureSingleton = FeatureSingletonValidateSAMLEmailsConfig
  objectSchema = pure ValidateSAMLEmailsConfig

type instance DeprecatedFeatureName ValidateSAMLEmailsConfig = "validate-saml-emails"

--------------------------------------------------------------------------------
-- DigitalSignatures feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data DigitalSignaturesConfig = DigitalSignaturesConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DigitalSignaturesConfig)
  deriving (RenderableSymbol) via (RenderableTypeName DigitalSignaturesConfig)

instance Default DigitalSignaturesConfig where
  def = DigitalSignaturesConfig

instance Default (LockableFeature DigitalSignaturesConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig DigitalSignaturesConfig where
  type FeatureSymbol DigitalSignaturesConfig = "digitalSignatures"
  featureSingleton = FeatureSingletonDigitalSignaturesConfig
  objectSchema = pure DigitalSignaturesConfig

type instance DeprecatedFeatureName DigitalSignaturesConfig = "digital-signatures"

instance ToSchema DigitalSignaturesConfig where
  schema = object "DigitalSignaturesConfig" objectSchema

--------------------------------------------------------------------------------
-- ConferenceCalling feature

data One2OneCalls = One2OneCallsTurn | One2OneCallsSft
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform One2OneCalls)

one2OneCallsFromUseSftFlag :: Bool -> One2OneCalls
one2OneCallsFromUseSftFlag False = One2OneCallsTurn
one2OneCallsFromUseSftFlag True = One2OneCallsSft

instance Default One2OneCalls where
  def = One2OneCallsTurn

instance Cass.Cql One2OneCalls where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure One2OneCallsTurn
    1 -> pure One2OneCallsSft
    _ -> Left "fromCql: Invalid One2OneCalls"
  fromCql _ = Left "fromCql: One2OneCalls: CqlInt expected"

  toCql One2OneCallsTurn = Cass.CqlInt 0
  toCql One2OneCallsSft = Cass.CqlInt 1

data ConferenceCallingConfig = ConferenceCallingConfig
  { one2OneCalls :: One2OneCalls
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConferenceCallingConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ConferenceCallingConfig)

instance Default ConferenceCallingConfig where
  def = ConferenceCallingConfig {one2OneCalls = def}

instance Default (LockableFeature ConferenceCallingConfig) where
  def = defLockedFeature {status = FeatureStatusEnabled}

instance IsFeatureConfig ConferenceCallingConfig where
  type FeatureSymbol ConferenceCallingConfig = "conferenceCalling"
  featureSingleton = FeatureSingletonConferenceCallingConfig
  objectSchema = fromMaybe def <$> optField "config" schema

instance ToSchema ConferenceCallingConfig where
  schema =
    object "ConferenceCallingConfig" $
      ConferenceCallingConfig
        <$> ((== One2OneCallsSft) . one2OneCalls)
          .= ( maybe def one2OneCallsFromUseSftFlag
                 <$> optField "useSFTForOneToOneCalls" schema
             )

--------------------------------------------------------------------------------
-- SndFactorPasswordChallenge feature

data SndFactorPasswordChallengeConfig = SndFactorPasswordChallengeConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SndFactorPasswordChallengeConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SndFactorPasswordChallengeConfig)

instance Default SndFactorPasswordChallengeConfig where
  def = SndFactorPasswordChallengeConfig

instance ToSchema SndFactorPasswordChallengeConfig where
  schema = object "SndFactorPasswordChallengeConfig" objectSchema

instance Default (LockableFeature SndFactorPasswordChallengeConfig) where
  def = defLockedFeature

instance IsFeatureConfig SndFactorPasswordChallengeConfig where
  type FeatureSymbol SndFactorPasswordChallengeConfig = "sndFactorPasswordChallenge"
  featureSingleton = FeatureSingletonSndFactorPasswordChallengeConfig
  objectSchema = pure SndFactorPasswordChallengeConfig

--------------------------------------------------------------------------------
-- SearchVisibilityInbound feature

data SearchVisibilityInboundConfig = SearchVisibilityInboundConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityInboundConfig)
  deriving (S.ToSchema) via Schema SearchVisibilityInboundConfig
  deriving (RenderableSymbol) via (RenderableTypeName SearchVisibilityInboundConfig)

instance Default SearchVisibilityInboundConfig where
  def = SearchVisibilityInboundConfig

instance Default (LockableFeature SearchVisibilityInboundConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig SearchVisibilityInboundConfig where
  type FeatureSymbol SearchVisibilityInboundConfig = "searchVisibilityInbound"
  featureSingleton = FeatureSingletonSearchVisibilityInboundConfig
  objectSchema = pure SearchVisibilityInboundConfig

instance ToSchema SearchVisibilityInboundConfig where
  schema = object "SearchVisibilityInboundConfig" objectSchema

----------------------------------------------------------------------
-- ClassifiedDomains feature

-- | This feature is quite special, in that it does not have any database
-- state. Its value cannot be updated dynamically, and is always set to the
-- server default taken from the backend configuration.
data ClassifiedDomainsConfig = ClassifiedDomainsConfig
  { classifiedDomainsDomains :: [Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ClassifiedDomainsConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ClassifiedDomainsConfig)

instance Default ClassifiedDomainsConfig where
  def = ClassifiedDomainsConfig []

deriving via (GenericUniform ClassifiedDomainsConfig) instance Arbitrary ClassifiedDomainsConfig

instance ToSchema ClassifiedDomainsConfig where
  schema =
    object "ClassifiedDomainsConfig" $
      ClassifiedDomainsConfig
        <$> classifiedDomainsDomains .= field "domains" (array schema)

instance Default (LockableFeature ClassifiedDomainsConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig ClassifiedDomainsConfig where
  type FeatureSymbol ClassifiedDomainsConfig = "classifiedDomains"

  featureSingleton = FeatureSingletonClassifiedDomainsConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- AppLock feature

data AppLockConfig = AppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AppLockConfig)
  deriving (Arbitrary) via (GenericUniform AppLockConfig)
  deriving (RenderableSymbol) via (RenderableTypeName AppLockConfig)

instance Default AppLockConfig where
  def = AppLockConfig (EnforceAppLock False) 60

instance ToSchema AppLockConfig where
  schema =
    object "AppLockConfig" $
      AppLockConfig
        <$> applockEnforceAppLock .= field "enforceAppLock" schema
        <*> applockInactivityTimeoutSecs .= field "inactivityTimeoutSecs" schema

instance Default (LockableFeature AppLockConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig AppLockConfig where
  type FeatureSymbol AppLockConfig = "appLock"

  featureSingleton = FeatureSingletonAppLockConfig
  objectSchema = field "config" schema

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON) via (Schema EnforceAppLock)

instance ToSchema EnforceAppLock where
  schema = EnforceAppLock <$> (\(EnforceAppLock v) -> v) .= schema

--------------------------------------------------------------------------------
-- FileSharing feature

data FileSharingConfig = FileSharingConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FileSharingConfig)
  deriving (RenderableSymbol) via (RenderableTypeName FileSharingConfig)

instance Default FileSharingConfig where
  def = FileSharingConfig

instance Default (LockableFeature FileSharingConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig FileSharingConfig where
  type FeatureSymbol FileSharingConfig = "fileSharing"
  featureSingleton = FeatureSingletonFileSharingConfig
  objectSchema = pure FileSharingConfig

instance ToSchema FileSharingConfig where
  schema = object "FileSharingConfig" objectSchema

----------------------------------------------------------------------
-- SelfDeletingMessagesConfig

newtype SelfDeletingMessagesConfig = SelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema SelfDeletingMessagesConfig)
  deriving (Arbitrary) via (GenericUniform SelfDeletingMessagesConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SelfDeletingMessagesConfig)

instance Default SelfDeletingMessagesConfig where
  def = SelfDeletingMessagesConfig 0

instance ToSchema SelfDeletingMessagesConfig where
  schema =
    object "SelfDeletingMessagesConfig" $
      SelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= field "enforcedTimeoutSeconds" schema

instance Default (LockableFeature SelfDeletingMessagesConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig SelfDeletingMessagesConfig where
  type FeatureSymbol SelfDeletingMessagesConfig = "selfDeletingMessages"
  featureSingleton = FeatureSingletonSelfDeletingMessagesConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- MLSConfig

data MLSConfig = MLSConfig
  { mlsProtocolToggleUsers :: [UserId],
    mlsDefaultProtocol :: ProtocolTag,
    mlsAllowedCipherSuites :: [CipherSuiteTag],
    mlsDefaultCipherSuite :: CipherSuiteTag,
    mlsSupportedProtocols :: [ProtocolTag]
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MLSConfig)
  deriving (RenderableSymbol) via (RenderableTypeName MLSConfig)

instance Default MLSConfig where
  def =
    MLSConfig
      []
      ProtocolProteusTag
      [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519]
      MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
      [ProtocolProteusTag, ProtocolMLSTag]

instance ToSchema MLSConfig where
  schema =
    object "MLSConfig" $
      MLSConfig
        <$> mlsProtocolToggleUsers .= fieldWithDocModifier "protocolToggleUsers" (S.description ?~ "allowlist of users that may change protocols") (array schema)
        <*> mlsDefaultProtocol .= field "defaultProtocol" schema
        <*> mlsAllowedCipherSuites .= field "allowedCipherSuites" (array schema)
        <*> mlsDefaultCipherSuite .= field "defaultCipherSuite" schema
        <*> mlsSupportedProtocols .= field "supportedProtocols" (array schema)

instance Default (LockableFeature MLSConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig MLSConfig where
  type FeatureSymbol MLSConfig = "mls"
  featureSingleton = FeatureSingletonMLSConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- ExposeInvitationURLsToTeamAdminConfig

data ExposeInvitationURLsToTeamAdminConfig = ExposeInvitationURLsToTeamAdminConfig
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform ExposeInvitationURLsToTeamAdminConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ExposeInvitationURLsToTeamAdminConfig)

instance Default ExposeInvitationURLsToTeamAdminConfig where
  def = ExposeInvitationURLsToTeamAdminConfig

instance Default (LockableFeature ExposeInvitationURLsToTeamAdminConfig) where
  def = defLockedFeature

instance IsFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  type FeatureSymbol ExposeInvitationURLsToTeamAdminConfig = "exposeInvitationURLsToTeamAdmin"
  featureSingleton = FeatureSingletonExposeInvitationURLsToTeamAdminConfig
  objectSchema = pure ExposeInvitationURLsToTeamAdminConfig

instance ToSchema ExposeInvitationURLsToTeamAdminConfig where
  schema = object "ExposeInvitationURLsToTeamAdminConfig" objectSchema

----------------------------------------------------------------------
-- OutlookCalIntegrationConfig

-- | This feature setting only applies to the Outlook Calendar extension for Wire.
-- As it is an external service, it should only be configured through this feature flag and otherwise ignored by the backend.
data OutlookCalIntegrationConfig = OutlookCalIntegrationConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform OutlookCalIntegrationConfig)
  deriving (RenderableSymbol) via (RenderableTypeName OutlookCalIntegrationConfig)

instance Default OutlookCalIntegrationConfig where
  def = OutlookCalIntegrationConfig

instance Default (LockableFeature OutlookCalIntegrationConfig) where
  def = defLockedFeature

instance IsFeatureConfig OutlookCalIntegrationConfig where
  type FeatureSymbol OutlookCalIntegrationConfig = "outlookCalIntegration"
  featureSingleton = FeatureSingletonOutlookCalIntegrationConfig
  objectSchema = pure OutlookCalIntegrationConfig

instance ToSchema OutlookCalIntegrationConfig where
  schema = object "OutlookCalIntegrationConfig" objectSchema

----------------------------------------------------------------------
-- MlsE2EId

data MlsE2EIdConfig = MlsE2EIdConfig
  { verificationExpiration :: NominalDiffTime,
    acmeDiscoveryUrl :: Maybe HttpsUrl,
    crlProxy :: Maybe HttpsUrl,
    useProxyOnMobile :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving (RenderableSymbol) via (RenderableTypeName MlsE2EIdConfig)

instance Default MlsE2EIdConfig where
  def = MlsE2EIdConfig (fromIntegral @Int (60 * 60 * 24)) Nothing Nothing False

instance Arbitrary MlsE2EIdConfig where
  arbitrary =
    MlsE2EIdConfig
      <$> (fromIntegral <$> (arbitrary @Word32))
      <*> arbitrary
      <*> fmap Just arbitrary
      <*> arbitrary

instance ToSchema MlsE2EIdConfig where
  schema :: ValueSchema NamedSwaggerDoc MlsE2EIdConfig
  schema =
    object "MlsE2EIdConfig" $
      MlsE2EIdConfig
        <$> (toSeconds . verificationExpiration) .= fieldWithDocModifier "verificationExpiration" veDesc (fromSeconds <$> schema)
        <*> acmeDiscoveryUrl .= maybe_ (optField "acmeDiscoveryUrl" schema)
        <*> crlProxy .= maybe_ (optField "crlProxy" schema)
        <*> useProxyOnMobile .= (fromMaybe False <$> optField "useProxyOnMobile" schema)
    where
      fromSeconds :: Int -> NominalDiffTime
      fromSeconds = fromIntegral

      toSeconds :: NominalDiffTime -> Int
      toSeconds = truncate

      veDesc :: NamedSwaggerDoc -> NamedSwaggerDoc
      veDesc =
        description
          ?~ "When a client first tries to fetch or renew a certificate, \
             \they may need to login to an identity provider (IdP) depending on their IdP domain authentication policy. \
             \The user may have a grace period during which they can \"snooze\" this login. \
             \The duration of this grace period (in seconds) is set in the `verificationDuration` parameter, \
             \which is enforced separately by each client. \
             \After the grace period has expired, the client will not allow the user to use the application \
             \until they have logged to refresh the certificate. The default value is 1 day (86400s). \
             \The client enrolls using the Automatic Certificate Management Environment (ACME) protocol. \
             \The `acmeDiscoveryUrl` parameter must be set to the HTTPS URL of the ACME server discovery endpoint for \
             \this team. It is of the form \"https://acme.{backendDomain}/acme/{provisionerName}/discovery\". For example: \
             \`https://acme.example.com/acme/provisioner1/discovery`."

instance Default (LockableFeature MlsE2EIdConfig) where
  def = defLockedFeature

instance IsFeatureConfig MlsE2EIdConfig where
  type FeatureSymbol MlsE2EIdConfig = "mlsE2EId"
  featureSingleton = FeatureSingletonMlsE2EIdConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- MlsMigration

data MlsMigrationConfig = MlsMigrationConfig
  { startTime :: Maybe UTCTime,
    finaliseRegardlessAfter :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving (RenderableSymbol) via (RenderableTypeName MlsMigrationConfig)

instance Default MlsMigrationConfig where
  def = MlsMigrationConfig Nothing Nothing

instance Arbitrary MlsMigrationConfig where
  arbitrary = do
    startTime <- fmap fromUTCTimeMillis <$> arbitrary
    finaliseRegardlessAfter <- fmap fromUTCTimeMillis <$> arbitrary
    pure
      MlsMigrationConfig
        { startTime = startTime,
          finaliseRegardlessAfter = finaliseRegardlessAfter
        }

instance ToSchema MlsMigrationConfig where
  schema =
    object "MlsMigration" $
      MlsMigrationConfig
        <$> startTime .= maybe_ (optField "startTime" utcTimeSchema)
        <*> finaliseRegardlessAfter .= maybe_ (optField "finaliseRegardlessAfter" utcTimeSchema)

instance Default (LockableFeature MlsMigrationConfig) where
  def = defLockedFeature

instance IsFeatureConfig MlsMigrationConfig where
  type FeatureSymbol MlsMigrationConfig = "mlsMigration"
  featureSingleton = FeatureSingletonMlsMigrationConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- EnforceFileDownloadLocationConfig

data EnforceFileDownloadLocationConfig = EnforceFileDownloadLocationConfig
  { enforcedDownloadLocation :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving (RenderableSymbol) via (RenderableTypeName EnforceFileDownloadLocationConfig)

instance Default EnforceFileDownloadLocationConfig where
  def = EnforceFileDownloadLocationConfig Nothing

instance Arbitrary EnforceFileDownloadLocationConfig where
  arbitrary = EnforceFileDownloadLocationConfig . fmap (T.pack . getPrintableString) <$> arbitrary

instance ToSchema EnforceFileDownloadLocationConfig where
  schema =
    object "EnforceFileDownloadLocation" $
      EnforceFileDownloadLocationConfig
        <$> enforcedDownloadLocation .= maybe_ (optField "enforcedDownloadLocation" schema)

instance Default (LockableFeature EnforceFileDownloadLocationConfig) where
  def = defLockedFeature

instance IsFeatureConfig EnforceFileDownloadLocationConfig where
  type FeatureSymbol EnforceFileDownloadLocationConfig = "enforceFileDownloadLocation"
  featureSingleton = FeatureSingletonEnforceFileDownloadLocationConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- Guarding the fanout of events when a team member is deleted.
--
-- FUTUREWORK: This is a transient flag that is to be removed after about 6
-- months of its introduction, namely once all clients get a chance to adapt to
-- a limited event fanout.

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data LimitedEventFanoutConfig = LimitedEventFanoutConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LimitedEventFanoutConfig)
  deriving (RenderableSymbol) via (RenderableTypeName LimitedEventFanoutConfig)

instance Default LimitedEventFanoutConfig where
  def = LimitedEventFanoutConfig

instance Default (LockableFeature LimitedEventFanoutConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig LimitedEventFanoutConfig where
  type FeatureSymbol LimitedEventFanoutConfig = "limitedEventFanout"
  featureSingleton = FeatureSingletonLimitedEventFanoutConfig
  objectSchema = pure LimitedEventFanoutConfig

instance ToSchema LimitedEventFanoutConfig where
  schema = object "LimitedEventFanoutConfig" objectSchema

----------------------------------------------------------------------
-- FeatureStatus

data FeatureStatus
  = FeatureStatusEnabled
  | FeatureStatusDisabled
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FeatureStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema FeatureStatus)

instance S.ToParamSchema FeatureStatus where
  toParamSchema _ =
    mempty
      { S._schemaType = Just S.OpenApiString,
        S._schemaEnum = Just (A.String . toQueryParam <$> [(minBound :: FeatureStatus) ..])
      }

instance FromHttpApiData FeatureStatus where
  parseUrlPiece =
    maybe (Left "must be 'enabled' or 'disabled'") Right
      . fromByteString'
      . fromStrict
      . T.encodeUtf8

instance ToHttpApiData FeatureStatus where
  toUrlPiece = T.decodeUtf8With lenientDecode . toByteString'

instance ToSchema FeatureStatus where
  schema =
    enum @Text "FeatureStatus" $
      mconcat
        [ element "enabled" FeatureStatusEnabled,
          element "disabled" FeatureStatusDisabled
        ]

instance ToByteString FeatureStatus where
  builder FeatureStatusEnabled = "enabled"
  builder FeatureStatusDisabled = "disabled"

instance FromByteString FeatureStatus where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "enabled" -> pure FeatureStatusEnabled
        Right "disabled" -> pure FeatureStatusDisabled
        Right t -> fail $ "Invalid FeatureStatus: " <> T.unpack t
        Left e -> fail $ "Invalid FeatureStatus: " <> show e

instance Cass.Cql FeatureStatus where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure FeatureStatusDisabled
    1 -> pure FeatureStatusEnabled
    _ -> Left "fromCql: Invalid FeatureStatus"
  fromCql _ = Left "fromCql: FeatureStatus: CqlInt expected"

  toCql FeatureStatusDisabled = Cass.CqlInt 0
  toCql FeatureStatusEnabled = Cass.CqlInt 1

-- | list of available features config types
type Features :: [Type]
type Features =
  [ LegalholdConfig,
    SSOConfig,
    SearchVisibilityAvailableConfig,
    SearchVisibilityInboundConfig,
    ValidateSAMLEmailsConfig,
    DigitalSignaturesConfig,
    AppLockConfig,
    FileSharingConfig,
    ClassifiedDomainsConfig,
    ConferenceCallingConfig,
    SelfDeletingMessagesConfig,
    GuestLinksConfig,
    SndFactorPasswordChallengeConfig,
    MLSConfig,
    ExposeInvitationURLsToTeamAdminConfig,
    OutlookCalIntegrationConfig,
    MlsE2EIdConfig,
    MlsMigrationConfig,
    EnforceFileDownloadLocationConfig,
    LimitedEventFanoutConfig
  ]

-- | list of available features as a record
type AllFeatures f = NP f Features

-- | 'AllFeatures' specialised to the 'LockableFeature' functor
type AllTeamFeatures = AllFeatures LockableFeature

class (Default (LockableFeature cfg)) => LockableFeatureDefault cfg

instance (Default (LockableFeature cfg)) => LockableFeatureDefault cfg

instance Default AllTeamFeatures where
  def = hcpure (Proxy @LockableFeatureDefault) def

-- | object schema for nary products
class HObjectSchema c xs where
  hobjectSchema :: (forall cfg. (c cfg) => ObjectSchema SwaggerDoc (f cfg)) -> ObjectSchema SwaggerDoc (NP f xs)

instance HObjectSchema c '[] where
  hobjectSchema _ = pure Nil

instance (HObjectSchema c xs, c x) => HObjectSchema c ((x :: Type) : xs) where
  hobjectSchema f = (:*) <$> hd .= f <*> tl .= hobjectSchema @c @xs f

-- | constraint synonym  for 'ToSchema' 'AllTeamFeatures'
class (IsFeatureConfig cfg, ToSchema cfg) => FeatureFieldConstraints cfg

instance (IsFeatureConfig cfg, ToSchema cfg) => FeatureFieldConstraints cfg

instance ToSchema AllTeamFeatures where
  schema =
    object "AllTeamFeatures" $ hobjectSchema @FeatureFieldConstraints featureField
    where
      featureField :: forall cfg. (FeatureFieldConstraints cfg) => ObjectSchema SwaggerDoc (LockableFeature cfg)
      featureField = field (T.pack (symbolVal (Proxy @(FeatureSymbol cfg)))) schema

class (Arbitrary cfg, IsFeatureConfig cfg) => ArbitraryFeatureConfig cfg

instance (Arbitrary cfg, IsFeatureConfig cfg) => ArbitraryFeatureConfig cfg

instance Arbitrary AllTeamFeatures where
  arbitrary = hsequence' $ hcpure (Proxy @ArbitraryFeatureConfig) (Comp arbitrary)

-- | FUTUREWORK: 'NpProject' and 'NpUpdate' can be useful for more than
-- features. Maybe they should be moved somewhere else.
class NpProject x xs where
  npProject' :: Proxy x -> NP f xs -> f x

instance {-# OVERLAPPING #-} NpProject x (x : xs) where
  npProject' _ (x :* _) = x

instance (NpProject x xs) => NpProject x (y : xs) where
  npProject' p (_ :* xs) = npProject' p xs

instance (TypeError ('ShowType x :<>: 'Text " not found")) => NpProject x '[] where
  npProject' = error "npProject': someone naughty removed the type error constraint"

-- | Get the first field of a given type out of an @'NP' f xs@.
npProject :: forall x f xs. (NpProject x xs) => NP f xs -> f x
npProject = npProject' (Proxy @x)

class NpUpdate x xs where
  npUpdate' :: Proxy x -> f x -> NP f xs -> NP f xs

instance {-# OVERLAPPING #-} NpUpdate x (x : xs) where
  npUpdate' _ x (_ :* xs) = x :* xs

instance (NpUpdate x xs) => NpUpdate x (y : xs) where
  npUpdate' p x (y :* xs) = y :* npUpdate' p x xs

instance (TypeError ('ShowType x :<>: 'Text " not found")) => NpUpdate x '[] where
  npUpdate' = error "npUpdate': someone naughty removed the type error constraint"

-- | Update the first field of a given type in an @'NP' f xs@.
npUpdate :: forall x f xs. (NpUpdate x xs) => f x -> NP f xs -> NP f xs
npUpdate = npUpdate' (Proxy @x)

deriving via (Schema AllTeamFeatures) instance (FromJSON AllTeamFeatures)

deriving via (Schema AllTeamFeatures) instance (ToJSON AllTeamFeatures)

deriving via (Schema AllTeamFeatures) instance (S.ToSchema AllTeamFeatures)
