{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}
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
    DbConfig (..),
    DbFeature (..),
    dbFeatureLockStatus,
    dbFeatureStatus,
    dbFeatureConfig,
    dbFeatureModConfig,
    featureToDB,
    resolveDbFeature,
    LockableFeature (..),
    defUnlockedFeature,
    defLockedFeature,
    LockableFeaturePatch (..),
    Feature (..),
    forgetLock,
    withLockStatus,
    FeatureTTL,
    FeatureTTLDays,
    FeatureTTL' (..),
    FeatureTTLUnit (..),
    EnforceAppLock (..),
    IsFeatureConfig (..),
    ParseDbFeature (..),
    FeatureSingleton (..),
    DeprecatedFeatureName,
    LockStatusResponse (..),
    One2OneCalls (..),
    -- Features
    LegalholdConfig (..),
    SSOConfig (..),
    SearchVisibilityAvailableConfig (..),
    SelfDeletingMessagesConfigB (..),
    SelfDeletingMessagesConfig,
    ValidateSAMLEmailsConfig (..),
    DigitalSignaturesConfig (..),
    ConferenceCallingConfigB (..),
    ConferenceCallingConfig,
    GuestLinksConfig (..),
    ExposeInvitationURLsToTeamAdminConfig (..),
    SndFactorPasswordChallengeConfig (..),
    SearchVisibilityInboundConfig (..),
    ClassifiedDomainsConfig (..),
    AppLockConfig,
    AppLockConfigB (..),
    FileSharingConfig (..),
    MLSConfigB (..),
    MLSConfig,
    OutlookCalIntegrationConfig (..),
    UseProxyOnMobile (..),
    MlsE2EIdConfigB (..),
    MlsE2EIdConfig,
    MlsMigrationConfigB (..),
    MlsMigrationConfig,
    EnforceFileDownloadLocationConfigB (..),
    EnforceFileDownloadLocationConfig,
    LimitedEventFanoutConfig (..),
    DomainRegistrationConfig (..),
    Features,
    AllFeatures,
    NpProject (..),
    npProject,
    NpUpdate (..),
    npUpdate,
    AllTeamFeatures,
    parseDbFeature,
    serialiseDbFeature,
    mkAllFeatures,
    TeamFeatureMigrationState (..),
  )
where

import Barbies
import Barbies.Bare
import Cassandra.CQL qualified as Cass
import Control.Applicative
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
import Data.Map qualified as M
import Data.Misc (HttpsUrl)
import Data.Monoid hiding (All, First)
import Data.OpenApi qualified as S
import Data.Proxy
import Data.SOP
import Data.Schema
import Data.Scientific (toBoundedInteger)
import Data.Semigroup hiding (All)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Encoding.Error
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time
import Deriving.Aeson
import GHC.TypeLits
import Generics.SOP qualified as GSOP
import Imports hiding (All, First)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck (getPrintableString)
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Wire.API.Conversation.Protocol
import Wire.API.MLS.CipherSuite
import Wire.API.Routes.Named hiding (unnamed)
import Wire.API.Team.Feature.Profunctor
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- FeatureTag

-- | Checklist for adding a new feature
--
-- Assume we want to add a new feature called @dummy@. Every appearance of
-- @dummy@ or @Dummy@ in the following has to be replaced with the actual name
-- of the feature being added.
--
-- 1. Create a new type in this module for the feature configuration, called
-- @DummyConfig@. If your feature doesn't have a config besides being 'status'
-- and 'lockStatus', then the config should be a unit type, e.g. @data
-- DummyConfig = DummyConfig@. Derive 'Eq', 'Show', 'Generic', 'Arbitrary',
-- 'RenderableSymbol', 'FromJSON', 'ToJSON' and 'S.ToSchema'. Implement a
-- 'ToSchema' instance. Add a singleton. Add the config type to 'Features'.
--
-- 2. Create a schema migration in galley, adding a column for each
-- configurable value of the feature. The new columns must contain all the
-- information needed to reconstruct a value of type 'LockableFeature
-- DummyConfig'.
--
-- 3. In 'Galley.Cassandra.MakeFeature', implement the 'MakeFeature' type
-- class: set 'FeatureRow' to the list of types of the rows added by the
-- migration. If the lock status is configurable (it should be in most cases),
-- it must be the first in the list. Set 'featureColumns' to the names of the
-- columns, in the same order. Implement `rowToFeature` and `featureToRow`.
--
-- 4. Implement 'GetFeatureConfig' and 'SetFeatureConfig' in
-- 'Galley.API.Teams.Features'. Empty instances will work fine unless this
-- feature requires custom logic.
--
-- 5. Add a public route to 'Wire.API.Routes.Public.Galley.Feature' and the
-- corresponding implementation in 'Galley.API.Public.Feature'.
--
-- 6. Add an internal route in 'Wire.API.Routes.Internal.Galley' and the
-- corresponding implementation in 'Galley.API.Internal'.
--
-- 7. If the feature should be configurable via Stern add routes to Stern.API.
-- Manually check that the swagger looks okay and works.
--
-- 8. In 'Galley.Types.Team', add a new data instance @DummyDefaults@ to
-- represent the server-wide feature defaults read from the configuration file.
-- In most cases, this should be a newtype over 'LockableFeature DummyConfig'.
-- Then derive all the instances like for the other features in that module.
-- Note that 'ParseFeatureDefaults' can be derived either via 'OptionalField'
-- or 'RequiredField', depending on whether the feature configuration should be
-- optional or required.
--
-- 9. If necessary, add configuration for the feature in
-- 'galley.integration.yaml', update the config map in
-- 'charts/galley/templates/configmap.yaml' and set defaults in
-- 'charts/galley/values.yaml'. Make sure that the configuration for CI matches
-- the local one, or adjust 'hack/helm_vars/wire-server/values.yaml'
-- accordingly.
--
-- 10. Add the default values of this feature in 'testAllFeatures'
-- ('Test.FeatureFlags'). Add feature-specific integration tests.
--
-- 11. Add a section to the documentation at an appropriate place
-- (e.g. 'docs/src/developer/reference/config-options.md' (if applicable) or
-- 'docs/src/understand/team-feature-settings.md')
class
  ( Default cfg,
    ToSchema cfg,
    Default (LockableFeature cfg),
    KnownSymbol (FeatureSymbol cfg),
    NpProject cfg Features,
    ParseDbFeature cfg
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
  FeatureSingletonDomainRegistrationConfig :: FeatureSingleton DomainRegistrationConfig

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

instance Default (DbFeature cfg) where
  def = mempty

dbFeatureLockStatus :: LockStatus -> DbFeature cfg
dbFeatureLockStatus s = DbFeature $ \w -> w {lockStatus = s}

dbFeatureStatus :: FeatureStatus -> DbFeature cfg
dbFeatureStatus s = DbFeature $ \w -> w {status = s}

dbFeatureConfig :: cfg -> DbFeature cfg
dbFeatureConfig c = DbFeature $ \w -> w {config = c}

dbFeatureModConfig :: (cfg -> cfg) -> DbFeature cfg
dbFeatureModConfig f = DbFeature $ \w -> w {config = f w.config}

featureToDB :: LockableFeature cfg -> DbFeature cfg
featureToDB = DbFeature . const

resolveDbFeature :: LockableFeature cfg -> DbFeature cfg -> LockableFeature cfg
resolveDbFeature defFeature dbFeature =
  let feat = applyDbFeature dbFeature defFeature
   in case feat.lockStatus of
        LockStatusLocked -> defFeature {lockStatus = LockStatusLocked}
        LockStatusUnlocked -> feat

newtype DbConfig = DbConfig {unDbConfig :: A.Value}
  deriving (Eq, Show)

instance Default DbConfig where
  def = DbConfig (A.object [])

instance Cass.Cql DbConfig where
  ctype = Cass.Tagged Cass.TextColumn

  fromCql (Cass.CqlText t) = fmap DbConfig . A.eitherDecode' . TL.encodeUtf8 . TL.fromStrict $ t
  fromCql _ = Left "service key pem: blob expected"

  toCql (DbConfig c) = Cass.CqlText . TL.toStrict . TL.decodeUtf8 . A.encode $ c

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

instance Default (LockableFeaturePatch cfg) where
  def = LockableFeaturePatch Nothing Nothing Nothing

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

--------------------------------------------------------------------------------
-- GuestLinks feature

data GuestLinksConfig = GuestLinksConfig
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform GuestLinksConfig)
  deriving (RenderableSymbol) via (RenderableTypeName GuestLinksConfig)
  deriving (ParseDbFeature, Default) via TrivialFeature GuestLinksConfig

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform LegalholdConfig)
  deriving (RenderableSymbol) via (RenderableTypeName LegalholdConfig)
  deriving (ParseDbFeature, Default) via TrivialFeature LegalholdConfig

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform SSOConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SSOConfig)
  deriving (ParseDbFeature, Default) via TrivialFeature SSOConfig

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityAvailableConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SearchVisibilityAvailableConfig)
  deriving (ParseDbFeature, Default) via TrivialFeature SearchVisibilityAvailableConfig

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform ValidateSAMLEmailsConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ValidateSAMLEmailsConfig)
  deriving (ParseDbFeature, Default) via (TrivialFeature ValidateSAMLEmailsConfig)

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform DigitalSignaturesConfig)
  deriving (RenderableSymbol) via (RenderableTypeName DigitalSignaturesConfig)
  deriving (ParseDbFeature, Default) via (TrivialFeature DigitalSignaturesConfig)

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

one2OneCallsSchema :: ValueSchema SwaggerDoc One2OneCalls
one2OneCallsSchema = one2OneCallsFromUseSftFlag <$> (== One2OneCallsSft) .= unnamed schema

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

data ConferenceCallingConfigB t f = ConferenceCallingConfig
  { one2OneCalls :: Wear t f One2OneCalls
  }
  deriving (BareB, Generic)

deriving instance FunctorB (ConferenceCallingConfigB Covered)

deriving instance ApplicativeB (ConferenceCallingConfigB Covered)

deriving instance TraversableB (ConferenceCallingConfigB Covered)

type ConferenceCallingConfig = ConferenceCallingConfigB Bare Identity

deriving instance (Eq ConferenceCallingConfig)

deriving instance (Show ConferenceCallingConfig)

deriving via (GenericUniform ConferenceCallingConfig) instance (Arbitrary ConferenceCallingConfig)

deriving via (RenderableTypeName ConferenceCallingConfig) instance (RenderableSymbol ConferenceCallingConfig)

deriving via (BarbieFeature ConferenceCallingConfigB) instance (ParseDbFeature ConferenceCallingConfig)

deriving via (BarbieFeature ConferenceCallingConfigB) instance (ToSchema ConferenceCallingConfig)

instance Default ConferenceCallingConfig where
  def = ConferenceCallingConfig def

instance Default (LockableFeature ConferenceCallingConfig) where
  def = defLockedFeature {status = FeatureStatusEnabled}

instance IsFeatureConfig ConferenceCallingConfig where
  type FeatureSymbol ConferenceCallingConfig = "conferenceCalling"
  featureSingleton = FeatureSingletonConferenceCallingConfig
  objectSchema = fromMaybe def <$> optField "config" schema

instance (OptWithDefault f) => ToSchema (ConferenceCallingConfigB Covered f) where
  schema =
    object "ConferenceCallingConfig" $
      ConferenceCallingConfig
        <$> one2OneCalls
          .= fromOpt
            (optField "useSFTForOneToOneCalls" one2OneCallsSchema)

--------------------------------------------------------------------------------
-- SndFactorPasswordChallenge feature

data SndFactorPasswordChallengeConfig = SndFactorPasswordChallengeConfig
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform SndFactorPasswordChallengeConfig)
  deriving (RenderableSymbol) via (RenderableTypeName SndFactorPasswordChallengeConfig)
  deriving (ParseDbFeature, Default) via (TrivialFeature SndFactorPasswordChallengeConfig)

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityInboundConfig)
  deriving (S.ToSchema) via Schema SearchVisibilityInboundConfig
  deriving (RenderableSymbol) via (RenderableTypeName SearchVisibilityInboundConfig)
  deriving (ParseDbFeature, Default) via (TrivialFeature SearchVisibilityInboundConfig)

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

instance ParseDbFeature ClassifiedDomainsConfig where
  parseDbConfig _ = fail "ClassifiedDomainsConfig cannot be parsed from the DB"

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

data AppLockConfigB t f = AppLockConfig
  { enforce :: Wear t f EnforceAppLock,
    timeout :: Wear t f Int32
  }
  deriving (Generic, BareB)

instance FunctorB (AppLockConfigB Covered)

instance ApplicativeB (AppLockConfigB Covered)

instance TraversableB (AppLockConfigB Covered)

type AppLockConfig = AppLockConfigB Bare Identity

deriving instance Eq AppLockConfig

deriving instance Show AppLockConfig

deriving via RenderableTypeName AppLockConfig instance RenderableSymbol AppLockConfig

deriving via (GenericUniform AppLockConfig) instance Arbitrary AppLockConfig

deriving via (BarbieFeature AppLockConfigB) instance ParseDbFeature AppLockConfig

deriving via (BarbieFeature AppLockConfigB) instance ToSchema AppLockConfig

instance Default AppLockConfig where
  def = AppLockConfig (EnforceAppLock False) 60

instance (FieldFunctor SwaggerDoc f) => ToSchema (AppLockConfigB Covered f) where
  schema =
    object "AppLockConfig" $
      AppLockConfig
        <$> (.enforce) .= extractF (fieldF "enforceAppLock" schema)
        <*> (.timeout) .= extractF (fieldF "inactivityTimeoutSecs" schema)

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform FileSharingConfig)
  deriving (RenderableSymbol) via (RenderableTypeName FileSharingConfig)
  deriving (ParseDbFeature) via (TrivialFeature FileSharingConfig)

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

data SelfDeletingMessagesConfigB t f = SelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Wear t f Int32
  }
  deriving (BareB, Generic)

instance FunctorB (SelfDeletingMessagesConfigB Covered)

instance ApplicativeB (SelfDeletingMessagesConfigB Covered)

instance TraversableB (SelfDeletingMessagesConfigB Covered)

type SelfDeletingMessagesConfig = SelfDeletingMessagesConfigB Bare Identity

deriving instance (Eq SelfDeletingMessagesConfig)

deriving instance (Show SelfDeletingMessagesConfig)

deriving via (GenericUniform SelfDeletingMessagesConfig) instance (Arbitrary SelfDeletingMessagesConfig)

deriving via (RenderableTypeName SelfDeletingMessagesConfig) instance (RenderableSymbol SelfDeletingMessagesConfig)

deriving via (BarbieFeature SelfDeletingMessagesConfigB) instance (ParseDbFeature SelfDeletingMessagesConfig)

deriving via (BarbieFeature SelfDeletingMessagesConfigB) instance (ToSchema SelfDeletingMessagesConfig)

instance Default SelfDeletingMessagesConfig where
  def = SelfDeletingMessagesConfig 0

instance (FieldFunctor SwaggerDoc f) => ToSchema (SelfDeletingMessagesConfigB Covered f) where
  schema =
    object "SelfDeletingMessagesConfig" $
      SelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= extractF (fieldF "enforcedTimeoutSeconds" schema)

instance Default (LockableFeature SelfDeletingMessagesConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig SelfDeletingMessagesConfig where
  type FeatureSymbol SelfDeletingMessagesConfig = "selfDeletingMessages"
  featureSingleton = FeatureSingletonSelfDeletingMessagesConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- MLSConfig

data MLSConfigB t f = MLSConfig
  { mlsProtocolToggleUsers :: Wear t f [UserId],
    mlsDefaultProtocol :: Wear t f ProtocolTag,
    mlsAllowedCipherSuites :: Wear t f [CipherSuiteTag],
    mlsDefaultCipherSuite :: Wear t f CipherSuiteTag,
    mlsSupportedProtocols :: Wear t f [ProtocolTag]
  }
  deriving (Generic, BareB)

deriving instance FunctorB (MLSConfigB Covered)

deriving instance ApplicativeB (MLSConfigB Covered)

deriving instance TraversableB (MLSConfigB Covered)

type MLSConfig = MLSConfigB Bare Identity

deriving instance Eq MLSConfig

deriving instance Show MLSConfig

deriving via (RenderableTypeName GuestLinksConfig) instance (RenderableSymbol MLSConfig)

deriving via (GenericUniform MLSConfig) instance (Arbitrary MLSConfig)

deriving via (BarbieFeature MLSConfigB) instance (ParseDbFeature MLSConfig)

deriving via (BarbieFeature MLSConfigB) instance (ToSchema MLSConfig)

instance Default MLSConfig where
  def =
    MLSConfig
      []
      ProtocolProteusTag
      [MLS_128_DHKEMP256_AES128GCM_SHA256_P256]
      MLS_128_DHKEMP256_AES128GCM_SHA256_P256
      [ProtocolProteusTag, ProtocolMLSTag]

instance (FieldFunctor SwaggerDoc f) => ToSchema (MLSConfigB Covered f) where
  schema =
    object "MLSConfig" $
      MLSConfig
        <$> mlsProtocolToggleUsers .= extractF (fieldWithDocModifierF "protocolToggleUsers" (S.description ?~ "allowlist of users that may change protocols") (array schema))
        <*> mlsDefaultProtocol .= extractF (fieldF "defaultProtocol" schema)
        <*> mlsAllowedCipherSuites .= extractF (fieldF "allowedCipherSuites" (array schema))
        <*> mlsDefaultCipherSuite .= extractF (fieldF "defaultCipherSuite" schema)
        <*> mlsSupportedProtocols .= extractF (fieldF "supportedProtocols" (array schema))

instance Default (LockableFeature MLSConfig) where
  def = defUnlockedFeature {status = FeatureStatusDisabled}

instance IsFeatureConfig MLSConfig where
  type FeatureSymbol MLSConfig = "mls"
  featureSingleton = FeatureSingletonMLSConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- ExposeInvitationURLsToTeamAdminConfig

data ExposeInvitationURLsToTeamAdminConfig = ExposeInvitationURLsToTeamAdminConfig
  deriving (Show, Eq, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform ExposeInvitationURLsToTeamAdminConfig)
  deriving (RenderableSymbol) via (RenderableTypeName ExposeInvitationURLsToTeamAdminConfig)
  deriving (Default, ParseDbFeature) via (TrivialFeature ExposeInvitationURLsToTeamAdminConfig)

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform OutlookCalIntegrationConfig)
  deriving (RenderableSymbol) via (RenderableTypeName OutlookCalIntegrationConfig)
  deriving (Default, ParseDbFeature) via (TrivialFeature OutlookCalIntegrationConfig)

instance Default (LockableFeature OutlookCalIntegrationConfig) where
  def = defLockedFeature

instance IsFeatureConfig OutlookCalIntegrationConfig where
  type FeatureSymbol OutlookCalIntegrationConfig = "outlookCalIntegration"
  featureSingleton = FeatureSingletonOutlookCalIntegrationConfig
  objectSchema = pure OutlookCalIntegrationConfig

instance ToSchema OutlookCalIntegrationConfig where
  schema = object "OutlookCalIntegrationConfig" objectSchema

----------------------------------------------------------------------
-- MlsE2EIdConfig

newtype UseProxyOnMobile = UseProxyOnMobile {unUseProxyOnMobile :: Bool}
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform UseProxyOnMobile)
  deriving (Semigroup) via First Bool
  deriving (ToSchema) via Bool

-- | This instance is necessary to derive ApplicativeB below, but it isn't
-- actually used.
instance Monoid UseProxyOnMobile where
  mempty = error "Internal error: UseProxyOnMobile is not a monoid"

instance Default UseProxyOnMobile where
  def = UseProxyOnMobile False

data MlsE2EIdConfigB t f = MlsE2EIdConfig
  { verificationExpiration :: Wear t f NominalDiffTime,
    acmeDiscoveryUrl :: Alt Maybe HttpsUrl,
    crlProxy :: Alt Maybe HttpsUrl,
    useProxyOnMobile :: UseProxyOnMobile
  }
  deriving (BareB, Generic)

deriving instance FunctorB (MlsE2EIdConfigB Covered)

deriving instance ApplicativeB (MlsE2EIdConfigB Covered)

deriving instance TraversableB (MlsE2EIdConfigB Covered)

type MlsE2EIdConfig = MlsE2EIdConfigB Bare Identity

deriving via (RenderableTypeName MlsE2EIdConfig) instance (RenderableSymbol MlsE2EIdConfig)

deriving via (BarbieFeature MlsE2EIdConfigB) instance ParseDbFeature MlsE2EIdConfig

deriving via (BarbieFeature MlsE2EIdConfigB) instance ToSchema MlsE2EIdConfig

deriving instance Eq MlsE2EIdConfig

deriving instance Show MlsE2EIdConfig

instance Default MlsE2EIdConfig where
  def = MlsE2EIdConfig (fromIntegral @Int (60 * 60 * 24)) empty empty def

instance Arbitrary MlsE2EIdConfig where
  arbitrary =
    MlsE2EIdConfig
      <$> (fromIntegral <$> (arbitrary @Word32))
      <*> arbitrary
      <*> fmap (Alt . pure) arbitrary
      <*> arbitrary

instance (FieldFunctor SwaggerDoc f) => ToSchema (MlsE2EIdConfigB Covered f) where
  schema =
    object "MlsE2EIdConfig" $
      MlsE2EIdConfig
        <$> (fmap toSeconds . verificationExpiration)
          .= extractF (fieldWithDocModifierF "verificationExpiration" veDesc (fromSeconds <$> schema))
        <*> (getAlt . acmeDiscoveryUrl)
          .= fmap Alt (maybe_ (optField "acmeDiscoveryUrl" schema))
        <*> (getAlt . crlProxy) .= fmap Alt (maybe_ (optField "crlProxy" schema))
        <*> useProxyOnMobile .= fmap (fromMaybe def) (optField "useProxyOnMobile" schema)
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

data MlsMigrationConfigB t f = MlsMigrationConfig
  { startTime :: Wear t f (Maybe UTCTime),
    finaliseRegardlessAfter :: Wear t f (Maybe UTCTime)
  }
  deriving (BareB, Generic)

deriving instance FunctorB (MlsMigrationConfigB Covered)

deriving instance ApplicativeB (MlsMigrationConfigB Covered)

deriving instance TraversableB (MlsMigrationConfigB Covered)

type MlsMigrationConfig = MlsMigrationConfigB Bare Identity

deriving instance Eq MlsMigrationConfig

deriving instance Show MlsMigrationConfig

deriving via (BarbieFeature MlsMigrationConfigB) instance (ParseDbFeature MlsMigrationConfig)

deriving via (BarbieFeature MlsMigrationConfigB) instance (ToSchema MlsMigrationConfig)

deriving via (RenderableTypeName MlsMigrationConfig) instance (RenderableSymbol MlsMigrationConfig)

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

instance (NestedMaybe f) => ToSchema (MlsMigrationConfigB Covered f) where
  schema =
    object "MlsMigration" $
      MlsMigrationConfig
        <$> startTime .= nestedMaybeField "startTime" (unnamed utcTimeSchema)
        <*> finaliseRegardlessAfter .= nestedMaybeField "finaliseRegardlessAfter" (unnamed utcTimeSchema)

instance Default (LockableFeature MlsMigrationConfig) where
  def = defLockedFeature

instance IsFeatureConfig MlsMigrationConfig where
  type FeatureSymbol MlsMigrationConfig = "mlsMigration"
  featureSingleton = FeatureSingletonMlsMigrationConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- EnforceFileDownloadLocationConfig

data EnforceFileDownloadLocationConfigB t f = EnforceFileDownloadLocationConfig
  { enforcedDownloadLocation :: Wear t f (Maybe Text)
  }
  deriving (BareB, Generic)

deriving instance FunctorB (EnforceFileDownloadLocationConfigB Covered)

deriving instance ApplicativeB (EnforceFileDownloadLocationConfigB Covered)

deriving instance TraversableB (EnforceFileDownloadLocationConfigB Covered)

type EnforceFileDownloadLocationConfig = EnforceFileDownloadLocationConfigB Bare Identity

deriving instance (Eq EnforceFileDownloadLocationConfig)

deriving instance (Show EnforceFileDownloadLocationConfig)

deriving via (RenderableTypeName EnforceFileDownloadLocationConfig) instance (RenderableSymbol EnforceFileDownloadLocationConfig)

deriving via (BarbieFeature EnforceFileDownloadLocationConfigB) instance (ToSchema EnforceFileDownloadLocationConfig)

deriving via (BarbieFeature EnforceFileDownloadLocationConfigB) instance (ParseDbFeature EnforceFileDownloadLocationConfig)

instance Default EnforceFileDownloadLocationConfig where
  def = EnforceFileDownloadLocationConfig Nothing

instance Arbitrary EnforceFileDownloadLocationConfig where
  arbitrary = EnforceFileDownloadLocationConfig . fmap (T.pack . getPrintableString) <$> arbitrary

instance (NestedMaybe f) => ToSchema (EnforceFileDownloadLocationConfigB Covered f) where
  schema =
    object "EnforceFileDownloadLocation" $
      EnforceFileDownloadLocationConfig
        <$> enforcedDownloadLocation .= nestedMaybeField "enforcedDownloadLocation" (unnamed schema)

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
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform LimitedEventFanoutConfig)
  deriving (RenderableSymbol) via (RenderableTypeName LimitedEventFanoutConfig)
  deriving (Default, ParseDbFeature) via (TrivialFeature LimitedEventFanoutConfig)

instance Default (LockableFeature LimitedEventFanoutConfig) where
  def = defUnlockedFeature

instance IsFeatureConfig LimitedEventFanoutConfig where
  type FeatureSymbol LimitedEventFanoutConfig = "limitedEventFanout"
  featureSingleton = FeatureSingletonLimitedEventFanoutConfig
  objectSchema = pure LimitedEventFanoutConfig

instance ToSchema LimitedEventFanoutConfig where
  schema = object "LimitedEventFanoutConfig" objectSchema

--------------------------------------------------------------------------------
-- DomainRegistration feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data DomainRegistrationConfig = DomainRegistrationConfig
  deriving (Eq, Show, Generic, GSOP.Generic)
  deriving (Arbitrary) via (GenericUniform DomainRegistrationConfig)
  deriving (RenderableSymbol) via (RenderableTypeName DomainRegistrationConfig)
  deriving (Default, ParseDbFeature) via (TrivialFeature DomainRegistrationConfig)

instance ToSchema DomainRegistrationConfig where
  schema = object "DomainRegistrationConfig" objectSchema

instance Default (LockableFeature DomainRegistrationConfig) where
  def = defLockedFeature

instance IsFeatureConfig DomainRegistrationConfig where
  type FeatureSymbol DomainRegistrationConfig = "domainRegistration"
  featureSingleton = FeatureSingletonDomainRegistrationConfig
  objectSchema = pure DomainRegistrationConfig

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
    LimitedEventFanoutConfig,
    DomainRegistrationConfig
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

--------------------------------------------------------------------------------
-- DB feature parsing

-- [Note: default values for configuration fields]
--
-- When reading values for configuration types with multiple fields, we fall
-- back to default values for each field independently, instead of treating the
-- whole configuration as a single value that can be set or not.
--
-- In most cases, either strategy would produce the same result, because there
-- is no way to set only *some* fields using the public API. However, that can
-- happen when a feature flag changes over time and gains new fields, as it has
-- been the case for mlsE2EId.
--
-- Therefore, we use the first strategy consistently for all feature flags,
-- even when it does not matter.

class ParseDbFeature cfg where
  parseDbConfig :: DbConfig -> A.Parser (cfg -> cfg)

newtype TrivialFeature cfg = TrivialFeature cfg

instance (GSOP.IsProductType cfg '[]) => ParseDbFeature (TrivialFeature cfg) where
  parseDbConfig _ = pure id

instance (GSOP.IsProductType cfg '[]) => Default (TrivialFeature cfg) where
  def = TrivialFeature (GSOP.productTypeTo Nil)

newtype BarbieFeature b = BarbieFeature {unBarbieFeature :: b Bare Identity}

instance
  ( BareB b,
    ApplicativeB (b Covered),
    ToSchema (b Covered Maybe)
  ) =>
  ParseDbFeature (BarbieFeature b)
  where
  parseDbConfig (DbConfig v) = do
    cfg <- schemaParseJSON v
    pure $ \(BarbieFeature defCfg) -> BarbieFeature (applyConfig defCfg cfg)
    where
      f :: Maybe a -> Identity a -> Identity a
      f m (Identity x) = Identity $ fromMaybe x m

      applyConfig :: b Bare Identity -> b Covered Maybe -> b Bare Identity
      applyConfig cfg1 cfg2 = bstrip $ bzipWith f cfg2 (bcover cfg1)

instance (BareB b, ToSchema (b Covered Identity)) => ToSchema (BarbieFeature b) where
  schema = (bcover . unBarbieFeature) .= fmap (BarbieFeature . bstrip) schema

parseDbFeature ::
  forall cfg.
  (ParseDbFeature cfg) =>
  LockableFeaturePatch DbConfig ->
  A.Parser (DbFeature cfg)
parseDbFeature feat =
  do
    f <- maybe (pure id) parseDbConfig feat.config
    pure $
      foldMap dbFeatureStatus feat.status
        <> foldMap dbFeatureLockStatus feat.lockStatus
        <> dbFeatureModConfig f

serialiseDbFeature :: (IsFeatureConfig cfg) => LockableFeature cfg -> LockableFeaturePatch DbConfig
serialiseDbFeature feat =
  LockableFeaturePatch
    { status = Just feat.status,
      lockStatus = Just feat.lockStatus,
      config = Just . DbConfig . schemaToJSON $ feat.config
    }

-- | Convert a map indexed by feature name to an NP value.
mkAllFeatures ::
  forall cfgs.
  ( All IsFeatureConfig cfgs,
    All ParseDbFeature cfgs
  ) =>
  Map Text (LockableFeaturePatch DbConfig) ->
  A.Parser (NP DbFeature cfgs)
mkAllFeatures m =
  hctraverse' (Proxy @ParseDbFeature) (parseDbFeature . unK) $
    hcpure (Proxy @IsFeatureConfig) get
  where
    get :: forall cfg. (IsFeatureConfig cfg) => K (LockableFeaturePatch DbConfig) cfg
    get = K $ M.findWithDefault def (featureName @cfg) m

--------------------------------------------------------------------------------
-- Team Feature Migration

data TeamFeatureMigrationState = MigrationNotStarted | MigrationInProgress | MigrationCompleted
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamFeatureMigrationState)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamFeatureMigrationState)

instance Default TeamFeatureMigrationState where
  def = MigrationNotStarted

instance ToSchema TeamFeatureMigrationState where
  schema =
    enum @Text "TeamFeatureMigrationState" $
      mconcat
        [ element "not_started" MigrationNotStarted,
          element "in_progress" MigrationInProgress,
          element "completed" MigrationCompleted
        ]

instance Cass.Cql TeamFeatureMigrationState where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure MigrationNotStarted
    1 -> pure MigrationInProgress
    2 -> pure MigrationCompleted
    _ -> Left "fromCql: Invalid TeamFeatureMigrationState value"
  fromCql _ = Left "fromCql: TeamFeatureMigrationState: CqlInt or CqlNull expected"

  toCql MigrationNotStarted = Cass.CqlInt 0
  toCql MigrationInProgress = Cass.CqlInt 1
  toCql MigrationCompleted = Cass.CqlInt 2
