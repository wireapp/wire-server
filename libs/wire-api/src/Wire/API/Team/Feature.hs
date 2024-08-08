{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
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

module Wire.API.Team.Feature
  ( FeatureStatus (..),
    featureName,
    featureNameBS,
    LockStatus (..),
    WithStatusBase (..),
    DbFeature (..),
    DbFeatureWithLock (..),
    dbFeatureStatus,
    dbFeatureTTL,
    dbFeatureConfig,
    dbFeatureModConfig,
    WithStatus,
    withStatus,
    withStatus',
    wsStatus,
    wsLockStatus,
    wsConfig,
    wsTTL,
    setStatus,
    setLockStatus,
    setConfig,
    setConfig',
    setTTL,
    setWsTTL,
    WithStatusPatch,
    wsPatch,
    wspStatus,
    wspLockStatus,
    wspConfig,
    wspTTL,
    WithStatusNoLock (..),
    forgetLock,
    withLockStatus,
    withUnlocked,
    FeatureTTL,
    FeatureTTLDays,
    FeatureTTL' (..),
    FeatureTTLUnit (..),
    convertFeatureTTLDaysToSeconds,
    convertFeatureTTLSecondsToDays,
    EnforceAppLock (..),
    defFeatureStatusNoLock,
    genericComputeFeature,
    computeFeatureConfigForTeamUser,
    IsFeatureConfig (..),
    FeatureSingleton (..),
    HasDeprecatedFeatureName (..),
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
    AllFeatures (..),
    AllFeatureConfigs,
    unImplicitLockStatus,
    ImplicitLockStatus (..),
  )
where

import Cassandra.CQL qualified as Cass
import Control.Lens (makeLenses, (?~))
import Data.Aeson qualified as A
import Data.Aeson.Types qualified as A
import Data.Attoparsec.ByteString qualified as Parser
import Data.ByteString.Conversion
import Data.ByteString.UTF8 qualified as UTF8
import Data.Default
import Data.Domain (Domain)
import Data.Either.Extra (maybeToEither)
import Data.Id
import Data.Json.Util
import Data.Kind
import Data.Misc (HttpsUrl)
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Scientific (toBoundedInteger)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
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
import Wire.API.Routes.Named (RenderableSymbol (renderSymbol))
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
-- 2. Add the config to 'AllFeatureConfigs'.
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
class IsFeatureConfig cfg where
  type FeatureSymbol cfg :: Symbol
  defFeatureStatus :: WithStatus cfg
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
  FeatureSingletonMlsMigration ::
    -- FUTUREWORK: rename to `FeatureSingletonMlsMigrationConfig` (or drop the `Config` from
    -- all other constructors)
    FeatureSingleton MlsMigrationConfig
  FeatureSingletonEnforceFileDownloadLocationConfig :: FeatureSingleton EnforceFileDownloadLocationConfig
  FeatureSingletonLimitedEventFanoutConfig :: FeatureSingleton LimitedEventFanoutConfig

class HasDeprecatedFeatureName cfg where
  type DeprecatedFeatureName cfg :: Symbol

featureName :: forall cfg. KnownSymbol (FeatureSymbol cfg) => Text
featureName = T.pack $ symbolVal (Proxy @(FeatureSymbol cfg))

featureNameBS :: forall cfg. KnownSymbol (FeatureSymbol cfg) => ByteString
featureNameBS = UTF8.fromString $ symbolVal (Proxy @(FeatureSymbol cfg))

----------------------------------------------------------------------
-- WithStatusBase

data WithStatusBase (m :: Type -> Type) (cfg :: Type) = WithStatusBase
  { wsbStatus :: m FeatureStatus,
    wsbLockStatus :: m LockStatus,
    wsbConfig :: m cfg,
    wsbTTL :: m FeatureTTL
  }
  deriving stock (Generic, Typeable, Functor)

--------------------------------------------------------------------------------
-- DbFeature

-- | Feature data stored in the database, as a function of its default values.
newtype DbFeature cfg = DbFeature
  {unDbFeature :: WithStatusNoLock cfg -> WithStatusNoLock cfg}

instance Semigroup (DbFeature cfg) where
  DbFeature f <> DbFeature g = DbFeature (f . g)

instance Monoid (DbFeature cfg) where
  mempty = DbFeature id

dbFeatureStatus :: FeatureStatus -> DbFeature cfg
dbFeatureStatus s = DbFeature $ \w -> w {wssStatus = s}

dbFeatureTTL :: FeatureTTL -> DbFeature cfg
dbFeatureTTL ttl = DbFeature $ \w -> w {wssTTL = ttl}

dbFeatureConfig :: cfg -> DbFeature cfg
dbFeatureConfig c = DbFeature $ \w -> w {wssConfig = c}

dbFeatureModConfig :: (cfg -> cfg) -> DbFeature cfg
dbFeatureModConfig f = DbFeature $ \w -> w {wssConfig = f (wssConfig w)}

data DbFeatureWithLock cfg = DbFeatureWithLock
  { lockStatus :: Maybe LockStatus,
    feature :: DbFeature cfg
  }

----------------------------------------------------------------------
-- WithStatus

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

-- FUTUREWORK: use lenses, maybe?
wsStatus :: WithStatus cfg -> FeatureStatus
wsStatus = runIdentity . wsbStatus

wsLockStatus :: WithStatus cfg -> LockStatus
wsLockStatus = runIdentity . wsbLockStatus

wsConfig :: WithStatus cfg -> cfg
wsConfig = runIdentity . wsbConfig

wsTTL :: WithStatus cfg -> FeatureTTL
wsTTL = runIdentity . wsbTTL

withStatus :: FeatureStatus -> LockStatus -> cfg -> FeatureTTL -> WithStatus cfg
withStatus s ls c ttl = WithStatusBase (Identity s) (Identity ls) (Identity c) (Identity ttl)

setStatus :: FeatureStatus -> WithStatus cfg -> WithStatus cfg
setStatus s (WithStatusBase _ ls c ttl) = WithStatusBase (Identity s) ls c ttl

setLockStatus :: LockStatus -> WithStatus cfg -> WithStatus cfg
setLockStatus ls (WithStatusBase s _ c ttl) = WithStatusBase s (Identity ls) c ttl

setConfig :: cfg -> WithStatus cfg -> WithStatus cfg
setConfig = setConfig'

setConfig' :: forall (m :: Type -> Type) (cfg :: Type). Applicative m => cfg -> WithStatusBase m cfg -> WithStatusBase m cfg
setConfig' c (WithStatusBase s ls _ ttl) = WithStatusBase s ls (pure c) ttl

setTTL :: forall (m :: Type -> Type) (cfg :: Type). Applicative m => FeatureTTL -> WithStatusBase m cfg -> WithStatusBase m cfg
setTTL ttl (WithStatusBase s ls c _) = WithStatusBase s ls c (pure ttl)

setWsTTL :: FeatureTTL -> WithStatus cfg -> WithStatus cfg
setWsTTL = setTTL

type WithStatus = WithStatusBase Identity

deriving instance (Eq cfg) => Eq (WithStatus cfg)

deriving instance (Show cfg) => Show (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg)) => ToJSON (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg)) => FromJSON (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg), Typeable cfg) => S.ToSchema (WithStatus cfg)

instance (ToSchema cfg, IsFeatureConfig cfg) => ToSchema (WithStatus cfg) where
  schema =
    object name $
      WithStatusBase
        <$> (runIdentity . wsbStatus) .= (Identity <$> field "status" schema)
        <*> (runIdentity . wsbLockStatus) .= (Identity <$> field "lockStatus" schema)
        <*> (runIdentity . wsbConfig) .= (Identity <$> objectSchema @cfg)
        <*> (runIdentity . wsbTTL) .= (Identity . fromMaybe FeatureTTLUnlimited <$> optField "ttl" schema)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".WithStatus"

instance (Arbitrary cfg, IsFeatureConfig cfg) => Arbitrary (WithStatus cfg) where
  arbitrary = WithStatusBase <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- WithStatusPatch

type WithStatusPatch (cfg :: Type) = WithStatusBase Maybe cfg

deriving instance (Eq cfg) => Eq (WithStatusPatch cfg)

deriving instance (Show cfg) => Show (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg)) => ToJSON (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg)) => FromJSON (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg), Typeable cfg) => S.ToSchema (WithStatusPatch cfg)

wsPatch :: Maybe FeatureStatus -> Maybe LockStatus -> Maybe cfg -> Maybe FeatureTTL -> WithStatusPatch cfg
wsPatch = WithStatusBase

wspStatus :: WithStatusPatch cfg -> Maybe FeatureStatus
wspStatus = wsbStatus

wspLockStatus :: WithStatusPatch cfg -> Maybe LockStatus
wspLockStatus = wsbLockStatus

wspConfig :: WithStatusPatch cfg -> Maybe cfg
wspConfig = wsbConfig

wspTTL :: WithStatusPatch cfg -> Maybe FeatureTTL
wspTTL = wsbTTL

withStatus' :: Maybe FeatureStatus -> Maybe LockStatus -> Maybe cfg -> Maybe FeatureTTL -> WithStatusPatch cfg
withStatus' = WithStatusBase

-- | The ToJSON implementation of `WithStatusPatch` will encode the trivial config as `"config": {}`
-- when the value is a `Just`, if it's `Nothing` it will be omitted, which is the important part.
instance ToSchema cfg => ToSchema (WithStatusPatch cfg) where
  schema =
    object name $
      WithStatusBase
        <$> wsbStatus .= maybe_ (optField "status" schema)
        <*> wsbLockStatus .= maybe_ (optField "lockStatus" schema)
        <*> wsbConfig .= maybe_ (optField "config" schema)
        <*> wsbTTL .= maybe_ (optField "ttl" schema)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".WithStatusPatch"

instance (Arbitrary cfg, IsFeatureConfig cfg) => Arbitrary (WithStatusPatch cfg) where
  arbitrary = WithStatusBase <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

----------------------------------------------------------------------
-- WithStatusNoLock

data WithStatusNoLock (cfg :: Type) = WithStatusNoLock
  { wssStatus :: FeatureStatus,
    wssConfig :: cfg,
    wssTTL :: FeatureTTL
  }
  deriving stock (Eq, Show, Generic, Typeable, Functor)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (WithStatusNoLock cfg))

instance Arbitrary cfg => Arbitrary (WithStatusNoLock cfg) where
  arbitrary = WithStatusNoLock <$> arbitrary <*> arbitrary <*> arbitrary

forgetLock :: WithStatus a -> WithStatusNoLock a
forgetLock ws = WithStatusNoLock (wsStatus ws) (wsConfig ws) (wsTTL ws)

withLockStatus :: LockStatus -> WithStatusNoLock a -> WithStatus a
withLockStatus ls (WithStatusNoLock s c ttl) = withStatus s ls c ttl

withUnlocked :: WithStatusNoLock a -> WithStatus a
withUnlocked = withLockStatus LockStatusUnlocked

withLocked :: WithStatusNoLock a -> WithStatus a
withLocked = withLockStatus LockStatusLocked

instance (ToSchema cfg, IsFeatureConfig cfg) => ToSchema (WithStatusNoLock cfg) where
  schema =
    object name $
      WithStatusNoLock
        <$> wssStatus .= field "status" schema
        <*> wssConfig .= objectSchema @cfg
        <*> wssTTL .= (fromMaybe FeatureTTLUnlimited <$> optField "ttl" schema)
    where
      inner = schema @cfg
      name = fromMaybe "" (getName (schemaDoc inner)) <> ".WithStatusNoLock"

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

convertFeatureTTLSecondsToDays :: FeatureTTL -> FeatureTTLDays
convertFeatureTTLSecondsToDays FeatureTTLUnlimited = FeatureTTLUnlimited
convertFeatureTTLSecondsToDays (FeatureTTLSeconds d) = FeatureTTLSeconds (d `div` (60 * 60 * 24))

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
  parseUrlPiece = maybeToEither "Invalid lock status" . fromByteString . cs

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

newtype ImplicitLockStatus (cfg :: Type) = ImplicitLockStatus {_unImplicitLockStatus :: WithStatus cfg}
  deriving newtype (Eq, Show, Arbitrary)

instance (IsFeatureConfig a, ToSchema a) => ToJSON (ImplicitLockStatus a) where
  toJSON (ImplicitLockStatus a) = A.toJSON $ forgetLock a

instance (IsFeatureConfig a, ToSchema a) => FromJSON (ImplicitLockStatus a) where
  parseJSON v = ImplicitLockStatus . withLockStatus (wsLockStatus $ defFeatureStatus @a) <$> A.parseJSON v

-- | Convert a feature coming from the database to its public form. This can be
-- overridden on a feature basis by implementing the `computeFeature` method of
-- the `GetFeatureConfig` class.
genericComputeFeature ::
  WithStatus cfg ->
  Maybe LockStatus ->
  DbFeature cfg ->
  WithStatus cfg
genericComputeFeature defFeature lockStatus dbFeature =
  case fromMaybe (wsLockStatus defFeature) lockStatus of
    LockStatusLocked -> setLockStatus LockStatusLocked defFeature
    LockStatusUnlocked -> withUnlocked $ unDbFeature dbFeature (forgetLock defFeature)

-- | This contains the pure business logic for users from teams
computeFeatureConfigForTeamUser :: Maybe (WithStatusNoLock cfg) -> Maybe LockStatus -> WithStatus cfg -> WithStatus cfg
computeFeatureConfigForTeamUser mStatusDb mLockStatusDb defStatus =
  case lockStatus of
    LockStatusLocked ->
      withLocked (forgetLock defStatus)
    LockStatusUnlocked ->
      withUnlocked $ case mStatusDb of
        Nothing -> forgetLock defStatus
        Just fs -> fs
  where
    lockStatus = fromMaybe (wsLockStatus defStatus) mLockStatusDb

--------------------------------------------------------------------------------
-- GuestLinks feature

data GuestLinksConfig = GuestLinksConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GuestLinksConfig)

instance RenderableSymbol GuestLinksConfig where
  renderSymbol = "GuestLinksConfig"

instance ToSchema GuestLinksConfig where
  schema = object "GuestLinksConfig" objectSchema

instance IsFeatureConfig GuestLinksConfig where
  type FeatureSymbol GuestLinksConfig = "conversationGuestLinks"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig FeatureTTLUnlimited
  featureSingleton = FeatureSingletonGuestLinksConfig

  objectSchema = pure GuestLinksConfig

--------------------------------------------------------------------------------
-- Legalhold feature

data LegalholdConfig = LegalholdConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalholdConfig)

instance RenderableSymbol LegalholdConfig where
  renderSymbol = "LegalholdConfig"

instance IsFeatureConfig LegalholdConfig where
  type FeatureSymbol LegalholdConfig = "legalhold"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked LegalholdConfig FeatureTTLUnlimited
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

instance RenderableSymbol SSOConfig where
  renderSymbol = "SSOConfig"

instance IsFeatureConfig SSOConfig where
  type FeatureSymbol SSOConfig = "sso"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SSOConfig FeatureTTLUnlimited
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

instance RenderableSymbol SearchVisibilityAvailableConfig where
  renderSymbol = "SearchVisibilityAvailableConfig"

instance IsFeatureConfig SearchVisibilityAvailableConfig where
  type FeatureSymbol SearchVisibilityAvailableConfig = "searchVisibility"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityAvailableConfig FeatureTTLUnlimited
  featureSingleton = FeatureSingletonSearchVisibilityAvailableConfig
  objectSchema = pure SearchVisibilityAvailableConfig

instance ToSchema SearchVisibilityAvailableConfig where
  schema = object "SearchVisibilityAvailableConfig" objectSchema

instance HasDeprecatedFeatureName SearchVisibilityAvailableConfig where
  type DeprecatedFeatureName SearchVisibilityAvailableConfig = "search-visibility"

--------------------------------------------------------------------------------
-- ValidateSAMLEmails feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data ValidateSAMLEmailsConfig = ValidateSAMLEmailsConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ValidateSAMLEmailsConfig)

instance RenderableSymbol ValidateSAMLEmailsConfig where
  renderSymbol = "ValidateSAMLEmailsConfig"

instance ToSchema ValidateSAMLEmailsConfig where
  schema = object "ValidateSAMLEmailsConfig" objectSchema

instance IsFeatureConfig ValidateSAMLEmailsConfig where
  type FeatureSymbol ValidateSAMLEmailsConfig = "validateSAMLemails"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked ValidateSAMLEmailsConfig FeatureTTLUnlimited
  featureSingleton = FeatureSingletonValidateSAMLEmailsConfig
  objectSchema = pure ValidateSAMLEmailsConfig

instance HasDeprecatedFeatureName ValidateSAMLEmailsConfig where
  type DeprecatedFeatureName ValidateSAMLEmailsConfig = "validate-saml-emails"

--------------------------------------------------------------------------------
-- DigitalSignatures feature

-- | This feature does not have a PUT endpoint. See [Note: unsettable features].
data DigitalSignaturesConfig = DigitalSignaturesConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DigitalSignaturesConfig)

instance RenderableSymbol DigitalSignaturesConfig where
  renderSymbol = "DigitalSignaturesConfig"

instance IsFeatureConfig DigitalSignaturesConfig where
  type FeatureSymbol DigitalSignaturesConfig = "digitalSignatures"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked DigitalSignaturesConfig FeatureTTLUnlimited
  featureSingleton = FeatureSingletonDigitalSignaturesConfig
  objectSchema = pure DigitalSignaturesConfig

instance HasDeprecatedFeatureName DigitalSignaturesConfig where
  type DeprecatedFeatureName DigitalSignaturesConfig = "digital-signatures"

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

instance Default ConferenceCallingConfig where
  def = ConferenceCallingConfig {one2OneCalls = def}

instance RenderableSymbol ConferenceCallingConfig where
  renderSymbol = "ConferenceCallingConfig"

instance IsFeatureConfig ConferenceCallingConfig where
  type FeatureSymbol ConferenceCallingConfig = "conferenceCalling"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusLocked def FeatureTTLUnlimited
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

instance RenderableSymbol SndFactorPasswordChallengeConfig where
  renderSymbol = "SndFactorPasswordChallengeConfig"

instance ToSchema SndFactorPasswordChallengeConfig where
  schema = object "SndFactorPasswordChallengeConfig" objectSchema

instance IsFeatureConfig SndFactorPasswordChallengeConfig where
  type FeatureSymbol SndFactorPasswordChallengeConfig = "sndFactorPasswordChallenge"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked SndFactorPasswordChallengeConfig FeatureTTLUnlimited
  featureSingleton = FeatureSingletonSndFactorPasswordChallengeConfig
  objectSchema = pure SndFactorPasswordChallengeConfig

--------------------------------------------------------------------------------
-- SearchVisibilityInbound feature

data SearchVisibilityInboundConfig = SearchVisibilityInboundConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityInboundConfig)
  deriving (S.ToSchema) via Schema SearchVisibilityInboundConfig

instance RenderableSymbol SearchVisibilityInboundConfig where
  renderSymbol = "SearchVisibilityInboundConfig"

instance IsFeatureConfig SearchVisibilityInboundConfig where
  type FeatureSymbol SearchVisibilityInboundConfig = "searchVisibilityInbound"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityInboundConfig FeatureTTLUnlimited
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

instance RenderableSymbol ClassifiedDomainsConfig where
  renderSymbol = "ClassifiedDomainsConfig"

deriving via (GenericUniform ClassifiedDomainsConfig) instance Arbitrary ClassifiedDomainsConfig

instance ToSchema ClassifiedDomainsConfig where
  schema =
    object "ClassifiedDomainsConfig" $
      ClassifiedDomainsConfig
        <$> classifiedDomainsDomains .= field "domains" (array schema)

instance IsFeatureConfig ClassifiedDomainsConfig where
  type FeatureSymbol ClassifiedDomainsConfig = "classifiedDomains"

  defFeatureStatus =
    withStatus
      FeatureStatusDisabled
      LockStatusUnlocked
      (ClassifiedDomainsConfig [])
      FeatureTTLUnlimited
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

instance RenderableSymbol AppLockConfig where
  renderSymbol = "AppLockConfig"

instance ToSchema AppLockConfig where
  schema =
    object "AppLockConfig" $
      AppLockConfig
        <$> applockEnforceAppLock .= field "enforceAppLock" schema
        <*> applockInactivityTimeoutSecs .= field "inactivityTimeoutSecs" schema

instance IsFeatureConfig AppLockConfig where
  type FeatureSymbol AppLockConfig = "appLock"

  defFeatureStatus =
    withStatus
      FeatureStatusEnabled
      LockStatusUnlocked
      (AppLockConfig (EnforceAppLock False) 60)
      FeatureTTLUnlimited
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

instance RenderableSymbol FileSharingConfig where
  renderSymbol = "FileSharingConfig"

instance IsFeatureConfig FileSharingConfig where
  type FeatureSymbol FileSharingConfig = "fileSharing"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked FileSharingConfig FeatureTTLUnlimited
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

instance RenderableSymbol SelfDeletingMessagesConfig where
  renderSymbol = "SelfDeletingMessagesConfig"

instance ToSchema SelfDeletingMessagesConfig where
  schema =
    object "SelfDeletingMessagesConfig" $
      SelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= field "enforcedTimeoutSeconds" schema

instance IsFeatureConfig SelfDeletingMessagesConfig where
  type FeatureSymbol SelfDeletingMessagesConfig = "selfDeletingMessages"
  defFeatureStatus =
    withStatus
      FeatureStatusEnabled
      LockStatusUnlocked
      (SelfDeletingMessagesConfig 0)
      FeatureTTLUnlimited
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

instance RenderableSymbol MLSConfig where
  renderSymbol = "MLSConfig"

instance ToSchema MLSConfig where
  schema =
    object "MLSConfig" $
      MLSConfig
        <$> mlsProtocolToggleUsers .= fieldWithDocModifier "protocolToggleUsers" (S.description ?~ "allowlist of users that may change protocols") (array schema)
        <*> mlsDefaultProtocol .= field "defaultProtocol" schema
        <*> mlsAllowedCipherSuites .= field "allowedCipherSuites" (array schema)
        <*> mlsDefaultCipherSuite .= field "defaultCipherSuite" schema
        <*> mlsSupportedProtocols .= field "supportedProtocols" (array schema)

instance IsFeatureConfig MLSConfig where
  type FeatureSymbol MLSConfig = "mls"
  defFeatureStatus =
    let config =
          MLSConfig
            []
            ProtocolProteusTag
            [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519]
            MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
            [ProtocolProteusTag, ProtocolMLSTag]
     in withStatus FeatureStatusDisabled LockStatusUnlocked config FeatureTTLUnlimited
  featureSingleton = FeatureSingletonMLSConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- ExposeInvitationURLsToTeamAdminConfig

data ExposeInvitationURLsToTeamAdminConfig = ExposeInvitationURLsToTeamAdminConfig
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform ExposeInvitationURLsToTeamAdminConfig)

instance RenderableSymbol ExposeInvitationURLsToTeamAdminConfig where
  renderSymbol = "ExposeInvitationURLsToTeamAdminConfig"

instance IsFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  type FeatureSymbol ExposeInvitationURLsToTeamAdminConfig = "exposeInvitationURLsToTeamAdmin"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
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

instance RenderableSymbol OutlookCalIntegrationConfig where
  renderSymbol = "OutlookCalIntegrationConfig"

instance IsFeatureConfig OutlookCalIntegrationConfig where
  type FeatureSymbol OutlookCalIntegrationConfig = "outlookCalIntegration"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked OutlookCalIntegrationConfig FeatureTTLUnlimited
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

instance RenderableSymbol MlsE2EIdConfig where
  renderSymbol = "MlsE2EIdConfig"

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

instance IsFeatureConfig MlsE2EIdConfig where
  type FeatureSymbol MlsE2EIdConfig = "mlsE2EId"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked defValue FeatureTTLUnlimited
    where
      defValue = MlsE2EIdConfig (fromIntegral @Int (60 * 60 * 24)) Nothing Nothing False
  featureSingleton = FeatureSingletonMlsE2EIdConfig
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- MlsMigration

data MlsMigrationConfig = MlsMigrationConfig
  { startTime :: Maybe UTCTime,
    finaliseRegardlessAfter :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Generic)

instance RenderableSymbol MlsMigrationConfig where
  renderSymbol = "MlsMigrationConfig"

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

instance IsFeatureConfig MlsMigrationConfig where
  type FeatureSymbol MlsMigrationConfig = "mlsMigration"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked defValue FeatureTTLUnlimited
    where
      defValue = MlsMigrationConfig Nothing Nothing
  featureSingleton = FeatureSingletonMlsMigration
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- EnforceFileDownloadLocationConfig

data EnforceFileDownloadLocationConfig = EnforceFileDownloadLocationConfig
  { enforcedDownloadLocation :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance RenderableSymbol EnforceFileDownloadLocationConfig where
  renderSymbol = "EnforceFileDownloadLocationConfig"

instance Arbitrary EnforceFileDownloadLocationConfig where
  arbitrary = EnforceFileDownloadLocationConfig . fmap (cs . getPrintableString) <$> arbitrary

instance ToSchema EnforceFileDownloadLocationConfig where
  schema =
    object "EnforceFileDownloadLocation" $
      EnforceFileDownloadLocationConfig
        <$> enforcedDownloadLocation .= maybe_ (optField "enforcedDownloadLocation" schema)

instance IsFeatureConfig EnforceFileDownloadLocationConfig where
  type FeatureSymbol EnforceFileDownloadLocationConfig = "enforceFileDownloadLocation"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked (EnforceFileDownloadLocationConfig Nothing) FeatureTTLUnlimited
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

instance RenderableSymbol LimitedEventFanoutConfig where
  renderSymbol = "LimitedEventFanoutConfig"

instance IsFeatureConfig LimitedEventFanoutConfig where
  type FeatureSymbol LimitedEventFanoutConfig = "limitedEventFanout"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked LimitedEventFanoutConfig FeatureTTLUnlimited
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
  parseUrlPiece = maybe (Left "must be 'enabled' or 'disabled'") Right . fromByteString' . cs

instance ToHttpApiData FeatureStatus where
  toUrlPiece = cs . toByteString'

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

defFeatureStatusNoLock :: IsFeatureConfig cfg => WithStatusNoLock cfg
defFeatureStatusNoLock = forgetLock defFeatureStatus

-- FUTUREWORK: rewrite using SOP
data AllFeatures f = AllFeatures
  { afcLegalholdStatus :: f LegalholdConfig,
    afcSSOStatus :: f SSOConfig,
    afcTeamSearchVisibilityAvailable :: f SearchVisibilityAvailableConfig,
    afcSearchVisibilityInboundConfig :: f SearchVisibilityInboundConfig,
    afcValidateSAMLEmails :: f ValidateSAMLEmailsConfig,
    afcDigitalSignatures :: f DigitalSignaturesConfig,
    afcAppLock :: f AppLockConfig,
    afcFileSharing :: f FileSharingConfig,
    afcClassifiedDomains :: f ClassifiedDomainsConfig,
    afcConferenceCalling :: f ConferenceCallingConfig,
    afcSelfDeletingMessages :: f SelfDeletingMessagesConfig,
    afcGuestLink :: f GuestLinksConfig,
    afcSndFactorPasswordChallenge :: f SndFactorPasswordChallengeConfig,
    afcMLS :: f MLSConfig,
    afcExposeInvitationURLsToTeamAdmin :: f ExposeInvitationURLsToTeamAdminConfig,
    afcOutlookCalIntegration :: f OutlookCalIntegrationConfig,
    afcMlsE2EId :: f MlsE2EIdConfig,
    afcMlsMigration :: f MlsMigrationConfig,
    afcEnforceFileDownloadLocation :: f EnforceFileDownloadLocationConfig,
    afcLimitedEventFanout :: f LimitedEventFanoutConfig
  }

type AllFeatureConfigs = AllFeatures WithStatus

instance Default AllFeatureConfigs where
  def =
    AllFeatures
      { afcLegalholdStatus = defFeatureStatus,
        afcSSOStatus = defFeatureStatus,
        afcTeamSearchVisibilityAvailable = defFeatureStatus,
        afcSearchVisibilityInboundConfig = defFeatureStatus,
        afcValidateSAMLEmails = defFeatureStatus,
        afcDigitalSignatures = defFeatureStatus,
        afcAppLock = defFeatureStatus,
        afcFileSharing = defFeatureStatus,
        afcClassifiedDomains = defFeatureStatus,
        afcConferenceCalling = defFeatureStatus,
        afcSelfDeletingMessages = defFeatureStatus,
        afcGuestLink = defFeatureStatus,
        afcSndFactorPasswordChallenge = defFeatureStatus,
        afcMLS = defFeatureStatus,
        afcExposeInvitationURLsToTeamAdmin = defFeatureStatus,
        afcOutlookCalIntegration = defFeatureStatus,
        afcMlsE2EId = defFeatureStatus,
        afcMlsMigration = defFeatureStatus,
        afcEnforceFileDownloadLocation = defFeatureStatus,
        afcLimitedEventFanout = defFeatureStatus
      }

instance ToSchema AllFeatureConfigs where
  schema =
    object "AllFeatureConfigs" $
      AllFeatures
        <$> afcLegalholdStatus .= featureField
        <*> afcSSOStatus .= featureField
        <*> afcTeamSearchVisibilityAvailable .= featureField
        <*> afcSearchVisibilityInboundConfig .= featureField
        <*> afcValidateSAMLEmails .= featureField
        <*> afcDigitalSignatures .= featureField
        <*> afcAppLock .= featureField
        <*> afcFileSharing .= featureField
        <*> afcClassifiedDomains .= featureField
        <*> afcConferenceCalling .= featureField
        <*> afcSelfDeletingMessages .= featureField
        <*> afcGuestLink .= featureField
        <*> afcSndFactorPasswordChallenge .= featureField
        <*> afcMLS .= featureField
        <*> afcExposeInvitationURLsToTeamAdmin .= featureField
        <*> afcOutlookCalIntegration .= featureField
        <*> afcMlsE2EId .= featureField
        <*> afcMlsMigration .= featureField
        <*> afcEnforceFileDownloadLocation .= featureField
        <*> afcLimitedEventFanout .= featureField
    where
      featureField ::
        forall cfg.
        (IsFeatureConfig cfg, ToSchema cfg, KnownSymbol (FeatureSymbol cfg)) =>
        ObjectSchema SwaggerDoc (WithStatus cfg)
      featureField = field (T.pack (symbolVal (Proxy @(FeatureSymbol cfg)))) schema

instance Arbitrary AllFeatureConfigs where
  arbitrary =
    AllFeatures
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary

makeLenses ''ImplicitLockStatus

deriving instance Show AllFeatureConfigs

deriving instance Eq AllFeatureConfigs

deriving via (Schema AllFeatureConfigs) instance (FromJSON AllFeatureConfigs)

deriving via (Schema AllFeatureConfigs) instance (ToJSON AllFeatureConfigs)

deriving via (Schema AllFeatureConfigs) instance (S.ToSchema AllFeatureConfigs)
