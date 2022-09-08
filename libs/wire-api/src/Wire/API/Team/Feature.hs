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
    FeatureTTL' (..),
    FeatureTTLUnit (..),
    convertFeatureTTLDaysToSeconds,
    convertFeatureTTLSecondsToDays,
    EnforceAppLock (..),
    defFeatureStatusNoLock,
    computeFeatureConfigForTeamUser,
    IsFeatureConfig (..),
    FeatureTrivialConfig (..),
    HasDeprecatedFeatureName (..),
    LockStatusResponse (..),
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
    AllFeatureConfigs (..),
    typeFeatureTTL,
    withStatusModel,
    withStatusNoLockModel,
    allFeatureModels,
    typeFeatureStatus,
    unImplicitLockStatus,
    ImplicitLockStatus (..),
  )
where

import qualified Cassandra.CQL as Cass
import Control.Lens (makeLenses, (?~))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion
import qualified Data.ByteString.UTF8 as UTF8
import Data.Domain (Domain)
import Data.Either.Extra (maybeToEither)
import Data.Id
import Data.Proxy
import Data.Schema
import Data.Scientific (toBoundedInteger)
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import Deriving.Aeson
import GHC.TypeLits
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Test.QuickCheck.Arbitrary (arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Wire.API.Conversation.Protocol (ProtocolTag (ProtocolProteusTag))
import Wire.API.MLS.CipherSuite (CipherSuiteTag (MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519))
import Wire.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- FeatureTag

-- | Checklist for adding a new feature
--
-- 1. Add a data type for your feature's "config" part, naming convention:
-- **<NameOfFeature>Config**. If your feature doesn't have a config besides
-- being enabled/disabled, locked/unlocked, then the config should be a unit
-- type, e.g. **data MyFeatureConfig = MyFeatureConfig**. Implement type clases
-- 'ToSchema', 'IsFeatureConfig' and 'Arbitrary'. If your feature doesn't have a
-- config implement 'FeatureTrivialConfig'.
--
-- 2. Add the config to to 'AllFeatureConfigs'. Add your feature to 'allFeatureModels'.
--
-- 3. If your feature is configurable on a per-team basis, add a schema
-- migration in galley and add 'FeatureStatusCassandra' instance in
-- Galley.Cassandra.TeamFeatures together with a schema migration
--
-- 4. Add the feature to the config schema of galley in Galley.Types.Teams.
-- and extend the Arbitrary instance of FeatureConfigs in the unit tests Test.Galley.Types
--
-- 5. Implement 'GetFeatureConfig' and 'SetFeatureConfig' in
-- Galley.API.Teams.Features which defines the main business logic for getting
-- and setting (with side-effects).
--
-- 6. Add public routes to Routes.Public.Galley: 'FeatureStatusGet',
-- 'FeatureStatusPut' (optional) and by by user: 'FeatureConfigGet'. Then
-- implement them in Galley.API.Public.
--
-- 7. Add internal routes in Galley.API.Internal
--
-- 8. If the feature should be configurable via Stern add routes to Stern.API.
-- Manually check that the swagger looks okay.
--
-- 9. If the feature is configured on a per-user level, see the
-- 'ConferenceCallingConfig' as an example.
-- (https://github.com/wireapp/wire-server/pull/1811,
-- https://github.com/wireapp/wire-server/pull/1818)
--
-- 10. Extend the integration tests with cases
class IsFeatureConfig cfg where
  type FeatureSymbol cfg :: Symbol
  defFeatureStatus :: WithStatus cfg

  -- | Swagger 1.2 model for stern and wai routes
  configModel :: Maybe Doc.Model
  configModel = Nothing

  objectSchema ::
    -- | Should be "pure MyFeatureConfig" if the feature doesn't have config,
    -- which results in a trivial empty schema and the "config" field being
    -- omitted/ignored in the JSON encoder / parser.
    ObjectSchema SwaggerDoc cfg

class FeatureTrivialConfig cfg where
  trivialConfig :: cfg

class HasDeprecatedFeatureName cfg where
  type DeprecatedFeatureName cfg :: Symbol

featureName :: forall cfg. (IsFeatureConfig cfg, KnownSymbol (FeatureSymbol cfg)) => Text
featureName = T.pack $ symbolVal (Proxy @(FeatureSymbol cfg))

featureNameBS :: forall cfg. (IsFeatureConfig cfg, KnownSymbol (FeatureSymbol cfg)) => ByteString
featureNameBS = UTF8.fromString $ symbolVal (Proxy @(FeatureSymbol cfg))

----------------------------------------------------------------------
-- WithStatusBase

data WithStatusBase (m :: * -> *) (cfg :: *) = WithStatusBase
  { wsbStatus :: m FeatureStatus,
    wsbLockStatus :: m LockStatus,
    wsbConfig :: m cfg,
    wsbTTL :: m FeatureTTL
  }
  deriving stock (Generic, Typeable, Functor)

----------------------------------------------------------------------
-- WithStatus

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
setConfig c (WithStatusBase s ls _ ttl) = WithStatusBase s ls (Identity c) ttl

setWsTTL :: FeatureTTL -> WithStatus cfg -> WithStatus cfg
setWsTTL ttl (WithStatusBase s ls c _) = WithStatusBase s ls c (Identity ttl)

type WithStatus (cfg :: *) = WithStatusBase Identity cfg

deriving instance (Eq cfg) => Eq (WithStatus cfg)

deriving instance (Show cfg) => Show (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg)) => ToJSON (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg)) => FromJSON (WithStatus cfg)

deriving via (Schema (WithStatus cfg)) instance (ToSchema (WithStatus cfg)) => S.ToSchema (WithStatus cfg)

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

withStatusModel :: forall cfg. (IsFeatureConfig cfg, KnownSymbol (FeatureSymbol cfg)) => Doc.Model
withStatusModel =
  let name = featureName @cfg
      mbModelCfg = configModel @cfg
   in Doc.defineModel ("WithStatus." <> name) $ do
        case mbModelCfg of
          Nothing -> Doc.description $ "Team feature " <> name <> " that has no configuration beyond the boolean on/off switch."
          Just modelCfg -> do
            Doc.description $ "Status and config of " <> name
            Doc.property "config" (Doc.ref modelCfg) $ Doc.description "config"

        Doc.property "status" typeFeatureStatus $ Doc.description "status"
        Doc.property "lockStatus" typeLockStatusValue $ Doc.description ""

----------------------------------------------------------------------
-- WithStatusPatch

type WithStatusPatch (cfg :: *) = WithStatusBase Maybe cfg

deriving instance (Eq cfg) => Eq (WithStatusPatch cfg)

deriving instance (Show cfg) => Show (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg)) => ToJSON (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg)) => FromJSON (WithStatusPatch cfg)

deriving via (Schema (WithStatusPatch cfg)) instance (ToSchema (WithStatusPatch cfg)) => S.ToSchema (WithStatusPatch cfg)

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
instance (ToSchema cfg, IsFeatureConfig cfg) => ToSchema (WithStatusPatch cfg) where
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

-- FUTUREWORK(fisx): remove this type.  we want all features to have fields `lockStatus` and
-- `status`, and we want them to have the same semantics everywhere.  currently we have
-- eg. conf calling, which was introduced before `lockStatus`, and where `status` means
-- `lockStatus`.  TTL always refers to `lockStatus`, not `status`.  In order to keep current
-- (desired) behavior, consider eg. conf calling: let's only allow setting `lockStatus`, but
-- if we switch to `unlocked`, we auto-enable the feature, and if we switch to locked, we
-- auto-disable it.  But we need to change the API to force clients to use `lockStatus`
-- instead of `status`, current behavior is just wrong.
data WithStatusNoLock (cfg :: *) = WithStatusNoLock
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

withStatusNoLockModel :: forall cfg. (IsFeatureConfig cfg, KnownSymbol (FeatureSymbol cfg)) => Doc.Model
withStatusNoLockModel =
  let name = featureName @cfg
      mbModelCfg = configModel @cfg
   in Doc.defineModel ("WithStatusNoLock." <> name) $ do
        case mbModelCfg of
          Nothing -> Doc.description $ "Team feature " <> name <> " that has no configuration beyond the boolean on/off switch."
          Just modelCfg -> do
            Doc.description $ "Status and config of " <> name
            Doc.property "config" (Doc.ref modelCfg) $ Doc.description "config"

        Doc.property "status" typeFeatureStatus $ Doc.description "status"

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
  toParamSchema _ = S.toParamSchema (Proxy @Int)

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

typeFeatureTTL :: Doc.DataType
typeFeatureTTL =
  Doc.int64'

invalidTTLErrorString :: Text
invalidTTLErrorString = "Invalid FeatureTTLSeconds: must be a positive integer or 'unlimited.'"

-- LockStatus

data LockStatus = LockStatusLocked | LockStatusUnlocked
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LockStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema LockStatus)

instance FromHttpApiData LockStatus where
  parseUrlPiece = maybeToEither "Invalid lock status" . fromByteString . cs

typeLockStatusValue :: Doc.DataType
typeLockStatusValue =
  Doc.string $
    Doc.enum
      [ "locked",
        "unlocked"
      ]

instance ToSchema LockStatus where
  schema =
    enum @Text "LockStatus" $
      mconcat
        [ element "locked" LockStatusLocked,
          element "unlocked" LockStatusUnlocked
        ]

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

newtype ImplicitLockStatus (cfg :: *) = ImplicitLockStatus {_unImplicitLockStatus :: WithStatus cfg}
  deriving newtype (Eq, Show, Arbitrary)

instance (IsFeatureConfig a, ToSchema a) => ToJSON (ImplicitLockStatus a) where
  toJSON (ImplicitLockStatus a) = A.toJSON $ forgetLock a

instance (IsFeatureConfig a, ToSchema a) => FromJSON (ImplicitLockStatus a) where
  parseJSON v = ImplicitLockStatus . withLockStatus (wsLockStatus $ defFeatureStatus @a) <$> A.parseJSON v

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

allFeatureModels :: [Doc.Model]
allFeatureModels =
  [ withStatusNoLockModel @LegalholdConfig,
    withStatusNoLockModel @SSOConfig,
    withStatusNoLockModel @SearchVisibilityAvailableConfig,
    withStatusNoLockModel @ValidateSAMLEmailsConfig,
    withStatusNoLockModel @DigitalSignaturesConfig,
    withStatusNoLockModel @AppLockConfig,
    withStatusNoLockModel @FileSharingConfig,
    withStatusNoLockModel @ClassifiedDomainsConfig,
    withStatusNoLockModel @ConferenceCallingConfig,
    withStatusNoLockModel @SelfDeletingMessagesConfig,
    withStatusNoLockModel @GuestLinksConfig,
    withStatusNoLockModel @SndFactorPasswordChallengeConfig,
    withStatusNoLockModel @SearchVisibilityInboundConfig,
    withStatusNoLockModel @MLSConfig,
    withStatusNoLockModel @ExposeInvitationURLsToTeamAdminConfig,
    withStatusModel @LegalholdConfig,
    withStatusModel @SSOConfig,
    withStatusModel @SearchVisibilityAvailableConfig,
    withStatusModel @ValidateSAMLEmailsConfig,
    withStatusModel @DigitalSignaturesConfig,
    withStatusModel @AppLockConfig,
    withStatusModel @FileSharingConfig,
    withStatusModel @ClassifiedDomainsConfig,
    withStatusModel @ConferenceCallingConfig,
    withStatusModel @SelfDeletingMessagesConfig,
    withStatusModel @GuestLinksConfig,
    withStatusModel @SndFactorPasswordChallengeConfig,
    withStatusModel @SearchVisibilityInboundConfig,
    withStatusModel @MLSConfig,
    withStatusModel @ExposeInvitationURLsToTeamAdminConfig
  ]
    <> catMaybes
      [ configModel @LegalholdConfig,
        configModel @SSOConfig,
        configModel @SearchVisibilityAvailableConfig,
        configModel @ValidateSAMLEmailsConfig,
        configModel @DigitalSignaturesConfig,
        configModel @AppLockConfig,
        configModel @FileSharingConfig,
        configModel @ClassifiedDomainsConfig,
        configModel @ConferenceCallingConfig,
        configModel @SelfDeletingMessagesConfig,
        configModel @GuestLinksConfig,
        configModel @SndFactorPasswordChallengeConfig,
        configModel @SearchVisibilityInboundConfig,
        configModel @MLSConfig,
        configModel @ExposeInvitationURLsToTeamAdminConfig
      ]

--------------------------------------------------------------------------------
-- GuestLinks feature

data GuestLinksConfig = GuestLinksConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform GuestLinksConfig)

instance ToSchema GuestLinksConfig where
  schema = object "GuestLinksConfig" objectSchema

instance IsFeatureConfig GuestLinksConfig where
  type FeatureSymbol GuestLinksConfig = "conversationGuestLinks"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked GuestLinksConfig FeatureTTLUnlimited

  objectSchema = pure GuestLinksConfig

instance FeatureTrivialConfig GuestLinksConfig where
  trivialConfig = GuestLinksConfig

--------------------------------------------------------------------------------
-- Legalhold feature

data LegalholdConfig = LegalholdConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform LegalholdConfig)

instance IsFeatureConfig LegalholdConfig where
  type FeatureSymbol LegalholdConfig = "legalhold"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked LegalholdConfig FeatureTTLUnlimited
  objectSchema = pure LegalholdConfig

instance ToSchema LegalholdConfig where
  schema = object "LegalholdConfig" objectSchema

instance FeatureTrivialConfig LegalholdConfig where
  trivialConfig = LegalholdConfig

--------------------------------------------------------------------------------
-- SSO feature

data SSOConfig = SSOConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SSOConfig)

instance IsFeatureConfig SSOConfig where
  type FeatureSymbol SSOConfig = "sso"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SSOConfig FeatureTTLUnlimited
  objectSchema = pure SSOConfig

instance ToSchema SSOConfig where
  schema = object "SSOConfig" objectSchema

instance FeatureTrivialConfig SSOConfig where
  trivialConfig = SSOConfig

--------------------------------------------------------------------------------
-- SearchVisibility available feature

-- | Wether a team is allowed to change search visibility
-- See the handle of PUT /teams/:tid/search-visibility
data SearchVisibilityAvailableConfig = SearchVisibilityAvailableConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityAvailableConfig)

instance IsFeatureConfig SearchVisibilityAvailableConfig where
  type FeatureSymbol SearchVisibilityAvailableConfig = "searchVisibility"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityAvailableConfig FeatureTTLUnlimited
  objectSchema = pure SearchVisibilityAvailableConfig

instance ToSchema SearchVisibilityAvailableConfig where
  schema = object "SearchVisibilityAvailableConfig" objectSchema

instance FeatureTrivialConfig SearchVisibilityAvailableConfig where
  trivialConfig = SearchVisibilityAvailableConfig

instance HasDeprecatedFeatureName SearchVisibilityAvailableConfig where
  type DeprecatedFeatureName SearchVisibilityAvailableConfig = "search-visibility"

--------------------------------------------------------------------------------
-- ValidateSAMLEmails feature

data ValidateSAMLEmailsConfig = ValidateSAMLEmailsConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ValidateSAMLEmailsConfig)

instance ToSchema ValidateSAMLEmailsConfig where
  schema = object "ValidateSAMLEmailsConfig" objectSchema

instance IsFeatureConfig ValidateSAMLEmailsConfig where
  type FeatureSymbol ValidateSAMLEmailsConfig = "validateSAMLemails"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked ValidateSAMLEmailsConfig FeatureTTLUnlimited
  objectSchema = pure ValidateSAMLEmailsConfig

instance HasDeprecatedFeatureName ValidateSAMLEmailsConfig where
  type DeprecatedFeatureName ValidateSAMLEmailsConfig = "validate-saml-emails"

instance FeatureTrivialConfig ValidateSAMLEmailsConfig where
  trivialConfig = ValidateSAMLEmailsConfig

--------------------------------------------------------------------------------
-- DigitalSignatures feature

data DigitalSignaturesConfig = DigitalSignaturesConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform DigitalSignaturesConfig)

instance IsFeatureConfig DigitalSignaturesConfig where
  type FeatureSymbol DigitalSignaturesConfig = "digitalSignatures"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked DigitalSignaturesConfig FeatureTTLUnlimited
  objectSchema = pure DigitalSignaturesConfig

instance HasDeprecatedFeatureName DigitalSignaturesConfig where
  type DeprecatedFeatureName DigitalSignaturesConfig = "digital-signatures"

instance ToSchema DigitalSignaturesConfig where
  schema = object "DigitalSignaturesConfig" objectSchema

instance FeatureTrivialConfig DigitalSignaturesConfig where
  trivialConfig = DigitalSignaturesConfig

--------------------------------------------------------------------------------
-- ConferenceCalling feature

data ConferenceCallingConfig = ConferenceCallingConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform ConferenceCallingConfig)

instance IsFeatureConfig ConferenceCallingConfig where
  type FeatureSymbol ConferenceCallingConfig = "conferenceCalling"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked ConferenceCallingConfig FeatureTTLUnlimited
  objectSchema = pure ConferenceCallingConfig

instance ToSchema ConferenceCallingConfig where
  schema = object "ConferenceCallingConfig" objectSchema

instance FeatureTrivialConfig ConferenceCallingConfig where
  trivialConfig = ConferenceCallingConfig

--------------------------------------------------------------------------------
-- SndFactorPasswordChallenge feature

data SndFactorPasswordChallengeConfig = SndFactorPasswordChallengeConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SndFactorPasswordChallengeConfig)

instance ToSchema SndFactorPasswordChallengeConfig where
  schema = object "SndFactorPasswordChallengeConfig" objectSchema

instance IsFeatureConfig SndFactorPasswordChallengeConfig where
  type FeatureSymbol SndFactorPasswordChallengeConfig = "sndFactorPasswordChallenge"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusLocked SndFactorPasswordChallengeConfig FeatureTTLUnlimited
  objectSchema = pure SndFactorPasswordChallengeConfig

instance FeatureTrivialConfig SndFactorPasswordChallengeConfig where
  trivialConfig = SndFactorPasswordChallengeConfig

--------------------------------------------------------------------------------
-- SearchVisibilityInbound feature

data SearchVisibilityInboundConfig = SearchVisibilityInboundConfig
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform SearchVisibilityInboundConfig)

instance IsFeatureConfig SearchVisibilityInboundConfig where
  type FeatureSymbol SearchVisibilityInboundConfig = "searchVisibilityInbound"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked SearchVisibilityInboundConfig FeatureTTLUnlimited
  objectSchema = pure SearchVisibilityInboundConfig

instance ToSchema SearchVisibilityInboundConfig where
  schema = object "SearchVisibilityInboundConfig" objectSchema

instance FeatureTrivialConfig SearchVisibilityInboundConfig where
  trivialConfig = SearchVisibilityInboundConfig

----------------------------------------------------------------------
-- ClassifiedDomains feature

data ClassifiedDomainsConfig = ClassifiedDomainsConfig
  { classifiedDomainsDomains :: [Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema ClassifiedDomainsConfig)

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
  configModel = Just $
    Doc.defineModel "ClassifiedDomainsConfig" $ do
      Doc.property "domains" (Doc.array Doc.string') $ Doc.description "domains"
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
  configModel = Just $
    Doc.defineModel "AppLockConfig" $ do
      Doc.property "enforceAppLock" Doc.bool' $ Doc.description "enforceAppLock"
      Doc.property "inactivityTimeoutSecs" Doc.int32' $ Doc.description ""
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

instance IsFeatureConfig FileSharingConfig where
  type FeatureSymbol FileSharingConfig = "fileSharing"
  defFeatureStatus = withStatus FeatureStatusEnabled LockStatusUnlocked FileSharingConfig FeatureTTLUnlimited
  objectSchema = pure FileSharingConfig

instance ToSchema FileSharingConfig where
  schema = object "FileSharingConfig" objectSchema

instance FeatureTrivialConfig FileSharingConfig where
  trivialConfig = FileSharingConfig

----------------------------------------------------------------------
-- SelfDeletingMessagesConfig

newtype SelfDeletingMessagesConfig = SelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema SelfDeletingMessagesConfig)
  deriving (Arbitrary) via (GenericUniform SelfDeletingMessagesConfig)

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
  configModel = Just $
    Doc.defineModel "SelfDeletingMessagesConfig" $ do
      Doc.property "enforcedTimeoutSeconds" Doc.int32' $ Doc.description "optional; default: `0` (no enforcement)"
  objectSchema = field "config" schema

----------------------------------------------------------------------
-- MLSConfig

data MLSConfig = MLSConfig
  { mlsProtocolToggleUsers :: [UserId],
    mlsDefaultProtocol :: ProtocolTag,
    mlsAllowedCipherSuites :: [CipherSuiteTag],
    mlsDefaultCipherSuite :: CipherSuiteTag
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform MLSConfig)

instance ToSchema MLSConfig where
  schema =
    object "MLSConfig" $
      MLSConfig
        <$> mlsProtocolToggleUsers .= fieldWithDocModifier "protocolToggleUsers" (S.description ?~ "allowlist of users that may change protocols") (array schema)
        <*> mlsDefaultProtocol .= field "defaultProtocol" schema
        <*> mlsAllowedCipherSuites .= field "allowedCipherSuites" (array schema)
        <*> mlsDefaultCipherSuite .= field "defaultCipherSuite" schema

instance IsFeatureConfig MLSConfig where
  type FeatureSymbol MLSConfig = "mls"
  defFeatureStatus =
    let config = MLSConfig [] ProtocolProteusTag [MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519] MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
     in withStatus FeatureStatusDisabled LockStatusUnlocked config FeatureTTLUnlimited
  objectSchema = field "config" schema

  configModel = Just $
    Doc.defineModel "MLSConfig" $ do
      Doc.property "protocolToggleUsers" (Doc.array Doc.string') $ Doc.description "allowlist of users that may change protocols"
      Doc.property "defaultProtocol" Doc.string' $ Doc.description "default protocol, either \"proteus\" or \"mls\""
      Doc.property "allowedCipherSuites" (Doc.array Doc.int32') $ Doc.description "cipher suite numbers,  See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5"
      Doc.property "defaultCipherSuite" Doc.int32' $ Doc.description "cipher suite number. See https://messaginglayersecurity.rocks/mls-protocol/draft-ietf-mls-protocol.html#table-5"

----------------------------------------------------------------------
-- ExposeInvitationURLsToTeamAdminConfig

data ExposeInvitationURLsToTeamAdminConfig = ExposeInvitationURLsToTeamAdminConfig
  deriving stock (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform ExposeInvitationURLsToTeamAdminConfig)

instance IsFeatureConfig ExposeInvitationURLsToTeamAdminConfig where
  type FeatureSymbol ExposeInvitationURLsToTeamAdminConfig = "exposeInvitationURLsToTeamAdmins"
  defFeatureStatus = withStatus FeatureStatusDisabled LockStatusUnlocked ExposeInvitationURLsToTeamAdminConfig FeatureTTLUnlimited
  objectSchema = pure ExposeInvitationURLsToTeamAdminConfig

instance ToSchema ExposeInvitationURLsToTeamAdminConfig where
  schema = object "ExposeInvitationURLsToTeamAdminConfig" objectSchema

instance FeatureTrivialConfig ExposeInvitationURLsToTeamAdminConfig where
  trivialConfig = ExposeInvitationURLsToTeamAdminConfig

----------------------------------------------------------------------
-- FeatureStatus

data FeatureStatus
  = FeatureStatusEnabled
  | FeatureStatusDisabled
  deriving stock (Eq, Ord, Enum, Bounded, Show, Generic)
  deriving (Arbitrary) via (GenericUniform FeatureStatus)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema FeatureStatus)

typeFeatureStatus :: Doc.DataType
typeFeatureStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

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

data AllFeatureConfigs = AllFeatureConfigs
  { afcLegalholdStatus :: WithStatus LegalholdConfig,
    afcSSOStatus :: WithStatus SSOConfig,
    afcTeamSearchVisibilityAvailable :: WithStatus SearchVisibilityAvailableConfig,
    afcSearchVisibilityInboundConfig :: WithStatus SearchVisibilityInboundConfig,
    afcValidateSAMLEmails :: WithStatus ValidateSAMLEmailsConfig,
    afcDigitalSignatures :: WithStatus DigitalSignaturesConfig,
    afcAppLock :: WithStatus AppLockConfig,
    afcFileSharing :: WithStatus FileSharingConfig,
    afcClassifiedDomains :: WithStatus ClassifiedDomainsConfig,
    afcConferenceCalling :: WithStatus ConferenceCallingConfig,
    afcSelfDeletingMessages :: WithStatus SelfDeletingMessagesConfig,
    afcGuestLink :: WithStatus GuestLinksConfig,
    afcSndFactorPasswordChallenge :: WithStatus SndFactorPasswordChallengeConfig,
    afcMLS :: WithStatus MLSConfig,
    afcExposeInvitationURLsToTeamAdmin :: WithStatus ExposeInvitationURLsToTeamAdminConfig
  }
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AllFeatureConfigs)

instance ToSchema AllFeatureConfigs where
  schema =
    object "AllFeatureConfigs" $
      AllFeatureConfigs
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
    where
      featureField ::
        forall cfg.
        (IsFeatureConfig cfg, ToSchema cfg, KnownSymbol (FeatureSymbol cfg)) =>
        ObjectSchema SwaggerDoc (WithStatus cfg)
      featureField = field (T.pack (symbolVal (Proxy @(FeatureSymbol cfg)))) schema

instance Arbitrary AllFeatureConfigs where
  arbitrary =
    AllFeatureConfigs
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

makeLenses ''ImplicitLockStatus
