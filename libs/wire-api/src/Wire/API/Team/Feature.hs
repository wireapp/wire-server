{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Wire.API.Team.Feature
  ( TeamFeatureName (..),
    TeamFeatureStatus,
    TeamFeatureAppLockConfig (..),
    TeamFeatureSelfDeletingMessagesConfig (..),
    TeamFeatureClassifiedDomainsConfig (..),
    TeamFeatureStatusValue (..),
    FeatureHasNoConfig,
    EnforceAppLock (..),
    KnownTeamFeatureName (..),
    TeamFeatureStatusNoConfig (..),
    TeamFeatureStatusWithConfig (..),
    HasDeprecatedFeatureName (..),
    AllFeatureConfigs (..),
    defaultAppLockStatus,
    defaultClassifiedDomains,
    defaultSelfDeletingMessagesStatus,

    -- * Swagger
    typeTeamFeatureName,
    typeTeamFeatureStatusValue,
    modelTeamFeatureStatusNoConfig,
    modelTeamFeatureStatusWithConfig,
    modelTeamFeatureAppLockConfig,
    modelTeamFeatureClassifiedDomainsConfig,
    modelTeamFeatureSelfDeletingMessagesConfig,
    modelForTeamFeature,
  )
where

import qualified Cassandra.CQL as Cass
import Control.Lens.Combinators (dimap)
import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), toByteString')
import Data.Domain (Domain)
import Data.Kind (Constraint)
import Data.Schema
import Data.String.Conversions (cs)
import qualified Data.Swagger as S
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import GHC.TypeLits (Symbol)
import Imports
import Test.QuickCheck.Arbitrary (arbitrary)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- TeamFeatureName

-- | If you add a constructor here, you need extend multiple defintions, which
--   aren't checked by GHC.
--
--   Follow this Checklist:
--
-- * libs/wire-api/test/unit/Test/Wire/API/Roundtrip/Aeson.hs
--   * add call to 'testRoundTrip'
-- * libs/wire-api/src/Wire/API/Routes/Public/Galley.hs
--   * add a GET (and possible PUT) route with name prefix teamFeature<FEATURE_NAME>
--   * add a GET route with name prefix featureConfig<FEATURE_NAME>
-- * services/galley/src/Galley/API/Internal.hs
--   * add a field to the 'InternalApi routes' record)
-- * libs/galley-types/src/Galley/Types/Teams.hs
--   * FeatureFlags for server config file
--   * roleHiddenPermissions ChangeTeamFeature and ViewTeamFeature
-- * services/galley/src/Galley/API/Teams/Features.hs
--   * extend getAllFeatureConfigs
--   * extend getAllFeatures
-- * services/galley/schema/src/
--   * add a migration like the one in "V43_TeamFeatureDigitalSignatures.hs"
-- * services/galley/test/integration/API/Teams/Feature.hs
--   * add an integration test for the feature
--   * extend testAllFeatures
-- * consider personal-account configurability (like for `conferenceCalling`, see
--     eg. https://github.com/wireapp/wire-server/pull/1811,
--     https://github.com/wireapp/wire-server/pull/1818)
--
-- An example of all the places to change (including compiler errors and failing tests) can be found
-- in eg. https://github.com/wireapp/wire-server/pull/1652.  (applock and conference calling also
-- add interesting aspects, though.)
--
-- Using something like '[minBound..]' on those expressions would require dependent types.  We
-- could generate exhaustive lists of those calls using TH, along the lines of:
--
-- @
-- forAllTeamFeatureNames ::
--   ExpQ {- [forall (a :: TeamFeatureName). b] -} ->
--   ExpQ {- [b] -}
-- forAllTeamFeatureNames =
--   error
--     "...  and then somehow turn the values from '[minBound..]' into \
--     \type applications in the syntax tree"
-- @
--
-- But that seems excessive.  Let's wait for dependent types to be ready in ghc!
data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  | TeamFeatureValidateSAMLEmails
  | TeamFeatureDigitalSignatures
  | TeamFeatureAppLock
  | TeamFeatureFileSharing
  | TeamFeatureClassifiedDomains
  | TeamFeatureConferenceCalling
  | TeamFeatureSelfDeletingMessages
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded, Typeable)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

{-

PUT /feature-configs/selfDeletingMessages

{
  "payment-status": "locked" | "unlocked",
  "status": "enabled" | "disabled",
  // Only present if status is "enabled".
  "config": {
    // 0 indicates not forced, any value greater indicates forced.
    "enforcedTimeoutSeconds": [0...)
  }
}

enforcedTimeoutSeconds: A non negative integer; The number of seconds of the timeout when the feature is forced. If it is 0, then the feature is not forced.

-}

class KnownTeamFeatureName (a :: TeamFeatureName) where
  knownTeamFeatureName :: TeamFeatureName
  type KnownTeamFeatureNameSymbol a :: Symbol

instance KnownTeamFeatureName 'TeamFeatureLegalHold where
  type KnownTeamFeatureNameSymbol 'TeamFeatureLegalHold = "legalhold"
  knownTeamFeatureName = TeamFeatureLegalHold

instance KnownTeamFeatureName 'TeamFeatureSSO where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSSO = "sso"
  knownTeamFeatureName = TeamFeatureSSO

instance KnownTeamFeatureName 'TeamFeatureSearchVisibility where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSearchVisibility = "searchVisibility"
  knownTeamFeatureName = TeamFeatureSearchVisibility

instance KnownTeamFeatureName 'TeamFeatureValidateSAMLEmails where
  type KnownTeamFeatureNameSymbol 'TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  knownTeamFeatureName = TeamFeatureValidateSAMLEmails

instance KnownTeamFeatureName 'TeamFeatureDigitalSignatures where
  type KnownTeamFeatureNameSymbol 'TeamFeatureDigitalSignatures = "digitalSignatures"
  knownTeamFeatureName = TeamFeatureDigitalSignatures

instance KnownTeamFeatureName 'TeamFeatureAppLock where
  type KnownTeamFeatureNameSymbol 'TeamFeatureAppLock = "appLock"
  knownTeamFeatureName = TeamFeatureAppLock

instance KnownTeamFeatureName 'TeamFeatureFileSharing where
  type KnownTeamFeatureNameSymbol 'TeamFeatureFileSharing = "fileSharing"
  knownTeamFeatureName = TeamFeatureFileSharing

instance KnownTeamFeatureName 'TeamFeatureClassifiedDomains where
  type KnownTeamFeatureNameSymbol 'TeamFeatureClassifiedDomains = "classifiedDomains"
  knownTeamFeatureName = TeamFeatureClassifiedDomains

instance KnownTeamFeatureName 'TeamFeatureConferenceCalling where
  type KnownTeamFeatureNameSymbol 'TeamFeatureConferenceCalling = "conferenceCalling"
  knownTeamFeatureName = TeamFeatureConferenceCalling

instance KnownTeamFeatureName 'TeamFeatureSelfDeletingMessages where
  type KnownTeamFeatureNameSymbol 'TeamFeatureSelfDeletingMessages = "selfDeletingMessages"
  knownTeamFeatureName = TeamFeatureSelfDeletingMessages

instance FromByteString TeamFeatureName where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Left e -> fail $ "Invalid TeamFeatureName: " <> show e
        Right "legalhold" -> pure TeamFeatureLegalHold
        Right "sso" -> pure TeamFeatureSSO
        Right "searchVisibility" -> pure TeamFeatureSearchVisibility
        Right "search-visibility" -> pure TeamFeatureSearchVisibility
        Right "validateSAMLemails" -> pure TeamFeatureValidateSAMLEmails
        Right "validate-saml-emails" -> pure TeamFeatureValidateSAMLEmails
        Right "digitalSignatures" -> pure TeamFeatureDigitalSignatures
        Right "digital-signatures" -> pure TeamFeatureDigitalSignatures
        Right "appLock" -> pure TeamFeatureAppLock
        Right "fileSharing" -> pure TeamFeatureFileSharing
        Right "classifiedDomains" -> pure TeamFeatureClassifiedDomains
        Right "conferenceCalling" -> pure TeamFeatureConferenceCalling
        Right "selfDeletingMessages" -> pure TeamFeatureSelfDeletingMessages
        Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

-- TODO: how do we make this consistent with 'KnownTeamFeatureNameSymbol'?  add a test for
-- that?  anyway do we really need both?
instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "searchVisibility"
  builder TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  builder TeamFeatureDigitalSignatures = "digitalSignatures"
  builder TeamFeatureAppLock = "appLock"
  builder TeamFeatureFileSharing = "fileSharing"
  builder TeamFeatureClassifiedDomains = "classifiedDomains"
  builder TeamFeatureConferenceCalling = "conferenceCalling"
  builder TeamFeatureSelfDeletingMessages = "selfDeletingMessages"

instance ToSchema TeamFeatureName where
  schema =
    enum @Text
      "TeamFeatureName"
      $ mconcat
        (map (\feat -> element (cs . toByteString' $ feat) feat) [minBound .. maxBound])

class HasDeprecatedFeatureName (a :: TeamFeatureName) where
  type DeprecatedFeatureName a :: Symbol

instance HasDeprecatedFeatureName 'TeamFeatureSearchVisibility where
  type DeprecatedFeatureName 'TeamFeatureSearchVisibility = "search-visibility"

instance HasDeprecatedFeatureName 'TeamFeatureValidateSAMLEmails where
  type DeprecatedFeatureName 'TeamFeatureValidateSAMLEmails = "validate-saml-emails"

instance HasDeprecatedFeatureName 'TeamFeatureDigitalSignatures where
  type DeprecatedFeatureName 'TeamFeatureDigitalSignatures = "digital-signatures"

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName = Doc.string . Doc.enum $ cs . toByteString' <$> [(minBound :: TeamFeatureName) ..]

----------------------------------------------------------------------
-- TeamFeatureStatusValue

data TeamFeatureStatusValue
  = TeamFeatureEnabled
  | TeamFeatureDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamFeatureStatusValue)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusValue)

typeTeamFeatureStatusValue :: Doc.DataType
typeTeamFeatureStatusValue =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToSchema TeamFeatureStatusValue where
  schema =
    enum @Text "TeamFeatureStatusValue" $
      mconcat
        [ element "enabled" TeamFeatureEnabled,
          element "disabled" TeamFeatureDisabled
        ]

instance ToByteString TeamFeatureStatusValue where
  builder TeamFeatureEnabled = "enabled"
  builder TeamFeatureDisabled = "disabled"

instance FromByteString TeamFeatureStatusValue where
  parser =
    Parser.takeByteString >>= \b ->
      case T.decodeUtf8' b of
        Right "enabled" -> pure TeamFeatureEnabled
        Right "disabled" -> pure TeamFeatureDisabled
        Right t -> fail $ "Invalid TeamFeatureStatusValue: " <> T.unpack t
        Left e -> fail $ "Invalid TeamFeatureStatusValue: " <> show e

instance Cass.Cql TeamFeatureStatusValue where
  ctype = Cass.Tagged Cass.IntColumn

  fromCql (Cass.CqlInt n) = case n of
    0 -> pure $ TeamFeatureDisabled
    1 -> pure $ TeamFeatureEnabled
    _ -> Left "fromCql: Invalid TeamFeatureStatusValue"
  fromCql _ = Left "fromCql: TeamFeatureStatusValue: CqlInt expected"

  toCql TeamFeatureDisabled = Cass.CqlInt 0
  toCql TeamFeatureEnabled = Cass.CqlInt 1

----------------------------------------------------------------------
-- TeamFeatureStatus

type family TeamFeatureStatus (a :: TeamFeatureName) :: * where
  TeamFeatureStatus 'TeamFeatureLegalHold = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureSSO = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureSearchVisibility = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureValidateSAMLEmails = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureDigitalSignatures = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureAppLock = TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
  TeamFeatureStatus 'TeamFeatureFileSharing = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureClassifiedDomains = TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig
  TeamFeatureStatus 'TeamFeatureConferenceCalling = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureSelfDeletingMessages = TeamFeatureStatusWithConfig TeamFeatureSelfDeletingMessagesConfig

type FeatureHasNoConfig (a :: TeamFeatureName) = (TeamFeatureStatus a ~ TeamFeatureStatusNoConfig) :: Constraint

-- if you add a new constructor here, don't forget to add it to the swagger (1.2) docs in "Wire.API.Swagger"!
modelForTeamFeature :: TeamFeatureName -> Doc.Model
modelForTeamFeature TeamFeatureLegalHold = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSSO = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSearchVisibility = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureValidateSAMLEmails = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureDigitalSignatures = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureAppLock = modelTeamFeatureStatusWithConfig name modelTeamFeatureAppLockConfig
modelForTeamFeature TeamFeatureFileSharing = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureClassifiedDomains = modelTeamFeatureStatusWithConfig name modelTeamFeatureClassifiedDomainsConfig
modelForTeamFeature TeamFeatureConferenceCalling = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureSelfDeletingMessages = modelTeamFeatureStatusWithConfig name modelTeamFeatureSelfDeletingMessagesConfig

----------------------------------------------------------------------
-- TeamFeatureStatusNoConfig

data TeamFeatureStatusNoConfig = TeamFeatureStatusNoConfig
  { -- | Payment status is used by our payment service ibis in the wire cloud to enable or
    -- disable features for teams based on their payment plan.  Default is `enabled` (compiled
    -- in) and can be changed in `galley.yaml`.  ibis would contact internal end-points to set
    -- only `payment-status` and neither `status` nor config.  This will be parsed to a
    -- complete default.
    --
    -- Parser rules: only one of the two attributes can be present.  If `payment-status` is
    -- `disabled`, `status` can not be updated.
    tfwoPaymentStatus :: TeamFeatureStatusValue,
    tfwoStatus :: TeamFeatureStatusValue
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving (Arbitrary) via (GenericUniform TeamFeatureStatusNoConfig)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureStatusNoConfig) -- where is this schema defined?  ask paolo.

-- TODO: tfwoPaymentStatus also complicates conference calling: ibis needs to push something
-- else to galley now for this to work, or we need to have a parser that supports the old API.
-- (latter's probably easier, and we can also support the new API, which will make migration
-- later easy.  we just need to check all the other features if anything goes wrong there.)

modelTeamFeatureStatusNoConfig :: Doc.Model
modelTeamFeatureStatusNoConfig = Doc.defineModel "TeamFeatureStatusNoConfig" $ do
  Doc.description "Team feature that has no configuration beyond the boolean on/off switch."
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"

instance ToSchema TeamFeatureStatusNoConfig where
  schema =
    object "TeamFeatureStatusNoConfig" $
      TeamFeatureStatusNoConfig
        <$> error "TODO: make this optional, with baked-in default `enabled`" -- tfwoPaymentStatus .= field "payment-status" schema
        <*> tfwoStatus .= field "status" schema

----------------------------------------------------------------------
-- TeamFeatureStatusWithConfig

-- | The support for disabled features with configs is intentional:
-- for instance, we want to be able to keep the config of a feature
-- that is turned on and off occasionally, and so not force the admin
-- to recreate the config every time it's turned on.
data TeamFeatureStatusWithConfig (cfg :: *) = TeamFeatureStatusWithConfig
  { -- | See 'TeamFeatureStatusNoConfig' for the semantics of 'tfwcPaymentStatus'.
    tfwcPaymentStatus :: TeamFeatureStatusValue,
    tfwcStatus :: TeamFeatureStatusValue,
    tfwcConfig :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema (TeamFeatureStatusWithConfig cfg))

instance Arbitrary cfg => Arbitrary (TeamFeatureStatusWithConfig cfg) where
  arbitrary = TeamFeatureStatusWithConfig <$> arbitrary <*> arbitrary <*> arbitrary

modelTeamFeatureStatusWithConfig :: TeamFeatureName -> Doc.Model -> Doc.Model
modelTeamFeatureStatusWithConfig name cfgModel = Doc.defineModel (cs $ show name) $ do
  Doc.description $ "Status and config of " <> cs (show name)
  Doc.property "payment-status" typeTeamFeatureStatusValue $ Doc.description "payment status"
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
  Doc.property "config" (Doc.ref cfgModel) $ Doc.description "config"

instance ToSchema cfg => ToSchema (TeamFeatureStatusWithConfig cfg) where
  schema =
    object "TeamFeatureStatusWithConfig" $
      TeamFeatureStatusWithConfig
        <$> tfwcStatus .= field "status" schema
        <*> error "TODO" -- tfwcPaymentStatus .= field "payment-status" schema
        <*> tfwcConfig .= field "config" schema

----------------------------------------------------------------------
-- TeamFeatureClassifiedDomainsConfig

newtype TeamFeatureClassifiedDomainsConfig = TeamFeatureClassifiedDomainsConfig
  { classifiedDomainsDomains :: [Domain]
  }
  deriving stock (Show, Eq, Generic)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema TeamFeatureClassifiedDomainsConfig)

deriving via (GenericUniform TeamFeatureClassifiedDomainsConfig) instance Arbitrary TeamFeatureClassifiedDomainsConfig

instance ToSchema TeamFeatureClassifiedDomainsConfig where
  schema =
    object "TeamFeatureClassifiedDomainsConfig" $
      TeamFeatureClassifiedDomainsConfig
        <$> classifiedDomainsDomains .= field "domains" (array schema)

modelTeamFeatureClassifiedDomainsConfig :: Doc.Model
modelTeamFeatureClassifiedDomainsConfig =
  Doc.defineModel "TeamFeatureClassifiedDomainsConfig" $ do
    Doc.property "domains" (Doc.array Doc.string') $ Doc.description "domains"

defaultClassifiedDomains :: TeamFeatureStatusWithConfig TeamFeatureClassifiedDomainsConfig
defaultClassifiedDomains = TeamFeatureStatusWithConfig TeamFeatureEnabled TeamFeatureDisabled (TeamFeatureClassifiedDomainsConfig [])

----------------------------------------------------------------------
-- TeamFeatureAppLockConfig

data TeamFeatureAppLockConfig = TeamFeatureAppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamFeatureAppLockConfig)

deriving via (GenericUniform TeamFeatureAppLockConfig) instance Arbitrary TeamFeatureAppLockConfig

instance ToSchema TeamFeatureAppLockConfig where
  schema =
    object "TeamFeatureAppLockConfig" $
      TeamFeatureAppLockConfig
        <$> applockEnforceAppLock .= field "enforceAppLock" schema
        <*> applockInactivityTimeoutSecs .= field "inactivityTimeoutSecs" schema

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (Arbitrary)
  deriving (FromJSON, ToJSON) via (Schema EnforceAppLock)

instance ToSchema EnforceAppLock where
  schema = EnforceAppLock <$> (\(EnforceAppLock v) -> v) .= schema

modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig =
  Doc.defineModel "TeamFeatureAppLockConfig" $ do
    Doc.property "enforceAppLock" Doc.bool' $ Doc.description "enforceAppLock"
    Doc.property "inactivityTimeoutSecs" Doc.int32' $ Doc.description ""

defaultAppLockStatus :: TeamFeatureStatusWithConfig TeamFeatureAppLockConfig
defaultAppLockStatus =
  TeamFeatureStatusWithConfig
    TeamFeatureEnabled
    TeamFeatureEnabled
    (TeamFeatureAppLockConfig (EnforceAppLock False) 60)

----------------------------------------------------------------------
-- TeamFeatureSelfDeletingMessagesConfig

data TeamFeatureSelfDeletingMessagesConfig = TeamFeatureSelfDeletingMessagesConfig
  { sdmEnforcedTimeoutSeconds :: Int32
  }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema TeamFeatureSelfDeletingMessagesConfig)
  deriving (Arbitrary) via (GenericUniform TeamFeatureSelfDeletingMessagesConfig)

instance ToSchema TeamFeatureSelfDeletingMessagesConfig where
  schema =
    object "TeamFeatureSelfDeletingMessagesConfig" $
      TeamFeatureSelfDeletingMessagesConfig
        <$> sdmEnforcedTimeoutSeconds .= field "enforcedTimeoutSeconds" schema

modelTeamFeatureSelfDeletingMessagesConfig :: Doc.Model
modelTeamFeatureSelfDeletingMessagesConfig =
  Doc.defineModel "TeamFeatureSelfDeletingMessagesConfig" $ do
    Doc.property "enforcedTimeoutSeconds" Doc.int32' $ Doc.description "optional; default: `0` (no enforcement)"

defaultSelfDeletingMessagesStatus :: TeamFeatureStatusWithConfig TeamFeatureSelfDeletingMessagesConfig
defaultSelfDeletingMessagesStatus =
  TeamFeatureStatusWithConfig
    TeamFeatureEnabled
    (TeamFeatureSelfDeletingMessagesConfig 0)

----------------------------------------------------------------------
-- internal

data LowerCaseFirst

instance StringModifier LowerCaseFirst where
  getStringModifier (x : xs) = toLower x : xs
  getStringModifier [] = []

newtype AllFeatureConfigs = AllFeatureConfigs {_allFeatureConfigs :: Aeson.Object}
  deriving stock (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via (Schema AllFeatureConfigs)

instance ToSchema AllFeatureConfigs where
  schema =
    named "AllFeatureConfigs" $
      dimap _allFeatureConfigs AllFeatureConfigs jsonObject
