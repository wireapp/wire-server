{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
    TeamFeatureStatusValue (..),
    FeatureHasNoConfig,
    EnforceAppLock (..),
    KnownTeamFeatureName (..),
    TeamFeatureStatusNoConfig (..),
    TeamFeatureStatusWithConfig (..),
    deprecatedFeatureName,

    -- * Swagger
    typeTeamFeatureName,
    typeTeamFeatureStatusValue,
    modelTeamFeatureStatusNoConfig,
    modelTeamFeatureStatusWithConfig,
    modelTeamFeatureAppLockConfig,
    modelForTeamFeature,
  )
where

import Data.Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), toByteString')
import Data.Kind (Constraint)
import Data.String.Conversions (cs)
import Data.Swagger.Build.Api
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import Imports
import Test.QuickCheck.Arbitrary (arbitrary)
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

----------------------------------------------------------------------
-- TeamFeatureName

data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  | TeamFeatureValidateSAMLEmails
  | TeamFeatureDigitalSignatures
  | TeamFeatureAppLock
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded, Typeable)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

class KnownTeamFeatureName (a :: TeamFeatureName) where
  knownTeamFeatureName :: TeamFeatureName

instance KnownTeamFeatureName 'TeamFeatureLegalHold where knownTeamFeatureName = TeamFeatureLegalHold

instance KnownTeamFeatureName 'TeamFeatureSSO where knownTeamFeatureName = TeamFeatureSSO

instance KnownTeamFeatureName 'TeamFeatureSearchVisibility where knownTeamFeatureName = TeamFeatureSearchVisibility

instance KnownTeamFeatureName 'TeamFeatureValidateSAMLEmails where knownTeamFeatureName = TeamFeatureValidateSAMLEmails

instance KnownTeamFeatureName 'TeamFeatureDigitalSignatures where knownTeamFeatureName = TeamFeatureDigitalSignatures

instance KnownTeamFeatureName 'TeamFeatureAppLock where knownTeamFeatureName = TeamFeatureAppLock

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
        Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "searchVisibility"
  builder TeamFeatureValidateSAMLEmails = "validateSAMLemails"
  builder TeamFeatureDigitalSignatures = "digitalSignatures"
  builder TeamFeatureAppLock = "appLock"

deprecatedFeatureName :: TeamFeatureName -> Maybe ByteString
deprecatedFeatureName TeamFeatureSearchVisibility = Just "search-visibility"
deprecatedFeatureName TeamFeatureValidateSAMLEmails = Just "validate-saml-emails"
deprecatedFeatureName TeamFeatureDigitalSignatures = Just "digital-signatures"
deprecatedFeatureName _ = Nothing

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName = Doc.string . Doc.enum $ cs . toByteString' <$> [(minBound :: TeamFeatureName) ..]

----------------------------------------------------------------------
-- TeamFeatureStatusValue

data TeamFeatureStatusValue
  = TeamFeatureEnabled
  | TeamFeatureDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamFeatureStatusValue)

typeTeamFeatureStatusValue :: Doc.DataType
typeTeamFeatureStatusValue =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToJSON TeamFeatureStatusValue where
  toJSON = \case
    TeamFeatureEnabled -> String "enabled"
    TeamFeatureDisabled -> String "disabled"

instance FromJSON TeamFeatureStatusValue where
  parseJSON = withText "TeamFeatureStatusValue" $ \case
    "enabled" -> pure TeamFeatureEnabled
    "disabled" -> pure TeamFeatureDisabled
    x -> fail $ "unexpected status type: " <> T.unpack x

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

----------------------------------------------------------------------
-- TeamFeatureStatus

type family TeamFeatureStatus (a :: TeamFeatureName) :: * where
  TeamFeatureStatus 'TeamFeatureLegalHold = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureSSO = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureSearchVisibility = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureValidateSAMLEmails = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureDigitalSignatures = TeamFeatureStatusNoConfig
  TeamFeatureStatus 'TeamFeatureAppLock = TeamFeatureStatusWithConfig TeamFeatureAppLockConfig

type FeatureHasNoConfig (a :: TeamFeatureName) = (TeamFeatureStatus a ~ TeamFeatureStatusNoConfig) :: Constraint

-- if you add a new constructor here, don't forget to add it to the swagger (1.2) docs in "Wire.API.Swagger"!
modelForTeamFeature :: TeamFeatureName -> Doc.Model
modelForTeamFeature TeamFeatureLegalHold = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSSO = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSearchVisibility = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureValidateSAMLEmails = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureDigitalSignatures = modelTeamFeatureStatusNoConfig
modelForTeamFeature name@TeamFeatureAppLock = modelTeamFeatureStatusWithConfig name modelTeamFeatureAppLockConfig

----------------------------------------------------------------------
-- TeamFeatureStatusNoConfig

newtype TeamFeatureStatusNoConfig = TeamFeatureStatusNoConfig
  { featureStatus :: TeamFeatureStatusValue
  }
  deriving newtype (Eq, Show, Generic, Typeable, Arbitrary)

modelTeamFeatureStatusNoConfig :: Doc.Model
modelTeamFeatureStatusNoConfig = Doc.defineModel "TeamFeatureStatusNoConfig" $ do
  Doc.description $ "Configuration for a team feature that has no configuration"
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"

instance FromJSON TeamFeatureStatusNoConfig where
  parseJSON = withObject "TeamFeatureStatus" $ \ob ->
    TeamFeatureStatusNoConfig <$> ob .: "status"

instance ToJSON TeamFeatureStatusNoConfig where
  toJSON (TeamFeatureStatusNoConfig status) = object ["status" .= status]

----------------------------------------------------------------------
-- TeamFeatureStatusWithConfig

data TeamFeatureStatusWithConfig (cfg :: *) = TeamFeatureStatusWithConfig
  { featureStatus :: TeamFeatureStatusValue,
    tfwcConfig :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable)

instance Arbitrary cfg => Arbitrary (TeamFeatureStatusWithConfig cfg) where
  arbitrary = TeamFeatureStatusWithConfig <$> arbitrary <*> arbitrary

modelTeamFeatureStatusWithConfig :: TeamFeatureName -> Doc.Model -> Doc.Model
modelTeamFeatureStatusWithConfig name cfgModel = Doc.defineModel (cs $ show name) $ do
  Doc.description $ "Status and config of " <> (cs $ show name)
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
  Doc.property "config" (Doc.ref cfgModel) $ Doc.description "config"

instance FromJSON cfg => FromJSON (TeamFeatureStatusWithConfig cfg) where
  parseJSON = withObject "TeamFeatureStatus" $ \ob ->
    TeamFeatureStatusWithConfig <$> ob .: "status" <*> ob .: "config"

instance ToJSON cfg => ToJSON (TeamFeatureStatusWithConfig cfg) where
  toJSON (TeamFeatureStatusWithConfig status config) = object ["status" .= status, "config" .= config]

----------------------------------------------------------------------
-- TeamFeatureAppLockConfig

data TeamFeatureAppLockConfig = TeamFeatureAppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)

deriving via (GenericUniform TeamFeatureAppLockConfig) instance Arbitrary TeamFeatureAppLockConfig

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, Arbitrary)

modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig =
  Doc.defineModel "TeamFeatureAppLockConfig" $ do
    Doc.property "enforceAppLock" bool' $ Doc.description "enforceAppLock"
    Doc.property "inactivityTimeoutSecs" int32' $ Doc.description ""

deriving via
  (StripCamel "applock" TeamFeatureAppLockConfig)
  instance
    ToJSON TeamFeatureAppLockConfig

deriving via
  (StripCamel "applock" TeamFeatureAppLockConfig)
  instance
    FromJSON TeamFeatureAppLockConfig

----------------------------------------------------------------------
-- internal

data LowerCaseFirst

instance StringModifier LowerCaseFirst where
  getStringModifier (x : xs) = toLower x : xs
  getStringModifier [] = []

type StripCamel str =
  CustomJSON
    '[FieldLabelModifier (StripPrefix str, LowerCaseFirst)]
