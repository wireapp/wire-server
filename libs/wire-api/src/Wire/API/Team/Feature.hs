{-# LANGUAGE DerivingVia #-}
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
    FeatureHasNoConfig (..),
    EnforceAppLock (..),
    KnownTeamFeatureName (..),
    TeamFeatureStatusWithConfig (..),

    -- * Swagger
    typeTeamFeatureName,
    typeTeamFeatureStatusValue,
    modelTeamFeatureStatusNoConfig,
    modelTeamFeatureStatusAppLock,
    modelTeamFeatureAppLockConfig,
    modelForTeamFeature,
  )
where

import Data.Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), toByteString')
import Data.String.Conversions (cs)
import Data.Swagger.Build.Api
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

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
        Right "search-visibility" -> pure TeamFeatureSearchVisibility
        Right "validate-saml-emails" -> pure TeamFeatureValidateSAMLEmails
        Right "digital-signatures" -> pure TeamFeatureDigitalSignatures
        Right "app-lock" -> pure TeamFeatureAppLock
        Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "search-visibility"
  builder TeamFeatureValidateSAMLEmails = "validate-saml-emails"
  builder TeamFeatureDigitalSignatures = "digital-signatures"
  builder TeamFeatureAppLock = "app-lock"

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName = Doc.string . Doc.enum $ cs . toByteString' <$> [(minBound :: TeamFeatureName) ..]

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

-- TODO TeamFeatureStatus -> TeamFeatureConfig
type family TeamFeatureStatus (a :: TeamFeatureName) :: * where
-- TODO rename TeamFeatureStatusValue -> TeamFeatureStatus
  TeamFeatureStatus 'TeamFeatureLegalHold = TeamFeatureStatusValue
  TeamFeatureStatus 'TeamFeatureSSO = TeamFeatureStatusValue
  TeamFeatureStatus 'TeamFeatureSearchVisibility = TeamFeatureStatusValue
  TeamFeatureStatus 'TeamFeatureValidateSAMLEmails = TeamFeatureStatusValue
  TeamFeatureStatus 'TeamFeatureDigitalSignatures = TeamFeatureStatusValue
  TeamFeatureStatus 'TeamFeatureAppLock = TeamFeatureStatusWithConfig TeamFeatureAppLockConfig

data TeamFeatureStatusWithConfig (cfg :: *) = TeamFeatureStatusWithConfig
  { tfwcStatus :: TeamFeatureStatusValue,
    tfwcConfig :: cfg
  }
  deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON cfg => FromJSON (TeamFeatureStatusWithConfig cfg) where
  parseJSON = withObject "TeamFeatureStatus" $ \ob ->
    TeamFeatureStatus <$> ob .: "status" <*> ob .: "config"

modelTeamFeatureStatusNoConfig :: Doc.Model
modelTeamFeatureStatusNoConfig =
  Doc.defineModel "FeatureStatusNoConfig" $ do
    Doc.description $ "Configuration for a team feature that has no configuration"
    Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"

modelForTeamFeature :: TeamFeatureName -> Doc.Model
modelForTeamFeature TeamFeatureLegalHold = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSSO = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureSearchVisibility = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureValidateSAMLEmails = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureDigitalSignatures = modelTeamFeatureStatusNoConfig
modelForTeamFeature TeamFeatureAppLock = modelTeamFeatureStatusAppLock

class FeatureHasNoConfig (a :: TeamFeatureName) where
  mkFeatureStatusNoConfig :: TeamFeatureStatusValue -> TeamFeatureStatus a

instance FeatureHasNoConfig 'TeamFeatureLegalHold where
  mkFeatureStatusNoConfig = id

instance FeatureHasNoConfig 'TeamFeatureSSO where
  mkFeatureStatusNoConfig = id

instance FeatureHasNoConfig 'TeamFeatureSearchVisibility where
  mkFeatureStatusNoConfig = id

instance FeatureHasNoConfig 'TeamFeatureValidateSAMLEmails where
  mkFeatureStatusNoConfig = id

instance FeatureHasNoConfig 'TeamFeatureDigitalSignatures where
  mkFeatureStatusNoConfig = id

-- instance FeatureHasNoConfig a => FromJSON (JSONNoConfig (TeamFeatureStatus a)) where
--   parseJSON = withObject "TeamFeatureStatus" $ \ob ->
--     JSONNoConfig . mkFeatureStatus @a <$> ob .: "status"

-- instance FeatureHasNoConfig a => ToJSON (JSONNoConfig (TeamFeatureStatus a)) where
--   toJSON (JSONNoConfig status) = object ["status" .= status]

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureLegalHold)) instance ToJSON (TeamFeatureStatus 'TeamFeatureLegalHold)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureLegalHold)) instance FromJSON (TeamFeatureStatus 'TeamFeatureLegalHold)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureSSO)) instance ToJSON (TeamFeatureStatus 'TeamFeatureSSO)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureSSO)) instance FromJSON (TeamFeatureStatus 'TeamFeatureSSO)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureSearchVisibility)) instance ToJSON (TeamFeatureStatus 'TeamFeatureSearchVisibility)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureSearchVisibility)) instance FromJSON (TeamFeatureStatus 'TeamFeatureSearchVisibility)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureValidateSAMLEmails)) instance ToJSON (TeamFeatureStatus 'TeamFeatureValidateSAMLEmails)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureValidateSAMLEmails)) instance FromJSON (TeamFeatureStatus 'TeamFeatureValidateSAMLEmails)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureDigitalSignatures)) instance ToJSON (TeamFeatureStatus 'TeamFeatureDigitalSignatures)

-- deriving via (JSONNoConfig (TeamFeatureStatus 'TeamFeatureDigitalSignatures)) instance FromJSON (TeamFeatureStatus 'TeamFeatureDigitalSignatures)

modelTeamFeatureStatusAppLock :: Doc.Model
modelTeamFeatureStatusAppLock =
  Doc.defineModel "TeamFeatureAppLockStatus" $ do
    Doc.description "Configuration of a the AppLock team feature"
    Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
    Doc.property "config" (Doc.ref modelTeamFeatureAppLockConfig) $ Doc.description "config"

modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig =
  Doc.defineModel "TeamFeatureAppLockConfig" $ do
    Doc.property "enforceAppLock" bool' $ Doc.description "enforceAppLock"
    Doc.property "inactivityTimeoutSecs" int32' $ Doc.description ""

-- instance FromJSON (TeamFeatureStatus 'TeamFeatureAppLock) where
--   parseJSON = withObject "TeamFeatureStatus" $ \ob ->
--     TeamFeatureStatus <$> ob .: "status" <*> ob .: "config"

newtype EnforceAppLock = EnforceAppLock Bool
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (FromJSON, ToJSON, Arbitrary)

data TeamFeatureAppLockStatus = TeamFeatureAppLockStatus
  { appLockStatus :: TeamFeatureStatusValue,
    appLockConfig :: TeamFeatureAppLockConfig
  }
  deriving stock (Eq, Show, Generic)

data TeamFeatureAppLockConfig = TeamFeatureAppLockConfig
  { applockEnforceAppLock :: EnforceAppLock,
    applockInactivityTimeoutSecs :: Int32
  }
  deriving stock (Eq, Show, Generic)

deriving via (GenericUniform TeamFeatureAppLockConfig) instance Arbitrary TeamFeatureAppLockConfig

instance ToJSON TeamFeatureAppLockStatus where
  toJSON (TeamFeatureAppLockStatus status config) = object ["status" .= status, "config" .= config]

data LowerCaseFirst

instance StringModifier LowerCaseFirst where
  getStringModifier (x : xs) = toLower x : xs
  getStringModifier [] = []

type StripCamel str =
  CustomJSON
    '[FieldLabelModifier (StripPrefix str, LowerCaseFirst)]

deriving via
  (StripCamel "applock" TeamFeatureAppLockConfig)
  instance
    ToJSON TeamFeatureAppLockConfig

deriving via
  (StripCamel "applock" TeamFeatureAppLockConfig)
  instance
    FromJSON TeamFeatureAppLockConfig
