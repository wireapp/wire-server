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
    TeamFeatureStatus (..),
    TeamFeatureAppLockConfig (..),
    TeamFeatureStatusValue (..),
    mkTeamFeatureStatusNoConfig,
    TeamFeatureConfig,

    -- * Swagger
    typeTeamFeatureName,
    modelTeamFeatureStatus,
    modelTeamFeatureAppLockConfig,
    typeTeamFeatureStatusValue,
  )
where

import Data.Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..), toByteString')
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Deriving.Aeson.Stock
import Imports
import Wire.API.Arbitrary (Arbitrary (arbitrary), GenericUniform (..))

data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  | TeamFeatureValidateSAMLEmails
  | TeamFeatureDigitalSignatures
  | TeamFeatureAppLock
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

type family TeamFeatureConfig (a :: TeamFeatureName) :: * where
  TeamFeatureConfig 'TeamFeatureLegalHold = ()
  TeamFeatureConfig 'TeamFeatureSSO = ()
  TeamFeatureConfig 'TeamFeatureSearchVisibility = ()
  TeamFeatureConfig 'TeamFeatureValidateSAMLEmails = ()
  TeamFeatureConfig 'TeamFeatureDigitalSignatures = ()
  TeamFeatureConfig 'TeamFeatureAppLock = TeamFeatureAppLockConfig

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

data TeamFeatureStatus a = TeamFeatureStatus
  { teamFeatureStatusValue :: TeamFeatureStatusValue,
    config :: a
  }

deriving stock instance Eq a => Eq (TeamFeatureStatus a)

deriving stock instance Show a => Show (TeamFeatureStatus a)

instance Arbitrary a => Arbitrary (TeamFeatureStatus a) where
  arbitrary = TeamFeatureStatus <$> arbitrary <*> arbitrary

mkTeamFeatureStatusNoConfig :: TeamFeatureStatusValue -> TeamFeatureStatus ()
mkTeamFeatureStatusNoConfig val = TeamFeatureStatus val ()

modelTeamFeatureStatus :: Doc.Model
modelTeamFeatureStatus = Doc.defineModel "TeamFeatureStatus" $ do
  Doc.description "Configuration of a feature for a team"
  Doc.property "status" typeTeamFeatureStatusValue $ Doc.description "status"
  -- TODO(stefan)
  Doc.property "config" undefined $ Doc.description "config"

instance {-# OVERLAPPABLE #-} ToJSON a => ToJSON (TeamFeatureStatus a) where
  toJSON (TeamFeatureStatus status config) =
    object
      [ "status" .= status,
        "config" .= config
      ]

instance {-# OVERLAPPING #-} ToJSON (TeamFeatureStatus ()) where
  toJSON (TeamFeatureStatus status _) =
    object
      ["status" .= status]

instance {-# OVERLAPPABLE #-} FromJSON a => FromJSON (TeamFeatureStatus a) where
  parseJSON = withObject "TeamFeatureStatus" $ \o ->
    TeamFeatureStatus <$> o .: "status" <*> o .: "config"

instance {-# OVERLAPPING #-} FromJSON (TeamFeatureStatus ()) where
  parseJSON = withObject "TeamFeatureStatus" $ \o ->
    TeamFeatureStatus <$> o .: "status" <*> pure ()

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

data TeamFeatureAppLockConfig = TeamFeatureAppLockConfig
  { teamFeatureAppLockConfigEnforceAppLock :: Bool,
    teamFeatureAppLockConfigInactivityTimeoutSecs :: Int
  }
  deriving stock (Eq, Show, Generic)

deriving via (GenericUniform TeamFeatureAppLockConfig) instance Arbitrary TeamFeatureAppLockConfig

deriving via (PrefixedSnake "teamFeatureAppLockConfig" TeamFeatureAppLockConfig) instance ToJSON TeamFeatureAppLockConfig

deriving via (PrefixedSnake "teamFeatureAppLockConfig" TeamFeatureAppLockConfig) instance FromJSON TeamFeatureAppLockConfig

-- TODO(stefan)
modelTeamFeatureAppLockConfig :: Doc.Model
modelTeamFeatureAppLockConfig = Doc.defineModel "TeamFeatureAppLockConfig" undefined
