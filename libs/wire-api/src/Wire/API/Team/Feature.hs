{-# LANGUAGE DerivingVia #-}
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

    -- * Swagger
    modelTeamFeatureStatus,
    typeTeamFeatureName,
    typeTeamFeatureStatus,
  )
where

import Data.Aeson
import qualified Data.Attoparsec.ByteString as Parser
import Data.ByteString.Conversion (FromByteString (..), ToByteString (..))
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

data TeamFeatureName
  = TeamFeatureLegalHold
  | TeamFeatureSSO
  | TeamFeatureSearchVisibility
  deriving stock (Eq, Show, Ord, Generic, Enum, Bounded)
  deriving (Arbitrary) via (GenericUniform TeamFeatureName)

instance FromByteString TeamFeatureName where
  parser = Parser.takeByteString >>= \b ->
    case T.decodeUtf8' b of
      Left e -> fail $ "Invalid TeamFeatureName: " <> show e
      Right "legalhold" -> pure TeamFeatureLegalHold
      Right "sso" -> pure TeamFeatureSSO
      Right "search-visibility" -> pure TeamFeatureSearchVisibility
      Right t -> fail $ "Invalid TeamFeatureName: " <> T.unpack t

instance ToByteString TeamFeatureName where
  builder TeamFeatureLegalHold = "legalhold"
  builder TeamFeatureSSO = "sso"
  builder TeamFeatureSearchVisibility = "search-visibility"

typeTeamFeatureName :: Doc.DataType
typeTeamFeatureName =
  Doc.string $
    Doc.enum
      [ "legalhold",
        "sso",
        "search-visibility"
      ]

data TeamFeatureStatus = TeamFeatureEnabled | TeamFeatureDisabled
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform TeamFeatureStatus)

modelTeamFeatureStatus :: Doc.Model
modelTeamFeatureStatus = Doc.defineModel "TeamFeatureStatus" $ do
  Doc.description "Configuration of a feature for a team"
  Doc.property "status" typeTeamFeatureStatus $ Doc.description "status"

typeTeamFeatureStatus :: Doc.DataType
typeTeamFeatureStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToJSON TeamFeatureStatus where
  toJSON status =
    object
      [ "status" .= case status of
          TeamFeatureEnabled -> String "enabled"
          TeamFeatureDisabled -> String "disabled"
      ]

instance FromJSON TeamFeatureStatus where
  parseJSON = withObject "TeamFeatureStatus" $ \o ->
    o .: "status"
      >>= \case
        "enabled" -> pure TeamFeatureEnabled
        "disabled" -> pure TeamFeatureDisabled
        x -> fail $ "unexpected status type: " <> T.unpack x
