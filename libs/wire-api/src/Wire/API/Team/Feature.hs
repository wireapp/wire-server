{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

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
  ( -- * LegalHoldTeamConfig
    LegalHoldTeamConfig (..),
    LegalHoldStatus (..),

    -- * SSOTeamConfig
    SSOTeamConfig (..),
    SSOStatus (..),

    -- * Swagger
    modelLegalHoldTeamConfig,
    modelSsoTeamConfig,
  )
where

import Data.Aeson
import Data.Json.Util ((#))
import qualified Data.Swagger.Build.Api as Doc
import qualified Data.Text as T
import Imports

--------------------------------------------------------------------------------
-- LegalHoldTeamConfig

data LegalHoldTeamConfig = LegalHoldTeamConfig
  { legalHoldTeamConfigStatus :: !LegalHoldStatus
  }
  deriving stock (Eq, Show, Generic)

modelLegalHoldTeamConfig :: Doc.Model
modelLegalHoldTeamConfig = Doc.defineModel "LegalHoldTeamConfig" $ do
  Doc.description "Configuration of LegalHold feature for team"
  Doc.property "status" typeFeatureStatus $ Doc.description "status"

instance ToJSON LegalHoldTeamConfig where
  toJSON s =
    object $
      "status" .= legalHoldTeamConfigStatus s
        # []

instance FromJSON LegalHoldTeamConfig where
  parseJSON = withObject "LegalHoldTeamConfig" $ \o ->
    LegalHoldTeamConfig <$> o .: "status"

data LegalHoldStatus = LegalHoldDisabled | LegalHoldEnabled
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

typeFeatureStatus :: Doc.DataType
typeFeatureStatus =
  Doc.string $
    Doc.enum
      [ "enabled",
        "disabled"
      ]

instance ToJSON LegalHoldStatus where
  toJSON LegalHoldEnabled = "enabled"
  toJSON LegalHoldDisabled = "disabled"

instance FromJSON LegalHoldStatus where
  parseJSON = withText "LegalHoldStatus" $ \case
    "enabled" -> pure LegalHoldEnabled
    "disabled" -> pure LegalHoldDisabled
    x -> fail $ "unexpected status type: " <> T.unpack x

--------------------------------------------------------------------------------
-- SSOTeamConfig

data SSOTeamConfig = SSOTeamConfig
  { ssoTeamConfigStatus :: !SSOStatus
  }
  deriving stock (Eq, Show, Generic)

modelSsoTeamConfig :: Doc.Model
modelSsoTeamConfig = Doc.defineModel "SSOTeamConfig" $ do
  Doc.description "Configuration of SSO feature for team"
  Doc.property "status" typeFeatureStatus $ Doc.description "status"

instance ToJSON SSOTeamConfig where
  toJSON s =
    object $
      "status" .= ssoTeamConfigStatus s
        # []

instance FromJSON SSOTeamConfig where
  parseJSON = withObject "SSOTeamConfig" $ \o ->
    SSOTeamConfig <$> o .: "status"

data SSOStatus = SSODisabled | SSOEnabled
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

-- also uses the modelFeatureStatus Swagger doc

instance ToJSON SSOStatus where
  toJSON SSOEnabled = "enabled"
  toJSON SSODisabled = "disabled"

instance FromJSON SSOStatus where
  parseJSON = withText "SSOStatus" $ \case
    "enabled" -> pure SSOEnabled
    "disabled" -> pure SSODisabled
    x -> fail $ "unexpected status type: " <> T.unpack x
