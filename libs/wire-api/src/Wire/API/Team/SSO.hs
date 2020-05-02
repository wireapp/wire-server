{-# LANGUAGE OverloadedStrings #-}
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

module Wire.API.Team.SSO where

import Data.Aeson
import Data.Json.Util
import qualified Data.Text as T
import Imports

-- TODO move to Team.Feature
data SSOStatus = SSODisabled | SSOEnabled
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON SSOStatus where
  toJSON SSOEnabled = "enabled"
  toJSON SSODisabled = "disabled"

instance FromJSON SSOStatus where
  parseJSON = withText "SSOStatus" $ \case
    "enabled" -> pure SSOEnabled
    "disabled" -> pure SSODisabled
    x -> fail $ "unexpected status type: " <> T.unpack x

data SSOTeamConfig = SSOTeamConfig
  { ssoTeamConfigStatus :: !SSOStatus
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SSOTeamConfig where
  toJSON s =
    object $
      "status" .= ssoTeamConfigStatus s
        # []

instance FromJSON SSOTeamConfig where
  parseJSON = withObject "SSOTeamConfig" $ \o ->
    SSOTeamConfig <$> o .: "status"
