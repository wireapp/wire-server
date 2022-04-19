{-# LANGUAGE StrictData #-}

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

module Wire.API.CustomBackend
  ( CustomBackend (..),
  )
where

import Control.Lens ((?~))
import Data.Misc (HttpsUrl)
import Data.Schema
import qualified Data.Swagger as S
import Deriving.Aeson
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

data CustomBackend = CustomBackend
  { backendConfigJsonUrl :: HttpsUrl,
    backendWebappWelcomeUrl :: HttpsUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CustomBackend)
  deriving (ToJSON, FromJSON, S.ToSchema) via (Schema CustomBackend)

instance ToSchema CustomBackend where
  schema =
    objectWithDocModifier "CustomBackend" (description ?~ "Description of a custom backend") $
      CustomBackend
        <$> backendConfigJsonUrl .= fieldWithDocModifier "config_json_url" (description ?~ "the location of the custom backend's config.json file") schema
        <*> backendWebappWelcomeUrl .= fieldWithDocModifier "webapp_welcome_url" (description ?~ "the location of the custom webapp") schema
