{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Wire.API.CustomBackend
  ( CustomBackend (..),
  )
where

import Data.Aeson
import Data.Json.Util ((#))
import Data.Misc (HttpsUrl)
import Imports

data CustomBackend = CustomBackend
  { backendConfigJsonUrl :: !HttpsUrl,
    backendWebappWelcomeUrl :: !HttpsUrl
  }
  deriving (Eq, Show)

-- Instances ----------------------------------------------------------------

-- JSON
--
instance ToJSON CustomBackend where
  toJSON j =
    object $
      "config_json_url" .= backendConfigJsonUrl j
        # "webapp_welcome_url" .= backendWebappWelcomeUrl j
        # []

instance FromJSON CustomBackend where
  parseJSON = withObject "CustomBackend" $ \o ->
    CustomBackend
      <$> o .: "config_json_url"
      <*> o .: "webapp_welcome_url"
