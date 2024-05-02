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

module Wire.API.Bot.Service
  ( Service (..),
    newService,
    serviceRef,
    serviceUrl,
    serviceToken,
    serviceFingerprints,
    serviceEnabled,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
import Data.OpenApi qualified as S
import Data.Schema
import Imports
import Wire.API.Provider.Service hiding (Service (..))

-- Service --------------------------------------------------------------------

-- | Internal service connection information that is needed by galley.
data Service = Service
  { _serviceRef :: !ServiceRef,
    _serviceUrl :: !HttpsUrl,
    _serviceToken :: !ServiceToken,
    _serviceFingerprints :: ![Fingerprint Rsa],
    _serviceEnabled :: !Bool
  }
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema Service

newService :: ServiceRef -> HttpsUrl -> ServiceToken -> [Fingerprint Rsa] -> Service
newService ref url tok fps = Service ref url tok fps True

instance ToSchema Service where
  schema =
    object "BotService" $
      Service
        <$> _serviceRef .= field "ref" schema
        <*> _serviceUrl .= field "base_url" schema
        <*> _serviceToken .= field "auth_token" schema
        <*> _serviceFingerprints .= field "fingerprints" (array schema)
        <*> _serviceEnabled .= field "enabled" schema

makeLenses ''Service
