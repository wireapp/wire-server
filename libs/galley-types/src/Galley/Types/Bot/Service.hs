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

module Galley.Types.Bot.Service
  ( Service (..),
    newService,
    serviceRef,
    serviceUrl,
    serviceToken,
    serviceFingerprints,
    serviceEnabled,

    -- * re-exports
    ServiceToken (..),
    ServiceRef (..),
    newServiceRef,
    serviceRefId,
    serviceRefProvider,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Misc (Fingerprint, HttpsUrl, Rsa)
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

makeLenses ''Service

newService :: ServiceRef -> HttpsUrl -> ServiceToken -> [Fingerprint Rsa] -> Service
newService ref url tok fps = Service ref url tok fps True

instance FromJSON Service where
  parseJSON = withObject "Service" $ \o ->
    Service <$> o .: "ref"
      <*> o .: "base_url"
      <*> o .: "auth_token"
      <*> o .: "fingerprints"
      <*> o .: "enabled"

instance ToJSON Service where
  toJSON s =
    object
      [ "ref" .= _serviceRef s,
        "base_url" .= _serviceUrl s,
        "auth_token" .= _serviceToken s,
        "fingerprints" .= _serviceFingerprints s,
        "enabled" .= _serviceEnabled s
      ]
