{-# LANGUAGE DeriveAnyClass #-}
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

module Federator.Options where

import qualified Control.Lens as Lens
import Data.Aeson
import Data.Domain (Domain ())
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options

newtype AllowedDomains = AllowedDomains {allowedDomains :: [Domain]}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

-- FUTUREWORK: Support a DenyList
data FederationStrategy
  = -- | This backend allows federating with any other Wire-Server backend
    AllowAll
  | -- | Any backend explicitly configured in a FederationAllowList
    AllowList AllowedDomains
  deriving (Eq, Show, Generic)

instance ToJSON FederationStrategy where
  toJSON AllowAll =
    object
      [ "allowAll" .= object []
      ]
  toJSON (AllowList domains) =
    object
      [ "allowedDomains" .= domains
      ]

instance FromJSON FederationStrategy where
  parseJSON = withObject "FederationStrategy" $ \o -> do
    -- Only inspect field content once we committed to one, for better error messages.
    allowAll :: Maybe Value <- o .:! "allowAll"
    allowList :: Maybe Value <- o .:! "allowedDomains"
    case (allowAll, allowList) of
      (Just _, Nothing) -> pure AllowAll -- accept any content
      (Nothing, Just l) -> AllowList <$> parseJSON l
      _ -> fail "invalid FederationStrategy: expected either allowAll or allowedDomains"

-- | Options that persist as runtime settings.
newtype RunSettings = RunSettings
  { -- | Would you like to federate with everyone or only with a select set of other wire-server installations?
    setFederationStrategy :: FederationStrategy
  }
  deriving (Show, Generic)

instance FromJSON RunSettings

data Opts = Opts
  { -- | Host and port for internal endpoint
    federatorInternal :: Endpoint,
    -- | Host and port for external endpoint
    federatorExternal :: Endpoint,
    -- | Host and port of brig
    brig :: Endpoint,
    -- | Log level (Debug, Info, etc)
    logLevel :: Level,
    -- | Use netstrings encoding (see <http://cr.yp.to/proto/netstrings.txt>)
    logNetStrings :: Maybe (Last Bool),
    -- | Logformat to use
    logFormat :: !(Maybe (Last LogFormat)),
    -- | Runtime settings
    optSettings :: !RunSettings
  }
  deriving (Show, Generic)

instance FromJSON Opts

Lens.makeLensesFor
  [ ("setFederationStrategy", "federationStrategy")
  ]
  ''RunSettings
