{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Data.Aeson as Aeson
import Data.Domain (Domain ())
import Imports
import System.Logger.Extended
import Util.Options

newtype FederationAllowedDomains = FederationAllowedDomains {allowedDomains :: [Domain]}
  deriving (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

defFederationAllowedDomains :: FederationAllowedDomains
defFederationAllowedDomains = FederationAllowedDomains []

-- FUTUREWORK: Support a DenyList
data FederationStrategy
  = -- | This backend allows federating with any other Wire-Server backend
    WithEveryone
  | -- | Any backend explicitly configured in a FederationAllowList
    WithAllowList
  deriving (Eq, Show, Generic, Bounded, Enum)

instance ToJSON FederationStrategy where
  toJSON WithEveryone = "with_everyone"
  toJSON WithAllowList = "with_allow_list"

instance FromJSON FederationStrategy where
  parseJSON = withText "FederationStrategy" $ \case
    "with_everyone" -> pure WithEveryone
    "with_allow_list" -> pure $ WithAllowList
    _ ->
      fail $
        "unexpected value for FederationStrategy settings: "
          <> "expected one of "
          <> show (Aeson.encode <$> [(minBound :: FederationStrategy) ..])

-- | Options that persist as runtime settings.
data RunSettings = RunSettings
  { -- | Would you like to federate with everyone or only with a select set of other wire-server installations?
    setFederationStrategy :: !(FederationStrategy),
    -- | setFederationAllowedDomains only has an effect if 'setFederationStrategy' is with_allow_list
    -- (defaults to empty list if unset)
    setFederationAllowedDomains :: !(Maybe FederationAllowedDomains)
  }
  deriving (Show, Generic)

instance FromJSON RunSettings

data Opts = Opts
  { -- | Host and port
    federator :: Endpoint,
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
  [ ("setFederationStrategy", "federationStrategy"),
    ("setFederationAllowedDomains", "federationAllowedDomains")
  ]
  ''RunSettings
