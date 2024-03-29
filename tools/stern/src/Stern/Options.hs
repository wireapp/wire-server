{-# LANGUAGE DeriveGeneric #-}

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

module Stern.Options where

import Data.Yaml (FromJSON (..))
import GHC.Generics
import Imports
import System.Logger.Extended (Level, LogFormat)
import Util.Options

-- | Options that are consumed on startup
data Opts = Opts
  { stern :: !Endpoint,
    brig :: !Endpoint,
    galley :: !Endpoint,
    gundeck :: !Endpoint,
    -- TODO: Both ibis and galeb should be made optional
    --       for installations where these services are not available
    ibis :: !Endpoint,
    galeb :: !Endpoint,
    -- Logging
    logLevel :: !Level,
    logNetStrings :: !(Maybe (Last Bool)),
    logFormat :: !(Maybe (Last LogFormat))
  }
  deriving (Show, Generic)

instance FromJSON Opts
