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

import Data.Aeson
import Imports
import System.Logger.Extended
import Util.Options

data Opts
  = Opts
      { -- | Host and port
        federator :: Endpoint,
        -- | Log level (Debug, Info, etc)
        logLevel :: Level,
        -- | Use netstrings encoding (see <http://cr.yp.to/proto/netstrings.txt>)
        logNetStrings :: Maybe (Last Bool),
        -- | Logformat to use
        logFormat :: !(Maybe (Last LogFormat))
      }
  deriving (Show, Generic)

instance FromJSON Opts
