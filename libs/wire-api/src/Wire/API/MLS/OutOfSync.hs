-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.OutOfSync (EnableOutOfSyncCheck (..)) where

import Data.Aeson
import Data.Default
import Data.OpenApi
import Imports
import Wire.API.Util.Aeson
import Wire.Arbitrary

data EnableOutOfSyncCheck = EnableOutOfSyncCheck | DisableOutOfSyncCheck
  deriving (Eq, Show, Generic)
  deriving (Arbitrary) via GenericUniform EnableOutOfSyncCheck
  deriving (ToJSON, FromJSON) via CustomEncoded EnableOutOfSyncCheck

instance Default EnableOutOfSyncCheck where
  def = DisableOutOfSyncCheck

instance ToSchema EnableOutOfSyncCheck
