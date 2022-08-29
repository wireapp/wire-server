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

module Wire.API.Federation.API.Common where

import Data.Aeson
import Imports
import Test.QuickCheck
import Wire.Arbitrary

-- | This is equivalent to '()', but JSONifies to an empty object instead of an
-- empty array.
data EmptyResponse = EmptyResponse
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform EmptyResponse)

instance FromJSON EmptyResponse where
  parseJSON = withObject "EmptyResponse" . const $ pure EmptyResponse

instance ToJSON EmptyResponse where
  toJSON EmptyResponse = object []
