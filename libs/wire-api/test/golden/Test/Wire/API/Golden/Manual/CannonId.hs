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

module Test.Wire.API.Golden.Manual.CannonId
  ( testObject_CannonId_1,
    testObject_CannonId_2,
    testObject_CannonId_3,
  )
where

import Data.Aeson
import Imports

newtype CannonId = CannonId {cannonId :: Text}
  deriving (Eq, Show, FromJSON, ToJSON)

testObject_CannonId_1 :: CannonId
testObject_CannonId_1 = CannonId ""

testObject_CannonId_2 :: CannonId
testObject_CannonId_2 = CannonId "sdfiou"

testObject_CannonId_3 :: CannonId
testObject_CannonId_3 = CannonId "1!_*`'\""
