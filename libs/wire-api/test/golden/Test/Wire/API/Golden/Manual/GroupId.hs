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

module Test.Wire.API.Golden.Manual.GroupId where

import Data.Domain
import Data.Id
import Data.Qualified
import qualified Data.UUID as UUID
import Imports
import Wire.API.MLS.GroupId

convId1 :: Qualified ConvId
convId1 =
  Qualified
    (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
    (Domain "mydomain.com")

testObject_GroupId_1 :: GroupId
testObject_GroupId_1 = convIdToGroupId convId1

convId2 :: Qualified ConvId
convId2 =
  Qualified
    (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")))
    (Domain "abcdef.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.testObject_GroupId_2.com")

testObject_GroupId_2 :: GroupId
testObject_GroupId_2 = convIdToGroupId convId2
