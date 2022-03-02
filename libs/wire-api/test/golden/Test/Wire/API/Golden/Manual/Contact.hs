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

module Test.Wire.API.Golden.Manual.Contact where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified))
import qualified Data.UUID as UUID
import Imports
import Wire.API.User.Search (Contact (..))

testObject_Contact_1 :: Contact
testObject_Contact_1 =
  Contact
    { contactQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))) (Domain "example.com"),
      contactName = "Foobar",
      contactColorId = Just 1,
      contactHandle = Just "foobar1",
      contactTeam = Just $ Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000002"))
    }

testObject_Contact_2 :: Contact
testObject_Contact_2 =
  Contact
    { contactQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000018-0000-0020-0000-000e00000003"))) (Domain "another.example.com"),
      contactName = "Foobar2",
      contactColorId = Nothing,
      contactHandle = Nothing,
      contactTeam = Nothing
    }
