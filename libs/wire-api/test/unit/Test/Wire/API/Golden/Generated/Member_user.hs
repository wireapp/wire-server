-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.Member_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation (Member (..), MutedStatus (MutedStatus, fromMutedStatus))
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

testObject_Member_user_1 :: Member
testObject_Member_user_1 =
  Member
    { memId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000")),
      memService =
        Just
          ( ServiceRef
              { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
              }
          ),
      memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -2}),
      memOtrMutedRef = Just "ref",
      memOtrArchived = False,
      memOtrArchivedRef = Just "\140694",
      memHidden = True,
      memHiddenRef = Just "\1032750",
      memConvRoleName =
        fromJust (parseRoleName "q4g4_8r4m6hz7hx5ob32nexko2ntb3dmv5vogdmm8dhbwzei6rv45b_90kzg11gw6zsq")
    }

testObject_Member_user_2 :: Member
testObject_Member_user_2 =
  Member
    { memId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002")),
      memService = Nothing,
      memOtrMutedStatus = Nothing,
      memOtrMutedRef = Nothing,
      memOtrArchived = True,
      memOtrArchivedRef = Nothing,
      memHidden = True,
      memHiddenRef = Nothing,
      memConvRoleName =
        fromJust (parseRoleName "cxrqwei1me7ftnrql1p2ew9aj1c5um89xip09ymj6wyj5cqfc4s903yxpv9e5j1j_8744acstc_a")
    }
