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
module Test.Wire.API.Golden.Generated.MemberUpdateData_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation (MutedStatus (MutedStatus, fromMutedStatus))
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Event.Conversation (MemberUpdateData (..))

testObject_MemberUpdateData_user_1 :: MemberUpdateData
testObject_MemberUpdateData_user_1 =
  MemberUpdateData
    { misTarget =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")))
          (Domain "target.example.com"),
      misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}),
      misOtrMutedRef = Just "#M\95696",
      misOtrArchived = Just False,
      misOtrArchivedRef = Just "a",
      misHidden = Just True,
      misHiddenRef = Just "1",
      misConvRoleName = Nothing
    }

testObject_MemberUpdateData_user_2 :: MemberUpdateData
testObject_MemberUpdateData_user_2 =
  MemberUpdateData
    { misTarget =
        Qualified
          (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")))
          (Domain "target.example.com"),
      misOtrMutedStatus = Nothing,
      misOtrMutedRef = Nothing,
      misOtrArchived = Nothing,
      misOtrArchivedRef = Nothing,
      misHidden = Nothing,
      misHiddenRef = Nothing,
      misConvRoleName =
        Just
          ( fromJust
              ( parseRoleName
                  "3fwjaofhryb7nd1hp3nwukjiyxxhgimw8ddzx5s_8ek5nnctkzkic6w51hqugeh6l50hg87dez8pw974dbuywd83njuytv0euf9619s"
              )
          )
    }
