{-# LANGUAGE OverloadedLists #-}

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

module Test.Wire.API.Golden.Generated.NewConv_user where

import Data.Id
import Data.Misc (Milliseconds (Ms, ms))
import Data.Set qualified as Set (fromList)
import Data.UUID qualified as UUID (fromString)
import Imports
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.User

testObject_NewConv_user_1 :: NewConv
testObject_NewConv_user_1 =
  NewConv
    { newConvUsers =
        [ Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
          Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
        ],
      newConvQualifiedUsers = [],
      newConvName = Nothing,
      newConvAccess = Set.fromList [PrivateAccess, InviteAccess],
      newConvAccessRoles = Just $ Set.fromList [TeamMemberAccessRole, GuestAccessRole],
      newConvTeam =
        Just
          ( ConvTeamInfo
              { cnvTeamId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))
              }
          ),
      newConvMessageTimer = Just (Ms {ms = 3320987366258987}),
      newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
      newConvUsersRole = fromJust (parseRoleName "8tp2gs7b6"),
      newConvProtocol = BaseProtocolProteusTag,
      newConvGroupConvType = GroupConversation,
      newConvCells = False
    }

testObject_NewConv_user_3 :: NewConv
testObject_NewConv_user_3 =
  NewConv
    { newConvUsers = [],
      newConvQualifiedUsers = [],
      newConvName = Nothing,
      newConvAccess = Set.fromList [InviteAccess, LinkAccess, CodeAccess],
      newConvAccessRoles = Just (Set.fromList [TeamMemberAccessRole, GuestAccessRole]),
      newConvTeam = Nothing,
      newConvMessageTimer = Nothing,
      newConvReceiptMode = Nothing,
      newConvUsersRole =
        fromJust
          ( parseRoleName
              "y3otpiwu615lvvccxsq0315jj75jquw01flhtuf49t6mzfurvwe3_sh51f4s257e2x47zo85rif_xyiyfldpan3g4r6zr35rbwnzm0k"
          ),
      newConvProtocol = BaseProtocolMLSTag,
      newConvGroupConvType = Channel,
      newConvCells = True
    }
