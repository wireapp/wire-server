{-# LANGUAGE OverloadedLists #-}

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
module Test.Wire.API.Golden.Generated.NewConvUnmanaged_user where

import Data.Domain (Domain (Domain))
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import Data.Qualified (Qualified (Qualified))
import qualified Data.Set as Set (fromList)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( Access (CodeAccess, InviteAccess, LinkAccess, PrivateAccess),
    AccessRole
      ( ActivatedAccessRole,
        NonActivatedAccessRole,
        PrivateAccessRole,
        TeamAccessRole
      ),
    ConvTeamInfo (ConvTeamInfo, cnvManaged, cnvTeamId),
    NewConv
      ( NewConv,
        newConvAccess,
        newConvAccessRole,
        newConvMessageTimer,
        newConvName,
        newConvQualifiedUsers,
        newConvReceiptMode,
        newConvTeam,
        newConvUsers,
        newConvUsersRole
      ),
    NewConvUnmanaged (..),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Role (parseRoleName)

testDomain :: Domain
testDomain = Domain "testdomain.example.com"

testObject_NewConvUnmanaged_user_1 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_1 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers =
            [ Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
            ],
          newConvQualifiedUsers = [],
          newConvName = Nothing,
          newConvAccess = Set.fromList [PrivateAccess, InviteAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    cnvManaged = False
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 3320987366258987}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 1}),
          newConvUsersRole = fromJust (parseRoleName "8tp2gs7b6")
        }
    )

testObject_NewConvUnmanaged_user_2 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_2 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) testDomain],
          newConvName = Just "\128527\1061495",
          newConvAccess = Set.fromList [],
          newConvAccessRole = Nothing,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
                    cnvManaged = True
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 2406292360203739}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = -1}),
          newConvUsersRole =
            fromJust
              ( parseRoleName
                  "vmao7psxph3fenvbpsu1u57fns5pfo53d67k98om378rnxr0crcpak_mpspn8q_3m1b02n2n133s1d7q5w3qgmt_5e_dgtvzon8an7dtauiecd32"
              )
        }
    )

testObject_NewConvUnmanaged_user_3 :: NewConvUnmanaged
testObject_NewConvUnmanaged_user_3 =
  NewConvUnmanaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Nothing,
          newConvAccess = Set.fromList [InviteAccess, LinkAccess, CodeAccess],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam = Nothing,
          newConvMessageTimer = Nothing,
          newConvReceiptMode = Nothing,
          newConvUsersRole =
            fromJust
              ( parseRoleName
                  "y3otpiwu615lvvccxsq0315jj75jquw01flhtuf49t6mzfurvwe3_sh51f4s257e2x47zo85rif_xyiyfldpan3g4r6zr35rbwnzm0k"
              )
        }
    )
