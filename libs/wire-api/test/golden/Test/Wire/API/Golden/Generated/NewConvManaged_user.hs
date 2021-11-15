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
module Test.Wire.API.Golden.Generated.NewConvManaged_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Misc (Milliseconds (Ms, ms))
import qualified Data.Set as Set (fromList)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (True), Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation
  ( AccessRole (ActivatedAccessRole),
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
    NewConvManaged (..),
    ReceiptMode (ReceiptMode, unReceiptMode),
  )
import Wire.API.Conversation.Role (parseRoleName)

testDomain :: Domain
testDomain = Domain "test.example.com"

testObject_NewConvManaged_user_1 :: NewConvManaged
testObject_NewConvManaged_user_1 =
  NewConvManaged
    ( NewConv
        { newConvUsers = [],
          newConvQualifiedUsers = [],
          newConvName = Nothing,
          newConvAccess = Set.fromList [],
          newConvAccessRole = Just ActivatedAccessRole,
          newConvTeam =
            Just
              ( ConvTeamInfo
                  { cnvTeamId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000")),
                    cnvManaged = True
                  }
              ),
          newConvMessageTimer = Just (Ms {ms = 193643728192048}),
          newConvReceiptMode = Just (ReceiptMode {unReceiptMode = 4}),
          newConvUsersRole = fromJust (parseRoleName "37q9eeybycp5972td4oo9_r7y16eh6n67z5spda8sffy8qv")
        }
    )
