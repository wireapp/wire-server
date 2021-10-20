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
module Test.Wire.API.Golden.Generated.ConversationRole_user where

import qualified Data.Set as Set (fromList)
import Imports (Maybe (Just), fromJust)
import Wire.API.Conversation.Role
  ( Action
      ( ModifyConversationAccess,
        ModifyConversationMessageTimer
      ),
    Actions (Actions),
    ConversationRole,
    convRoleWireAdmin,
    convRoleWireMember,
    parseRoleName,
    toConvRole,
  )

testObject_ConversationRole_user_1 :: ConversationRole
testObject_ConversationRole_user_1 = convRoleWireAdmin

testObject_ConversationRole_user_2 :: ConversationRole
testObject_ConversationRole_user_2 = convRoleWireMember

testObject_ConversationRole_user_3 :: ConversationRole
testObject_ConversationRole_user_3 =
  fromJust
    ( toConvRole
        (fromJust (parseRoleName "32s49begziet8bw2zajkjk5flc26_pl8lnx5vs"))
        (Just (Actions (Set.fromList [ModifyConversationMessageTimer, ModifyConversationAccess])))
    )
