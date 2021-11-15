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
module Test.Wire.API.Golden.Generated.ConversationRolesList_user where

import qualified Data.Set as Set (fromList)
import Imports (Maybe (Just), fromJust)
import Wire.API.Conversation.Role
  ( Action
      ( AddConversationMember,
        ModifyOtherConversationMember
      ),
    Actions (Actions),
    ConversationRolesList (..),
    parseRoleName,
    toConvRole,
  )

testObject_ConversationRolesList_user_1 :: ConversationRolesList
testObject_ConversationRolesList_user_1 =
  ConversationRolesList
    { convRolesList =
        [ fromJust
            ( toConvRole
                (fromJust (parseRoleName "0g843hmarr"))
                (Just (Actions (Set.fromList [AddConversationMember, ModifyOtherConversationMember])))
            )
        ]
    }

testObject_ConversationRolesList_user_2 :: ConversationRolesList
testObject_ConversationRolesList_user_2 =
  ConversationRolesList
    { convRolesList = []
    }
