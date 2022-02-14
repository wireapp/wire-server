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

module Test.Wire.API.Golden.Generated.TeamConversationList_team where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports
import Wire.API.Routes.Version
import Wire.API.Team.Conversation (TeamConversationList, newTeamConversation, newTeamConversationList)

testObject_TeamConversationList_team_until_v2 :: TeamConversationList (Until 'V2)
testObject_TeamConversationList_team_until_v2 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000012-0000-0018-0000-00260000002b"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000002d-0000-0063-0000-006900000013")))))
      ]
  )

testObject_TeamConversationList_team_from_v2 :: TeamConversationList (From 'V2)
testObject_TeamConversationList_team_from_v2 =
  ( newTeamConversationList
      [ (newTeamConversation ((Id (fromJust (UUID.fromString "00000064-0000-0045-0000-007d00000023"))))),
        (newTeamConversation ((Id (fromJust (UUID.fromString "0000000d-0000-0080-0000-00550000001b")))))
      ]
  )
