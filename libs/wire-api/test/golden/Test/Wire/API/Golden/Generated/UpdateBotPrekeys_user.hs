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
module Test.Wire.API.Golden.Generated.UpdateBotPrekeys_user where

import Wire.API.Conversation.Bot (UpdateBotPrekeys (..))
import Wire.API.User.Client.Prekey (Prekey (Prekey, prekeyId, prekeyKey), PrekeyId (PrekeyId, keyId))

testObject_UpdateBotPrekeys_user_1 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_1 =
  UpdateBotPrekeys
    { updateBotPrekeyList =
        [ Prekey {prekeyId = PrekeyId {keyId = 81}, prekeyKey = "Ox\DLE*\120423\&8m\DC4%J\34541\"/r"},
          Prekey {prekeyId = PrekeyId {keyId = 29}, prekeyKey = "r\GS"}
        ]
    }

testObject_UpdateBotPrekeys_user_2 :: UpdateBotPrekeys
testObject_UpdateBotPrekeys_user_2 =
  UpdateBotPrekeys
    { updateBotPrekeyList = []
    }
