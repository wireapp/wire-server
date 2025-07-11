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

module Test.Wire.API.Golden.Generated.AddBotResponse_user where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, read, (.))
import Wire.API.Asset
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.User

testObject_AddBotResponse_user_1 :: AddBotResponse
testObject_AddBotResponse_user_1 =
  AddBotResponse
    { rsAddBotId = (BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000001")),
      rsAddBotClient = ClientId 0xe,
      rsAddBotName =
        Name
          { fromName =
              "\77844\129468A\1061088\30365\142096\40918\USc\DC3~0g\ENQr\v\29872\f\154305\1077132u\175940.\1018427v\v-/\bi\bJ\ETXE3\ESC8\53613\1073036\&0@\14466\51733;\27113\SYN\153289\b&\ae]\1042471H\1024555k7\EMJ\1083646[;\140668;J^`0,B\STX\95353N.@Z\v\ENQ\r\19858|'w-\b\157432V\STX \GSW|N\1072850\&3=\22550K245\DC1\142803\168718\7168\147365\ETX"
          },
      rsAddBotColour = ColourId {fromColourId = -3},
      rsAddBotAssets =
        [ ImageAsset
            (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring)
            Nothing,
          ImageAsset
            (AssetKeyV3 (Id (fromJust (UUID.fromString "034efa97-f628-450e-b212-009801b1470b"))) AssetExpiring)
            (Just AssetPreview)
        ],
      rsAddBotEvent =
        Event
          (Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000003"))) (Domain "faraway.example.com"))
          Nothing
          (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000004"))) (Domain "faraway.example.com"))
          (read "1864-05-12 19:20:22.286 UTC")
          Nothing
          (EdConvRename (ConversationRename {cupName = "6"}))
    }

testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 =
  AddBotResponse
    { rsAddBotId = (BotId . Id) (fromJust (UUID.fromString "00000001-0000-0003-0000-000200000004")),
      rsAddBotClient = ClientId 0xe,
      rsAddBotName =
        Name
          { fromName =
              "\162949t\DEL\\\DC2\52420Jn\1069034\997789t!\ESC\STX\1009296~jP]}|8\1106819\11112\SYNR\985193\&8H\1056222\ETBL\189886V\99433Q\1013937\133319\EOTM\DC4kc\a V"
          },
      rsAddBotColour = ColourId {fromColourId = 3},
      rsAddBotAssets = [],
      rsAddBotEvent =
        Event
          (Qualified (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000300000001"))) (Domain "faraway.example.com"))
          Nothing
          (Qualified (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001"))) (Domain "faraway.example.com"))
          (read "1864-05-08 19:02:58.6 UTC")
          Nothing
          (EdTyping StartedTyping)
    }
