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
module Test.Wire.API.Golden.Generated.PushToken_user where

import Data.Id (ClientId (ClientId, client))
import Wire.API.Push.Token
  ( AppName (AppName, appNameText),
    PushToken,
    Token (Token, tokenText),
    Transport (APNSSandbox),
    pushToken,
  )

testObject_PushToken_user_1 :: PushToken
testObject_PushToken_user_1 =
  pushToken
    APNSSandbox
    (AppName {appNameText = "G\1008289"})
    (Token {tokenText = "\EOT8M\NAKAv\1104873"})
    (ClientId {client = "16"})
