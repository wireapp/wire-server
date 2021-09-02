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
module Test.Wire.API.Golden.Generated.PushTokenList_user where

import Data.Id (ClientId (ClientId, client))
import Wire.API.Push.Token
  ( AppName (AppName, appNameText),
    PushTokenList (..),
    Token (Token, tokenText),
    Transport (APNS, APNSSandbox, APNSVoIP, APNSVoIPSandbox, GCM),
    pushToken,
  )

testObject_PushTokenList_user_1 :: PushTokenList
testObject_PushTokenList_user_1 =
  PushTokenList
    { pushTokens =
        [ (pushToken (GCM) (AppName {appNameText = "p\DELU2r"}) (Token {tokenText = "MK8p\f"}) (ClientId {client = "4"}))
        ]
    }

testObject_PushTokenList_user_2 :: PushTokenList
testObject_PushTokenList_user_2 =
  PushTokenList []
