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
module Test.Wire.API.Golden.Generated.PubClient_user where

import Data.Id (ClientId (ClientId, client))
import Imports (Maybe (Just, Nothing))
import Wire.API.User.Client
  ( ClientClass
      ( DesktopClient,
        LegalHoldClient,
        PhoneClient,
        TabletClient
      ),
    PubClient (..),
  )

testObject_PubClient_user_1 :: PubClient
testObject_PubClient_user_1 = PubClient {pubClientId = ClientId {client = "1f4"}, pubClientClass = Nothing}

testObject_PubClient_user_2 :: PubClient
testObject_PubClient_user_2 = PubClient {pubClientId = ClientId {client = "502"}, pubClientClass = Just TabletClient}

testObject_PubClient_user_3 :: PubClient
testObject_PubClient_user_3 = PubClient {pubClientId = ClientId {client = "376"}, pubClientClass = Just DesktopClient}

testObject_PubClient_user_4 :: PubClient
testObject_PubClient_user_4 = PubClient {pubClientId = ClientId {client = "47f"}, pubClientClass = Just TabletClient}

testObject_PubClient_user_5 :: PubClient
testObject_PubClient_user_5 = PubClient {pubClientId = ClientId {client = "377"}, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_6 :: PubClient
testObject_PubClient_user_6 = PubClient {pubClientId = ClientId {client = "e4f"}, pubClientClass = Just DesktopClient}

testObject_PubClient_user_7 :: PubClient
testObject_PubClient_user_7 = PubClient {pubClientId = ClientId {client = "8ab"}, pubClientClass = Nothing}

testObject_PubClient_user_8 :: PubClient
testObject_PubClient_user_8 = PubClient {pubClientId = ClientId {client = "69b"}, pubClientClass = Just PhoneClient}

testObject_PubClient_user_9 :: PubClient
testObject_PubClient_user_9 = PubClient {pubClientId = ClientId {client = "357"}, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_10 :: PubClient
testObject_PubClient_user_10 = PubClient {pubClientId = ClientId {client = "4aa"}, pubClientClass = Just PhoneClient}

testObject_PubClient_user_11 :: PubClient
testObject_PubClient_user_11 = PubClient {pubClientId = ClientId {client = "599"}, pubClientClass = Nothing}

testObject_PubClient_user_12 :: PubClient
testObject_PubClient_user_12 = PubClient {pubClientId = ClientId {client = "dcd"}, pubClientClass = Just TabletClient}

testObject_PubClient_user_13 :: PubClient
testObject_PubClient_user_13 = PubClient {pubClientId = ClientId {client = "fdc"}, pubClientClass = Just PhoneClient}

testObject_PubClient_user_14 :: PubClient
testObject_PubClient_user_14 =
  PubClient {pubClientId = ClientId {client = "a98"}, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_15 :: PubClient
testObject_PubClient_user_15 = PubClient {pubClientId = ClientId {client = "f7f"}, pubClientClass = Just DesktopClient}

testObject_PubClient_user_16 :: PubClient
testObject_PubClient_user_16 =
  PubClient {pubClientId = ClientId {client = "5b4"}, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_17 :: PubClient
testObject_PubClient_user_17 = PubClient {pubClientId = ClientId {client = "a83"}, pubClientClass = Nothing}

testObject_PubClient_user_18 :: PubClient
testObject_PubClient_user_18 = PubClient {pubClientId = ClientId {client = "3d6"}, pubClientClass = Just PhoneClient}

testObject_PubClient_user_19 :: PubClient
testObject_PubClient_user_19 = PubClient {pubClientId = ClientId {client = "4c7"}, pubClientClass = Just TabletClient}

testObject_PubClient_user_20 :: PubClient
testObject_PubClient_user_20 = PubClient {pubClientId = ClientId {client = "787"}, pubClientClass = Just TabletClient}
