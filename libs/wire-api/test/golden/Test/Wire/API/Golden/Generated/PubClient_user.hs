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

module Test.Wire.API.Golden.Generated.PubClient_user where

import Data.Id
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
testObject_PubClient_user_1 = PubClient {pubClientId = ClientId 0x1f4, pubClientClass = Nothing}

testObject_PubClient_user_2 :: PubClient
testObject_PubClient_user_2 = PubClient {pubClientId = ClientId 0x502, pubClientClass = Just TabletClient}

testObject_PubClient_user_3 :: PubClient
testObject_PubClient_user_3 = PubClient {pubClientId = ClientId 0x376, pubClientClass = Just DesktopClient}

testObject_PubClient_user_4 :: PubClient
testObject_PubClient_user_4 = PubClient {pubClientId = ClientId 0x47f, pubClientClass = Just TabletClient}

testObject_PubClient_user_5 :: PubClient
testObject_PubClient_user_5 = PubClient {pubClientId = ClientId 0x377, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_6 :: PubClient
testObject_PubClient_user_6 = PubClient {pubClientId = ClientId 0xe4f, pubClientClass = Just DesktopClient}

testObject_PubClient_user_7 :: PubClient
testObject_PubClient_user_7 = PubClient {pubClientId = ClientId 0x8ab, pubClientClass = Nothing}

testObject_PubClient_user_8 :: PubClient
testObject_PubClient_user_8 = PubClient {pubClientId = ClientId 0x69b, pubClientClass = Just PhoneClient}

testObject_PubClient_user_9 :: PubClient
testObject_PubClient_user_9 = PubClient {pubClientId = ClientId 0x357, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_10 :: PubClient
testObject_PubClient_user_10 = PubClient {pubClientId = ClientId 0x4aa, pubClientClass = Just PhoneClient}

testObject_PubClient_user_11 :: PubClient
testObject_PubClient_user_11 = PubClient {pubClientId = ClientId 0x599, pubClientClass = Nothing}

testObject_PubClient_user_12 :: PubClient
testObject_PubClient_user_12 = PubClient {pubClientId = ClientId 0xdcd, pubClientClass = Just TabletClient}

testObject_PubClient_user_13 :: PubClient
testObject_PubClient_user_13 = PubClient {pubClientId = ClientId 0xfdc, pubClientClass = Just PhoneClient}

testObject_PubClient_user_14 :: PubClient
testObject_PubClient_user_14 =
  PubClient {pubClientId = ClientId 0xa98, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_15 :: PubClient
testObject_PubClient_user_15 = PubClient {pubClientId = ClientId 0xf7f, pubClientClass = Just DesktopClient}

testObject_PubClient_user_16 :: PubClient
testObject_PubClient_user_16 =
  PubClient {pubClientId = ClientId 0x5b4, pubClientClass = Just LegalHoldClient}

testObject_PubClient_user_17 :: PubClient
testObject_PubClient_user_17 = PubClient {pubClientId = ClientId 0xa83, pubClientClass = Nothing}

testObject_PubClient_user_18 :: PubClient
testObject_PubClient_user_18 = PubClient {pubClientId = ClientId 0x3d6, pubClientClass = Just PhoneClient}

testObject_PubClient_user_19 :: PubClient
testObject_PubClient_user_19 = PubClient {pubClientId = ClientId 0x4c7, pubClientClass = Just TabletClient}

testObject_PubClient_user_20 :: PubClient
testObject_PubClient_user_20 = PubClient {pubClientId = ClientId 0x787, pubClientClass = Just TabletClient}
