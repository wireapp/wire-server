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

module Test.Wire.API.Golden.Generated.PrekeyBundle_user where

import Data.Id (ClientId (ClientId, client), Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.User.Client.Prekey
  ( ClientPrekey (ClientPrekey, prekeyClient, prekeyData),
    Prekey (Prekey, prekeyId, prekeyKey),
    PrekeyBundle (..),
    PrekeyId (PrekeyId, keyId),
  )

testObject_PrekeyBundle_user_1 :: PrekeyBundle
testObject_PrekeyBundle_user_1 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000046-0000-0011-0000-007200000022"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "8"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\rOx"}}]}

testObject_PrekeyBundle_user_2 :: PrekeyBundle
testObject_PrekeyBundle_user_2 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000043-0000-002b-0000-00550000002a"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_3 :: PrekeyBundle
testObject_PrekeyBundle_user_3 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000001-0000-002b-0000-002e00000010"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\n"}}]}

testObject_PrekeyBundle_user_4 :: PrekeyBundle
testObject_PrekeyBundle_user_4 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000037-0000-0050-0000-005900000043"))), prekeyClients = []}

testObject_PrekeyBundle_user_5 :: PrekeyBundle
testObject_PrekeyBundle_user_5 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000b-0000-0075-0000-00620000001e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "i"}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "L"}}]}

testObject_PrekeyBundle_user_6 :: PrekeyBundle
testObject_PrekeyBundle_user_6 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000004c-0000-007e-0000-004300000034"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_7 :: PrekeyBundle
testObject_PrekeyBundle_user_7 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000001e-0000-0066-0000-000200000002"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "$"}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_8 :: PrekeyBundle
testObject_PrekeyBundle_user_8 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000050-0000-0050-0000-00760000005f"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_9 :: PrekeyBundle
testObject_PrekeyBundle_user_9 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000024-0000-0074-0000-000b0000001d"))), prekeyClients = []}

testObject_PrekeyBundle_user_10 :: PrekeyBundle
testObject_PrekeyBundle_user_10 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000062-0000-003a-0000-006c0000001e"))), prekeyClients = []}

testObject_PrekeyBundle_user_11 :: PrekeyBundle
testObject_PrekeyBundle_user_11 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000025-0000-0061-0000-005f0000000a"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "4"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ">"}}]}

testObject_PrekeyBundle_user_12 :: PrekeyBundle
testObject_PrekeyBundle_user_12 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000073-0000-0034-0000-004c00000024"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "a"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1092897\990773-"}}]}

testObject_PrekeyBundle_user_13 :: PrekeyBundle
testObject_PrekeyBundle_user_13 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000000c-0000-006a-0000-00650000007c"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_14 :: PrekeyBundle
testObject_PrekeyBundle_user_14 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000012-0000-0024-0000-006700000016"))), prekeyClients = []}

testObject_PrekeyBundle_user_15 :: PrekeyBundle
testObject_PrekeyBundle_user_15 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000079-0000-0057-0000-004200000037"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\RS"}}]}

testObject_PrekeyBundle_user_16 :: PrekeyBundle
testObject_PrekeyBundle_user_16 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000002b-0000-0032-0000-00140000006e"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "f"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1066568\149661?"}}]}

testObject_PrekeyBundle_user_17 :: PrekeyBundle
testObject_PrekeyBundle_user_17 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000006f-0000-0036-0000-00560000002d"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_18 :: PrekeyBundle
testObject_PrekeyBundle_user_18 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000069-0000-007c-0000-000f0000004a"))), prekeyClients = []}

testObject_PrekeyBundle_user_19 :: PrekeyBundle
testObject_PrekeyBundle_user_19 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "0000006f-0000-0072-0000-003e00000008"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "0"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}]}

testObject_PrekeyBundle_user_20 :: PrekeyBundle
testObject_PrekeyBundle_user_20 = PrekeyBundle {prekeyUser = (Id (fromJust (UUID.fromString "00000073-0000-0017-0000-00690000007a"))), prekeyClients = [ClientPrekey {prekeyClient = ClientId {client = "1"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}}, ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\1014040"}}, ClientPrekey {prekeyClient = ClientId {client = "2"}, prekeyData = Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\SO"}}]}
