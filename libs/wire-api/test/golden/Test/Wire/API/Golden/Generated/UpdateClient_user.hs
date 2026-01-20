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

module Test.Wire.API.Golden.Generated.UpdateClient_user where

import Data.Map qualified as Map
import Imports
import Wire.API.MLS.CipherSuite
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

testObject_UpdateClient_user_1 :: UpdateClient
testObject_UpdateClient_user_1 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = ","},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "G\1039380"}
        ],
      updateClientLastKey = Just (lastPrekey ""),
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_2 :: UpdateClient
testObject_UpdateClient_user_2 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "~"},
          Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "\STX"}
        ],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "\14793\13068\SOH\74214\US",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_3 :: UpdateClient
testObject_UpdateClient_user_3 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "vi"}
        ],
      updateClientLastKey = Just (lastPrekey "L\100005"),
      updateClientLabel = Just "\NUL\12245B\ACK",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_4 :: UpdateClient
testObject_UpdateClient_user_4 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\997860"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "u"}
        ],
      updateClientLastKey = Just (lastPrekey ""),
      updateClientLabel = Just "M\1066358^YH:l",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_5 :: UpdateClient
testObject_UpdateClient_user_5 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1022268"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      updateClientLastKey = Just (lastPrekey "Cs \74536="),
      updateClientLabel = Just "I\1038139\tCzGW\1034813",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_6 :: UpdateClient
testObject_UpdateClient_user_6 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "+"}
        ],
      updateClientLastKey = Just (lastPrekey ""),
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_7 :: UpdateClient
testObject_UpdateClient_user_7 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "D9",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_8 :: UpdateClient
testObject_UpdateClient_user_8 =
  UpdateClient
    { updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 4}, prekeyKey = "_Xx;"}],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "8\NAKD\57788\111128",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = Map.fromList [(Ed25519, "bm90IHJlYWxseSBhIHB1YmxpYyBrZXk=")]
    }

testObject_UpdateClient_user_9 :: UpdateClient
testObject_UpdateClient_user_9 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "a\24415\\<c\1047390.\1050080",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_10 :: UpdateClient
testObject_UpdateClient_user_10 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      updateClientLastKey = Just (lastPrekey ""),
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_11 :: UpdateClient
testObject_UpdateClient_user_11 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "("},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ":"},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "d"},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "\DEL"}
        ],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "`9q)\24055",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_12 :: UpdateClient
testObject_UpdateClient_user_12 =
  UpdateClient
    { updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 3}, prekeyKey = "W\24095S"}],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "o\SUB",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_13 :: UpdateClient
testObject_UpdateClient_user_13 =
  UpdateClient
    { updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "z\28560]"}],
      updateClientLastKey = Just (lastPrekey "/\CAN\1031188\DLEk"),
      updateClientLabel = Just "",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_14 :: UpdateClient
testObject_UpdateClient_user_14 =
  UpdateClient
    { updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "1h"}],
      updateClientLastKey = Just (lastPrekey "'S\asm"),
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_15 :: UpdateClient
testObject_UpdateClient_user_15 =
  UpdateClient
    { updateClientPrekeys = [],
      updateClientLastKey = Nothing,
      updateClientLabel = Nothing,
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_16 :: UpdateClient
testObject_UpdateClient_user_16 =
  UpdateClient
    { updateClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "Q}"}],
      updateClientLastKey = Just (lastPrekey "Ll\SO\996866k\176052\EOTf\DC2"),
      updateClientLabel = Just "\993565\&6",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_17 :: UpdateClient
testObject_UpdateClient_user_17 =
  UpdateClient
    { updateClientPrekeys = [],
      updateClientLastKey = Nothing,
      updateClientLabel = Just "c\1003571S\NAK\DLE}0 6\RS",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_18 :: UpdateClient
testObject_UpdateClient_user_18 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ">\1068703"},
          Prekey {prekeyId = PrekeyId {keyId = 2}, prekeyKey = "E"}
        ],
      updateClientLastKey = Just (lastPrekey "\1010230\1006024\&4/"),
      updateClientLabel = Just "\SOH\n\16789?a\1034213\DC4O",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_19 :: UpdateClient
testObject_UpdateClient_user_19 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "b"},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      updateClientLastKey = Just (lastPrekey "\191090t\96624\113765i"),
      updateClientLabel = Just "\f&QO\"u\t-\\",
      updateClientCapabilities = Nothing,
      updateClientMLSPublicKeys = mempty
    }

testObject_UpdateClient_user_20 :: UpdateClient
testObject_UpdateClient_user_20 =
  UpdateClient
    { updateClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      updateClientLastKey = Just (lastPrekey "\DC4 }Kg\ve3"),
      updateClientLabel = Just "\ESC\EOT\SOHccn\US{Y5",
      updateClientCapabilities = Just (ClientCapabilityList [ClientSupportsLegalholdImplicitConsent]),
      updateClientMLSPublicKeys = mempty
    }
