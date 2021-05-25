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
module Test.Wire.API.Golden.Generated.AccessToken_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.User.Auth (AccessToken (..), TokenType (Bearer))

testObject_AccessToken_user_1 :: AccessToken
testObject_AccessToken_user_1 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00002525-0000-2bc3-0000-3a8200006f94"))),
      access = "{\CAN\243\188\157\141\SOq\240\171\167\184w",
      tokenType = Bearer,
      expiresIn = 1
    }

testObject_AccessToken_user_2 :: AccessToken
testObject_AccessToken_user_2 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00007ace-0000-630b-0000-718c00000945"))),
      access = "o6Q\243\184\187\164\ETB\243\181\156\157",
      tokenType = Bearer,
      expiresIn = -24
    }

testObject_AccessToken_user_3 :: AccessToken
testObject_AccessToken_user_3 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00004286-0000-22c5-0000-5dba00001818"))),
      access = "\DC3u\240\171\168\183N<E#\244\138\156\176>dWTm\SIi\244\139\166\169",
      tokenType = Bearer,
      expiresIn = 6
    }

testObject_AccessToken_user_4 :: AccessToken
testObject_AccessToken_user_4 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00005c1d-0000-2e06-0000-278a00002d91"))),
      access = "\233\152\185\&0)&9\\\SI\NULO",
      tokenType = Bearer,
      expiresIn = -9
    }

testObject_AccessToken_user_5 :: AccessToken
testObject_AccessToken_user_5 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00002891-0000-27e1-0000-686000002ba0"))),
      access = "n\227\154\185 {'\FS\240\159\147\150\DC1C*\234\186\142\ESC",
      tokenType = Bearer,
      expiresIn = 27
    }

testObject_AccessToken_user_6 :: AccessToken
testObject_AccessToken_user_6 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "0000195e-0000-7174-0000-1a5c000030dc"))),
      access = "+\231\145\167\&8J\243\176\183\137\SOHw",
      tokenType = Bearer,
      expiresIn = 2
    }

testObject_AccessToken_user_7 :: AccessToken
testObject_AccessToken_user_7 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "000038d1-0000-3dd4-0000-499a000014ca"))),
      access =
        "`gS\DEL\DLE\ETXe\243\187\169\134o\243\191\130\131\244\129\152\137\243\178\160\150+Htv\244\130\172\190\EMdh\STX\240\169\169\185\239\130\169",
      tokenType = Bearer,
      expiresIn = -12
    }

testObject_AccessToken_user_8 :: AccessToken
testObject_AccessToken_user_8 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "000065e0-0000-3b8c-0000-492700007916"))),
      access = "\NULYn\DELC&X9\243\189\191\169_",
      tokenType = Bearer,
      expiresIn = 27
    }

testObject_AccessToken_user_9 :: AccessToken
testObject_AccessToken_user_9 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "000023d8-0000-406e-0000-3277000079f9"))),
      access = "\244\132\147\179\CAN\b\243\187\136\177\244\141\160\129\CANf\243\179\172\128DDNNR\240\160\183\154`H",
      tokenType = Bearer,
      expiresIn = 23
    }

testObject_AccessToken_user_10 :: AccessToken
testObject_AccessToken_user_10 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "0000376e-0000-4673-0000-1e1800004b06"))),
      access = " \243\180\155\169+\244\143\128\190G_\240\161\128\142Xj\NULef\232\159\186.&U]J\240\166\182\187=<hrt3",
      tokenType = Bearer,
      expiresIn = -20
    }

testObject_AccessToken_user_11 :: AccessToken
testObject_AccessToken_user_11 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "000018c7-0000-4f75-0000-29b0000065fc"))),
      access = "pT\240\164\134\146\DC1@\244\140\169\164\DC43",
      tokenType = Bearer,
      expiresIn = -19
    }

testObject_AccessToken_user_12 :: AccessToken
testObject_AccessToken_user_12 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00000f4f-0000-0499-0000-78b4000029de"))),
      access = "\ACK\n\244\136\183\166FY\ETXuu\SOH",
      tokenType = Bearer,
      expiresIn = 25
    }

testObject_AccessToken_user_13 :: AccessToken
testObject_AccessToken_user_13 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00001225-0000-145a-0000-277600007725"))),
      access =
        "\244\128\147\170Q\224\176\149\243\186\134\162oV|#Hp-\243\184\189\134js\244\139\171\189\243\176\166\182.E\244\139\145\148\243\184\176\189",
      tokenType = Bearer,
      expiresIn = 27
    }

testObject_AccessToken_user_14 :: AccessToken
testObject_AccessToken_user_14 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00007469-0000-0eae-0000-1a8400004582"))),
      access = "\243\177\160\147",
      tokenType = Bearer,
      expiresIn = -12
    }

testObject_AccessToken_user_15 :: AccessToken
testObject_AccessToken_user_15 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "000011e5-0000-29e4-0000-550400003888"))),
      access = "\243\177\159\190r",
      tokenType = Bearer,
      expiresIn = 26
    }

testObject_AccessToken_user_16 :: AccessToken
testObject_AccessToken_user_16 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "0000633c-0000-6653-0000-772e00005669"))),
      access = "p\"\244\130\163\145\v-\238\143\147\ETX\b<\240\147\141\128+\SO\DEL\244\131\172\144",
      tokenType = Bearer,
      expiresIn = 18
    }

testObject_AccessToken_user_17 :: AccessToken
testObject_AccessToken_user_17 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00006032-0000-470b-0000-544b00001c88"))),
      access = "\240\167\149\178z}\bRH\ENQ@o\EMm,\240\159\146\156\228\155\169\244\140\181\157]\EOT\FS\rZm,Z",
      tokenType = Bearer,
      expiresIn = -13
    }

testObject_AccessToken_user_18 :: AccessToken
testObject_AccessToken_user_18 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00006b0d-0000-792a-0000-3fb800003867"))),
      access =
        "\244\129\184\152\\\244\136\157\138\v2!\243\188\172\183\240\174\169\150ZE|3(\CAN=Q\ENQb\DC3[\243\176\144\149\243\188\182\133MW",
      tokenType = Bearer,
      expiresIn = -8
    }

testObject_AccessToken_user_19 :: AccessToken
testObject_AccessToken_user_19 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00004fc5-0000-08b5-0000-0ad800002c12"))),
      access = "h\a",
      tokenType = Bearer,
      expiresIn = -16
    }

testObject_AccessToken_user_20 :: AccessToken
testObject_AccessToken_user_20 =
  AccessToken
    { user = (Id (fromJust (UUID.fromString "00005c43-0000-6c4c-0000-461200000976"))),
      access = "\243\190\143\130~\240\164\141\143#\t\FS\244\133\141\138 ~_W\244\139\185\159z_\243\179\169\167A",
      tokenType = Bearer,
      expiresIn = 17
    }
