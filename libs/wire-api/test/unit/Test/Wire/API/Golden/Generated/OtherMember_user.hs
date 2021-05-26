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
module Test.Wire.API.Golden.Generated.OtherMember_user where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation (OtherMember (..))
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_OtherMember_user_1 :: OtherMember
testObject_OtherMember_user_1 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000008-0000-0009-0000-000f00000001"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000004"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "kd8736"))
    }

testObject_OtherMember_user_2 :: OtherMember
testObject_OtherMember_user_2 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001f-0000-000c-0000-001c0000000f"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "y9z93u3kbwt873eghekqgmy0ho8hgrtlo3f5e6nq9icedmjbzx7ao0ycr5_gyunq4uuw"))
    }

testObject_OtherMember_user_3 :: OtherMember
testObject_OtherMember_user_3 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001d-0000-0012-0000-001f0000001f"))) domain,
      omService = Nothing,
      omConvRoleName =
        ( fromJust
            (parseRoleName "224ynn27l35zqag2j8wx3jte0mtacwjx5gqfj8bu6v6z4iab5stg5fu4k7mviu1oi5sgmw3kovmgx6rxtfrzz72")
        )
    }

testObject_OtherMember_user_4 :: OtherMember
testObject_OtherMember_user_4 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000001a-0000-000f-0000-000900000008"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000300000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000000")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "y_yyztl9rczy3ptybi5iiizt2"))
    }

testObject_OtherMember_user_5 :: OtherMember
testObject_OtherMember_user_5 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000015-0000-001b-0000-00020000000d"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000004"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000002")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "osr9yoilhf5s_7jhibw87rcc1iclohtngeqp7a9k2s4ty8537v"))
    }

testObject_OtherMember_user_6 :: OtherMember
testObject_OtherMember_user_6 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0014-0000-000f0000000d"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "46c5d6qq5s5act5gzme7z1q5w9vhep"))
    }

testObject_OtherMember_user_7 :: OtherMember
testObject_OtherMember_user_7 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000003-0000-001c-0000-002000000016"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000300000003")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "p0hv86628m_rzt4ganpw2r3"))
    }

testObject_OtherMember_user_8 :: OtherMember
testObject_OtherMember_user_8 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000011-0000-001f-0000-000d0000000f"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000002"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000001")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "wzjf8x1tw3e6m7e2zrle1teerh9e9bzba"))
    }

testObject_OtherMember_user_9 :: OtherMember
testObject_OtherMember_user_9 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0016-0000-000f00000010"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000003"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000000")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "vfd81bco01v07l2gsgg9c5bolm759sicys0epfrb"))
    }

testObject_OtherMember_user_10 :: OtherMember
testObject_OtherMember_user_10 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000005-0000-0020-0000-000b0000001f"))) domain,
      omService = Nothing,
      omConvRoleName = (fromJust (parseRoleName "uneetc9i9j3"))
    }

testObject_OtherMember_user_11 :: OtherMember
testObject_OtherMember_user_11 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000004-0000-001e-0000-001d0000001a"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000003")))
              }
          ),
      omConvRoleName =
        (fromJust (parseRoleName "j_0wepobygx3ejil7wdiinpmgp16d4n6lp2chqdtk64ic5lspht_4m0y83o9zltergmkhiisc4rk6lauh7s"))
    }

testObject_OtherMember_user_12 :: OtherMember
testObject_OtherMember_user_12 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000016-0000-0019-0000-001200000013"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000002"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000000")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "s0cumbx6k0vnriouzagmjk5vl9r7k6mw7cp1rrdx8_kcybuo5x9m6wp7a98pzfio6s"))
    }

testObject_OtherMember_user_13 :: OtherMember
testObject_OtherMember_user_13 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000008-0000-0009-0000-000d00000011"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "clxtcuty2d1g_clojjevjpb5ca4a1"))
    }

testObject_OtherMember_user_14 :: OtherMember
testObject_OtherMember_user_14 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000016-0000-000c-0000-000e00000016"))) domain,
      omService = Nothing,
      omConvRoleName = (fromJust (parseRoleName "gxe_4agkvb3"))
    }

testObject_OtherMember_user_15 :: OtherMember
testObject_OtherMember_user_15 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "0000000d-0000-001f-0000-00180000001c"))) domain,
      omService = Nothing,
      omConvRoleName =
        (fromJust (parseRoleName "vp25612u_4o84sy2rigmst6j7zd54d6502f0zogeb2zm93b5vcdcf5z8mm_0by9syvet_u_7a"))
    }

testObject_OtherMember_user_16 :: OtherMember
testObject_OtherMember_user_16 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000014-0000-001e-0000-000600000008"))) domain,
      omService = Nothing,
      omConvRoleName =
        ( fromJust
            (parseRoleName "89u0cp528kwlognk222yl5epk322mwjgip8k4atbko1u_q_3mmlalbdxtbrfdysnp0mii7dugiujclxbil1cjq1")
        )
    }

testObject_OtherMember_user_17 :: OtherMember
testObject_OtherMember_user_17 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000016-0000-001b-0000-00110000000c"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000004"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000003")))
              }
          ),
      omConvRoleName =
        ( fromJust
            (parseRoleName "vi4v4en0v5vnq7nohnqqr18rfh82_tz3kndf38p8_vfr8ogae54h9nbkt0_ysm3isafx6dz57kfzkxu6f73gfkc9")
        )
    }

testObject_OtherMember_user_18 :: OtherMember
testObject_OtherMember_user_18 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000017-0000-000c-0000-001a00000000"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000000"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000000")))
              }
          ),
      omConvRoleName = (fromJust (parseRoleName "htfohjl1uoehr8upvg_eete17sr304vqbm9imt_l_znz_rd1cq6n"))
    }

testObject_OtherMember_user_19 :: OtherMember
testObject_OtherMember_user_19 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-00060000000f"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000002"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000001")))
              }
          ),
      omConvRoleName =
        ( fromJust
            ( parseRoleName
                "lm96_nocxpxllzob9onqrzfawv5eru442jrri387wol1o4affst6zfga9mmxdr3_s_ote0np2dfc4w4otqls3nozgne6frclb"
            )
        )
    }

testObject_OtherMember_user_20 :: OtherMember
testObject_OtherMember_user_20 =
  OtherMember
    { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000007-0000-0010-0000-002000000001"))) domain,
      omService =
        Just
          ( ServiceRef
              { _serviceRefId = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000001"))),
                _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000000")))
              }
          ),
      omConvRoleName =
        ( fromJust
            ( parseRoleName
                "3nzv9edmf6rv54vgw3xl5fxrwzwm38u3vu2gpyx786hqjimctl7l9aqq01af0_h6nix6111vcm4dujjufqxvlx7f84j8koumw8ws0u5xe8u7al1ba1wj31ob381"
            )
        )
    }
