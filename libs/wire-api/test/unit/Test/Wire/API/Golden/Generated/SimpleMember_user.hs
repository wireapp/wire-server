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
module Test.Wire.API.Golden.Generated.SimpleMember_user where

import Data.Id (Id (Id))
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Event.Conversation (SimpleMember (..))

testObject_SimpleMember_user_1 :: SimpleMember
testObject_SimpleMember_user_1 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000003a-0000-0042-0000-007500000037"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName "wire_admin"
            )
        )
    }

testObject_SimpleMember_user_2 :: SimpleMember
testObject_SimpleMember_user_2 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000046-0000-0027-0000-003c00000022"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName "wire_member"
            )
        )
    }

testObject_SimpleMember_user_3 :: SimpleMember
testObject_SimpleMember_user_3 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000039-0000-0070-0000-005700000019"))),
      smConvRoleName = (fromJust (parseRoleName "7uzp7961dyf_666xqxwvq6uro"))
    }

testObject_SimpleMember_user_4 :: SimpleMember
testObject_SimpleMember_user_4 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000007c-0000-0075-0000-005b00000049"))),
      smConvRoleName = (fromJust (parseRoleName "4vr9oed4nvhs625ri_cz1cv5kodntk3edmkpu"))
    }

testObject_SimpleMember_user_5 :: SimpleMember
testObject_SimpleMember_user_5 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000004c-0000-004e-0000-002400000009"))),
      smConvRoleName = (fromJust (parseRoleName "wst92x"))
    }

testObject_SimpleMember_user_6 :: SimpleMember
testObject_SimpleMember_user_6 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000052-0000-0053-0000-000400000000"))),
      smConvRoleName =
        (fromJust (parseRoleName "nkyx6ypx0p0b_fvx6mt6w5w6n2qpivv9svj2myn5n86isy7n2e07m92t7ostflj4lq1py50bqzdi4smzd"))
    }

testObject_SimpleMember_user_7 :: SimpleMember
testObject_SimpleMember_user_7 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000003d-0000-006f-0000-00480000006e"))),
      smConvRoleName = (fromJust (parseRoleName "d8027w_w7pr9fj"))
    }

testObject_SimpleMember_user_8 :: SimpleMember
testObject_SimpleMember_user_8 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000004c-0000-006c-0000-000800000044"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName
                "_rgnqtn1bdc2eb4nr8ilpka1sm6kt5bvonqm742npdpro1s4b_ydcahfm4q7i0getmnp0vdpod_eye8c_1kb72d_96qypb"
            )
        )
    }

testObject_SimpleMember_user_9 :: SimpleMember
testObject_SimpleMember_user_9 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000000a-0000-007a-0000-003f0000001b"))),
      smConvRoleName = (fromJust (parseRoleName "sr5pfubd0_cpdp"))
    }

testObject_SimpleMember_user_10 :: SimpleMember
testObject_SimpleMember_user_10 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000001d-0000-000f-0000-002900000072"))),
      smConvRoleName = (fromJust (parseRoleName "paru"))
    }

testObject_SimpleMember_user_11 :: SimpleMember
testObject_SimpleMember_user_11 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000007d-0000-0076-0000-001e00000019"))),
      smConvRoleName =
        ( fromJust
            (parseRoleName "e0u15rrzql4y8jymut86vv84l4tjzpfti0_b1w44gy13j3d0dq1y22ws75tkgd4n_9tju4pq34_ddk_g9qpypwu4z3b5")
        )
    }

testObject_SimpleMember_user_12 :: SimpleMember
testObject_SimpleMember_user_12 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000003c-0000-0001-0000-004a00000014"))),
      smConvRoleName =
        ( fromJust
            (parseRoleName "telj17ej33ilgtqvqajp0ofng9qm6v9b1n32n_l6_vw_xxtk4o7n6r50ea3w1xgzh3eapah1jytfpz0f65utf9xqc4pv")
        )
    }

testObject_SimpleMember_user_13 :: SimpleMember
testObject_SimpleMember_user_13 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000052-0000-002c-0000-004500000067"))),
      smConvRoleName = (fromJust (parseRoleName "bfamau83n6sskso4rod8fz1tb4tf1zfz8mfd1v0ae1sx17po1"))
    }

testObject_SimpleMember_user_14 :: SimpleMember
testObject_SimpleMember_user_14 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000018-0000-006d-0000-000600000017"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName
                "tu7zi7d5va224nfegt84g0argkadivw4hlvkj_bpixff19r8j2lf1uhde2rex9ery9xskxm2f_2mpbgutdj6kt56n5proalpciwttcomv3j1pzev6qw3ism"
            )
        )
    }

testObject_SimpleMember_user_15 :: SimpleMember
testObject_SimpleMember_user_15 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "0000006e-0000-0037-0000-00610000007e"))),
      smConvRoleName = (fromJust (parseRoleName "rt25zies0df"))
    }

testObject_SimpleMember_user_16 :: SimpleMember
testObject_SimpleMember_user_16 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000042-0000-006a-0000-000800000052"))),
      smConvRoleName = (fromJust (parseRoleName "pknq1f2x"))
    }

testObject_SimpleMember_user_17 :: SimpleMember
testObject_SimpleMember_user_17 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000020-0000-0000-0000-00500000005c"))),
      smConvRoleName = (fromJust (parseRoleName "w1bcl23oz4ax6dg14h3y8nxqb77sx9ajonsvx7qd"))
    }

testObject_SimpleMember_user_18 :: SimpleMember
testObject_SimpleMember_user_18 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000049-0000-000c-0000-004d00000043"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName
                "u1c8n7lhvsnr5cdavje5wbezt4an_h92yp0bma6l_6h6dn67lh8_jpk8_eznfja7qhh7wkczfanq5esl7b9y2g16afnnsvgt6i48pmjeo1msq7uuvm"
            )
        )
    }

testObject_SimpleMember_user_19 :: SimpleMember
testObject_SimpleMember_user_19 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000031-0000-003d-0000-003800000024"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName
                "jzmvwd4h3ji2yc2wbog57546ono56qpsobzbszmed5y5436ub8lrvfydxmfleq4j6yj04vdivxpagt5lm5luplyy9zwcbjwyhgcom2njlzvj3ydbmol2onhp75p3"
            )
        )
    }

testObject_SimpleMember_user_20 :: SimpleMember
testObject_SimpleMember_user_20 =
  SimpleMember
    { smId = (Id (fromJust (UUID.fromString "00000074-0000-0010-0000-001600000078"))),
      smConvRoleName =
        ( fromJust
            ( parseRoleName
                "qbb0jgv5yq8ur0ogawcj0gx3f6yau5cnc5x3q8rnq5pn3mn4160ipvryoa2cpz0beg34ur64klqk5a2r9rqvc38w_gp6rbli54r46417_5mmylx5usc9"
            )
        )
    }
