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
module Test.Wire.API.Golden.Generated.LimitedQualifiedUserIdList_2020_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Id (Id (Id))
import Data.Qualified (Qualified (Qualified, qDomain, qUnqualified))
import Data.Range (unsafeRange)
import qualified Data.UUID as UUID (fromString)
import Imports (fromJust)
import Wire.API.User (LimitedQualifiedUserIdList (..))

testObject_LimitedQualifiedUserIdList_2020_user_1 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_1 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005bed-0000-0771-0000-447a00005b32"))),
                    qDomain = Domain {_domainText = "sh7636.gcg-c"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005953-0000-2b40-0000-567100002d0c"))),
                    qDomain = Domain {_domainText = "dw14y-97764r.26en.wa9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000012f7-0000-581b-0000-377600006eb9"))),
                    qDomain = Domain {_domainText = "6e-4.ic.paf"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000c1b-0000-59c0-0000-3ff000001533"))),
                    qDomain = Domain {_domainText = "wsy0vskgzy.7zb.u-g-p-58"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002419-0000-37e8-0000-329900001118"))),
                    qDomain = Domain {_domainText = "ug.mph1359u6sttb3w28v-968"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006112-0000-1fd0-0000-5ca500001e6f"))),
                    qDomain = Domain {_domainText = "hgrc.1y.kyvg3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005052-0000-09ec-0000-74d50000574e"))),
                    qDomain = Domain {_domainText = "p.q0h.b"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003e30-0000-3f72-0000-23ec000019a0"))),
                    qDomain = Domain {_domainText = "r376-462.74o6.0zo-z.9w50a2f9jn9.7rto1q7r.t-99"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003ae9-0000-4d95-0000-08f30000606b"))),
                    qDomain = Domain {_domainText = "dt-1.76n6.5-1.n.6-ax81lr5.k13tld-9.k.a0-bd-c.s-jt"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005051-0000-6a16-0000-6a3c00007a41"))),
                    qDomain = Domain {_domainText = "njz-3741-78-2--36.e0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000068c0-0000-2fb4-0000-2891000020d3"))),
                    qDomain = Domain {_domainText = "t335k19.mpv.i31k9.pnks6s"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000055b6-0000-236b-0000-52cb00002561"))),
                    qDomain = Domain {_domainText = "8-9.z3--ga"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007189-0000-04f6-0000-318500001872"))),
                    qDomain = Domain {_domainText = "ey4.09g.0wqpp01091.i-y.6pufi5-8vp2g8.lg.ad9p2"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_2 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_2 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000016a5-0000-000e-0000-2ad50000589c"))),
                    qDomain = Domain {_domainText = "t02vm1.gm810xwh1l4rb"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004bd5-0000-363b-0000-1af6000051cf"))),
                    qDomain = Domain {_domainText = "620au-6.2j.x23-5w"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006375-0000-417b-0000-3da900004015"))),
                    qDomain = Domain {_domainText = "809.b9m0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000075be-0000-770a-0000-471d00005410"))),
                    qDomain = Domain {_domainText = "4bc1.k7-z"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002999-0000-35d4-0000-413300001831"))),
                    qDomain = Domain {_domainText = "010.2bu3-2hu.s164s1-2f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000016d9-0000-5086-0000-65cb00000e53"))),
                    qDomain = Domain {_domainText = "z8.q.l"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006c57-0000-16c2-0000-5eb200001985"))),
                    qDomain = Domain {_domainText = "g-40a.pa2-4"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005572-0000-1ad1-0000-75d700000dc8"))),
                    qDomain = Domain {_domainText = "0e-1-5-9.h085rwr815"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007231-0000-30dc-0000-692e00006a70"))),
                    qDomain = Domain {_domainText = "8cp.0.b5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000458d-0000-373c-0000-2799000037cd"))),
                    qDomain = Domain {_domainText = "0.s-16"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004a0c-0000-185d-0000-472d00002ef4"))),
                    qDomain = Domain {_domainText = "9mk.mbz.75307.e9vg.n1.m7kv"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000069d1-0000-3674-0000-31db0000330b"))),
                    qDomain = Domain {_domainText = "u86.7z-v0.q8-78"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007645-0000-015b-0000-41470000445d"))),
                    qDomain = Domain {_domainText = "22.r-r8k86"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000c73-0000-44b8-0000-5712000045fc"))),
                    qDomain = Domain {_domainText = "1-fx97tg2.j7"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_3 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_3 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000027a-0000-42bd-0000-2fdb00000811"))),
                    qDomain = Domain {_domainText = "06s.eaq.xbih-26z--5.jqaqc"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007fce-0000-1b96-0000-2d96000022bc"))),
                    qDomain = Domain {_domainText = "v8nj2.c11974lq.go"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002688-0000-4fed-0000-15550000504b"))),
                    qDomain = Domain {_domainText = "d3.q6h2k"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007f08-0000-3f81-0000-183d00000a31"))),
                    qDomain = Domain {_domainText = "1s4hu-4-j.63ja.o"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003717-0000-6494-0000-38f100001684"))),
                    qDomain = Domain {_domainText = "33.uz"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003f18-0000-4543-0000-04b800007af9"))),
                    qDomain = Domain {_domainText = "s.49-412g.tq38odf83m"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000043e7-0000-0e87-0000-42c000003d59"))),
                    qDomain = Domain {_domainText = "8q7i3.d.4.z1es5.t53.jj540u82s13u.5-2-0y.yf41vaq.ii8-9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002858-0000-2ed0-0000-75b800000f63"))),
                    qDomain = Domain {_domainText = "1qses.gw.7j6e-97-n5c--41aep6-ka.p5-i4ju1k-f-qgj.5bf.u83-88"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000df6-0000-1c5c-0000-524e0000722d"))),
                    qDomain = Domain {_domainText = "1.d--h6n7-7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006298-0000-4211-0000-46e600007d43"))),
                    qDomain = Domain {_domainText = "f.pe25"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003de8-0000-3ec3-0000-471900001e22"))),
                    qDomain = Domain {_domainText = "660v.w.x-3m"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002770-0000-067d-0000-6c2700004c25"))),
                    qDomain = Domain {_domainText = "e05.tk"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000025ff-0000-320c-0000-1f3000005a89"))),
                    qDomain = Domain {_domainText = "6628-mnod16vk-q.s087"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005748-0000-16a9-0000-6ea200001808"))),
                    qDomain = Domain {_domainText = "y15-4i.1.v-v--3x.za2o.j-jcxy13m3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000031a0-0000-026f-0000-4b1100002e59"))),
                    qDomain = Domain {_domainText = "4-0jag.z5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000a8f-0000-31bb-0000-758d00001228"))),
                    qDomain = Domain {_domainText = "k89l3.s84u.j"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003989-0000-7b83-0000-658b00003037"))),
                    qDomain = Domain {_domainText = "6lht3--5028.n901-d.e8v.l6k-5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000033c5-0000-1bcd-0000-52770000345c"))),
                    qDomain = Domain {_domainText = "8-4p5.j7a.jw"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000078f9-0000-5175-0000-281f00007560"))),
                    qDomain = Domain {_domainText = "1i0.w0u02oh-664c1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005639-0000-6968-0000-5e500000794f"))),
                    qDomain = Domain {_domainText = "7--04wq.q-r"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_4 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_4 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000070a1-0000-7aa2-0000-09d80000161f"))),
                    qDomain = Domain {_domainText = "c.8-023o-842jfqd-n11womr81-2.q3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002210-0000-7baf-0000-444a00001684"))),
                    qDomain = Domain {_domainText = "z.tjab089t"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003ce0-0000-6f08-0000-184100000079"))),
                    qDomain = Domain {_domainText = "rzsu.5rxxw.a2-q505c58i"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006f6f-0000-5ae7-0000-59f600001ce9"))),
                    qDomain = Domain {_domainText = "7nr.b09-0zj5r-x.gqeb9d.f9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000659b-0000-07b5-0000-4db800002c52"))),
                    qDomain = Domain {_domainText = "7c1irw39.wkc.u.0--h05.37wo.yx0mj.d1sdwqmgy0t7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000228e-0000-2891-0000-439000006693"))),
                    qDomain = Domain {_domainText = "8x3-0t.o3.n99"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000446f-0000-043f-0000-284e00006e73"))),
                    qDomain = Domain {_domainText = "79.j03xh73n66-pc-6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003fcd-0000-4f99-0000-2ae600002d4b"))),
                    qDomain = Domain {_domainText = "66v.y-3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000053a3-0000-3ddd-0000-19820000294d"))),
                    qDomain = Domain {_domainText = "8.e5.e76.wws05el.z"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001b98-0000-3008-0000-544c0000707d"))),
                    qDomain = Domain {_domainText = "0drfgvr38.5-t-4.z87.8v.o-2j9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000060bf-0000-6cc0-0000-4c6800003505"))),
                    qDomain = Domain {_domainText = "b5.4-2-y1.p7h.3urzu-pc5j.krq36.s498a"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006510-0000-44c8-0000-4840000034db"))),
                    qDomain = Domain {_domainText = "68.1.9dyn.135h-e4i5.s83"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001467-0000-7458-0000-25930000232e"))),
                    qDomain = Domain {_domainText = "v--50gum0.05u.u"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000c43-0000-62c2-0000-59ce00000650"))),
                    qDomain = Domain {_domainText = "77ek.f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000461f-0000-64cd-0000-0d8400007072"))),
                    qDomain = Domain {_domainText = "0-3s94.44h47uy.fdzis4xj-yywzd"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000037c4-0000-1e74-0000-531800003136"))),
                    qDomain = Domain {_domainText = "8l9t4.qy8"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_5 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_5 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007de3-0000-1c8f-0000-1f92000009f5"))),
                    qDomain = Domain {_domainText = "o-5--9-gk4.8q626hgb.1147.m.oj-8.f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003c26-0000-2af0-0000-517700000473"))),
                    qDomain = Domain {_domainText = "w.x.m"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_6 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_6 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000064b7-0000-4eda-0000-493c00004a3c"))),
                    qDomain = Domain {_domainText = "07313.51n.r-3545op"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005ab2-0000-0fae-0000-7dc20000611f"))),
                    qDomain = Domain {_domainText = "t7452-c5c.b67"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004bf5-0000-065d-0000-6d9900002a46"))),
                    qDomain = Domain {_domainText = "6ru2-i.fm.537f.kx-j-c45"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_7 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_7 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000036da-0000-4904-0000-3000000033bf"))),
                    qDomain = Domain {_domainText = "yz.s69"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007eef-0000-3fc4-0000-707d00004ca9"))),
                    qDomain = Domain {_domainText = "up-a0.f-d5s2"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000181f-0000-4315-0000-5c8500002fca"))),
                    qDomain = Domain {_domainText = "4-0.l4383.sv7ris"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000330d-0000-6a09-0000-09c600001846"))),
                    qDomain = Domain {_domainText = "87k-1.1rq1.q"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000277c-0000-6769-0000-6a4500007223"))),
                    qDomain = Domain {_domainText = "56.3an.g810-5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004876-0000-3492-0000-496a00004ece"))),
                    qDomain = Domain {_domainText = "u42h.m450s29"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002f16-0000-4420-0000-6dbc00002c63"))),
                    qDomain = Domain {_domainText = "q.vv"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006b8f-0000-40ac-0000-419c00000293"))),
                    qDomain = Domain {_domainText = "0.is898n-43"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004e89-0000-2935-0000-5377000011b0"))),
                    qDomain = Domain {_domainText = "r0pbc6.r-d"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002002-0000-0eca-0000-2e87000057c6"))),
                    qDomain = Domain {_domainText = "53jmrm174-h3e-6dc-8.w"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003722-0000-4db5-0000-3ccd00000a87"))),
                    qDomain = Domain {_domainText = "5.m-5a121y7626qx.u--ow93937"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006aae-0000-751f-0000-33010000320f"))),
                    qDomain = Domain {_domainText = "66f-9s.vd-08g"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004861-0000-2d48-0000-515100003b38"))),
                    qDomain = Domain {_domainText = "1fu.p-77zp"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000070d4-0000-0839-0000-172400003284"))),
                    qDomain = Domain {_domainText = "p7-n.f1028"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000044c8-0000-436d-0000-61d800005c3a"))),
                    qDomain = Domain {_domainText = "2pj.f14.f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007e01-0000-208b-0000-5bde00007e98"))),
                    qDomain = Domain {_domainText = "t.m.xq-e17-o.jz432"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_8 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_8 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001533-0000-0cd6-0000-488c00000223"))),
                    qDomain = Domain {_domainText = "jq2.h"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001ca9-0000-3d4c-0000-5b7600000a4f"))),
                    qDomain = Domain {_domainText = "98n0.ktvkb0qlx.5.i"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000757d-0000-13f8-0000-53fb000059d8"))),
                    qDomain = Domain {_domainText = "si320k62.5wh.rwnj"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007dc1-0000-0874-0000-499200005920"))),
                    qDomain = Domain {_domainText = "t255.nd8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000079b5-0000-3602-0000-5fb500006d48"))),
                    qDomain = Domain {_domainText = "0u.q5z1sy"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000169c-0000-0d25-0000-3fbf00007bb7"))),
                    qDomain = Domain {_domainText = "a4a.z3b.nao2---0uv-il"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_9 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_9 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000406b-0000-0c05-0000-7a7f00006547"))),
                    qDomain = Domain {_domainText = "2.07io.g464-sf8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006cc0-0000-3409-0000-094700002637"))),
                    qDomain = Domain {_domainText = "wrbsoq.0s-2.u6.xnd5.k9r-8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000017d9-0000-4cc8-0000-5dc8000009e0"))),
                    qDomain = Domain {_domainText = "u21.v6n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002139-0000-07f7-0000-20210000535c"))),
                    qDomain = Domain {_domainText = "e8kr600xi.pb81-7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001ae2-0000-056b-0000-2bdc000030a9"))),
                    qDomain = Domain {_domainText = "38y2.2iip7.e-i.0893.f7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001568-0000-3c21-0000-73eb00001a78"))),
                    qDomain = Domain {_domainText = "2x2.53.4a-5x7ad.cay2zjw--z9-1.avpv9-1x1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000453b-0000-26c2-0000-622d00007b3b"))),
                    qDomain = Domain {_domainText = "2im.g"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000abe-0000-5b93-0000-1130000033d9"))),
                    qDomain = Domain {_domainText = "4.31evw16.kgcf.u6m.0-s3j745.0h2.aavp99h"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000044d4-0000-56d6-0000-41a8000040e0"))),
                    qDomain = Domain {_domainText = "c9.k0s.02----2-8nk2q50.cr5-ns.g5-n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000015d6-0000-4f61-0000-72e500004d5f"))),
                    qDomain = Domain {_domainText = "i9.c12"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007b49-0000-758d-0000-73d9000074c9"))),
                    qDomain = Domain {_domainText = "s8y-j-cw3.u8e.6.o5xje.ms9gq-290.3.m2n756j101q.t690"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002562-0000-5a59-0000-05a100000c18"))),
                    qDomain = Domain {_domainText = "q9--p3c0hw.1.dkd"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000427f-0000-3c33-0000-4629000077b8"))),
                    qDomain = Domain {_domainText = "4u9.a-1j3p"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002f4c-0000-17f0-0000-51ba00001b95"))),
                    qDomain = Domain {_domainText = "f33.s1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000458-0000-1a4f-0000-6b3400000c30"))),
                    qDomain = Domain {_domainText = "126.o7-2"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000634-0000-2ff7-0000-62600000322b"))),
                    qDomain = Domain {_domainText = "0v.4qytdlh7t066p59m-km-40.k"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000021ea-0000-5833-0000-707b00005042"))),
                    qDomain = Domain {_domainText = "wp.o.3-s.h4-t6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000e79-0000-4ab3-0000-57bb00007ca8"))),
                    qDomain = Domain {_domainText = "377.h9m"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_10 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_10 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007b0e-0000-3489-0000-075c00005be7"))),
                    qDomain = Domain {_domainText = "k30p.u-q5.z8--9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007dd8-0000-45af-0000-23c400001cc9"))),
                    qDomain = Domain {_domainText = "54e.75-24.ycx80-0j9hl"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000020c6-0000-0c74-0000-6c0200007917"))),
                    qDomain = Domain {_domainText = "1.b8u6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006465-0000-4dfb-0000-011100004ced"))),
                    qDomain = Domain {_domainText = "09js-x.q3dh0.400.c"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000069ce-0000-77fd-0000-063b0000215a"))),
                    qDomain = Domain {_domainText = "17845k.kj9juu63k.79j6x3b.x5rzt"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000768e-0000-027e-0000-2630000069c7"))),
                    qDomain = Domain {_domainText = "9h.0.0.fbi7.l32er.b-0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004eea-0000-4b92-0000-64840000084e"))),
                    qDomain = Domain {_domainText = "1.s.z9ykaf5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000078f-0000-4f8e-0000-07a20000002b"))),
                    qDomain = Domain {_domainText = "37h.w84715.m4"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000757-0000-203e-0000-74ce0000158b"))),
                    qDomain = Domain {_domainText = "b6.k5g3.3ozcd.2.0.z2-1hj"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000dbb-0000-7821-0000-7c8500003661"))),
                    qDomain = Domain {_domainText = "b47-0o2.b335"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005695-0000-4799-0000-461d00004c32"))),
                    qDomain = Domain {_domainText = "87.lo-nc"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000b19-0000-7cf6-0000-4c1f000040ca"))),
                    qDomain = Domain {_domainText = "a5d.z40n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000302e-0000-6a65-0000-21d90000268a"))),
                    qDomain = Domain {_domainText = "ez9.lc-3h8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000032ae-0000-2713-0000-2286000031b4"))),
                    qDomain = Domain {_domainText = "2g-c.h6569.602.j5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003993-0000-0d94-0000-167f00006327"))),
                    qDomain = Domain {_domainText = "125-x.g6l8"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_11 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_11 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005ac5-0000-11dd-0000-578100007f21"))),
                    qDomain = Domain {_domainText = "8---v6s64rx1.t.y3d"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000056cc-0000-6489-0000-6fbd00001428"))),
                    qDomain = Domain {_domainText = "s3j86.r00"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000d55-0000-2626-0000-1be500002927"))),
                    qDomain = Domain {_domainText = "2q.t"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001088-0000-489f-0000-73f2000068b8"))),
                    qDomain = Domain {_domainText = "k8.7bk---2es.mbq"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006310-0000-7d5d-0000-65d20000555c"))),
                    qDomain = Domain {_domainText = "5s9s--1.d8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001592-0000-1b6b-0000-011d00005365"))),
                    qDomain = Domain {_domainText = "qo.q443.2c-l61-73.8sy269.k30"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005b03-0000-76b4-0000-5311000050f9"))),
                    qDomain = Domain {_domainText = "n.h6p"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001e66-0000-6c43-0000-3a3d00002e55"))),
                    qDomain = Domain {_domainText = "th-28m.y00"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000062a9-0000-147b-0000-0aa6000034ce"))),
                    qDomain = Domain {_domainText = "77uc.v4-ob.ty"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_12 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_12 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005baf-0000-0b85-0000-5fe6000002bc"))),
                    qDomain = Domain {_domainText = "5.g"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000215e-0000-0070-0000-1b2e00007d34"))),
                    qDomain = Domain {_domainText = "yw3y.h8.9u1p.n70-8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002b43-0000-548f-0000-30b800007417"))),
                    qDomain = Domain {_domainText = "i1.f7i"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007bf1-0000-3b3c-0000-23f600006104"))),
                    qDomain = Domain {_domainText = "231wo62.u296"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000074e8-0000-60de-0000-502000007602"))),
                    qDomain = Domain {_domainText = "y6.c7b9pn.dyj.wbj7-jf0-mjw6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001650-0000-2acc-0000-48cf0000105f"))),
                    qDomain = Domain {_domainText = "54.rft"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000144f-0000-2acd-0000-637000002760"))),
                    qDomain = Domain {_domainText = "1tb4.67sg7.m60x804lm"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006bc4-0000-5e39-0000-43120000670f"))),
                    qDomain = Domain {_domainText = "d-5-5.k"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000013a1-0000-4d65-0000-1f400000055c"))),
                    qDomain = Domain {_domainText = "9024-r.35.s6.w5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004445-0000-21ae-0000-5bc500003975"))),
                    qDomain = Domain {_domainText = "2fgsir.iwu"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000063b4-0000-2dbb-0000-60e8000012f1"))),
                    qDomain = Domain {_domainText = "8ed.ty0tno"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000469c-0000-4553-0000-625e00002207"))),
                    qDomain = Domain {_domainText = "u-6.f8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004ec6-0000-429d-0000-5d5000005a14"))),
                    qDomain = Domain {_domainText = "9x.0.0-8--v.d-93u-7-k.l3104"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_13 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_13 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000475e-0000-38ec-0000-391400001acc"))),
                    qDomain = Domain {_domainText = "ze-e.j2r"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000043ba-0000-7985-0000-182a0000506e"))),
                    qDomain = Domain {_domainText = "23.jpt063n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007e10-0000-7798-0000-624100004f2d"))),
                    qDomain = Domain {_domainText = "9ha.p80.o1u-1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006c62-0000-1c79-0000-2b9800006efd"))),
                    qDomain = Domain {_domainText = "tj46.v8.qaqm"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001698-0000-27fd-0000-7db100003861"))),
                    qDomain = Domain {_domainText = "n64o9z-br.j15-q.g915i51c.e8194"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006cd3-0000-24fd-0000-09f400005ba9"))),
                    qDomain = Domain {_domainText = "915-1w.85782-0.a"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000300a-0000-162c-0000-3ee700005878"))),
                    qDomain = Domain {_domainText = "487q7.y164on2"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000686a-0000-4dc3-0000-5ad200001895"))),
                    qDomain = Domain {_domainText = "880eu0.y9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000035d9-0000-1499-0000-7ef40000702d"))),
                    qDomain = Domain {_domainText = "3qvah.sx1-2"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007097-0000-0f45-0000-43ca0000574a"))),
                    qDomain = Domain {_domainText = "g0-c.t78ob0f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004643-0000-0913-0000-263d00002ec2"))),
                    qDomain = Domain {_domainText = "d0-j75303.k-e4-946q0.8h5nb-t--w-of.gm7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002d65-0000-31b9-0000-20df00007924"))),
                    qDomain = Domain {_domainText = "d6-fo.y028"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000053a0-0000-053e-0000-47c100006543"))),
                    qDomain = Domain {_domainText = "n2q0ano4z3.o111-3v-5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004080-0000-0c3e-0000-65b900000a32"))),
                    qDomain = Domain {_domainText = "to.7z.39.lb.noxz"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006e11-0000-3fd3-0000-7d660000121a"))),
                    qDomain = Domain {_domainText = "9-1u.5--2zb.dh-485un.a.b455b"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000430-0000-2075-0000-4cd500004b77"))),
                    qDomain = Domain {_domainText = "3281.wk55l"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_14 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_14 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000042a7-0000-32cb-0000-1b6100007e97"))),
                    qDomain = Domain {_domainText = "j-ip-l.7-1v.k95u43z3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004a01-0000-719f-0000-687e000065e8"))),
                    qDomain = Domain {_domainText = "1fuok9.6-l.b-t926.171.l-8pfu"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005d4f-0000-38a9-0000-1d6b00007d93"))),
                    qDomain = Domain {_domainText = "227ot.61z.tr"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000173c-0000-1a35-0000-48120000671e"))),
                    qDomain = Domain {_domainText = "s-0.x1lw1.l940-5-1ip"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007329-0000-386b-0000-08c300001a02"))),
                    qDomain = Domain {_domainText = "48lg-78-mp.j.2g.v6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002714-0000-6ff3-0000-661200005afc"))),
                    qDomain = Domain {_domainText = "5.mc.642.z6ezu0n24.5dmb9.p32940g"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005d7b-0000-42c8-0000-4c2100007901"))),
                    qDomain = Domain {_domainText = "y---wl.01w5.j5j72.z--d0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000072c5-0000-5e32-0000-24810000445d"))),
                    qDomain = Domain {_domainText = "zf-njrau.hnzq"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004b43-0000-5516-0000-557400000a48"))),
                    qDomain = Domain {_domainText = "z0-atdfh7.j"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000002a1-0000-6666-0000-598f00005906"))),
                    qDomain = Domain {_domainText = "z.i89r"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000063f8-0000-4d4f-0000-18e400005d2e"))),
                    qDomain = Domain {_domainText = "54l.v"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000005d7-0000-446c-0000-412000003819"))),
                    qDomain = Domain {_domainText = "5mway6.a5"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_15 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_15 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000c25-0000-79e0-0000-541600004086"))),
                    qDomain = Domain {_domainText = "f01r.m"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000050bc-0000-4f16-0000-21d400005201"))),
                    qDomain = Domain {_domainText = "58amj.if8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002856-0000-4dc3-0000-767200003807"))),
                    qDomain = Domain {_domainText = "6.u5vi2.ue.4-2-e.43.nw"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000014a4-0000-3d56-0000-03e3000054d8"))),
                    qDomain = Domain {_domainText = "k.ii"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000206e-0000-5537-0000-2e0800006b16"))),
                    qDomain = Domain {_domainText = "7688.e9h.95-r--2dbdvt6.j.x9ol.dp6e"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000007a0-0000-18b0-0000-5d9200006699"))),
                    qDomain = Domain {_domainText = "2ru96lpzoyh7t5u.t9.d-t.re"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006edc-0000-5d78-0000-652400003edc"))),
                    qDomain = Domain {_domainText = "08y.42w-9v-10.ak5---5w"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007e47-0000-6da7-0000-26fd00003b7b"))),
                    qDomain = Domain {_domainText = "3.b7po"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000cee-0000-6ee0-0000-106700007a67"))),
                    qDomain = Domain {_domainText = "6w-5.1xe26.80lg.jw9.mex.oegh0706n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000021b1-0000-245c-0000-7a9100007085"))),
                    qDomain = Domain {_domainText = "j0de1800---s0.9.psqs"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006a1a-0000-6b70-0000-1994000077fa"))),
                    qDomain = Domain {_domainText = "p8coz0-tebsr85f.f.zxk.l-yq-y--y79222"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000018b3-0000-2d82-0000-2bc1000074ae"))),
                    qDomain = Domain {_domainText = "s2.e-dkwb.o.465903al--y.q5gn"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003080-0000-1228-0000-50f500006f76"))),
                    qDomain = Domain {_domainText = "v.62-5.gnk6.i7b"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000b92-0000-0818-0000-4392000026f4"))),
                    qDomain = Domain {_domainText = "8wd7.n990"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005589-0000-19ca-0000-24da00003073"))),
                    qDomain = Domain {_domainText = "qh-m.1.xgihs80"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005238-0000-7ca4-0000-4139000066a8"))),
                    qDomain = Domain {_domainText = "7q-wi6d-42a-t-j.egp2.9z1h-2-n-0--y.r2fej7.7.v-6.80g0.d4"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_16 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_16 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002d40-0000-6639-0000-3e6300005645"))),
                    qDomain = Domain {_domainText = "72086-5g6.n4d6.r"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004351-0000-78e5-0000-22ec0000582e"))),
                    qDomain = Domain {_domainText = "26mg.x.rw9h.44o22.k54"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000cce-0000-565a-0000-640400007618"))),
                    qDomain = Domain {_domainText = "c02dw6e-17.7---ps8.q7-3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000003cb-0000-2902-0000-3225000013e2"))),
                    qDomain = Domain {_domainText = "96i8.z7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001ecc-0000-0ca4-0000-5be000003aa3"))),
                    qDomain = Domain {_domainText = "4c86.fto6un"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003ce3-0000-25bb-0000-5dff00007832"))),
                    qDomain = Domain {_domainText = "38gp7.i73"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000453-0000-2978-0000-2cbd00001358"))),
                    qDomain = Domain {_domainText = "s7h6-8.ut2"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000c0a-0000-7937-0000-271000007ae6"))),
                    qDomain = Domain {_domainText = "2kb.9--kl.hyj"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006d99-0000-163f-0000-179c000076de"))),
                    qDomain = Domain {_domainText = "k3v.q21.8dlz.y4"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000413e-0000-2f3d-0000-5e2f000006cb"))),
                    qDomain = Domain {_domainText = "y-f78.72.aqk6a"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002eb2-0000-74bc-0000-028000006a15"))),
                    qDomain = Domain {_domainText = "85-n-v.a-5j"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_17 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_17 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000307a-0000-5e89-0000-1ac9000011e7"))),
                    qDomain = Domain {_domainText = "m-26n-8248.w-y.q3-p"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001b46-0000-45d9-0000-6c53000054b2"))),
                    qDomain = Domain {_domainText = "h-5-20.g-yd"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004013-0000-5a56-0000-27b20000118a"))),
                    qDomain = Domain {_domainText = "p9tn1y-0e.f7.k-7-7tp"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003bf9-0000-28d3-0000-6f300000431d"))),
                    qDomain = Domain {_domainText = "u1wvly.x348y848-3f917ae"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000017c9-0000-5c6d-0000-30f8000063f5"))),
                    qDomain = Domain {_domainText = "unw5.u07"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000613c-0000-29c0-0000-49ac00003fea"))),
                    qDomain = Domain {_domainText = "2-r.x"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000064f6-0000-74b2-0000-791700002965"))),
                    qDomain = Domain {_domainText = "s0--aw-4-e0-3.dm.r0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000d5f-0000-4e9a-0000-400400003da9"))),
                    qDomain = Domain {_domainText = "x5.bm.6-36.o9-v2-4"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000022b3-0000-12ad-0000-3f170000060e"))),
                    qDomain = Domain {_domainText = "m-200.hz8-790bfb974"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000894-0000-41f7-0000-6e0d00002aaf"))),
                    qDomain = Domain {_domainText = "3s250z0.2-dd8a0f2.dp89d"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002c22-0000-01fb-0000-02f600005b08"))),
                    qDomain = Domain {_domainText = "ih.0n0--9--qlk.736.16v.5.8l.9.e"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000026c-0000-4409-0000-2bc200003589"))),
                    qDomain = Domain {_domainText = "9jp0.r.b1.1kex3k7o8-7s.e.4vx.7yhx-y.82pwmd.u5zv5cfv-a435.3.1.i52"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000041b8-0000-4a59-0000-390b00004250"))),
                    qDomain = Domain {_domainText = "3.n3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003caa-0000-4dac-0000-747d0000204a"))),
                    qDomain = Domain {_domainText = "1d4i9.e-8.nc3698q-tp7bu"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005d7c-0000-1dfa-0000-323f00000e43"))),
                    qDomain = Domain {_domainText = "3--7.w"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006665-0000-0b5b-0000-31e300000435"))),
                    qDomain = Domain {_domainText = "17.c9"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006671-0000-3602-0000-5064000037f6"))),
                    qDomain = Domain {_domainText = "y4-le9r6.295j-v-3oad.lx64.j6"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000561-0000-7d9b-0000-513a00007677"))),
                    qDomain = Domain {_domainText = "3---e59---u.1-7b.hl-o.mg.qnm292"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000bdf-0000-6e75-0000-44dd00001d49"))),
                    qDomain = Domain {_domainText = "c.dya-8w625yg9"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_18 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_18 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001804-0000-6e5a-0000-4eab000018cc"))),
                    qDomain = Domain {_domainText = "g.n596a.ab.q09g-7--a11"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000052b4-0000-3543-0000-486300007862"))),
                    qDomain = Domain {_domainText = "2rt.l5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000033cf-0000-515d-0000-636300006773"))),
                    qDomain = Domain {_domainText = "253.n8l-g85"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000649a-0000-6461-0000-639e000000e2"))),
                    qDomain = Domain {_domainText = "h1-r.5l13.6-4.t4z3.so"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000338d-0000-4926-0000-17ad00001921"))),
                    qDomain = Domain {_domainText = "5i57.v1-y"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003e1a-0000-56ab-0000-597a0000325a"))),
                    qDomain = Domain {_domainText = "0n-c.e72-yd"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000cb1-0000-12d5-0000-52d700007bd4"))),
                    qDomain = Domain {_domainText = "xb2717.7pjc.e1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003395-0000-5ba0-0000-057d00004458"))),
                    qDomain = Domain {_domainText = "u6-p.ap"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002ab6-0000-48a8-0000-00b60000292a"))),
                    qDomain = Domain {_domainText = "x13.u2mw.o5i.w--l172t"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000025b9-0000-0bb8-0000-7ac5000016a7"))),
                    qDomain = Domain {_domainText = "rf.qw4"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000045d5-0000-4768-0000-1ef5000078c4"))),
                    qDomain = Domain {_domainText = "f49c7ot.61.e8xkl5a-k90-x59.8087d-o.p7m-bg"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001884-0000-0afc-0000-340b00005592"))),
                    qDomain = Domain {_domainText = "joua87.elmdl5i"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005ae5-0000-6d3c-0000-73ae00004c05"))),
                    qDomain = Domain {_domainText = "ck.n8-8gv27-b-h"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005726-0000-14ea-0000-17480000204e"))),
                    qDomain = Domain {_domainText = "yx3.z0ok.f-3"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006f73-0000-7cac-0000-247f00001533"))),
                    qDomain = Domain {_domainText = "i60k7.e-9"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_19 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_19 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005908-0000-331e-0000-50e90000660e"))),
                    qDomain = Domain {_domainText = "2r.vxj87-e--91-50.5.n-s.0r09o-n4w0ax.6n-1b.m--j"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003595-0000-586f-0000-74f70000747f"))),
                    qDomain = Domain {_domainText = "13rrbh.8-0.5.c4.3n.q"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000697-0000-7891-0000-37bc0000652d"))),
                    qDomain = Domain {_domainText = "7t7s.nd1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000013c3-0000-2387-0000-5bd200005a7c"))),
                    qDomain = Domain {_domainText = "154ailfu.4i3p-in9.a53w"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000576e-0000-3c78-0000-486a00000ebb"))),
                    qDomain = Domain {_domainText = "56.2.fu.22q.8--14-3u.9v11v.ll-d"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007550-0000-1992-0000-535d00007f78"))),
                    qDomain = Domain {_domainText = "9of.j.2.l.1-xy3o2-6jm5-3b.a5qk91.9ie1oc.cl--ia955"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006ac3-0000-50c7-0000-71d100003262"))),
                    qDomain = Domain {_domainText = "w.s"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000d04-0000-686a-0000-1efe00002502"))),
                    qDomain = Domain {_domainText = "8a30.4.pqffe"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004183-0000-1706-0000-1cc100004a6b"))),
                    qDomain = Domain {_domainText = "3y9.51h6a3.w2z8"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000199c-0000-0c7a-0000-361c00004756"))),
                    qDomain = Domain {_domainText = "b00qi1d.2-b.ux0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007e44-0000-521f-0000-610800003437"))),
                    qDomain = Domain {_domainText = "6rdda39---p.0-707gp6cf.vwt.n"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005f32-0000-173a-0000-467200002d38"))),
                    qDomain = Domain {_domainText = "x6u-q-t.u0"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00003b2f-0000-3345-0000-611900001081"))),
                    qDomain = Domain {_domainText = "3e.l1.pd.m-ph2t"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000ad1-0000-64e7-0000-6f78000079cf"))),
                    qDomain = Domain {_domainText = "5--k.qd-3.i-01-k"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005ebe-0000-2820-0000-09880000152e"))),
                    qDomain = Domain {_domainText = "211.li37"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004438-0000-3d15-0000-454100003176"))),
                    qDomain = Domain {_domainText = "8-vi.v.z"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007f68-0000-1cc6-0000-4d41000000d5"))),
                    qDomain = Domain {_domainText = "5.h5"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000035b4-0000-56e0-0000-4bde00004ace"))),
                    qDomain = Domain {_domainText = "0aj.wgm.i3ql.w-m8-0fkh.a9gr"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006a6f-0000-5b10-0000-65fe000011f1"))),
                    qDomain = Domain {_domainText = "mk.a3ylw"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000024b2-0000-05a6-0000-0ed200007cdc"))),
                    qDomain = Domain {_domainText = "n238.a.3.zt-x"}
                  }
              ]
            )
        )
    }

testObject_LimitedQualifiedUserIdList_2020_user_20 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_20 =
  LimitedQualifiedUserIdList
    { qualifiedUsers =
        ( unsafeRange
            ( [ Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001eb7-0000-5c21-0000-1418000038be"))),
                    qDomain = Domain {_domainText = "3fb5t7-7x.d6893-5-g-23-37"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000025cc-0000-362c-0000-098e00004748"))),
                    qDomain = Domain {_domainText = "r9ig5y.mkg-16"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000656b-0000-3adb-0000-365e000020e6"))),
                    qDomain = Domain {_domainText = "v.p491.e"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00004338-0000-1fd8-0000-0dd90000728e"))),
                    qDomain = Domain {_domainText = "4.f"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000001b8-0000-6314-0000-1dd600005284"))),
                    qDomain = Domain {_domainText = "10xem.0w5b77n.of1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00007b75-0000-30e5-0000-6f9f000027fc"))),
                    qDomain = Domain {_domainText = "3u2.39.y1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006fae-0000-0e42-0000-132f00000f89"))),
                    qDomain = Domain {_domainText = "3--4-ojhd.l-4t0.czw"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000011ad-0000-6124-0000-261900003e84"))),
                    qDomain = Domain {_domainText = "yj.gkm"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000058d-0000-1b5b-0000-6b6400004f75"))),
                    qDomain = Domain {_domainText = "6613.r462559qf"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00005147-0000-0e15-0000-75e0000012b0"))),
                    qDomain = Domain {_domainText = "27zw.tu1w3w8.u9di6.d"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000886-0000-4a6c-0000-1f6400001429"))),
                    qDomain = Domain {_domainText = "v38n.753.j-0-1o"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00000a49-0000-6a09-0000-3f3200004517"))),
                    qDomain = Domain {_domainText = "c7os-n.yjp"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006213-0000-211e-0000-6e3500001d96"))),
                    qDomain = Domain {_domainText = "17.2p6-6.y626-m0.8e.m4f.g"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002dfc-0000-6836-0000-4fc00000572b"))),
                    qDomain = Domain {_domainText = "0n-0h.cx"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001097-0000-2761-0000-74b100004a0b"))),
                    qDomain = Domain {_domainText = "k2-6.gqub.t1"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "0000384b-0000-3c2a-0000-181d000011e6"))),
                    qDomain = Domain {_domainText = "15abt9-hk.qr"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00001826-0000-3e92-0000-11b800000c4d"))),
                    qDomain = Domain {_domainText = "g9-2.1cc.tv.b7"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "000000c4-0000-33ac-0000-796a00003581"))),
                    qDomain = Domain {_domainText = "6.m.g56-y"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00006c54-0000-12e3-0000-521c000040fe"))),
                    qDomain = Domain {_domainText = "cs.q56t-s"}
                  },
                Qualified
                  { qUnqualified = (Id (fromJust (UUID.fromString "00002e84-0000-3519-0000-3b410000306f"))),
                    qDomain = Domain {_domainText = "l59n63b-r4.jq-1"}
                  }
              ]
            )
        )
    }
