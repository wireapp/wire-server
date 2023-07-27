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

module Test.Wire.API.Golden.Generated.BotConvView_provider where

import Data.Domain
import Data.Id (Id (Id))
import Data.Qualified
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation.Member (OtherMember (..))
import Wire.API.Conversation.Role (parseRoleName)
import Wire.API.Provider.Bot (BotConvView, botConvView)
import Wire.API.Provider.Service (ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider))

domain :: Domain
domain = Domain "golden.example.com"

testObject_BotConvView_provider_1 :: BotConvView
testObject_BotConvView_provider_1 =
  botConvView
    (Id (fromJust (UUID.fromString "00000006-0000-0012-0000-001900000009")))
    Nothing
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "t4vroye869mch4")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "bi8z5mc78lg3bqqk29yd36x2_haz6b05t6ybil8p7zbkj")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "ncz23zan6fw786izkcx")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "_brjrjrldhybr251gl72y3_nqqwhdh8k2c0oznqgiwrhzf0szdd15laruwrrm640pa_z8eg5d2mvm_nppm51rszf20dwpshy7ushykyavtq5dq2mwdqqcpv_nb7lkl"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "jpy159h7vqij1p08dgsehcpyxg6_ovkcpjruqg6xp8b4lpegp7qrfr_qsyoo3qnngi7btjxrt6bbjcfmit2p6g_j5abxj4o5xliz"
              )
        }
    ]

testObject_BotConvView_provider_2 :: BotConvView
testObject_BotConvView_provider_2 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001a-0000-0015-0000-00200000000a")))
    Nothing
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName =
            fromJust (parseRoleName "1p003q7r9_fcclm1gcds98jwmgt7ilnw2p50cvvdmgu0gp2swep5k9kjs_iilqse9qkqtj7b")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "ejicn7cgzgb5qmbd2u7azzyuxk3s7_lp1g9vq74qklpqjjpi")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "bdddc4zidwriaaj33u9qf87lwt757280x1ov2fp61al58353p79ngwnd002")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))
                  }
              ),
          omConvRoleName =
            fromJust (parseRoleName "k_l3q0tp4vkvnbld_k4gd6d45pyjk8u41aom2y2yh1ysfkd0cg3st9_bf2qu8genm7_r6yop0")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "u34kdore13nneih4_yvz6hrzdn1fbknebgfn40wqub4_at4wltiovo4jnezqqm7zkjtywx0w48v3z461f5ec2v245g"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "rm3w3leb1_9")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "vp2rd8w7lmf6vrs10fm7pulw")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "kkmo22xks1qlyei2_bfp44b0")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "z8ebnqfymon")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "3of35z0gjruit7b_duy8xi3bgykdsftb2ryoj_grnzfp18oqqs2jtv5q4ep0gcgd2wsjtmhf6pmdzz5ahrczci5o4mczjazfgxcno405k8azr771s4kh5at91l5yx"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "i567cp5_vkae2dtra19lvhwcwj9ssgkg_r19ozt9it9gqzo14k9xed87kxpx27")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "txuml7po8e05djfvcd0zk4_bn0hiq_kgvyp15nxnqn14zw1r")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "hxcolas4lrgc4olpsi1vbdhoc6_1u89w9hywuh88_wfx859x2c_ff2wigldmoily_2agyh00476wxpwutn6d4pu22l33tugr6snuoi1teofgqr7bw49d4e8apqn5w"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "214ujq5558xx8_9mjfja0pd24itn6uadzx")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "22mdoy9vdwodcp2ms7fxjbpdlcbn_kgv0u3crai4wu57uz_41psgk5utjiv9ubef8vvck2wd4t3_obgapty8230lml462j02kc9qb5hjz50pee5cp_wn"
              )
        }
    ]

testObject_BotConvView_provider_3 :: BotConvView
testObject_BotConvView_provider_3 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001c-0000-0000-0000-000b00000015")))
    (Just "n\44648")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "629omy1y3sul2_dc6zk1v5vzfw636emtn7y4flf9em_6r1ef9dmruyf_54t1su8e4mtiswmuertnec_7m1w0f05vrwfbit8k75gmgc53ls9hcx2txudhxvi39"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
          omService = Nothing,
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "42x9zfnob1_hgp1rg64rvfts9msejhx35dpnbmxdl57vyzlp619mrjmi32hce89_lw1j5glj3hx64p7wvbc8mz8riemi"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "ro15_31l0ltsuoq8ifvlnhmhb")
        }
    ]

testObject_BotConvView_provider_4 :: BotConvView
testObject_BotConvView_provider_4 =
  botConvView
    (Id (fromJust (UUID.fromString "00000011-0000-0011-0000-00160000001d")))
    (Just "\ESC`G1w\FS\6340:")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "qoxen0u30fay_2le0peay0p0uo_sq2p38ti0j3zb8cl_js3r8llahlcho1xkr2o6d66g01tkgwuurg9vtwmtmcam2zvxgey7nmbvzubmphffoo788mgequau6hkos"
              )
        }
    ]

testObject_BotConvView_provider_5 :: BotConvView
testObject_BotConvView_provider_5 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001d-0000-000b-0000-002000000000")))
    (Just "\1075229\1009724#nzj\173391")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "fann_bweu4i1u_wa_n5ucx6xn8s3_ozc0ynq5exwdiucsrd9k2_kmpshmvekk")
        }
    ]

testObject_BotConvView_provider_6 :: BotConvView
testObject_BotConvView_provider_6 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001b-0000-0010-0000-001c00000006")))
    Nothing
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "wmap0y")
        }
    ]

testObject_BotConvView_provider_7 :: BotConvView
testObject_BotConvView_provider_7 =
  botConvView
    (Id (fromJust (UUID.fromString "00000009-0000-0006-0000-001600000013")))
    (Just "\n\167215&;&S")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "_zbpj8sdk6oib4_v1d0zq6znpur47kigpqp6zxv66z01y68y4h3zl9p2_5e60_l4hjmhgtrjf7hi4l5egngw5w5dlbq5fpkrdc_sb49y"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "9_klbkp15t972yt659kdor1nskyqpow0hf9ir")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "lxp4vgb4v2ij1rkqwm3uv4sybo5p0dku54d3")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "yzltfr9pcpap1pfs8jas1s7dxckgayce3jhl_6nd_k4zc_5ofutl_kprv83m9gdsqh2qcu2a_2a7tnfzm2ie8ldudjrvvd"
              )
        }
    ]

testObject_BotConvView_provider_8 :: BotConvView
testObject_BotConvView_provider_8 =
  botConvView (Id (fromJust (UUID.fromString "00000013-0000-0005-0000-000800000007"))) (Just "\RS") []

testObject_BotConvView_provider_9 :: BotConvView
testObject_BotConvView_provider_9 =
  botConvView (Id (fromJust (UUID.fromString "0000001c-0000-001d-0000-001a00000006"))) (Just "\1005935\DLE_^w") []

testObject_BotConvView_provider_10 :: BotConvView
testObject_BotConvView_provider_10 =
  botConvView
    (Id (fromJust (UUID.fromString "00000001-0000-000b-0000-001300000020")))
    (Just "\1062483#\179740\165276")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "mofz")
        }
    ]

testObject_BotConvView_provider_11 :: BotConvView
testObject_BotConvView_provider_11 =
  botConvView
    (Id (fromJust (UUID.fromString "00000002-0000-0015-0000-000d0000001f")))
    (Just "\ENQ\US\62200\1113594\&1N_\1016373Bo")
    []

testObject_BotConvView_provider_12 :: BotConvView
testObject_BotConvView_provider_12 =
  botConvView (Id (fromJust (UUID.fromString "0000001f-0000-0020-0000-00170000000d"))) (Just "Q") []

testObject_BotConvView_provider_13 :: BotConvView
testObject_BotConvView_provider_13 =
  botConvView
    (Id (fromJust (UUID.fromString "0000000c-0000-0014-0000-001a00000017")))
    (Just "O$:")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "7argokhlu22zw7um1_4anu2_q13ldqtz2mgeszjizp9qrr8m1wn1yy0lv1bta1cjhxjp_du_5vaatnt94upydlr0v2xqx12ivlbva5eza4c"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "_tzj3fgev1_6jgm5uuhbqnskv04r7k0bkk6si04ylakfznc1qttv6pv98l07_afzg_r_hw2xszllzu49u7x9eeu2hamh4ew2g"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService = Nothing,
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "x8k0vqtenaqv3tj5elrnuwxuhgjl0iugwd3v0uk_8sejey5lgyq4fr746msrtk4eqxl7r3rvaljdyrmjtqvfisx0ml512oneq3bbh7mwr_k3f36od70t3ttj_dc"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "89hefsk")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "65gk5l2gvypqgykq35etz1df_7")
        }
    ]

testObject_BotConvView_provider_14 :: BotConvView
testObject_BotConvView_provider_14 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001f-0000-0012-0000-000100000010")))
    (Just "T")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "0oabkv381mgh54t8zcgvwg19ru1qbjub_0i8gidad9j7")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "ns3h9jzrfx8_o")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "_5kwpvh_ud02gj31kh4wz0ev55qmfoiknvib6auu8nkufhe1t63871_0k52ptbydxbwiw8z0fsht6oigc1geezhsw7uosy88xhvxf4iorzc9_ji2v5760f434aem0ti"
              )
        }
    ]

testObject_BotConvView_provider_15 :: BotConvView
testObject_BotConvView_provider_15 =
  botConvView
    (Id (fromJust (UUID.fromString "00000009-0000-000a-0000-00010000000b")))
    (Just "")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "614zvjitytbb_zu")
        }
    ]

testObject_BotConvView_provider_16 :: BotConvView
testObject_BotConvView_provider_16 =
  botConvView
    (Id (fromJust (UUID.fromString "0000001d-0000-0013-0000-00030000001a")))
    (Just "\6249y\ETX\167710K")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "q3jaysbh_g77zk0mdqsxwswvy5z9no3pk3fhy434ns6ednnzikl7n49hyc59rggbiszeor2nj1g7zqbr934nh06gnal2hlpdvtgm87smu1nqlxtibkfo5z"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "jjul6e4r5t730pq")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "k3bw4yhyumit9o1lpk7iy9ogve8u6nznowc1alk3x0bdl1uyaqrw_efoeypetjmwrh_g8nrjs05p5tqbxh4owg26um942kwd3dm4j284ainzekcumltvybeiy_6h_"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))) domain,
          omService = Nothing,
          omConvRoleName = fromJust (parseRoleName "bl59s90cn3twutjvl959knjlt")
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "han76ra8y4b7bhu9ozk0100nya3m2v1zsjsxp6oyjop06elopq7x87b5dxp808_6sa856be5qemzd2ut0nksn22udjbktkyz436b2x9qsw8_8tjj1lon9ph9"
              )
        }
    ]

testObject_BotConvView_provider_17 :: BotConvView
testObject_BotConvView_provider_17 =
  botConvView
    (Id (fromJust (UUID.fromString "00000016-0000-000a-0000-000c00000004")))
    Nothing
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))
                  }
              ),
          omConvRoleName =
            fromJust
              ( parseRoleName
                  "qxa3cm0_p03cad6xvgfkbk7to7hxiqhvg9dfylkv6ih9nhoox94xr_1qujwkkuge61w4cu9ybwskueizi1i_8flutj9"
              )
        },
      OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))
                  }
              ),
          omConvRoleName =
            fromJust (parseRoleName "vwls3852jzjut8buz_w68y2z6ske30vctv0r9zyrp7uu_lb0ffglegoje0wd4zrl7")
        }
    ]

testObject_BotConvView_provider_18 :: BotConvView
testObject_BotConvView_provider_18 =
  botConvView
    (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-001900000003")))
    (Just "e\"")
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "jnrstoi6mxzddy6f8u80ih39")
        }
    ]

testObject_BotConvView_provider_19 :: BotConvView
testObject_BotConvView_provider_19 =
  botConvView
    (Id (fromJust (UUID.fromString "00000000-0000-0009-0000-001500000004")))
    Nothing
    [ OtherMember
        { omQualifiedId = Qualified (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))) domain,
          omService =
            Just
              ( ServiceRef
                  { _serviceRefId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
                    _serviceRefProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))
                  }
              ),
          omConvRoleName = fromJust (parseRoleName "jiv6dw")
        }
    ]

testObject_BotConvView_provider_20 :: BotConvView
testObject_BotConvView_provider_20 =
  botConvView (Id (fromJust (UUID.fromString "00000013-0000-000c-0000-000b00000013"))) (Just "(\\Fj\991184a") []
