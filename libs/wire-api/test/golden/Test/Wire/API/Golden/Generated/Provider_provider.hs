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

module Test.Wire.API.Golden.Generated.Provider_provider where

import Data.Coerce (coerce)
import Data.Id (Id (Id))
import Data.Misc (HttpsUrl (HttpsUrl))
import Data.UUID qualified as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import URI.ByteString
  ( Authority
      ( Authority,
        authorityHost,
        authorityPort,
        authorityUserInfo
      ),
    Host (Host, hostBS),
    Query (Query, queryPairs),
    Scheme (Scheme, schemeBS),
    URIRef
      ( URI,
        uriAuthority,
        uriFragment,
        uriPath,
        uriQuery,
        uriScheme
      ),
  )
import Wire.API.Provider (Provider (..))
import Wire.API.User.Identity
import Wire.API.User.Profile (Name (Name, fromName))

testObject_Provider_provider_1 :: Provider
testObject_Provider_provider_1 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000700000002")),
      providerName =
        Name
          { fromName =
              "\985673j\STX8'\DC45;QDq,z:4\1057870TQdrz\40798r\995165 o&\v\52034pVe\1063021x\159799\61750\50279'w#\64412X\1082139\1107884\168414\1012920)\74268\41299U[{bK\DLE\CAN\29409\986819\164552\US,\1086175Inu\33596E\1003632,\1027339\ETB\b\"\r\1030903\SUB@y\1096050\66190m\1063929epM3Q{\NAK\18907\&8g2b\NUL\1053652\EM1\DC1\FS1\1076600Ov\183986u\988457\1006783\SOH=\ENQ$\186820\NAK\1104119\20911l\DC4/K)Y'\10984M\ESCX\RS"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "=\990618"
    }

testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000300000006")),
      providerName =
        Name
          { fromName =
              "X\NULci-\10273)\986480(kt\1030019F\1025747\DC2[ja\1048990!\175457\155549@1\NUL\DC4.B57!_5s\DC2\51921\1061344\1083552\&9\ETB \DC3\NULOx\1075778\1008360\DLE\16208F\182674\EMa\1041618_\ENQ\\D\44245F-&A]M|\185391\DC2M\f\b\1041002 \ENQH\SO\37489\149358\&5*PVN\US|j|\1029597O\GS\DC1a\179050ki\21481\1016177!\SUB\1100350\&3g\996453\1057252\n@\127775\11291o\185336]-\NAK\145370\991015Z$\n}'B\32491\26626\NUL"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\1083941mU\182225."
    }

testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000002-0000-0008-0000-000800000006")),
      providerName = Name {fromName = "\US\SO\157167r/y$SCws_g\STX5\fq\1076378&\SO2\1069444Tqu\1001074Z>\1018458G"},
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\f"
    }

testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000300000003")),
      providerName = Name {fromName = "p\1048508q\EOTJO\996837\992614[=\CAN\ETB\998955D\v|{\158703n"},
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "/\13648"
    }

testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000800000005")),
      providerName =
        Name
          { fromName =
              "\139639\31914#\179062\NAK<\1078644\RS\14549\32585M\1074311~\ACKZ)x@_\ETB\SOH\DC4\145888\1052881~b\b\SYN\\\41481\6219j\1037452Cp>\1034483\SUB\126495{=\DLEC\134481\DEL\141010\SUB]\1002444\1112311[\37348;\SYN\ACKTpXQT|-\29557\1027018\&0\CAN\1051401h]\1011840}\1074404DH\157697\SI~\DC3^"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = ""
    }

testObject_Provider_provider_6 :: Provider
testObject_Provider_provider_6 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000300000006")),
      providerName =
        Name
          { fromName =
              "OT;/hR\83260\33881!~<\984126\153553FP\DLEpW.0\51651f\1011306\SI8\EOTZIy\135141\65603\61638|du#k\n2\ESC}W\DC4\23956TI5G\DC3\1103775\SYN)>&?\1068070~\n\f"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "j#G\r"
    }

testObject_Provider_provider_7 :: Provider
testObject_Provider_provider_7 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000800000001")),
      providerName =
        Name
          { fromName =
              "\51386\CAN\181093\&1\\\FSV\993109\68313\DC2\1101703\152460\SYN\15414Y\ACK}.b\159771\t;\156388u%0\1711}f\STX\137364\184946I]\1081074c"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = ")m"
    }

testObject_Provider_provider_8 :: Provider
testObject_Provider_provider_8 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000100000004")),
      providerName =
        Name
          { fromName =
              "x\1102083\DC3cQ\ETBw\1016153k\DEL}\ETB\6414\&8\b`\54226\1074680JC\44257,1\DC3^\161363{\DLE:c\r+\ENQ"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\184469\&2\1079211\&4"
    }

testObject_Provider_provider_9 :: Provider
testObject_Provider_provider_9 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000600000006")),
      providerName =
        Name
          { fromName =
              "+QH\1073419\&2$DH\GS\134345oz&SQ\1026703Apl\1042851Dai\177568`~\3933G\GS@$i\1110073b\USlBR\fIg\985261\138569\&4Pg[h\ETX\DC2\1054353\&4\146082\ETXM*\\`(U&?yinFa(\173335J,<\DC15R@\1015581\aAH\1100383\DLE\"\180922\144814\t"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = ""
    }

testObject_Provider_provider_10 :: Provider
testObject_Provider_provider_10 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000400000007")),
      providerName =
        Name
          { fromName =
              "U\132167\&2uXTV\DC2\1095957\"\f=K7}\DELws\110737\74533*1\1060311_\DC4\10795\ENQ^4xt.\1048954\27633m\1024412{\ESCt\EM2\1034112\fs\SUBi1\8889T\1085233=e\36669\54937\&1"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "E\127866:\FSJ\1027416"
    }

testObject_Provider_provider_11 :: Provider
testObject_Provider_provider_11 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000100000003")),
      providerName =
        Name
          { fromName =
              "-\DC2\GS\SOHq=~ \167475d\165469HF9\USxT;x|2@a\RS|\t,;Z\NAK\1024830\23889\1046412\CAN\29608l\NAK4\SUB\ETX1b\139361TSP\35608\t\"\118992;P\92279\SYN\ENQ S\ACKEkM\36723h\144165\1001722\99428w\f@\ETXO\f~P\"E6\a\DC3[7yu\1113396\DC2-\EOT\1045766\&2"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\SO\GS\1088870\"'"
    }

testObject_Provider_provider_12 :: Provider
testObject_Provider_provider_12 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000300000006")),
      providerName =
        Name
          { fromName =
              ",U5>pD\1016421O\169341Rbk\EM\SI'V\DC1\1057308-]\1001679Q\\r-\1072997\SUB\1026052E\986458\n!\ACK\1014509F|\ESCz\1039329s}h\43257O\1012066(v\1028947,C6\51907p\b\1045199\&5\n\t\59073\18458\1089314\14527\&1\DC3}\1014091\1100946h\1007583\150277>\48890\&7\1034816LI\12957Ck\999322\162123@\EM\US\1000098`&zd7b\1061406Q\1835\1110183"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "x\17225@I%\r"
    }

testObject_Provider_provider_13 :: Provider
testObject_Provider_provider_13 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000008")),
      providerName =
        Name
          { fromName =
              "\48448\DLE\1063675\r;\DC1T\1089400\ENQ\r\EOT\DELjtW\992523S\49358z\149756(\DC1v\1007617\158502QOl\1086127\1040356i#\SUB{\1102236(i\ETBJ\ETX/s\51572?rre\EOTuf\1070135~\EM\65542\1073079E\1047897?-\165550W6|A\EM\19032@\2342p\DLE\r\166662Y5\NAKp=,\SO\165808\rY\t\68196\DLE\149742r\ESC3XO\1096163!\GS<\SUB2\RS\t\ACK\1029836\ESC6\fa\DC3\1079218\"\GSU|\1067159\SOH\ESC\DEL?5EO=4\DLE\1038347+e"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\DC2$=l\DC14"
    }

testObject_Provider_provider_14 :: Provider
testObject_Provider_provider_14 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000400000007")),
      providerName =
        Name
          { fromName =
              "uo;)\177699/KN\ACK_#D{\1034585\182761\&6X\1072777]<\1046068*%#\1106045uHJ\1018037q\SOxu\1047970\1063074\1005021\1057327\1073391M\121169v<\1096384z\35225M\97178:t:I`Q3Vx\18237\148361U~\5394\DC4`\DEL\DEL\ETX~\1036744\1039262\EM`\"{VL\53765@\1094535\1045964\SUB"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "HY"
    }

testObject_Provider_provider_15 :: Provider
testObject_Provider_provider_15 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000008")),
      providerName =
        Name
          { fromName =
              "\1034369\16232\SYN;\t\1049296ln5\54653z\r\"hdPTT\15720\&9S}oV?x>U\SUB\62879\58674\148214G\FS\DLEK\44599tO\1109580i\GS_v\\\CAN\1018104\DC18<t\STX[|\164253/\1041340a`x\DLEfZ\r@\ESC\153639uG"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = ""
    }

testObject_Provider_provider_16 :: Provider
testObject_Provider_provider_16 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000700000007")),
      providerName =
        Name
          { fromName =
              "\1079589\180171\v\1060945\92693\1006339G\168793\1033816\DC1\7298v_eIxC\174962(\f\1045922 C\UST$dgFS;d`\FSJL\t[\1023414{)|R~z#\120581\993017\&2ym3\144933"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "J4\1004803"
    }

testObject_Provider_provider_17 :: Provider
testObject_Provider_provider_17 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000500000001")),
      providerName =
        Name
          { fromName =
              "\153228\148797`\US\95839\1012502;\DC3\22565$w\1002251\ENQ~h2%]??.\1043465,\1071358*\54025\tV[\1081741\1096720\1049249e\992756\&5(\12771\1089336\STX?\1106730\1098847\&6%U\EOTd\DC2\156138\1038817\1074267\142540BI\38623\1314\SOH\NAK*&\1046849\995758"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "4"
    }

testObject_Provider_provider_18 :: Provider
testObject_Provider_provider_18 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000600000007")),
      providerName =
        Name
          { fromName =
              "\1099982\17840bj\179649'n\n\194839\1110662q,PY\t}pF\986655\36217\1088188\&2b8\136335r\990584[\SYN\DC4\DC3Y\190868x\1009539\GS\141771Q&b=\1084132f\1100812u$\51841\1025602\ACK\1098539\NAKG\137473/9rp;\1077834\RSso0P~\STX8\1010578q\996427fsV\998212W\GS\1051537,\n;#Z\n_C\ETXY\13898mNL\NULlJ6\2047"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = ""
    }

testObject_Provider_provider_19 :: Provider
testObject_Provider_provider_19 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000800000007")),
      providerName =
        Name
          { fromName =
              " \1007848?\\%\r\DEL:<7,\ESC\ETXkw&\SYN\97264E\13759'~R\1020268gL\178471&\1094930AN\SOHri\nl\t\a\989491.JD\121158Z\13554\FS\SUBw\31544\1111856}\SYNM][=\1002954\&9\1113850|\53991k\1065488MV?\1029321\vY:\bK'\SO\1108390\&7\ETX\DC4QQt1"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "W'\1027291\DC1"
    }

testObject_Provider_provider_20 :: Provider
testObject_Provider_provider_20 =
  Provider
    { providerId = Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000005")),
      providerName =
        Name
          { fromName =
              "\STX\40925\NULY<\DC1\DC2Z3\1091560\b\1102151\1002024g?\136632xHZ*\FS\146152LxQ@{\1073473\ETXDC.a\1113115$\1060648\183402\ACK\1019481\996002\SUBS^q\137687/b\v;j\DC2\f\NAK\160809GK~\DC3DPM\DC4+\1093354\97178\998562o\1011941/\29306\SIz\USc'z\DLE{\15999x\DC3\65369\1011316\v1'[G<\1061407L\ETB\179133\&4\SUB"
          },
      providerEmail = unsafeEmailAddress "some" "example",
      providerUrl =
        coerce
          URI
            { uriScheme = Scheme {schemeBS = "https"},
              uriAuthority =
                Just
                  ( Authority
                      { authorityUserInfo = Nothing,
                        authorityHost = Host {hostBS = "example.com"},
                        authorityPort = Nothing
                      }
                  ),
              uriPath = "",
              uriQuery = Query {queryPairs = []},
              uriFragment = Nothing
            },
      providerDescr = "\SOH}\20647uT\1107214"
    }
