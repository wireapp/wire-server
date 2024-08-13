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

module Test.Wire.API.Golden.Generated.ProviderProfile_provider where

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
import Wire.API.Provider
  ( Provider
      ( Provider,
        providerDescr,
        providerEmail,
        providerId,
        providerName,
        providerUrl
      ),
    ProviderProfile (..),
  )
import Wire.API.User.Identity
import Wire.API.User.Profile (Name (Name, fromName))

testObject_ProviderProfile_provider_1 :: ProviderProfile
testObject_ProviderProfile_provider_1 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000700000006")),
          providerName = Name {fromName = "\46338\DC4"},
          providerEmail = unsafeEmailAddress "OR\32966c" "\RS\ENQr",
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
          providerDescr = "3\51645)S"
        }
    )

testObject_ProviderProfile_provider_2 :: ProviderProfile
testObject_ProviderProfile_provider_2 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000000000004")),
          providerName =
            Name
              { fromName =
                  "?\1050399\62357\12541$?\150548uTY\1101349fH\ETB\STX\ENQ\b\DLE%:!Y\ETB\92301\53905\1096036\1012090*<x2|\STX\FS5-@]=w\1039842\&0_3m`\1042674\917945I\174531N:v"
              },
          providerEmail = unsafeEmailAddress "" "\STX7\99332",
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
          providerDescr = "}[N"
        }
    )

testObject_ProviderProfile_provider_3 :: ProviderProfile
testObject_ProviderProfile_provider_3 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000800000000")),
          providerName =
            Name
              { fromName =
                  "\189455ur\45443\DC3U\ETX~[\170890x\DLE\987435\tN\n<9ZE\SUB\1018824x\1000998@\99259\1056343\1070454\GSC\\ml\DEL\1113168\n\1017846\150657}r\ENQ{asS\1038951\143265/CE\94832`\169109\"mfI2\FS\97504c~\1071258il\986898T<h\SOH\984390\155702Ky\169155\145155n]\SYN\119248>]/Z\1050093>-\EOT\1041175\1025575!_*--7\SItEg\t\1028966\DC3\1079962\CANvE\DLE\134924?=\SO\1026118\40813\167977O\24641k\NUL\1019104\32399.\SI"
              },
          providerEmail = unsafeEmailAddress "" "_ \1060474\990125",
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
          providerDescr = "K\\\SYN"
        }
    )

testObject_ProviderProfile_provider_4 :: ProviderProfile
testObject_ProviderProfile_provider_4 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000004")),
          providerName =
            Name
              { fromName =
                  "\SO\1046147n\1016911\&7f\1077840i\SI8|\STXe\nN~$[vAU\62541r1`\NAK\f/\b~\1084745PEhV={\1037388\160696\f\EM\1063647}}\3137x\994880\994942\1069553%\foA\50458\98884~t\182452\12080\t\1073906\rWA\186565\1104351t"
              },
          providerEmail = unsafeEmailAddress "h\161768\t\1097554G" "\134955/\DC4",
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
          providerDescr = "\184562j>W"
        }
    )

testObject_ProviderProfile_provider_5 :: ProviderProfile
testObject_ProviderProfile_provider_5 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000700000003")),
          providerName = Name {fromName = "\6923gr\n\35429-\37180f\fJ9\RSl)\f\20518_H^Xh\bA;O|"},
          providerEmail = unsafeEmailAddress "%>" "\1075658\17096q",
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
          providerDescr = "T\1030510]\SYN/o"
        }
    )

testObject_ProviderProfile_provider_6 :: ProviderProfile
testObject_ProviderProfile_provider_6 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000008")),
          providerName =
            Name
              { fromName =
                  "\DC3 &3\DLE\n\163723'\65487\&7\7618\ESCEwP\\\125089\DC2^\"\1023814\1002704\DC3\DEL-g\29654<\v\4324hAjOZ)\1045139W_\154260\135873s|+\1030412\"~D<!\1013798\1026521\181302kotW*\v\184061)(ds]"
              },
          providerEmail = unsafeEmailAddress "\1019096" "_q[w\NUL(",
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
          providerDescr = "r"
        }
    )

testObject_ProviderProfile_provider_7 :: ProviderProfile
testObject_ProviderProfile_provider_7 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000800000001")),
          providerName =
            Name
              { fromName =
                  "J\43341\1029779\f\147964?=\DC2\EOT\ACK\153581\RS\NAKK(\r\1029744!\1102710\SYNK\39801ge M#\994675\SYN\1021492\51704"
              },
          providerEmail = unsafeEmailAddress "\1095636\995243\DELb^" "\60455\n\1007982l",
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
          providerDescr = "\172020J\182086r\158961"
        }
    )

testObject_ProviderProfile_provider_8 :: ProviderProfile
testObject_ProviderProfile_provider_8 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000700000008")),
          providerName =
            Name
              { fromName =
                  "45\SOH\ETX^\194886d\SYN\153404\1013899:\SOH=*Nh/\179322\47015\&2J\ACK5_\SUB\47350]cn\f\n\1064205\n\35602\f[kux\GS\179771{\DELbt*\b*o\NAK9|x\1079260\nl\74195\SO"
              },
          providerEmail = unsafeEmailAddress "\DC2,[" "\31313/0",
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
          providerDescr = "\DC3\v"
        }
    )

testObject_ProviderProfile_provider_9 :: ProviderProfile
testObject_ProviderProfile_provider_9 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000100000002")),
          providerName =
            Name
              { fromName =
                  "\n)\1083865\1016965B\1044743\1063222?\ESCyKyrZ*\1106071a\167420\157587\FSR\v5HP\125013\ENQ\74222?\1113574h$`X\ETB\ENQ6#\ETX\CAN){YhC\160029\NAK\\\1061737J5MK\GS>\1039156C\DLE\ETX\95249\ETBw\SOH\DC191\"\"6D\b\DLE\NUL.PC\RS\SO\1094846\1044317<\171750iuN\182436\1088261U{wgq\FSD\v\1034790\SUB\"\nw{Rl\ACKUa3\RSNx\SI"
              },
          providerEmail = unsafeEmailAddress "\142265.\EMk\1035106" "n}\190773\n",
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
          providerDescr = "*\1012632\1015879"
        }
    )

testObject_ProviderProfile_provider_10 :: ProviderProfile
testObject_ProviderProfile_provider_10 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000500000008")),
          providerName =
            Name
              { fromName =
                  "\1027347|\US\187412-C:\v\CAN\1007173;|\DC3d\ACK>@\95987\165903\&2\DLE\138359\SUBM7/\1069218b\ACKO3m[{"
              },
          providerEmail = unsafeEmailAddress "gd\1046608\1072562e" "",
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
          providerDescr = "\1025315\38572E\SOH"
        }
    )

testObject_ProviderProfile_provider_11 :: ProviderProfile
testObject_ProviderProfile_provider_11 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000700000004")),
          providerName =
            Name
              { fromName =
                  "T\35190nJwq\65943[\FSV\DC2I\179267\SOH\ENQJ:\"ay\1021260\998962\1026006L\SOH&%lT[l/?\1044443_\DLErW\1012807\1017169]\137723\1082379\83105wM\DC4#\39095"
              },
          providerEmail = unsafeEmailAddress "" "W",
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
          providerDescr = "\78083ME\DC1["
        }
    )

testObject_ProviderProfile_provider_12 :: ProviderProfile
testObject_ProviderProfile_provider_12 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000100000000")),
          providerName =
            Name
              { fromName =
                  "h-\rE,\148173 \1088186t\DC3S\EM\14287&8Nf\EOTE$;;\163703\SO\SYN\191282D,pE\STX?'X*\STX\DC1>&\1103170WCGM=Ey\1088250,\44485$"
              },
          providerEmail = unsafeEmailAddress "Z\148819" "!\1045867\69665\f\23358",
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
          providerDescr = "\fE\1036789\1046186f\ENQ"
        }
    )

testObject_ProviderProfile_provider_13 :: ProviderProfile
testObject_ProviderProfile_provider_13 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000800000004")),
          providerName =
            Name
              { fromName =
                  "r\1096559\DC2%_izF\33135e\46380\DC1/\r`u\1022998\a\SIf\60524\1098075f\1073391oP\EM&\131116\SUB\1059302\1108967jY\992453\1111715\ETXd\1063946e\1001823HK\129359\ETXy\1106634TE\SOH?\148357 \ENQ\ESC\1015779q#\SO\"(Q^\DEL\183337&\SYN\18804\DELPI]Q\"X\SUB\14938\145510x '"
              },
          providerEmail = unsafeEmailAddress "\SOH#\134551" "u\SO\19353",
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
          providerDescr = "\vY/\r"
        }
    )

testObject_ProviderProfile_provider_14 :: ProviderProfile
testObject_ProviderProfile_provider_14 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000700000007")),
          providerName = Name {fromName = "\SOH4\ENQ\ACK>\rx~J$k!~\t\DC14\985222\DLE\ETB\r\ETBy!9"},
          providerEmail = unsafeEmailAddress "<" "M\SI",
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
          providerDescr = "-)\ENQ/\r"
        }
    )

testObject_ProviderProfile_provider_15 :: ProviderProfile
testObject_ProviderProfile_provider_15 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000300000005")),
          providerName =
            Name
              { fromName =
                  "uU1;\34969b \ENQ%\1090011\51919\64324\&2f\1054192\b\1076489\DC40({\983593\1096114\997924}z\168790Lq\STX5.\STX\1092385s\1024579[\t\CANE\67601\95200W\1105521'\1036690_\1103544\&6g\a\160335\1033905X5j\1041586Q\100988\53621o/<\DC3wm\1069822'\135972F=\167089d\164390da\1010656\&6fbnN\ETB=\1062861\ETBx\v?{3\46096u\154857K\153847\26427"
              },
          providerEmail = unsafeEmailAddress "\2355\CAN:c5" "G\1049614E`",
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
          providerDescr = "$\DC3\ETXE"
        }
    )

testObject_ProviderProfile_provider_16 :: ProviderProfile
testObject_ProviderProfile_provider_16 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000300000001")),
          providerName =
            Name
              { fromName =
                  "sl\n\1104119\SUB\989977\a\143145\1017719></Ak\175680x\EOT\ESC(\NAKmE\1085219\fU\GS\141526\NAK\155030[\1007194l\ETXE\NUL r?\995146hL%\DC3\17102\SOH\ESC:>\DC4\SO\1016279\160098\&7\DELm7\NUL\35472\RS\54106uXwwA\1062534!\156472\EMN\1082758\164617\5214\ESCEZa\1030079\186679ZWY\189148|&\21785ikM\SUB\1061267\1056212\160249\988858\1020580K\SOHY:\ESC*Wzc\"\ACK\1038549\1092558\rWB8jSl\GS\EM\1003586\23627\STXf`\132324LN\nje"
              },
          providerEmail = unsafeEmailAddress "\180899" "\34121z\16843'",
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
          providerDescr = "\v"
        }
    )

testObject_ProviderProfile_provider_17 :: ProviderProfile
testObject_ProviderProfile_provider_17 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000400000006")),
          providerName = Name {fromName = "\37146|_;\1090300\48254\STX4/\13124yqDttZ\SUB\1065843y\17715\177370"},
          providerEmail = unsafeEmailAddress "X\1050408J1\SYN" "\1024482%",
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
          providerDescr = "\137893\&5\16491"
        }
    )

testObject_ProviderProfile_provider_18 :: ProviderProfile
testObject_ProviderProfile_provider_18 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000600000008")),
          providerName =
            Name
              { fromName =
                  "~\US;@\1081284Oa\184911\DC2\SOHnw0\DC3Y\1044296.Qn\1111681\1078852\na>\ENQ<\1008904g\DC3\1017402x\1051129*8-T*\ACK'\NAK[/\1043140g\142008VT\EOT\12290,\179242\1069014kC\98612s\DLE+\GSz\78289\1040663`l"
              },
          providerEmail = unsafeEmailAddress "" "\b<u\165127",
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
          providerDescr = "$\1037052\&3\1068673 L"
        }
    )

testObject_ProviderProfile_provider_19 :: ProviderProfile
testObject_ProviderProfile_provider_19 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000200000008")),
          providerName =
            Name
              { fromName =
                  "\RS\SYN1\187335,%7\SO\1112910\SO\DC4\1018484k0*\1001101j\SYN\14478W\1019754]za\31909\120971\"9O-o"
              },
          providerEmail = unsafeEmailAddress "\46253`kmI" "\1104187",
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
          providerDescr = "^\64352o"
        }
    )

testObject_ProviderProfile_provider_20 :: ProviderProfile
testObject_ProviderProfile_provider_20 =
  ProviderProfile
    ( Provider
        { providerId = Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000400000004")),
          providerName = Name {fromName = "q\1073609\138472T,$"},
          providerEmail = unsafeEmailAddress "%)Z'" "\1023312f",
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
          providerDescr = "|N\1030588"
        }
    )
