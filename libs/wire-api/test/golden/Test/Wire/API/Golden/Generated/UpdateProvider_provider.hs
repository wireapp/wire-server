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
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where

import Data.Coerce (coerce)
import Data.Misc (HttpsUrl (HttpsUrl))
import Imports (Maybe (Just, Nothing))
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
import Wire.API.Provider (UpdateProvider (..))
import Wire.API.User.Profile (Name (Name, fromName))

testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 =
  UpdateProvider
    { updateProviderName = Just (Name {fromName = "\ETX\GS\SOH5\SI\31013\11317~D\172577\1013828mxD_s"}),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Nothing
    }

testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "r\1023906w\1077671\&6m\164018\18611]@\SUB0K\aTl\RS]6p\1011778|_.\EM!E\157372u\STX/sN\US0.>GRo\177600\1030955+bQ\ESC\1018405\1090568Nloe\1032921\&1_\SI\1107418l\1075124\NULR\143964;tsQ\60338_ |"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "\US>"
    }

testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "H\DC2#%\161743Zl}[\29622>%\26386j\147853C\1019265=[Ghhh\100262\SO>\EOTSgmH*\13504m\998874]\ESC\7402(mj\DLEG(\v\142408\1017790\tf\n\t\a\tF\SUB(\n\"\994198TW\1093896&=\FS\97959\DC2?\RS\1040876SMC@\1066402\&4\1057935V]\r\STXt\EOT\1004566F\GSkX\NAK\EM\DC2\1033511\&6\STX\170497BtlzO\v\CANG\1040359\63927\157865\1008121\174841J\EM*\1021307h\CANm\b\EM$k"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just ""
    }

testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "\169589\n\GS2z~\1026369\50430n\42405\af&B\1001559^Z\1094990%\126129\1034278v\1092963X%\12076<%lA\1088652\DC3Za)\SUB\78649\FS|u:]la<\1012101\&6Zc\ETB?3\DC4)\119062\51235r\DC3\ACKO\1014870`O\1112288\NUL\DLEv\1003750/\27134E@\DC1A\FS)S\63967\NAK-W2_q\rB\DLE>\95091\ENQ\189522t-\NULX\190066\DC4I<\997927mNVz\68053\180713U\SOHT\tVy\30770%^\ACK\RS)\b"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "n"
    }

testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "\1023513tW}\1105843\1099664\r\1012423xL\120427q\96664\1024589\158455MJS:\988393/\a\tw\179476\1050497\46138}\ESCr\SOG\1108248Ndx&,\t#\98425\&2\1002245\SOH\32217TI`\996792\RS\FS!\USIL\DC1v\1034804{\1099333K\46843\SOH\1073439\1032058#\1042594e4*\157517iKfn61nJr\ENQ\DC1lT4G\4520Y\SIl\1058533;lD`n\ETXM+\US\\\"\1058456ec\SUBnC\b\tX"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Nothing
    }

testObject_UpdateProvider_provider_6 :: UpdateProvider
testObject_UpdateProvider_provider_6 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "/>u\ACK:\48861*\991685\&7I\fo\1095113/\180424#o^b?*b\1910;V8@\ETB\r\DC3`\140658r\US\159767\DC4hk5a\DLEL\1046970\1012707rx\te\138289\1061902j{Q$T\1070843\n)KOuxE2\US4\1031134m\FS\US}\1084018Q\ETBy^d\38568\RS\189268Y%\FS\152092T\1077076i2S :\1023427\GS?).\1033112\DC2D\1042605\n\DELy\DC2&.y\43589'$&]U#b\SYNJ\DC1w5\189157=9\68921cj\1072427x"
              }
          ),
      updateProviderUrl = Nothing,
      updateProviderDescr = Just "\154926#n\ACKN\DC4\DEL\ACK."
    }

testObject_UpdateProvider_provider_7 :: UpdateProvider
testObject_UpdateProvider_provider_7 =
  UpdateProvider
    { updateProviderName = Just (Name {fromName = "\98982<B\DC2^>Z#"}),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Nothing
    }

testObject_UpdateProvider_provider_8 :: UpdateProvider
testObject_UpdateProvider_provider_8 =
  UpdateProvider
    { updateProviderName =
        Just (Name {fromName = "\162791&Rn=sv\64275\&5!(\1085717\NUL5<\DC3=\ETB\r\tw\SI\1088534\1074404"}),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "<\".\1055555CP\NAK"
    }

testObject_UpdateProvider_provider_9 :: UpdateProvider
testObject_UpdateProvider_provider_9 =
  UpdateProvider
    { updateProviderName = Just (Name {fromName = "\n\ETX)\ENQMx\45946N|i02.m\34232k=<*g#(BC*M\DELC!\ESC}\SO"}),
      updateProviderUrl = Nothing,
      updateProviderDescr = Just "%"
    }

testObject_UpdateProvider_provider_10 :: UpdateProvider
testObject_UpdateProvider_provider_10 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  ">5\9162\&8\t)\183947\1079734\DLEQl&z[\ETB\SUB\\\ENQz\DC1^\DC2N\a\SUBl$y&5?7T\n\1032145\77940\161721\STX\52237Es\995678*+&>\1064282@3T\SUB\SOHL\1024950\\l\DC1h\aoo<R\1073438\ESC\SYN\r\1077611\1042598\EM\a^"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "qV\1005844\DC2J"
    }

testObject_UpdateProvider_provider_11 :: UpdateProvider
testObject_UpdateProvider_provider_11 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "\1039255\DC1\rVn\f\96009\1077222e\DLEgb\SIG\21733\&2X\DC2\NUL\EM7O\33045\180682$K1O\STXo\5687^H>\1000203\&2\ETX:h:\157554/\1015499\1014550\&1kS\185728\194745J>\150718ff\SO\1091145\n6\65031\987006:p^\1087715`S"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "\ACKlw\FSfA\1089458u"
    }

testObject_UpdateProvider_provider_12 :: UpdateProvider
testObject_UpdateProvider_provider_12 =
  UpdateProvider
    { updateProviderName = Nothing,
      updateProviderUrl = Nothing,
      updateProviderDescr = Just "w\1007782f\1108900\tEhk\1000408M"
    }

testObject_UpdateProvider_provider_13 :: UpdateProvider
testObject_UpdateProvider_provider_13 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "\1083677\135944HEaL9>\27897\181610\1010662\1041306\SO}\1078550K\40298=/\172959\v\139354\54291tNE\31183\135461>\ACK]\1091758\1095891h\162285\flA\1001167\&9\29752\SIo5\STX-z\993540\1076495ho*^[BZ\984192\ENQf\no@\78119ug,rt\f`\17031\SUB\1067016\CANrLtu\9344:*!mS\SO0(Y_&p^>5(|\1022905\NUL\DC1\SOHV\1006003\SUB*]\DC2,s\rKr"
              }
          ),
      updateProviderUrl = Nothing,
      updateProviderDescr = Nothing
    }

testObject_UpdateProvider_provider_14 :: UpdateProvider
testObject_UpdateProvider_provider_14 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "8f\DC1@\99196FY\4461\r&C1UFyu&*\1085235\&7\1022362y\SYNxe\165630p\DC1'\993w\1088517\188511\1027835&t-\162844\149544\DLE\1094984c\996167G\1000153$p\168781\53089\&5o\DC2]d\DC2\13523G\1112320\&4\NAKy:\1064068F\SOHj\1099493\1076409c!;E^\35242z!I\tjbe9>L,zB?jZ\154189\EM\EMx\\\1018949;\CANs\US\f\NUL^\DLE\1029116\46457\160237\988963U"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "+$QN\999265fh\1014276/"
    }

testObject_UpdateProvider_provider_15 :: UpdateProvider
testObject_UpdateProvider_provider_15 =
  UpdateProvider
    { updateProviderName = Nothing,
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "\1003783\GS\1013754\DC3\ACK"
    }

testObject_UpdateProvider_provider_16 :: UpdateProvider
testObject_UpdateProvider_provider_16 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "55\1100456I\1034540O\RS\1022267\1037928Z\1007516\DLEnO0w)+u15xE\r\NUL\aHo\1031764c4\1062151\ETX\1027513VcK#\SOH\US\RSb\1011015\&7\t7>_/K[,+\133485\1082683.O&r\ETX-\DC3K{6l\74775\1105957&\1114058,a\SOH-!\18327{o\tm\1046908{Fk>/\STX\1070580\"\1067569\6421\1074450\ETB\ETX\ENQP.m\186476`\150152\DEL\189102\SI\EMiF\SYN\986198A\177614{.\GSw\1095267\&1[\ENQR=>C{\EOT"
              }
          ),
      updateProviderUrl = Nothing,
      updateProviderDescr = Just "B"
    }

testObject_UpdateProvider_provider_17 :: UpdateProvider
testObject_UpdateProvider_provider_17 =
  UpdateProvider
    { updateProviderName = Just (Name {fromName = "\1039534s\t}"}),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "\FSa\127907\ETX\182226G5\1087949_"
    }

testObject_UpdateProvider_provider_18 :: UpdateProvider
testObject_UpdateProvider_provider_18 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "!\ACK\EM?JlD\ACKZ\DC4X(\CANH01\145637\f+\ENQ\f\CAN\1072714\181333\US\1064816J\NAKY*\NAK\SOA;\178340\&3eY:#\1054739YN\SOH\1026547`\1073701}UAT>"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "\177071\1038872\1077445"
    }

testObject_UpdateProvider_provider_19 :: UpdateProvider
testObject_UpdateProvider_provider_19 =
  UpdateProvider
    { updateProviderName =
        Just
          ( Name
              { fromName =
                  "\RS\ACKf\146958\1026483\191341n\1051469\DC3S+b\1025958\ESC\1065777br\180428K/'J\43560\USx`J\"\29038U\US\137180aD!\"\n\78851\f\EOTMLm\1068145\1084144=C"
              }
          ),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just "&"
    }

testObject_UpdateProvider_provider_20 :: UpdateProvider
testObject_UpdateProvider_provider_20 =
  UpdateProvider
    { updateProviderName = Just (Name {fromName = "\1013562"}),
      updateProviderUrl =
        Just
          ( coerce
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
                }
          ),
      updateProviderDescr = Just ""
    }
