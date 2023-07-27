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

module Test.Wire.API.Golden.Generated.ServiceProfilePage_provider where

import Data.Id (Id (Id))
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True), Maybe (Just), fromJust)
import Wire.API.Asset
import Wire.API.Provider (ServiceTag (BusinessTag, MusicTag))
import Wire.API.Provider.Service
  ( ServiceProfile
      ( ServiceProfile,
        serviceProfileAssets,
        serviceProfileDescr,
        serviceProfileEnabled,
        serviceProfileId,
        serviceProfileName,
        serviceProfileProvider,
        serviceProfileSummary,
        serviceProfileTags
      ),
    ServiceProfilePage (..),
  )
import Wire.API.User.Profile (Asset (ImageAsset), AssetSize (AssetPreview), Name (Name, fromName))

testObject_ServiceProfilePage_provider_1 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_1 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "&m\1027072\32293\DC35\183940\1087588\\w\DC2\166718\DC2\t\\/SRd\1006720\b|\ENQ\1063226 n\n0\8084l\24833\EOT\4031\RSt\110672\18036C5Ucg\US\92984\&8\DC3)u\DC2f\FSF#i&\37582\RS\EOTqv0\CAN\1058334\\Nq%\1108082\GS_\95821"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\36956\SUB\7584x\178053Z8x1Zu@\42016y8I\1076417\&7)\1074157_+\23643\SIxQJ^TT\n\997425\"\DC3"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\1012033\1085831\128369\SYN-BD4\r|\1032484C\SYN1<t<3\1014637K\b\v2B\1013746\58324vF\184415Y\ACK\1040696W\171494)vVh\ESC\1018417.\DLEc|D\EOT7\SUBG\SO\186366/YH\EOTNa\\s\121063Di?\ETByrbu\GS\SUB\t\173707pQ\39805\1033382v\NUL_\CANw\61003\EM\ETX\47935XME\DC4#;\21984+\a\1079808\1070793\SOHs-\181026d\DC1\54438\1020675&H~"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "'+!60H\SI\1022035\&4\EOTcO\28315h|A,, y\42350\ETB\DC1\1073582\1106433sU7\164435\159753ZLu\1109823]\SI`c}\NAK\1064613\n9#T[\ai\DC2l\191028?Ckb\989751nI\984504c\1027397\ETXl\188719"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "cYM0ke\STX40u\44805+\EOTmb P\NAKcmXI\DC2g\67092\CAN\SOH|\FSW\1110542\1065824\US\v\SO)\DC1\ENQ(Pi"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "*\149961}\1018011\DC1r\3076\1062816\57949|\1057124?\1099146J\1067289\98782Qa\1029474Fp>?"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_2 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_2 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\f\32343u\GS\t\990932,\vRG*\ETXg\NAK\CANO\73970\174170\DC3\54717x4A\195057\STX\NUL\27460%\1047898J_\CAN\6703aL|\1008412$w\985489\a\988936+\61215&Hbf\98640\37993\125255\1054400DQjJ8;\1096148\96626\994591\1072824\42940\1063819\ACK\100163\996754l\138199\1074908H=\"n)\GS\1023752u0d}\"2N\EM"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "~\SOH\"\US\FS?\STX&\1042152H\183674wKJ\65549~h\SO[P\989267a\150735\&8\1030296\aF\168375do1\\cT\1077282\1026416\\lk\1065118u\1044394Ca\1051554!syBL;+\141835Ly*Z\bhR`\NAKN\41496Q\1097706\1070009\bc\1026668x\1024991V\ENQ\992375z\1069570%6nAgh\ETXQ\b&.1\163749\&9\151085QnS 5KD^\23235Ah\150146\10594\186523+HKt\ACKU\\\SI0[@\EM:"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "!N\18121(W\SO\b4\USz\RST\73856B\94681\1033678CC\27619\1066178\1034429\&9\128965%\DC2-Q\54457\23101\EOT>p%\1079483\&89a\ETB,\STX\173614ZF\t\SI\DC4D\RSS:\RS,D\1113208\b5P\v\999200pjhXpJy\SOH\SO\DC1'\GS\US\136383:\1057922;\t0U\DC4\USM\1091749yE.\RSD8\986970\15943=\1043980~(\\cp\182885\34777\SO`HU\DC4\42123<\54540\167005\65131\134277\146847*L\998757<\ETB%K2\121071\25138QWXH\v"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_3 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_3 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "Y\EOT\1041731\1090454{g+c\b)F\nR$\14008\171005L/\997301\ENQjd\160659Q7{\\I0\986053S\SOH\DC2\135767I>c.)\t\EMKQ\SUB\1040059N!;K\"F\1041255k\53043\170978F\1036556.c [5U\b/\1065671g\993033x\148046\26138\SOH\60836/_'\DEL\EM\DLE9QW|6\1044669%\14648,4PH@j\1057271\1038134=\34080\1105891`8L4\rqCJDq\182577\ACK+\142454%\1053811S\47684j1<"
                  },
              serviceProfileSummary = "\48583",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_4 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_4 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\DC2\2580\EOT?\rE59 5\SUB\1025970IE\26056\17964\\\SUB\1095277\ETB\190163v\1091512z\RS\39100\DLE\135499\1102163>Dohc%v\"\1097155\38052\12651\1013885oku\t J\1034994._TG\1054597\121199\185208^\1001040M\120685\10165\NUL!r?\EM\5533\986789\993448x\166441\ACK\50015+\DC2\EOT\74079N\a,\1092968p>\1064269I~\n0\GS\ACK\DC4\190990Z\38331\4813\ACK%c\177832-[\25447cK"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName = "~\1048124M\tq\NULD\24766.!\1023601\a\54866r(\160676>OFM\f\1099670(\ENQa\1002463\1031190"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName = Name {fromName = "B\n\v/m\187731,\4141+/-*\1103865Z+"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\CAN\SUB\ESCf\CAN\bE\DLE\ENQB0\16043\STX\DLE9vs\DC4)\1054592\t\138028dmH7UD\DC1\1067670\136680\ESC7^!A6pJHS\f\DC3OK[7\ETBg"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "V5Fa\133261CSDmKNpM\ENQEQ-\ESC\48983\994370\&8\1081546O\SI?dT\36350AEWXm\1043092Wo\RS\ESCu\\+L\n\GSZRAu-,\1021325\1024215\r\1048618\DC1\49634\1008891\SO\DC2cn\EM"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "s\t3\SYNn\181942J\19445uTA\1021929\DC3O\992886i0\DC3\DC3\EMN\SYNN\ACKxF<\EM\CAN<c\ETX\ETB\vw\DEL=\ETB\nr,f\1998\STX\SOH\189172(\ETX\ETX\SO_\a\SYN\NAKd|LG\1021309D#\152663x{\21461\61880M\134109y\999340\1084026'M\1053937\EMN\155902"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileName = Name {fromName = "C\991783\1075342i\37668"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "a$\EOTE\b&\1008879;b&\STXZsy\DC3tT\153053Rje\GS\FSn\139670\1015333\31629i\aDolXo\CANk\1104861>iNJR\1004663m`\SIR\EMT\1016732?GV\EOT%#^a(;;j-\1088077uS\ETX\GSi:\ESCZ\DC3\155595?\39249\SUBVItQ\1003062$\189753P\1052448\CAN\191218"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\15306tP!+\18660.\r0\n\1032335\v\\\ESC\ETXqW\1104041x'S2_\1022311\&15b\1000729jm\187963\168793x\46307`\1051283&k\1021948\r?\1042383\187758\ACKL"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\n\1054987\NAKVYO{\1016150\ACK\1019886\134314\SI4\ETB\1039786\vS'~\1097626v\100694\1108028b(\61354*8\999731\SO\SOH\133696&H3LAG\1059553\DC1\DC35\\<\SIU\32127\&5t\141836j\rOe\1090095t\1111361P_OW;|L`\"e@v\DC1:e7e\15622\&8h1\ETB[w\1011040\a"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\b\DC4S\DC41\SOH.\1051812\tw\1075698\180083\NAK*\t\ESC\1078303\&1jp\1059165\34499\1098070\&3(\1835\1086313\DLE8\6156-\1095528 \53568co\ESC\ETX3\nt\ACK\bM)}\1076843r\n\DEL|xrZ\1076260D@\SO\35886\&0A8\a\1047555\1037481Q|QqiB%\1045741\NAK$>u\1077292A<\ro&p\n\rmMC\61532\&9b.\1030245\94450]\1098227P>\NAK\187584yXg~j[\1064998\DLE\7656,\1065535 \ACK\ETXi\1055090\1011219d0\ETBX\1000551%!f\ETX\DC4"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\177115'gK\f\154629U\EM]^\STX\ACK:\t\\\1060555_\187274\1075696\&3n\158394~\1084718\SO\ETXv\190062\1783\60867W\1017222K\b\1021798\r\1073851\&4!et%\1058442\51474w2\1112782\8295f\FS\ACK2\1003645U\a\\\1066783\189178s\1025043\&9{\STXOac\169091hdak\169889|\ESC\1027064\1055469+b\DC1\b_\175484\&5\189678>:\EM\SO^U9\SOH\CAN\71125p\US\SUB5\383\96979KN0L&S\n\ETXQR\1038694YjR,t\"b\ENQ\f'"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_5 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_5 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\168380\USA4\168144\1007321bx\166155G=7tdW5UW\\\187461\&8\ETXh\157263y)YiF\1080016 \CAN\163432\SUB*nuV=@L\1042283c\33534"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName = Name {fromName = "\"\f\1037434\DC4\a\fdP]%k"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "vDj!\NAK\f&\96380`\1092261\1070351x]\169248\SI\RS\177409\RSF^N\SYNuW\t)\ETB\SIxB\ETB\19210yr\1063214\172345\DC4\1101088#J\bk`t&}\1028509\aAhUOh&>:V3\6824ED`S\58960\150600T\SI\65720\46631^^B\1082190\1039098\1087437xy\37284\25670\DC2o\US\1062144\SIr*]\78023cP\9969E\1053890|\SUBWlhtY\SO\&H\f\1074923\\A!\b+\fg\DEL\32942a\SIG\173417\n<\CAN\163211r\SOHl"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "$\1090273&?f6\1045777\b\151817/:\1018046-\GS9\174`_b\1073406\NUL\169331Y;xA?8!22*K\DC2\DC2~\SYN\1050000Pg\a\DC3#\143427\DLE\1068297\1092426\8568\&6\159058"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_6 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_6 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\STX\111187\1004021/g\185454\ESC\67104/u\997959\19912`\NAK{@.\ENQ^k Z\SYNQiL\1072123h|Y\NAKy\ETB4s\1043537V\83033N@N\SIu5\1106608\159806dqs9\DEL\ETBw,\ETB\SYNhr9\DC4\1072944"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "%",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [MusicTag],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_7 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_7 =
  ServiceProfilePage {serviceProfilePageHasMore = False, serviceProfilePageResults = []}

testObject_ServiceProfilePage_provider_8 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_8 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "8\59317\RS\26030\NUL\184957\1073634\SUB\SOH];\US\ESC%\1057003BpW\SOH~\6380mF\NAK;q\1053542tV\153506\&7\GS\1069954T+\DEL;j\ETX].\DC3\DC2kcr\NUL\1004870\1050629\&2"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\GS+G4\35632SvT&\33211Gp4>\94215U\DC4nQL\"\132365Rg\1073524>*OX0\136361\&1|/\993115C\150788\n\SOXE\SUBD{n\SOH\EM\DC2\ETB>\SOH\34830\SO'U2m\99983r;'y\\\FS\f@\1011610-ZP#\1003333\48126\100207!j\1107114,\44017\STX\ETB\RS\FS\DLEQ\SOH\1006882\94283\DLE"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\EM\1060801\991839M\f\ENQ\a\SOM\r\FS>\78087/\1072909\&3\8607\NUL\1042927\1038775\1056193[\1028081\1001626M f\DC3z5\58536\1048881\"\1032989\10618\FSL\1078437\99841}kwA\183650Cn\14351\92654\NAKE\ETX\1036772\55054\a#`\CANsFIAk\49846`\146855;\SOH\1104643Lf\a\GS=\ENQ}\1110879\&6y\29834_"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName = Name {fromName = "\US!e"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\EM\1049724A\1041513M\9067\113674\t&^\"\58363\n\11371\21229J\SO0Z\1092578&\DC1\CANl\143431),yQ\60895\1029684\&8\1017395d\1081549\155602\127589\&3\191412\64521ev\ETBY+H\SIXf\127105t\178081_\1064136a\129349\&1\996241\984339'\r\rQ7|\1085568\SYNp\173176\NUL}\99160)\b\NUL\7981n\25922\1023251j\SO\21817\&2I\rSN\158407I\1014911[\DC2\STXQ\46364-\1096643"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name {fromName = "/e\DC4>P\147469\EOT\SOH\ETXA-\ACK\168594E'\1113832h\\\1003827V1\1018011"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\DC1<I+i\EOT\1100095\47886\993893\1054422\53577\5433\34235lE\ACK-S>\1024298\&3\72300\b\191097\&7H\60416>\ETX-$C\ETX>\1052243~\ETX\96868\EOTY~\v\ESCy+\\\1064544D?\98372tn5*\CAN\STX\b\ACKIG:yW\113755\EOT\167204\STXg\984230S\"\bv58\RS^\1066724\CAN:\US\t \SOH\1105233\fd6:m\t\46026S\DC40\SYN\ETB\EOT\128398kb\1031455\b\100275o=\RS\DEL\9463\1109757f\136221=\113737g\GS26"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "dT\148173\31047\EM\ETX9k%\127261\fd\53686O\STX\1056188\NUL]\DEL]b$AB8\t\CAN\GS)y\SUB 4\1083835\&7`\149105A3(O\1088386\US \1092134b\161908!c@\1022870&h$\GS|F\EOT\v-Xqt\STX>\161467\f\145444q{\DC1\t\DLE\14551d6\51086\USwA\1049758\SUB\1108664\1066390\EOT\ETBN]\a\1039768I\ETB7\59557\1024567\6966\v8\175887("
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_9 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_9 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name {fromName = "1\59914rq\156963KEi\b\151982#U\\\1025181>W5y+=*.\\*&r\1069846#&nL\147426\no\vv"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\DC3\DC3B0\EOT\EOT=[\11455\13782\&1\n?KegvHn5\rK\1008349D{ >!{}\1009979\&5\v1\ESC@\t&\DC1r6\176244\1002320-i\1113777-:\EM\154301\n} ^M\1000012\GS\298\1084860y]\51560\ENQ|l\1010139[\1041197\&5OdZJ{"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "O\120312>\\js|\1014721\38376\SI/\fh\100763\&2b\"\1050751\68430\1047912\&6\169609U\1017613\2085Z\NAK&\GSi\1097125\STX\DC2\DC1\1098341\41413\51935\32309\SYN.*\DC2lPV\STX\nwe\173971=\"\1096594-Q _s\ESC\989185\EMp\NAKF"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "N\n3\CAN%3Ep0ZG3b\1085668)\a\27253\&8\35980L\CANBi\1000925u#W\94644\t\1054562K\DC2c \GSL\133730hL5K,zh?V\1045556-\STXztZ\rX:=W\SYN1Z\13666e`\1053541\STXl7psf\SO\1034233\SOH'\1048732\NULz5>dL\167319\n\1088761,08\1029672Q\CANAF\95966K[\t\1036769\1111618\1039016\DC1}\132221!I\SI0e \1047982;\ETXP[!g\179382"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\1030487%D\1042788\167957ZT\SOH\994943\1093068N&z\vq\CAN\CANZa\172501u\rWIu\34200JeJ(\STX\179545\FS\1104793\ETBu\SO\165360\DC2CP\ENQxgmPMhR\DELb\EOT*DS]P^q\1015780Wl\97493\a;bQ7\16854C"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName = Name {fromName = "_%#\188987T"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\f\15387\NAK\nJ\1064005\33570\n\NUL\ESC\b(\GSeIi\169858\37449}X\1069819{4e\1073760\133008fESi\ACK\1112826m\CAN\1017083"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "'Y\STX\1096784\34408\DEL\SUB\ACK~\NAK\1107215\DEL.;1\b>9\190068\&8.Cn%\131789J\12070\&4\a\1008199\\Q)\55230AD[/~5=sl\30466s@\179524s1f\51118\DEL"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "=\36406JN'W\187232yVb\DC4X=`\USGL\57425'3\1024510\68236\n\CANO\\!ti<\SO\\\139905\998491I&<"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\41806K\GSj\131285\DC4\n(k<Dp\ESCpNa\139418\&3\141983\&0\bp\FS\v \140339K$\DC29t \18039E\73750)lL\1038155(\177815\28250#\36896\39917\DC1\165185@\ACKR\61472fL,6\1001448\STX?\1049003\1058272B!k\DC4M\v\1087622Y$ES$\f\SUBQm\41661\DC2Gn\1113068>\184101\996555|\1079701@)\1005597mA}\50620D]=qcW\1050951\&8mc\DLEw4\NAK\DLE"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "P\1064969\n\DELl&\"\f\SOH\24897\&5;\1011843\ENQ\38424\&1,\n-\1112014\161501c\\\1058207dIl\ETB\1028233\173667\2993`\DC2\46641~\CAN\SOH.{q\SOH\"Al\1107299\187771|\NAKx\59423\DLE\1101240t\DC4\SUB\RS\fN\ACK\1025510\985228\NULK9%2\25155jA\GS\1078641/\1081524\184015\f<\1099231\144754\""
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_10 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_10 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name {fromName = "yk\1008919\1007203\SUB\CAN$Z\59204\SOH*q\172316\1059368K,\62488\134081\1034503\&0s"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "N\1046682?R\DC2SJ\1073002\8564t=)62&K\996547\991427\ESC\156336dO[\NUL\rn\r9}\1104846{*x\94880Q|0c\1010405I\1094273+\29239\1105228\1081526\&9\150531iE\46330\RS\ENQ\1063756sM\7078"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "3\ESC\40716\NAK1}\989078\&1\24817H\1002767ro\57893\SOH\ETX\1093353$C$*ej\SOH.\999883i-\1005682\160153\40550\32039\ETBr\ACKc]q@^\72254\b2s\1009275b=s~K\1057767/\n4\ENQ"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            }
        ]
    }

testObject_ServiceProfilePage_provider_11 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_11 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      ":\ENQ]\DEL\160193\16893T\1009617hhT\SO\1108592\988506$iu\r`=\1043243;\162917^%\"\139313\182611nF-%\NAK\17595\STXQDJ?Ls6\DEL\998118o\SI\35590}Z\1112207\v\ENQ\"$`\1108778\152623S\bFt\DEL,k\SOHQ\ETBw\1113620\176148\1109306\153544&;\71079:A\1049379r\176123eC;\GS+)@[2\32287|S\ENQx[|y\175454Fh{\95732\120144\1009299cN\995918+\135151\&6"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "\42170",
              serviceProfileAssets = [ImageAsset (AssetKeyV3 (Id (fromJust (UUID.fromString "5cd81cc4-c643-4e9c-849c-c596a88c27fd"))) AssetExpiring) (Just AssetPreview)],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            }
        ]
    }

testObject_ServiceProfilePage_provider_12 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_12 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName = Name {fromName = "h\CAN"},
              serviceProfileSummary = "",
              serviceProfileDescr = "\FS",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_13 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_13 =
  ServiceProfilePage {serviceProfilePageHasMore = True, serviceProfilePageResults = []}

testObject_ServiceProfilePage_provider_14 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_14 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "M\39871d\989389|\v.5\1061414P\126486\DC1\vvfpH\157842\v`\1020492Yv\1046954\NULsj\b-liD\1024234\62056"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName = Name {fromName = "w0\1017076!.i+iz0@86X]\b\46893U"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              serviceProfileName =
                Name {fromName = "e\1023873\148734\DC4\DLE8X\nB\98187\992445=!t;~^*qZ[IW\186026m;Y\STX"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            }
        ]
    }

testObject_ServiceProfilePage_provider_15 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_15 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\ESC6O*\190263[|\"\SYNC<]\1021276\61333\"K\1098339um\ETB^\NULE7oiv\DC2Xs\EM\1000430;\GS\RShm5T\ESC\65342\b;P\tk/]5Fp\149157\n\ENQr4 \""
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "7Y\a{\STX\SI(\DEL5~b\EM\DC1V}\1072786m\1085366\&5yOo'j\STXQER\RS\917850'Xn\1087395\1004790\tW#\ETB\NAK.m\DC3\ESCW\183163!~\n,5\189060U_p\STXW\ACK\44806\v\985228\1109886\&0@i\1032985hKm$f\1011030\58911\b\\\119651p\154085Y\NULAG*_PvoGi\1081609L7IA\1000519\&0\1071033d\GS\SOHW\ESCv:$\DC1;TJX\"8\t"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "n\11250\1098625\1084584\1015732r\1094761^\CAN!Go\45730r\1011584\ETXV\US3L#\5016WX+\1092425Zv\r%qhS\50619\NAK\ETX\US\177153\ENQ\1100195P\1092863BWk\GS\143617O\1000377\52346\2297%w\1031965\&5^k\DC1{RYT\GS\995909\1091262C%\DC1\DEL\DC2\NAK\EOTm\GS~\NAK\173859|h*"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\1087307\&0\ETB>1=\DEL\ENQ\FS,Q\aJ&\1074108!\1038542\1027420\t\1083521(\DC1\1052400\SI[o~&.\NAK,-U\1081779e/Z\64806B6Jy\144713\&3\187132\SOT>\CANKY\SUBD-\1095114\&9tr\1002436pK02rEy\"`\10003XO.\997536B:P<\1030419\139054#\990571}>\DC2,\\\GS\SUBUg3F\1029064z/"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "*ao(\EOT\DC1\1109164B\vu\STX\USxU\STX\NUL\996134e\1000309Zp\DC1~m!S\997489\ACK9-d\DLEt#`\b?\1096474<G\RS&%q\132480\&5\61025\ESC\EM:\147983\CANJ\190470\NAK\ETXPtHu1\t\US\STX3\998917"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            }
        ]
    }

testObject_ServiceProfilePage_provider_16 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_16 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\ETBdm\ESC\1081681*\31224\1003912\EM\DC4H=t\DC47BQv\ESCf\1090474\\\23976.\139117)>(O\15388o"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "\61644",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [BusinessTag],
              serviceProfileEnabled = True
            }
        ]
    }

testObject_ServiceProfilePage_provider_17 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_17 =
  ServiceProfilePage {serviceProfilePageHasMore = False, serviceProfilePageResults = []}

testObject_ServiceProfilePage_provider_18 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_18 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "(B\DEL:}Y%\ETXZfv\USe\USx\1069109\1071941\GS\5609`|1(\r\EOT9\129423\US\1079833\&7\1080369\32727\rn\1019863\NAKL\DC3HD\1043094\&1\997284\1038339_\2915d\ETX\1106836Q(KZR\1003408?q-9\EM\\\1017435nt?\53018\ETX\142573\NAK\NAK\1040110\SYN\170102\1104177!\NUL\25872bV\148613\r!\1029763J&xJ{Gb2"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")),
              serviceProfileName = Name {fromName = "|GR\1107656($\ESCM\151608#uX\v&\1067261\ENQ\NUL.7o-J~\97827~M>r"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_19 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_19 =
  ServiceProfilePage
    { serviceProfilePageHasMore = False,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "Cu8a\v\ETX\138054\ENQX?A\13571\vNap\DC20\DC1\n\NUL=\ESC\"\1043813\EOT2\1100776\DLE\180830[,b\b\184640ijG$\98191PG\SO\NUL\1010442c\RS4_X}{n?h\DLE\1047583.\SI@)\GS^\185259$X:\100995\1082919&\38461\NAKS,\183321\1079411_\161252y$#}O\183932bE\n8(i`O5bC\37579W\17977@\92468W'\1013996\110992\ra"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\144813p\150180#=\136757-\n\162012\r\1068260\162311\169336\FS;\12416lK^<o=\141607R\137392+\17794wo\SUB\v\ETB,!PXRY\18665cUG0KU\FS\DC1`e{\1021588.\992382\FS\fB\r\US\b1<Y^e=\US\142625|V\f4\ETBH\viMr\184683k-\1012791t?\tPo\146977\4659\EOT\NUL\1109746\&5vzg@\ETB\158956s&Q9#&Xsr<\1101847\f\NAKF<]\1090002\DLE\1015498#\rS"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name {fromName = "\1061758.\1012347T\187344\SUB\1024660\1015134\DC2_x\1035579\DEL\1065185mm"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileName = Name {fromName = "W!\1067757Gx0K$\62009_|/U[H"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")),
              serviceProfileName = Name {fromName = "\DC1z\DC2\52395"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "BgU}_\119881:\70330\EMm\1002791\158544uO\1047781\1103013G>\1046899y5K/e]hJ(\146403;\145481Y\1102149n\1109252\&5\ACK\"O\156604k\NAKV\1082088\EM\ETXzd8Z\DEL\1104586?3\165585J\SYN/\9987\143473%\45496 \\\CANeu\1074876E||\1070460L\119199\46570mfo(\142677\53434\a\GSeQ\155640\1007125,"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            }
        ]
    }

testObject_ServiceProfilePage_provider_20 :: ServiceProfilePage
testObject_ServiceProfilePage_provider_20 =
  ServiceProfilePage
    { serviceProfilePageHasMore = True,
      serviceProfilePageResults =
        [ ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "\1067311!2Rlox\1000749\t\156085\24854\1106630\1090943 'T1\999616P\187839YzF5Qe\t>|}C\RS\NAKG\EMe\53004x\44796\1083118\CANCe\1077102\98826')\n\1080036\&7\129543D\EOT3p19!O\138397^\1020214\1079262\nL}Cj&\USX'hP{Pux>\44314\DC4\DC4\SOH\a[\\5\t"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")),
              serviceProfileName =
                Name
                  { fromName =
                      "!\1099574\STX\DEL\ETX 1\a\EOT\SO(C\STX\r\1053536a\\\GS\1104173~D\78236On\SI6\DC4\155457\&4\SYN\DC4\136838f\1097309T\SI\b21X,!\733\1096241\&7\133420EM*\166567d\97782\aA\29198\&7\\\987008\1002663>7\ACKs\1066816!\DC3\DELBq0\FS\185165-i{)\1009537\1069653K\14562Y\SUBK\SO\DC3W\ENQ\1056024E(VD]S&%\179190@E'x`\EMH\1057682\31381\48711f\SO"
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")),
              serviceProfileName =
                Name
                  { fromName =
                      "\DC3\DC4yY\1075954I\STX\ETX1o\138786ers ]A^5\ESC`@\\\SYNQD8'=,:(\1044332\145454? \1024196B+\EMn\ENQ\1087353\ACKr#z;]\1015902\at\STX\1114075\&3\1087612\1016779{\rL\DEL\\\5937'l\168601\EOT[\US\RSR(\EMPfT\1015344\STXJ\1008372\64583\1018586\NAK[\f\n\55180B`H\184191\1071946v)5\STX])~\EOTy|G\SYN\1079346\996777:h\DLE\97217n("
                  },
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = False
            },
          ServiceProfile
            { serviceProfileId = Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")),
              serviceProfileProvider = Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")),
              serviceProfileName = Name {fromName = "\51044> \DC4rM]\SUB\157493\DC1\n$0\1043945P\ACK-G\111084\9729"},
              serviceProfileSummary = "",
              serviceProfileDescr = "",
              serviceProfileAssets = [],
              serviceProfileTags = fromList [],
              serviceProfileEnabled = True
            }
        ]
    }
