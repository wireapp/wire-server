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
module Test.Wire.API.Golden.Generated.UserConnectionList_user where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import Imports (Bool (False, True), Maybe (Just, Nothing), fromJust)
import Wire.API.Connection
  ( Message (Message, messageText),
    Relation (Accepted, Blocked, Cancelled, Ignored, Pending, Sent),
    UserConnection
      ( UserConnection,
        ucConvId,
        ucFrom,
        ucLastUpdate,
        ucMessage,
        ucStatus,
        ucTo
      ),
    UserConnectionList (..),
  )

testObject_UserConnectionList_user_1 :: UserConnectionList
testObject_UserConnectionList_user_1 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:44:37.367Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\CAN\DEL8\STX\1035092-\1004074z.a&Kho\1002576\1027966}9nF\173234\1056412l\DC4q}\SUB\ETB\1024717m8#\39185A\1099890\1051825}Z\52677\&6r-/D*\USL\1024293\1113794D\1074633\EM\1023212\41115%PZ5\1069612\\}\54494\SI\60922\5521`\1009755'1x\DELNcFhN|*\986465\DLE\t\29903\1024334\&6\DC2/IK\SO\1053777\1017375s\147291\EMBM\60429*a\1036814~j\v#\DC1-D\186618\98625\ENQ7{\189669N\ETBC!~\128543EGS=\ENQ\1000128\SI\1013327\2908b\1018074'K1\NULE\58916\STX\163444J1\NULAI%\53820QV&Z`O\EM~Vu6t\"\1018406_I}a\1031578\&5\1009784Y#\ETB:U\140341\a\1033389\&29^2t\1073180\51546dC\95215\ENQ\EOT\FS,\"5.9;@\b`gl\GSEp5\NAK\EM.VYE_\f\SUB\EOT\FS\ENQbRb;=\1068524\990222a\SOn\NAK1\63452fF"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:43:52.049Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\20301\NAKO\DEL \RSz\133473\GS\991007\140213\CAN8:\1025282bQ(XKw}\f\t\1111001\&0\17916kx\a0=\1005888;L\f*\DLEko\EMz^x2(\1046242\\ep\46530w\63639\183850\&71\n 3\GS\EOT\EOT?z\154271\17377->\128707a,\133082j;-\64635\DC3\993804(L\DC2\999665\t{\1094858\51353<P\EMB\7044T$\1025619\62812\1022653\1047249G\1005846)c\5707\1088238a\"\38644\148359\1094293KS\1093022\&3\13287\EMzO\SI\1023445\31665L@b\94261\nW\ETX\SOH\NAKq\58173\1045716\185704\1069194AK."
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:40:27.247Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "f7T.\983177mRM`F_\SI.\144465Z\SI\142229A~g}\3779c\"8\142788\NAKK\a z.6\DC3\1019820\151791\50367\b_\181934S%v{_"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:29:04.518Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "Wz\189323ONx\SIB_\176367t\USHu\r\1046423\fE\24890[\ETB\1106226&\1013399-\1093330rCcyQ\DC1*\1100505\US\1068463\"\"^\29996\ETB\DLE\187299[j\50732\4863e$`5\101087^\b\1055708\SYN\25418)<\GS\1085205\17195U\1088789d\ETB>R\1007\14028Cf\1101438\994536\181048<\a\FS\92678\173113ff..\NAKkY(\152791t\SOc.d\SYN4a\997289*\1091628*/\1052425<\39469i\nC2v\63426O\59258\NUL_N#Ryra\1090243\&7+\1071913axE\1100117\DC2Ofu\GSyt{\997364q\1032583\DLE\4186\995325G0Jgf\1015132\SI\bgiBbI\1098755\41799\CAN\68883J\57572dfge@\26601\n1P\1046052a\EOT\f\RS\SUBf+xa&\174355\SYN)\FS\1110001z\FS\SYN4'\189190p\94911L\ESC\1113365WR"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:26:54.689Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\159324(\142498=q\b\140958\1057874\987683\36938Gn\DLE473\STXe\1066278\990364\v\94856\999474*\177897?\CAN\149013\&1\"\fP\45177on\aY6?5k\SOH\b+\162517\73737\&5\1072245\&02g\1036051\22548C6Nt=6\83506=\151441^#\1089515\US!\v\RS3}T!\185346\1012136\SOH\SI\SOH\1006762\131362#LIZ}H\984014Cg3\48293H.+}\DC1\DELx:"
                      }
                  ),
              ucConvId = Nothing
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_2 :: UserConnectionList
testObject_UserConnectionList_user_2 = UserConnectionList {clConnections = [], clHasMore = True}

testObject_UserConnectionList_user_3 :: UserConnectionList
testObject_UserConnectionList_user_3 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T02:11:50.603Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "X\tS\66256\1107078\1011188@\175171\44202]\1069880\187983F\149763+\SO\78742_uq`Y\1015350G\EM'/\1039206!iE\159225\175697(\1068052\159524)19\STX\ACK7ns\bHJ\96593xM^\1045192\1024360oc\191311\1012582\"\ENQ ju}:\GS\173705A\169860\"\t\1046860\1009593\&1\20440.@\v\1085413\EOT\FS\1047324\b(X\NAK{\b\1009263:!\1034330\f|\SYNJg9\ETX6\44659_F[\50726<'\RS`2y\SYN\DC1i\14897\1058366\152300\b!Q\ACK\1022273\39186\NUL\1003141\1109082\1058204\997124/?r,|y\DC4\r\149501sj=\SUBn0\720\EMb\37946\CANPqrX\98266?w\ENQ\44914^O\1029932t?\1019028\154679E`\987484l&h+\13881eN\NAK\23509\1044981\&3!UGK\SUB\DC1i\\O\174453{\993206`?WC\13978\&5)\40762_+o\986780@R\1074648\fT%jp0e)\1111614x1\26517"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-10T18:09:42.397Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "&\SOH0\1071342:0{\1069512$t\43240j\188869\145502\DEL[E^\1010039\EOT\nT@qo&`L(SP\1067057\SIX\nA\t\1070255g e!h\999334y\\pQ\57566\DC2*)\1077039I\GSF\990841\NUL\92446YA\18144=H\1041712A\97462\1073385T%YYD\tj\31928C~S\1038705\DC4X\1082958\n=\188828"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_4 :: UserConnectionList
testObject_UserConnectionList_user_4 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T10:45:56.744Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1020726\&7\1094842 \51875m~\45229}\ENQ \1026039xo7{|\1081241\DLEQ^\994829\182853\ACK'r1-!j\996636\1111358LU\166604C\1022639\&9\1109990\ACK+@\DC3\SUB]\176912\1084563Z)\DEL\US\1009904\DLE\47790\1009804{\189061\1000513h'\SYN\99312U\ENQ`\83023,\DLE\120300\1012919&\49365\GSO\188796\f#\r!\GS{\t\EM\1040937l\1110399\&1[\DC3\EOT0\1049624oBa\1002117+I\CAN$\blL\190547:\12457\SYN\16393\DC1nb0\74228zq\156778\SYNo4:\nA\994985\1059331E\158130\46915\ETX\b\1038295\165049\139293zr60\1078809\40610\&64d_\EM<d\1079591XWtm\181612{\USl$\SOH\EOT\a9g\1026615\NAKu\DC2~2\SYN\SOOWO\1070029|\1102446\1011695k_SyVR\1010532\DC2\ACK4N|4+g\64432X\1060834A^\8651[\fT\1043735\NUL\DC3~D1U|m-\ETXKd\v\b\51568%\1073691\145362>\72421\1054663\ACK9LT<\DC4\33315Z\177\13705\32737-\1092490i5ge\CAN8\ETX\1069015\126221q\34710\&0\SO\DC4T4+\DC3\4633"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T09:59:06.024Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "E\SI\DELK\994235\188953l\96401\a\52326\46295\60583'\191254fBm\fT\SIL\150239\&7\169874\1012278\184629lE\FS\35763b^\r\152021)qs`R\917976\1073323\&2]b5k }\3942~\156110j\101026\SOmZ\159095~5\NUL2\995257\187515\NAKM\CAN0\986803/h\DC2.T9\DC1\50343[.i\GS,PV<\ACK\15606\"\EM=r\175239\1082552\&7SA%\51649\DC12E\53449yG\DC25Q\n\51279\21520VD<\STX$\NULl|\v\1103466!\36856$\51570|T@n\EM\"B\EOT\a\EMI\EOT\ETXj\1076852\58037m\ESC%y\GSe36\1019389\f\1028311fI|lt\SOH,\22216a\EOT\1084630i"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_5 :: UserConnectionList
testObject_UserConnectionList_user_5 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:48:55.532Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1000064so!\NUL-i\171299W:/Q.&\n\1097166z\SOS5\EOTp)?D#!\DLE\38530\r\1030435\1103377\1090128\1047559\DEL"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:06:01.784Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "0j\RS[#\1035280C\EM;}\1110566\78808\r\b\1101258\163465M\1040960%\1002498\NUL\1097061\1086628\fvi\1070767\125241m\US\EOT1YfdxIS\STXP\ACK>\ENQ|5c}n\1102612\NAKG\69688jm\995912&:xpJ]F{\EM\STX#\ESCp\29892v\1082524b\DC3~\991651\984608\1036862q\1067654\&4\1082391<\166677\a\EM\1073621n\DC3"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:03:25.908Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:10:46.357Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\175878\118917\38030B\USXtX\CANpf\1001639QU@\EOT$\170247LA\24174N_x\"\178375\GS`\DC3\CAN]l\1093246c#w\1020092kTn\15042*W'7\NAKR\nX\US>Ewvz3<\95413,A\US)\1039314}mL\178609\1114040\1074049\&4P+\74917j\v\aW0\64531P\54915\SUBS\142374\f\179810\1009958h\990950?\RSli7{\1020785\162795j\151580\DC4\r\74980\ENQ\DC39x-\993414c\ENQ\68675\ETXSLNu\983233,\GSQc\984851O7\DC4P\SUB\SO<\n\1008934)\DC13\155022d4T,#\DC4.\DC1@JF\989696\ENQiW\r#\NUL\CAN\1113290k\DLE\26007\1002959ajsL\"u\44986\99131\194978\1014367\&5v\148791\1097237W\1071539\1012568|4&9\10584\1063396\ACK\1016599$\1046395\&1\177797\1104771\DLE\DC1)^64\62900\173017y\59199h_R\1078499\DC1G\1010560Z\1033272\71098'\1006385\1093864\EM\GS\GS\b\b#"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:25:25.817Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:12:01.153Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:01:44.742Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:15:38.478Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "07\DC1\164007%g\CANlWU\1052133/\FS;l6\34221!r\36875\1002203\1055161\126245\183810Y%\FS]q0fk\1093981\ETX\184324\r\140804pah\DC3\34544Z\1083153'\SUBQ\190669\DC1\ETB\ENQ\148292+we\31387\DC4_Z&z\1070563!I\179225v\1105016\20766\DEL\1008649\SI>I[1\1095569y\1070030\161495\t\SUBR\"\989608\188373\1077799\rW\DLE\NAK\t\DC4\177531\128974\DC1 \SYN55b\984462\NUL\DC2`\1040715n\173593\DC4\DC2o\1011143$/:\990503:HzuX\187426\STX\DEL`4x\NAKU\EOT\38447.\DC4Q\NULk\SI/m*/\142569\17974\1050848)\GSD{a\1030978a\DC2\22504a\1085303r\NUL\46143"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:21:07.732Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "7\14846\DC4X\95425\1079442\ACK\US\1015640AtX\1093513\1000713\EM>\DC3\SOHxN=b\t>\1007874G\1111779E+Qhcc5\\=j\142274l'#\1045472\1025367\b"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:01:49.115Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\120021H\1114042,;\999358\60861\1049918V8C[<\f\986342\NAKK\40598\95034\180349\&5N\vG\1112515#!\SIFu%!\1017270\1095429R\FS\1097793$)\1059368\GS,\NAKV\43874k\SYN\\}~"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:47:40.390Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "b\DC3v\r?\v\DELZ\SUBZ\78052Y\DLEn4\DC1\SI%vE\173184p\ns\DC4\USq#\EMa\997760\USa-1 E\1074731\983101\156662\156680=\v\r\984101\133253u\33327F+\11077Gv\11520\DC1l!s\992060\190313X:q\FS\ETX\SUB\NAK(X\"y\ENQ\nxq\US}\GS\159825j\DLE\DC4h\DC2\150005\ESC8n}j-{eUY\ETB\6128pG,\DELx\DC4K9\25510\1039141\148442\SUB\STX\41583\6982 \NUL\185381\DC37]\43015_\DLE\ESC\7774\ETBH\RS3\165060\r\a\tzu\\F/\1098924\98233\t!\1087882G\187341L\DELk>\1009226\1005523\1108889\v8o{t\ETX\DC2\44837\13463L\FSTk_b\1024357\ACKP\v\12139/\SOH\1072087Z\t\n{@\EM\DC2\EM\USY\1004347L\NULF}*?\984171\fA\1074231E\RS\172397~V8TX.\985079\ACKL<\74849"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:21:31.150Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:13:33.637Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\SO_\147554\&6I1x\1076626\DC2\53441\146421\DC3P7H\1065494\141929u\SI^DZ\1052542\FSMMc\1021200Pi\1005635\1061590*D\1086192rRp\DEL\1031896[]#\174651\&1#\120740\NAK\ETX\168922\GSM\DC1/\tq\1048293M\13015/\57792\&8\1046668\v<\39070d9*\1096131vkjUr\1034271\51171ra\vCp"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:43:15.087Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:30:28.519Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T10:24:31.073Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "A\DC3'\STX\\\1083877\52570\b\b\DLE\DELl1mY.N\STX*\178322\&0E\131327@ n|nm!E(\US\DC4.\7097\DLEK\997293\1103782\&6i\174056\1047262?p\1039039\991594\CANB\"\1013113\EME\177988\50931\1047279\171923W\SI\FS\52437\987843\&0\153852\40202\t\RS\1015831& i;HsO}I\146662\RSwbJdVAvRvQ\SI\1014384|\SI?`z\b}\ACK\ETX\1085904\v\136068{\aB\t\1093744_?\1066649\t\9617F\STX5\1091613-XTXiuX:1\97652#\SUBt\723\94978\a\134573\157479.\1017009\EM\DLE\1000028S#iwv \SOH\34599L\1101835,\161329\n\DEL\1113078aU\175398^>\NAKU\US\1018898h\1060682o\STX2L\1061739[\136926\98823z\1043814\30382h&\GS"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T17:15:55.011Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "q\DC3\32010C@\1042776q\1036465/[l\"wK7Q\STX\1107323W= `sN\1085208zi\142637p\1093399\&6~\146466r/!\GS#g\t\11422!d\EOT\996226\ACK\biB>\ACKso\\o?`\19765\GSD\78041\1055257\133592\164164\161197e\ETB-GG\DC2\1106504\1073803\&0) NodP\46708\DC2\NAKx\128782\168001J\96942\&5\1090945\1041654o#@Rou\1086919#M2\EOTa6\137441\DC2\SYNS\178862=\ACK9\1061804\77890q39~\CAN7T8\164474\"\DLE\USe\1057174q=_ *#\1070349$=Iu7O$3\ESC\182322?yI\991436\&88\164465^\993334$\n?P\1104410>:/,\13280q|T\EM\n\19279*|\42322B\f:\62443\ESC*m3\29297\1047550')\171600a\DC3\GS5\22391`?\5230\DC2`b\98740\162027f<]\28935U5}b\45221\1081960v4\1043655Fs3\188795\EMlPoMU1<\GSc~\ETB\1012643O\1044586k,\SOH*\NAK:t\1036433k\142417hdn>C8"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:54:32.384Z")),
              ucMessage = Just (Message {messageText = "C"}),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:12:52.624Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\n\RSW*M\r\"\185764\SOH\145407-~#\a[_bN{r1\EOT\39214\v\170987jX\54213D_\ETX|\1023557vh\1012747\1067378=\160714q\180140*h\ETX,g\1056625"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_6 :: UserConnectionList
testObject_UserConnectionList_user_6 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T10:08:52.228Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "C\1107727\&6\EOTx\190050|\STX\1002373\78739,Y\EM!\EOTC,i\a\1038260\917909\&9\1085268\EM#\145209\ENQKD6zB\DC2\161152\989430\ACK\CAN.\GS\1059986\54625z\5302F\38490%\1019505\25767\10099Z3]\163471wv9\61682\1112374\ENQ\v\140366z')5y\36508/%P S\1004030\DC4\13879\\B\ETBt\93970\DELh\4897\ETXv\13349zQ\159003[\144310\\|bx\1092830\nr\\\57380%\RS\1065451\ETXq\1068824].\1113589d\1040756\ESC,WwI4>&q\990381jR\DC22<=f"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:47:23.081Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1095433|\v\1052127\&2Q\DLEO\DC1rF\1081141^K\aU\DC3\1095810>B\149695`)\44487\92669\t.\1093025\&87|w\164779i\fY\DC1|(th#!w\ETB|J\RST]N1\6452\&3*C\1020662\DC2\140492U#D\no\EM\995284I)\RS\178388@>x\a\159536L\171762RUys\DC1\125038bF)\DC4\1013474\\i\36131\1014151\EOT\987228\994971Y5^\1108885C`\f\CAN^\16801\t*EL&bQjq\1101242c\169108\1020509{3\25636\40860\NAK2>2X~rw\1056229\167743 Z\1084092|\ACK\1036785\1004291\1041185}\r\988564_<kQ+\49258\&3_\US;\1057452\NULV\1007751\27895&6v\36487\a/\ESC\b T\f\997790\1001052T.g\137456ub\1098875K\30295\14770l!Q\29789\"\1014257K\1074902\185716p\ESCE\DC3\1032295I\f:\1015020\97539v\1052764w\STX\GS#%s-\181142\&1\1053959J\165728W[\166~}\NAK\1046179\v\ETB%\EOT\152955K\37461\66871\1057163\1070044\1111811\983270\DC1U%\147113\1089131\DC4\63068\1048964>e"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:20:55.672Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\\\16521>\1104678:w\SOH\144047\&0\1071809\ETB{\1024170\ETB:w\46784\CAN\139731L<\SUB\182831Q5\DC4\1102218\174648q\DC4\60110\66009I4@\167034\54317F\1095206>'q\1083855v\DC2z\EMF\61092\176061\1081861\DC3\GS@\SO\188908H{0\1057163]y\ETXE\STX9X6gWL\SO\"R/(`M[\1038661H\1000792~\SO\n1JZSjw\1104488od\94407u\1017121pEsXF:9\162588I{\50369b:\46937%\995662\SIM\1051346\ETB\DC39\CAN\180190d\t.}\99176_]=\1110273fEIaaKGI>\1064515\998582\DC3\83253\GS\vII)~\172177QJ|"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T20:42:08.294Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "-\132651\DC1Wxe$?\4123\NAK}\16538.\EM<v\1001092\ETB?\FS/'\187049C3\1008876\1046257\t\DC3\100450b\1061268-\SOoH\15729L\1111121s\FSzo@\1034291z}2\1031601\165634\1029534:\DLE0\54355\&6=\tm-@\SYN\NAK\1107672\STX\nT\GS1r\DC3f%%),s83\DC4H\93954:\DEL\"4\1097541\6791\169983\59513p\358\1093837\a]g\DC4\1046366\&5{#,9\1031967\&7#\t\143692r\bU\NAKfD\EOTB\1067214b\NAKe\167678\61752+de;>\1030980.l"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:40:59.296Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "G#tU\25280\984208\nl9T\"\39372\&4n.\13236V\EOT\183807\n/\1110282Y\EM\br821\SOXK\ESCN13dY\158786m\a(>6[\1018226!c*\1011466\994635\1113231\NUL5\1093436Z\b\ESCW\121124\DEL,W\1034365\DC4%\RSX\DC3\13973\ACKZ4B68\1031015\1080955\1107252\1063460i$\DC4\139850\ETX\188106K\46371^:)\10784D\62678\EOTW{|0\DC3g1\1099785cp\1000256u1q\120713j\DC4*hs\24930c8S\1091414yO1+\t\f\999061\1084615()\ENQ_$gJq\DC3\b\135974eXF\33427\NUL'B*\1000794:r\SYN8\12863\1019721Q\119933\DC1N\1090957T\990684+\RS\SO\17516\SYN\46340\175762>G\1032678\66582\48305\t\1040066O\1040819(\1065262\6129QvT*\RS\58598\&8\140707_D{8\ESC'N(b\23556PaH\94524#DY(/rV}\t<5\146924\1086975Q^\988386\998136w\1039254.\SYN\vh@\28654cHZ;\SI\a\71193O>~s\10589!\SUBa\1001082\998935_7<\1046685F\SOHVr"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:17:19.100Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1059270J&\1058594[\ACKsk\af\a\t2\DC3hUx\46363\1060559\1701\169533\&8\1041589=\1068117\30078ozg\41643o\US.\25896\RS\49286{\NUL\166622<\ETX@\\\"\1079961\US\989420\162265%\27068^x\1082128#\28895\42377TQ\ESC1-3\ESC -\ACK;\SYNn\51937QS=\RSd\1031088g`+?\NAKA\a\SOv\182182\&9p7_?\FS'41K\159378y\v\1043835Is5*\1066772GZ\DC1\1048964\ETB\RS\40096)\DLE=\DC1\6715W\NUL\RS"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:50:29.653Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T20:32:08.864Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "x\1096332e\1010643\SUBEz\120309\&1$!\15283Yo\1030600J\65757Bb9\4465@[I@\USe\NUL\23020kksdr\EM-P\30623\22665j\165783\1066838C\STX\ESC\96325[wri\154327\"\60501v'\63842xH\1074589 \GS\NAKp\CANm\139708\ESC+%\1081427\NULq#\38415Zdd,\NAKuI\ACKs*\134712\DEL[3z\1032545\ACK\1110819hO7i\EM\DC2\167616\998660\1048344A1\EM=\1109112_\149008\t\USh\1083289#"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:00:02.922Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "@\98901\a\5130u\n[v\NAK\1001559\189426*l\CAN6!r@sb\ENQ\50290+\15831\58156C6R\1060713\GS\61263q8\FSA"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:34:54.989Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\EM\tG\1039336~\ENQV/U/\1074956\149557u\DC1J\1032001\"\RSw@.\1105902\1062762\bbV\142375\1054385\DC4X}se\1066803L\1031565\DC3\SUBK\154297\176212\SYNLG%\CAN/`#KD\1062811-\1070318m\DC2\984177\RSd\180412!\NULy\1057936BU\177019\&1\68244\42194Y\1003389u\GS\1026864\SUB4\1087240\19667\180438TaOS\1012290\&6\16026;\\\33053FCd\CAN[\DC3(\EOTv\83099%x6\165494O=\tF*\DEL9%Bu@\140073\92708\NUL\n\DC2#\NULb\FS\8258D*\9958\SYNR/\110991K\997588\94816r\163318%[\1072088!\NAK`\1012363qsH\98309\SI\20566#YgS#\78183d-S\1035366\38799f\STXx<Bs\32651q\138375\&4Q[cNs\DLE\992146+ZK\EOTnLG.(LX \DEL\1094542=}\96236\SUBt\129527Pz\ACKU\GS\b\EM#>\1105022\DC2|\SUB*\1022144E8\NAK\152628i7\34243{\ACK\RS\NUL@ \US\1074167r0&rpQ^\\\GS$+"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:35:16.125Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "&\1050348@2H\1070628F\SO\120054\ETBX\1021891-\US\SI\ETBW@\22202\172478?a*Al2\1055120\&4\146195"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:41:33.128Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "k}\SYN\1055801F\1094901<wc\159918\tb\a\STX*:\12423\SItQ\RSo$j'\GS1\1033425\44729\41694\1103026^'A\"Cy\r\19524PN!J\NAK\v\1020018\&8(\38990n\RSs\SOH[\1094632\1030458\ETBG\DELW\1012517\DEL}\171610\1041465\1065730.\1096266\ESCHS\42277(\189073&\23646?7}\SUBN\182293\1040004\1112991RC\52134H[i\ESC*:\EM?\146330\991482.\133597\32640sI_\STX\SOH\182729DUe7_\1086596*O\ETB\ETX\1008735^\1029810\1083461\53622\29912\&4N}\RS\vwbFT'F\\BaHM\1061890J\\(Rq\185312\189686\&2g|;\NAK\171839_J;\999599\SObw5uw\\^\33852\rx\SUB{mk\99103\146794H\100918\&4\1055880\12699\SI;W\1088983\16644\1017077h~\1106022\DC2}\1105946\1084332j-l\1082340\41021\n\1092711u+\ETX\988506U6\nA\1020134"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T22:36:15.880Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText = "\1054587LUx\EMVU>\150057n\985921|y\68643\169530\1068113g\983889\DLE\92884=\DC1G"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:52:37.732Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "I3\135077e\158503s3Od\ACK\EOT-\SYNg\34328\1041118[9s1\1106741\DC3HBGHymJ\1015832)f\vg!<da\DC25\SOHhv\SUBp\95785y6\ratxy\\$.E:\CAN\1065434\aF\1022362;>\EM\990568%ah\22316|\"\DC2g}\RS\1083407;\ACK\128569@faH\1080307\992993~\6356E\NUL[\1067994\SO\EOTZ"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:11:11.073Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "!|\168599\ETB\EM~\1047714\ETB8\37746\&1[u\1021200\NAK\58839\"@J\44393G\139447P.\1068880\45871\95788\167658u\1051023\&2f\100770=\12840\ETX]\70786\NAKu\"\159194S\185154Q\128488\EOTv\1088416\160578\173369\1097424b\NULN \171404\1052116\1081190\DELw\GS\1088472\DC4K\SYN(J\1009050\1077623y\54301\v+\STX\DLEZk\CAN\1109776e\100179L\159924T\STX\22437\v@k\1026823lV\160847\v\996035\SYN\1041033\RS`\119042\158129_"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:23:28.620Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:02:21.118Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:53:32.635Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            }
        ],
      clHasMore = True
    }

testObject_UserConnectionList_user_7 :: UserConnectionList
testObject_UserConnectionList_user_7 = UserConnectionList {clConnections = [], clHasMore = False}

testObject_UserConnectionList_user_8 :: UserConnectionList
testObject_UserConnectionList_user_8 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:16:45.156Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "PVR(Fb\1004499wp]\170943\DC1\126996nz\1049945`=>\1110825N\1009968.&\RSJ\SO!j\SO\\\SUB(q\DC4k:bP$a3\47725\38888\1055166\148233Y\1070604[\1028169v?\FS\188397\"\67373\70300!)p\r\1039997\152215\1095636*q|\ENQ+yQt2\135701\151326\CAN\1014042s\40170\1080034\DC4\98416\&0\SYN\1060713\169629R\1079319=\1073100;\139256\&6Fg~\SYN\999907IG2ij\1091860x\1012721\DC4\1114038\"9\1722\110867e\DC2\SUB\171063O'U#-E\995348'\13477P@+\"\32885\\\1043854\f\1035081Q\993733O\53183\38918d\\,d#\b\1048102~6\SI\11312\EM\b9#\1056960\FS}D\FS\1110667"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:34:26.091Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\SYNj+WQ\NAK\1090319Q+\1002343\DC4\30427\1063316QOA^~SV\99938k\DC10K\141752b\16546<\ETX\1090112dU\n\DC2|x\1080757\996064b\1022377H{\DC2\SI\SUB\74325\&3C\163679u\a\35964;e\ENQN\NULJI\ETB%*mq\"z\92336A\1006098em}\1050398_\1037864\SIKjj\1065234}eAL1\b\1058519\US\148795\ENQ\995146f\179980\DLEF>\DC3U\65185;\US\1083732\994238Ve1Nl\DLE?a\1019676\1109364@\59150\v\EM\CANngkMt\49564]#4\ACK\1099558\1103317|_\SI\EM\GSO8\51010\NUL3\\Z\44959\175567e$hN\DC1]I\DC3\fIo\GS\1100530c\1042086\RS\t&pX\96425aR\1074313\51138\34857\DELLC"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:56:03.881Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "o\ETX\trxo\EM[+kw\DC1-\1074291~ZDT%\DC3J\65224\&9D~\DC4\SYNhC,\145694`\\\1025848!i\ENQX*{\EOT\4123\GS\tL\RS\4204/\r\128684\1051702ji\DLE#\132831=\r\DC1\181436qH\985245{\989824\&2\148677]R\ESC\1061087\16931eJ\35949\95996_\40820a\1076642\GS/1*5WF\ETBN'\120921\\\FS1h=\DLEX\CAN[NiP\1065488!c<[6!WU\STX\1095329\14487\50895a\1074542D\987943\1084551zhcc_.c3\SO\999735\STXz\DC2\DEL\96170\"f\STX\RS\38327\188658\&61\8863W\\\a\48719M\21107sr<X&\1023867\60684%\STX\t\1049318N\SOH\127242iIwj\ENQ|\EOT\1087525\983209\99253\EMC~\\JT\vD\165828\1084476}9;\DC1o\rp\DC4u^]?+>v?{3w\a\EOTI\190375\1000953~lP\30757\SUB\1111250\a96\39393\SYNg"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:03:58.277Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_9 :: UserConnectionList
testObject_UserConnectionList_user_9 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:18:43.747Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T22:10:45.987Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\f\SO9\987294\138584\r&\EOTT\182652\"j]6@i\16829+bc\SOY/\ACK\1058404BF@Q}'1\DC3\DELYP#Z\73105\983408\&43\164375]V\1025378s\SO(\23424zv6\DC2;\77914\&1\20071\\\NAK\DC4\61537\\\ESC^\4737\1087803P\191280^~\144282Y2b\179910.\reg'y?\1024797\994311&\39945\149192\SUB\39730+*7*#\GS\1065520\28645\&6\NAK\EOT\SOC\1093932\1056355\986125\SYN\64368\US\EOT\50983k\fM$T\tJAP K%E\187411o1\100886&\ETX\1089788\DC31\NUL\146767\138849_HBZDJdy\np|l9\1010092Ot\SYN[\1097917g\1023993A\154871\SO\SYN\57574\1088627sP\fk\1016869bd\\\ACKB\f`\14679\995206q\57567N\1074455V\DC1 \180544l\44329XL\t\SYN\1038872\173431KH\1107878l\ENQPm\FSQKVl\47170\1082113r\DC1Eo\74571i\NAK\181988\SOM9\ACK"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:21:58.696Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\fQvQ U:%B=PF\172530\173067\179872v\f\1073595M-I\FS01\1060999I\\\GS\1067460/Cz|l</\1085561\t\t\DLEY<W\b\r\1036282\1092010p\"u\42435\1046554o\185339\1101653\95500-\1056284\&3Vkx6\185238i<\SI\1026577\60415\28671p\137853\99382\1105150\&5m\DELO\1101197%j\SYN\147916.m\1069145d\1110463\177623NUD5\NAKa\ACK\GS#\DLEK\12185@C\171837.P\a\97368\1038202ql\NUL\vP\171831\1065968{\4260PfJ,R\985968\&9*h\1038989# \646\&4c\147632\1075171\NAK{{L67,\180965#-Ddhy=\1021823\rZ\1011632\68020\1091480A\CAN\DC2\187014D\ETBd\120911\&4~M\\k5\135454O\ETXZSk~'\NUL$\EMEt\1113137\1067956b\3795\154488"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:26:16.369Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:22:06.661Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_10 :: UserConnectionList
testObject_UserConnectionList_user_10 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:59:02.193Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\EM\1012688\DLE~\173552:\993685O;B\DC2\1088447\50601g0J\70339\ESC\SOHmxB\"\EM\DC4G\ESC\133781\b:\1069669~\SYN\1082660\154527\SUB.oZmNI\1004166S\DC4t\ETB\CANT\FSP\ETBm{&\99016\20682\DC4S\b\38823\fWe\1023549fe\140331D\DEL\151004WCpp\73061\1109217(mW\4888db/{\DC3\8817\"6h\180681GYik\DC3\f<A7qf\1110076\1056303\121299\149816(5E\1009922\NULEu\r\"%\CAN\95392\DEL2\1049933.\153818\f>Mn7L\r\31090\" 7\1003294\1047612\&5l'\f\37811P\ENQ'\167604\42671\1062122N\ETBN\SI^"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T20:29:57.465Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\133075\150134H|\159625\98422d\172508F<eu\20165\97866XKc\1101039\v\185266\"1\83341>4u_H\DC4|\136921m\STX \37490\24295|\370\51285!^ \NAK"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T10:12:06.916Z")),
              ucMessage = Just (Message {messageText = "\43836\30674\CAN\19343!{i4&nd+\r\n\DC2s0.&\12484k"}),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_11 :: UserConnectionList
testObject_UserConnectionList_user_11 = UserConnectionList {clConnections = [], clHasMore = True}

testObject_UserConnectionList_user_12 :: UserConnectionList
testObject_UserConnectionList_user_12 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:34:11.432Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "k\1098630H\DC1G\1089355(*4w\160057B\US\995978\1074065|0\NUL\fb\a9\1025520R\STX\viC\1058003hmX\1076599-\52939=\1079816%<\989745\1013598R\985198\187458A_5@vV\111041\US\DC2'\160391\DC1\41067glKQ\v\"\36026.>+\vs$\USC\1022220uX4Z9\6332&\61884iS|\1082227$4FV:No\39644\SYN\1092639\FS\1001843R2yE6f\1003112j\t\1032148\131836I\ENQ\SI{"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T10:36:09.883Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\FSN\169889!N\DC2\32905d% X$d/>k\RS\vnH\n\181554j\1089490wj\94413\177791\ENQ~\173030\1035420 W\1015349#\153325'\fs2\ESCi\181714\147309\ACK\1076060v*-\21162\8549P\154039\&5\STX&Y\38928@\52277\SOH&o\1023521(\EM_:2-\RSW\"cQ\11172\987747\RSg,\SYNpY\DEL\n\DELY(Bl\2725#~k0\1113357\133662\ACKv\t\US\1085452cr"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:37:23.018Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            }
        ],
      clHasMore = True
    }

testObject_UserConnectionList_user_13 :: UserConnectionList
testObject_UserConnectionList_user_13 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:28:33.117Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "H\1085827L\DC1]\n}F\DLE\DLE0\1023537\ACK\1088855\EM\1020483adWNy?\SOl){R\16938\59729\CAN9`\24558r\1036753SCMY=\194706DA\SUBeg\SI\EM\58376\ESC\51629\1060711\DELBN\1020062-\SUB\190625>,4{[\STX;DP\n\ENQd01# \ESC\1085236\DC1gF%\RS\1087133=\177456\DC2>lrO>\EOTtrz~<\r}g-\SUB#8\NAK\183740Q\ACK\26901e^\1005217>\1052237\&7\DC2\GS2=cB+j|L[E`\42081~vdxD\GSl?;\63450\EOTFl\f\1051205*\RS\51778 \1087164Q`\4711\b}\ETBMn.\829CVX(\1065683y\ETX\1092727`L#\DC2\NUL>(@3\20333\149171V\STX\180972'"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T00:04:09.041Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\DLE\ESC\CANbe\33068\1062179\26270A\120854\178381ZA\STX\9019\983277)\v0\vx\45983\DLEP\10177\ETX\DLE5\r\16709\SOHNq.\119529Bdm\SO,\vo\172725\1004103g\b\GS\\\1048257`[\137373x\1052823L[\22474E\142128\DEL\158828\1037208a\1017335?\1056638\DLE\EOT:\DEL[\147476.la\35556\DLE9\DC1@\1010585\DEL\25014\DLE\171098\SYN9>\145590\ENQv\CANbm7\1010003N.\SUB=\34097\SYN-\ETB\1052112\1007095':$P\188987]c:3(R\1113667\SO3h\96338\983477=\266*\NUL\1020101\ETBI\ESC*|}gD\13565w:\\\DLE{\49902\1075829\95251\aw|\ACK\1028158a}&q1)\a\fJ4\n\n\STX\1059207\995089\1012850\SUB\31672\52743{^\190651piuXo{jHa\13804;!$0 \USi\150219\36053\4668\"}.Q&\1094480+I|]h3Tt\SOg\SUB\DC2\f+\EM\f\1008424r}9uJ\1024142N(\1032865\vOz\1069114Y\1065451~/\1029239\b]"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:48:54.495Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "=m\1019673D\\0\163789\EOTJ*@\136719\DC2\fO$\SUB\65723I8w}\1218\75035T\188300!\1077414y\FS\1112157)M4A\ETX\DC1\8873+Bb&\144266.a\DEL\141139*\1006904F&[d\NULu\52476A3\151243|$8\52545w9%l\CAN3\t\14387z\"v\f9T\USqH]V\990194OH\\\14323/\153522bF\65602+\DELQ(@S\SUB?\984881b\ETBM~%\n\131132gG2\DC4 u\1077308\DLEX\\J\1042480;\rT-\1063862\RS\1004542u\b\168104y\1026741_\994437\GS\164387O\1047987\FS\SUB'/&\178636x\66293\12946\164141\&0\n\nm\f\EOT\DC4d\STX%\ACK\nl\DC3\FSPIW\\\"{Y\1084209\DC3\1062168\178553\6509\1019408=s+a\ETB?\1093446eh\NULy\1063071\EMl\n\1028548\GSM\b\181969#\ETB\139707\&22\SO\1072440\FSK\r>\98248]\1029169}v\151577 3\r\140430]\1019781,Ju'3Co\83166\1032444\&2\97422"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_14 :: UserConnectionList
testObject_UserConnectionList_user_14 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:40:39.351Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:28:18.216Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1022021N`CxL\1035271\STX\168128\&6}A\188075Ib7x\1033151|+t7H\1006717*$J\DC2\1017654\FS{ 9Q\DC4\FS~\v\STX?\1103196\100383\DC1\1097169Y\180957L1\989555%T\DC4P\NAK\23640o\1057845\DLE\bPB\30653|fy\1030751\ETB\CAN\95418\"\1092397\SUBci\RS\1047643\917569\1025757^%\61839\1334\2542\ACK\DC4s1w4a!5\127159eYUCb\DC1\127009\1057898A\NUL-~\1014027*l\DC2\f9iy\110622K1WG6\fV\nsg\ACK\NUL\1112135\1019440\DLE\n[\158865\1073237vh\1014465\63346qI\a/*+\50126\63797\SO0\1105418}E\STXII"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:34:36.808Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          ":0e\1011680\DC2t' \1017040\v\990953E3\ESChHM\27428\ACKw\DEL\989385\FSi\1054044:\DC4\39953,H\DC3\1042457\190434\163911\NAK\SOH\1094239b\996969\v\83116\984449\1038945\1111848Y\n\1093060-\EM\ETBU{ir+\1070229\CAN\ESC\b\50660\EOTrac\v\ESCo\DC4\177201\1004035I\1108143TI\DC3=H\DC2:q4NC\1044987\&9.Iy\r%\995783)\72804i\DLEv\987323u|\azQkm\1016981\50658\162947\131814Qr\GSnVd\STXEsb4\STX]\1083398k\95029\SUB\1033137G\1094533{\ACKmG\993963'g\1053135d\ESC9k\5125e\NULdT3\1676\1068186Q/\byH\135301\CANtU\996195;\1085096\1053891\94973\SUB\78578\1095502\95309\"R\DC2\998593\1053926S!d`!9\160183lq\a\ETX\61853X\62435bL$?K\1014570U\DEL\1017557\1107286Lt\n\64126^U/T\ESC51\\kwV_F1K \ETXp*1%)\998531D\172723m?\1045106\1022022u$|d7\SUB\1016526-\EOTu\47140"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:03:31.446Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T03:28:16.129Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "foi\DC4\US\136626=~\138729<\147840NLS@\1095654\&5\fP2\64806\60376@\1108319p\SUB\NAK\SYN \\X\EMo\10556\1058889KR\78671vi\1043408?\96504"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T06:45:16.969Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          " \DELY\1036396\DC1\vo\1008287\STXO\127854\FSM\994957\46159,_\120192\41318\1087604j\1105576V\156479\v\144892q\EM-D\ETBNk\189932\tL\1083571\1076916\"FI\137350BV)@o (zi1\1042284\137194\1048404\CAN\NAK]\1052698;<wr.\STXjC7\STX\DC3\1028402\94535{Z6g\NAKr3\1070725s]\1043955nDFv>3\30395\USgKf\vU\EM*\t\2285\DC1\r9\a\1020812\NAKLS\ETX\1074258&fUy\DELB\1097217\DELM\1010851q\1076411/-\188370\1083822\&3+?8B\1094472\n\1008266\987977\DLE>#a1\147487\&9\69849\1107933%@u5Z4MDCDzl\1101944\SIk\ACK)|\1083707!\1046597b\b4\143947u\fl`Jg\n\144747\SIK\"|"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:40:24.320Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:26:50.261Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "W\ENQ{;I\7155N\1004308SC+\135847c(\EOTA\CAN\159895\af*\190951\f\16948\1035017/G_\23776|A\1015337\13268\vVZ&\13626L\DLET\1001024u\1098272\&5I\SI[\1108454~\1081072\\\177159\171817Y\30197\b\1013525cI\1098882\DC3\1093803"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T18:21:03.201Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      clHasMore = True
    }

testObject_UserConnectionList_user_15 :: UserConnectionList
testObject_UserConnectionList_user_15 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:13:47.534Z")),
              ucMessage = Nothing,
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:38:27.967Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "Q\ETX\ETBHy\142969>\GSZ{\US&9Z\1041885k\GS2\1007345\64380H\NAK\1047423/\SYN\174117)\1003692{b\1100600QG\185733c\1062118\139750\DC3Q!\SYN\STX\EM0\CAN\v!\ETBlT\1497\&05\vh\1105406\1099636\SI\1011080\21581wqo;\SOH>4rx\46201L\1063856c>F):I<\1035883'MC \GSX[\1097988\US\73014|\1032211-BQ]8W\nWp\CAN\163387Vg"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T19:11:48.008Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "|z |\SOH^\\\"{\RS*7\1046872\25934:Y\132810Eth\1112349%<!+!\EMF\992837\1004981\DC1\1079942\984980\DEL\SOH\23454\n\134005P{&D*$I\140590!]\69923\1083850\FSFz\NUL\1042276\DLEwN\r*\1089457\119923\1011579\1042529\&2&\STX2\37408\DC4#\ENQsw%\DC1\tHap9s\ETB\166232\r\61848[\n\1079925\72399x\ETBf\28827n\119631Fd:\1064921\&8\1091639\DLEX0\1016374\&5\185980Z\155624\DC2T[Khk0\DC2\SOH{)\990624hLA\175576B\to\a\1018478\SYNHG0J+^\US\SOH\ACKU\ACKvR\DLE\RSq\ETBC\US\1001622-\v\1003236r\RS\"\1103849\1106991\GSJ0J\EMD\SYN{d't\1058073_\1108001\NUL\1020728}\28550\NAKHn\1049290\985861\176719\1095126RxP8"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:37:11.962Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "[\989170(\vr\34066O'6znFZ\ETX\58038\SUBag\134677G\ESC0\139687\984765C=Nn\vM\SOm\1000255A|\ENQ\DLE!hhN\140292g\149052\US\1005429\66851\US9\174730i\1008247\&5\1093581\189652\&7\FS\DC2\f\12064wc:*2J\94010.\178269@g\ESC\CAN\161690B\28283\FSI\v5A\b}e2|9\n#U.9\ACK71\n\aiU\FSE\FSeU\1037846N;#Z*\1059022\22731\42025>?\1017385e\DC12RPJ\ESC_y\v\1057458\SI.\SOH\n\n\917543_6|t\DC3\127195^\50827\146196\155662Q=u\181415\SUB%;L\f0M\DC4|W\1016438\191017\1062762\b\ENQ\SUB\ETB\US\46740t\1050772QJ\ACK\USi\DC1+\147442#\1097339"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:33:27.281Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\138989?@V4\DELz\182487\1042251\1012614\DC1y\CAN\1054165\1008632\1010901\NAK\4311,\SUBqu6\3747\1057819U\186461\24780*\1016499\"\v\EM<fn\EM\SUB\1029050\1048680w?}-8}lnHp3\45737t_\152210\fu\\O\72811b\2097Y\1041921\STXiJEz\23685[q\169175+#\1034931\DC4k#y\58473\68447^6\177917\&6|\1984\&8a0y]p#\n\SUBXoK;/\1046222zT\1071466\1033792w\58913\166297I\1061063|\b\1013014\987942\28541"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:19:42.145Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\174702\DLEh\123641\"BTT82\ENQ\SYN\28500\&3i\f\134112w~<zY\143034a\1020017\1001854\176941=\180801\CAN4B|sz\DC3I%\1112309\US\168861\tL\DC2'\74973`_\EM\NUL\\\169643\SOZb\1095697\1036232\SOH \r\169450\&8\3793-(~\EOT\1086937 4\SYN9\129496\SOHrx\987677]\34109\1108630\31765WUG8"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:39:36.233Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1025712a\1010520\1027993lJ\995051\50552\FS0XJl\13868-'BRU\118917J-^\1049380\ETX\1003769\&8\991224\22895\1109499\&9>z5F:\ENQ\100482B|\185825Tx\DLE]8=G(f\19248\162948.d'M\1031561(U\STXLf\1009847\EMMtw\ETBwS1^wBj\ETX86@\DLE~I\1111573\30763j\DC2e?\\\170026V{\11911P\ACK\8119\1001684\1056421\&36\189853\1016747)\1004934ro\131409\r-\47445zX-\STX\189402\&8qM.33@vXT\57415Q/7{\1057544e&\966`GN\ETBp9\DLE\1023842\188272([wW\997939\120900\ETB\SUB\DC26EL\59036t\168358-\1015832|\189627r\DC4-2H\2925\DC1RK\f\10775\SUB\1042937/\EOT\1073548',S\1088952\f4~\151575Hat[_o\SI->DS150\1112728\1005946\&7\US\38506\62493\&4N\ENQXJc"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:38:17.908Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\ESC4\ACK\1051828\97461d.\t\1074160\b\ESC\RS\ao\1071785#\rP\14263@o\151099\1050193{RM\1073620\141933\t8I\ACKZ\1052419j\\\fz\ETB52\46878\DC1\n[K^\RSQ\172515&\1110176u|M\US\180895\&2=2\SUBz+\1103593\SUBR<Q_\1019412\EM\162497xdd\163516\151906f\RS{\1064747\152672\1018106\1090398\NUL<`6wA\n\1067780\GS=a$\1017262\DC2\n\nz\a\988764Z\29095\48667"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      clHasMore = True
    }

testObject_UserConnectionList_user_16 :: UserConnectionList
testObject_UserConnectionList_user_16 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:58:16.368Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\70664d\1016618\&8\r\190175`N\1048899Y\1013329\98037%\ENQ\STX\ETX0\615<A^I7Zb?H?\1001465TU]\1072197\138893\176021:R\DC1p[\ESC\8266F\ETX\59296\&297\ENQ\94037\&4*25\EOT\fEc\DC1YWR*\SUBp\SYN7\ACK-\122897MHh\1103104z\1093592p<"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T11:05:42.209Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\31706]/]|\1048792\162426Y^\1031207x\148607\USN\48441\1099262\141289\DC4R\nE\STX\175721+3+\144449Ycd0b\1026064\&7dp_~U\DC1\DC4\STXK?\10432\78783f_\EOT_\15143Xrn\NUL\SOj'\STXr\1040998VOcq4\a\1088384\157370\1051504\135130s\ESC\ve\1097283\49624\v\1050443\1101780\1012628WP\FS\98965h7zO\52406\v\194642\rK\1051319\&1!\SYNq5\1099648\1061214\161507\ETBt<\"KY=\nUL%;7\21391K@\42041d\123150\985726\\U\1086756W\1093392O\a\NUL#\28289pI^B\\~\ETB\DC1|\SO"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:40:41.004Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "N\\\38858\SI\NAKV\\$\22604SLq\RS\113812\&1| \46524H*?C4\147955J\120221Q-\98363\ACK\1025002J\b\146184\164205&y\1010122V:f\nA\1030292\&3\996083|\DC1\SIkm\184599\1105885,\29991\NAKcr\SOz*\SO\993779\ACK"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            }
        ],
      clHasMore = True
    }

testObject_UserConnectionList_user_17 :: UserConnectionList
testObject_UserConnectionList_user_17 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T13:02:33.244Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "MQ\ACK\ACK!\99562\1070114\72280\vwRu\ETX\32410r2\tuk\110709f\1035392\ESC\1047893\EOT7<t/]\16664Z\"Ye\1026030\1102115\"S.\1108304\STXY\1013005=nK.q\1046194ZsF\1108142/\58070Wo\t\b\DC3\DC3C\1102996\&1\4938\&8\SO3\ENQRHN\v`N/\36794Y.\ETX[o\SOHUVbQMH;\1014848 pU\DC1\EOT\31597\&2\152576\EOT\1048577h#\20370$\ESC\DC3\EMih_\38457z\SIn\173843\&8m\SYN\160021p\161810\r\STX!f\1026495\1073143n(\r"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:56:20.205Z")),
              ucMessage = Just (Message {messageText = " {\30527r9C\1041864$\DLE\187048<\t\ESChR\DC1+\137777?"}),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T01:20:04.372Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\v\fM\1098609\"R\997647T>w:\"#sa5{Q\SI_\2529\171567Y|\9379\t\1061994^\RSR)q3V\ENQ\1087889rw\a\97862\1029017BU\152370\DEL\1015271aLo!\1057961\1017526(\FS\17402n\1087588\25665\1047452\44817?\RS\1096005{j\137733f\1082427\1076891\120967*\190138\FS^gfU\ESC:r"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T09:59:13.470Z")),
              ucMessage = Just (Message {messageText = "\185149d\ESC\1020675Q\SUBn\a\96659wz\EM8Nb"}),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:52:36.952Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "0<jv\1051724\1069281\RS!\ETBx\997245\169697!HF\1111812\1029781\RS\10856HU\59823m\1061583\1150:e\GS\1014304h\37349V0\v\1050963;\1105645\DEL)\1082236(\1002119!Jy76n\44717H\137362\1102272?aMjJ\1030614*\SOH\29102h</\EOTl/\1101888\ETBj>i\SI@vt}]S'8B\41395*\ETB>\rK\a5$\47470]\aN|\DC16\63431g\EM\ENQ\1028896\1020089>@\t<-C\1026592\DC1::\1031263I\2409e\US<\RSu\1077304tam\fy*j\DC3I\100030"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:16:19.530Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "?\1031288kn\1060642)\992453FoZ<]\ENQ\NUL\STX\61662Cn\b[eF\9989Ar;\r\bO\991209\&8\1095832\DC1\STX).%\151001r,\fpt\DC2x\138592j\EMc\DC1\v\1000771)s\ACK\148575.a4S\f\1052702\1668\172290\fh&)MGv\126215\STX9a\DC1\57355h{QS4A`\1064463oBvmbrP\"/c\1102070\1027344}2\989471B\t\1017717FzX\f\143398\1031797,\997091y,G\149129s\fl\\\EM&\FS\n\FS\DC4\DC1:\48701\"\1057293\2839\DC4\DLE\ETB\1024536A**\DC3XSy\DC1f\US#\4142\vF\1096807[\1055891\44136\SUB\996589"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T16:26:01.827Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\b~|\1011779\GSZ-aE\CANc\USY>[4h{\54340\EMI\997834\&2+;\t\168841\993691\&3\134096\n\SI\ACK\136028\&6\78243O8\\a\n\1037507\998948\1094294\52846=\92320\&7m\ESCZt\989195\71912uj|\CANv\74636/eg\1010923\22878*\64866@\32648\n\DC1v\177781\US"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T21:57:41.050Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "=9\340(D tF>Bm\1040265F\v\178968m*d\NAK\DC4\bn\SOH\GSU6\DELm\20107\1061312&\SOHQ\179789\1064914\NULu9\EOT!\986008i{\1039167b\152033\FS)NV\20636g\SUB\EM\96218\DLE\4947(X\DEL\1111695\32398l\18870\40058\t6\NAK\993509"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T02:41:56.943Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "E\ETXwrEfC\1044288\1003000\1039947\DC2\983347--\988172!\1003170\162525/\1109545\ENQ\SO\1105578\1030840{!\DEL}`\187156c$y\FSW[\v\140779\1091720\1069136\SUB1xW\1048055'$2\"\SYN\ACK\1107887%\1111512_[\1068734uAhof\94670)\SYN?\1015696\48273\EM\bP\1113006\163053\1006962\n@\1102736\992129m\n\156819\&4\45878\139921\1071692o#\ACK\180723\&1\21510\1002846\21916|\136509\ETX8\988645\ETX\DC3\RS\ENQt*\96440\ENQG\1077130\146290q\CAN&\DC4\165642NZ4v\GSEu}\61932^~\1102663H\STX\SYN\f0#f@\SIX\EMd}\39215\ACKf\DC3Fy\NAKMC\EOT\SO:/Y\NUL\\\1061497*\DC1*\110812\60660>G#O\55169yGl8\159144\NUL\1036793\50784Y\64593B\RSb\SUB<fn\rvj\1059591\996918\1084009\USI\"\62047\&1\1100707,HW\SO\EM\121184Y\1051379\f:\CAN\139734c\\0C.\SIi!\189026\1048899\70178\32711\63694*/\ACK\179918Y&<k7,u\SUB\1035527ny[\1005094i"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T04:01:44.990Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_18 :: UserConnectionList
testObject_UserConnectionList_user_18 = UserConnectionList {clConnections = [], clHasMore = True}

testObject_UserConnectionList_user_19 :: UserConnectionList
testObject_UserConnectionList_user_19 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T05:02:53.576Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\1044181\988575}\f_\98225rB_ib\1051+\41229\ESCWq\38772+dS;\FS\31404.\1046327\GS+\1025249\173434\bb\1036521i\RS&"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Pending,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:27:07.932Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "4%ofb?$M#8XH\1046186\SO|9\1083417J\DC3\1027748\60892\1054775\39048c\DEL\ENQ\47461w\37885\30097M\50366wR.\DC3\DC4~(p\1003679\v`\1112851\SO!\138010\GS\78787O/\182247B\GS\ACK<|v?c\1072588\NAK[D?\v\83526t?\t~q_?\NAK\DC2zU[H\1074378>\SI\54777D\1013841\NAK\SI\DEL72#\1107776\r3^**\ENQ\EM\a\163719,7\21067\r>\\\NAKQ+\1037102\r\1008612\50607r\ETX\141437^\16473\175822\SO\143812\ENQ]b\1097819CK\a\SI\1041437\998215[4k\63276\ESCpibKR\1094830\1098267IO\GS[\ETXds\1088893\CANN+@\\E5\1042229\ENQ$\STX\1105616#Z\STX\994642)\1049100E\1112695\1104256\ACK\SOWN\SOOT\13370\1028638G!<\8090\176360$"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:45:48.151Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "C\1102756\51705\&1\DC3(Iy,P\46788\1102840\161791\CAN\1070487^,+\1017869O\USzxjLy67\29859p\1101628\DLEv\92400\SYNL\146180'\EOTL\159515b\DC4&S\51275\1057035\1010799\159404K<oOk\1040965ad&\f\167621&?\182324\&7\RS<\983914.\1025195\11806\1034115R,\1014945\147030\1017226\EOTP?\NULHY[\1018588\1042150\1035037\1045897x\149249V,\1021385\1076560\180136R\SI0\1083705\SIg\1010438\139970:\SYN\v\DC3y,\DC1\1087717@\1047260K4,B\SI,\164600\f\19055E\1043231\DLE\GSk\CAN\"\59315\DEL)/\4973r\989734/J\ENQ\1060460O\155393\100856\&9UI\ENQL\1001671\143897\USq\1006356\DC4V\174321a\172727\SYN2\59686F{C,Rh\1023800"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T08:33:46.140Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "KI\SIF]\1014286>H\EOTg~\NAK\33243\187339M\ESCD\NAK=B`\DC40\1055429J\1093791H5\132504\992678)\28722R\63968\1065079 P\1021207\&4\rk\172221C7b\EM\14283x\27071M>\EMa\165696@\1097499\140298@\165046ZD\1093916\58047YM\161665(\159468\1095283\30760}2\DC1\1094031\132430r\10859\1021443\EOT\1069428zn\182131 4\USb:\SUB\DC1\RS\1022012\156821\ACK\49017<m\t\984207\&8L\171380\ETB\SOH\FS\1104934\FShx\13200\SUB\n2\95811\996931\DC1|@\SOHa9$K\SOH2\DELY\t8}G\1086187\&5!\1043862P\67631\1007005S\ETBCXc\53753\1048303\&1a\24780'\9314WI\ESCQ$\DC2\133846\984867\1088954e\1050603\1096841\1108375\DC1\1004408@)\9070C\STX\EM\SYN(aH[\5645\1079915x\ENQ~_\997085\&8+)\1090228K\t:\STX\DC3\1040469P\CAN\ENQ\21664l|y\EM\v\137766M\n)JK\DC1\DC4Z((\NAK\1098777\&7NYZa\FSxS3\DC14W\v*~\1070514gf {!\\YJ\"x"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Accepted,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:42:58.803Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\DC3\1041913\18893\DELO6wKea\141468\999928L\SUB \"|^\29801uC\1090290/M-c{~\179948\187813Xt\1079504\1067829\&5\STX\1018705\1013538)aGKe^X Fs\DLE\nP\16980Pi\\>\t\1002220\NAK\GSQ&\ENQ\ETX\39427\RS\138931\&2(9\t~\1040831\137528^\RSc\NULi\159996|_\DLEAG\1059935L!zk@\SI"
                      }
                  ),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucStatus = Cancelled,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T07:44:32.326Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "4\DC4\28777<[~\FS>]v-s\GS>Mp\190630@n\\8uUP\ACKMb-%fX0%\1065303o1\5352\b\152405i)\br\SYN[0;\DLE\DC1Cv\NAKL\NAK\1014877h/a5\36283N`d\1003392U{=$I\174468\&4\1016687.\DC3\EM\1025963N\1092454g\153731\68099\1016147"
                      }
                  ),
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:02:00.432Z")),
              ucMessage =
                Just
                  ( Message
                      { messageText =
                          "\STX,C\ESCS\CANK'\ETB\CANg(9<\134453@\1060280l\US\EOT-A3\987924\29144Lh#\61925\1072862\4536\DC1\30769\137641LO\t\131401\1043868ty1\1027181\168252\42059\DC4\ETXn1\\\ESC\NULJ\12949v\DC2M\GS(\1051797r7\165949\169380\33795\186414\986306eu*n\1043756EMMB9#e\54432\ENQZwr\ETB\f\987156\SOtnE[I\1007950\8289@\989839\20674)_37S\CANo '\1082259,J\a\1019753\1038142|o\f1?A,f\aeI\STX\153937f\\\aS\DLEbZKl\19664U\181337\&1T\EOT\STX[=\1847\1027529\r)BO+=V3\1074095\1081936;*9\1044408kSJBHw\DELH\\qzp\v\172796M\FS\SI\FS\52184\US\173186mh?.\f%f\1083942V\EOT6gQ\SO\1090865g*7\140748*\74482\GS:\""
                      }
                  ),
              ucConvId = Nothing
            }
        ],
      clHasMore = False
    }

testObject_UserConnectionList_user_20 :: UserConnectionList
testObject_UserConnectionList_user_20 =
  UserConnectionList
    { clConnections =
        [ UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucStatus = Ignored,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:43:50.202Z")),
              ucMessage = Just (Message {messageText = "s\t\1062978g'"}),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              ucStatus = Blocked,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:11:43.015Z")),
              ucMessage = Nothing,
              ucConvId = Nothing
            },
          UserConnection
            { ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              ucStatus = Sent,
              ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T15:40:02.929Z")),
              ucMessage = Just (Message {messageText = "X'\EMb)\\\ESC\ESC9\1057698`\1111921n#\1086087Ki"}),
              ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))
            }
        ],
      clHasMore = False
    }
