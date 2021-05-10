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

module Test.Wire.API.Golden.Generated.QueuedNotification_user where

import Data.Aeson
  ( Value (Array, Bool, Null, Number, Object, String),
  )
import Data.Id (Id (Id))
import qualified Data.List.NonEmpty as NonEmpty (fromList)
import Data.List1 (List1 (List1))
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Bool (False, True), fromJust)
import Wire.API.Notification
  ( QueuedNotification,
    queuedNotification,
  )

testObject_QueuedNotification_user_1 :: QueuedNotification
testObject_QueuedNotification_user_1 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005f-0000-007b-0000-001a0000000a")))) ((List1 (NonEmpty.fromList [fromList [], fromList [("\179372\&3", Array [])], fromList [], fromList [("\\", String "\1054252\1015250")], fromList [("\1065105\DC4", Null), ("\1067229U", String "#\53284")]]))))

testObject_QueuedNotification_user_2 :: QueuedNotification
testObject_QueuedNotification_user_2 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005d-0000-0048-0000-00720000007c")))) ((List1 (NonEmpty.fromList [fromList [("\1058751\181510", Number (1500.0)), ("\a\156473", Number (9.0e13)), ("y", Number (3000000.0))], fromList [], fromList [("O>\16513\1022654\FS", Array [String "", Bool True, Bool True, Null, Bool False, Bool False, Bool True, Null]), ("\1044716P\DC1~", Array [Number (200.0), Bool False]), ("\1090029", Object (fromList [("F\160193|", Bool False)])), ("/e\25928\SOH[@", Null), (")=\US\n", Object (fromList [])), ("\DLE\1029229\997140", Array [Null, Bool True, Bool True]), ("B.d\ETX`", String "n\DC3")]]))))

testObject_QueuedNotification_user_3 :: QueuedNotification
testObject_QueuedNotification_user_3 = (queuedNotification ((Id (fromJust (UUID.fromString "00000009-0000-007a-0000-000e00000026")))) ((List1 (NonEmpty.fromList [fromList [("\95007P\GS7\170882l[\t", Array [Number (7000000.0), Null]), ("", Bool True), ("y_\"K\1060452\160314\t\SYN\bg\EOT", Null), ("T{k\1107462\189395\5487\9460\132016D", Number (-9.0)), ("5Katxb<3\989929\1036697f\"f\1039375", Number (-500.0)), ("u#\54486\1055378N~W\CAN7k\1015057g\1072424", Array [String "{", Null, Number (-1.0), Bool True, String "\1031270", String "u", String "\t", Null, Null, Number (0.0), Bool False, Number (-1.0), Bool False]), ("0=\1009587;\SOHD", Array [String "\34912!"]), ("\bD9", Object (fromList [])), ("\52750q\EOT?F~`xL\DC3-w\t\187110", String "\vme\162923\SOH\SUB\DELL\181039\&5#\SYN\SI\1070836?"), ("0i\97932i}\NAK\DC1\ETB\ETB-\1069291\31712\&1\1015121", Array [Number (7.0e-4), String "mUX|9uC"])], fromList [("", Object (fromList []))], fromList [], fromList [], fromList [], fromList [("g", Null)], fromList [], fromList [("", Array [Number (0.0), String ""])], fromList [], fromList [], fromList []]))))

testObject_QueuedNotification_user_4 :: QueuedNotification
testObject_QueuedNotification_user_4 = (queuedNotification ((Id (fromJust (UUID.fromString "00000069-0000-0074-0000-003a0000001b")))) ((List1 (NonEmpty.fromList [fromList [("=3n?\ENQnD\FS\SO5", Array [Null]), ("*\ACK\169030\ace\NUL", Object (fromList [])), ("\SOH\aqy3_Q\153721\986977\35098\f\CAN5x", Null), ("B\58373x\1023997\SOH", String "4\US"), ("bRe\172258Y$#Aq3", Null), ("\24755\1004897\NAK,(\NUL \DELn", Null)], fromList [("", Array [Null, Number (0.0), Number (0.0), Bool True, String ""])], fromList [], fromList [("\DC2X", Object (fromList [])), ("\1022854s\13205", Number (2.0))], fromList [("", Array [Number (-10.0), Bool True]), ("\SI", Bool True), ("X", Array [])]]))))

testObject_QueuedNotification_user_5 :: QueuedNotification
testObject_QueuedNotification_user_5 = (queuedNotification ((Id (fromJust (UUID.fromString "0000000d-0000-0019-0000-002600000076")))) ((List1 (NonEmpty.fromList [fromList [("\45236\ENQ\124956\1096938~\NAK\999591\1062012H\1092159\1019668]AT\1035524", Object (fromList [("t\fg\ACK\155286\998134\1048593\97184U\SO@L\n\29410", Number (4.0e7))])), ("\t\1093623\t\165331", Array [Bool True, String "", Number (-0.1), Null, String "", Number (0.0), Null, String "\152288"]), ("", Object (fromList [("", String "")])), ("\EOTX\DLEd\1109960\DC42\CAN7\1034463\&5v\NAK3", String "\1034700;\DC4wZ\998770%"), ("\DC3hj7\ahO/NoE\149371y\19456", Array []), ("IFm A\138428\NAK\DC1h\40599\CAN", Array [Bool False, Bool False, Number (-500.0)]), ("\ENQ\SI\n\ETB#P\1015596\166378=F", Array []), ("\1107408=\123179\GS", Array [Bool False, Number (1.0), Number (1.0), Bool True, Number (-2.0e-2), Number (0.0)]), ("\EMgMH\155071\DC1b\SO\1099952FH\DC1GX", Object (fromList [])), ("\95819o\n4\160628\1014499", Array [Number (0.0), Bool False, String "\1029519", Null, Number (-2000.0)]), ("G\CAN\1033498Jv\RSm7\"\24281\&3\GSj<\64723", Array [Bool False]), ("B|R\1081521K8^\DC3\1073100<}", Array [Number (7.0e-10)]), ("\EM\f\EOTz\ETXF,}\1075552BD\178719m", Object (fromList [("2\"", Number (0.0)), ("#", Null), ("\145471\ETX", String ""), ("D2", Number (2.0e-3))]))], fromList [("\1083930K/", Array [String "$\RS", String "\t$", String "\SI\1099452", Bool True, String "\999505", String "j\DC1"]), ("M+b<\27207\1015173\28289}\a", Null), ("\1047334v", Object (fromList [("L\ETXE\DC3\1036794^\1041116", Number (0.0)), ("#  ffX", Number (-20000.0))])), ("N", Object (fromList [("\1077730\&6;", String " "), ("^\DC2\1038128j\1020596", Bool False), ("h", String "[\29591q\SI")])), ("\GS-", Array [String "t\1052943", Bool True, Number (-0.2), Null])]]))))

testObject_QueuedNotification_user_6 :: QueuedNotification
testObject_QueuedNotification_user_6 = (queuedNotification ((Id (fromJust (UUID.fromString "00000016-0000-0025-0000-00720000005b")))) ((List1 (NonEmpty.fromList [fromList [("\183735\CANZdP_\53614\SYN3/1", Array [Number (0.0), Null, Number (0.0), String "\NAK", Bool True, Number (-20.0)]), ("D\1072658\1007557\DLEOk\1107143", Array [Null, Number (2.0), Number (0.0), Number (-100.0), Number (2.0e-2), Bool False]), ("/\1105704f", Number (-6.0e-7)), ("\1007185\&0W3\25378\&8ZZZ9k", Array [Null, String " ", Null, Number (20.0), Number (-1.0e-2), Bool False, Bool True]), ("5\\\22774\1110197<\51695Y", Object (fromList [("", Bool False)])), ("YUqBj\143696T\ESC\164377V\1016566\1106151w", Null), ("F192", Array []), ("S\1032695", Object (fromList [("&<", Number (1.0e-6)), ("%r`\1010329\SO\ETX\ETB", Number (-7.0e-6))])), ("\188827~SJ[u\155261\SOH2C\15525", String "$.\92209DO,\NUL%\149436\&6"), ("\1109692", Array [Null, Bool False]), ("K#C", Array [Bool True, Null, Null, Number (-3.0), String "\DEL\DC4"]), ("wGIB1z\1052731C\995558\1018090\166423\1099552", String "\51474\1111609\133087\&1vxb\1004342\60873"), ("\31664\ENQ\FS`\1019100\1075958)e,c]r", String "J")], fromList [("\100003`\1060268m%\177299", Array [])], fromList [("\US\169781\1021396q", Object (fromList [("; ", Null)]))]]))))

testObject_QueuedNotification_user_7 :: QueuedNotification
testObject_QueuedNotification_user_7 = (queuedNotification ((Id (fromJust (UUID.fromString "00000015-0000-0050-0000-00370000006c")))) ((List1 (NonEmpty.fromList [fromList [("", Object (fromList [("m", Null), ("\DLE", Null), ("", Number (-10.0)), ("+", Number (-0.1)), ("[", Bool False), ("A", Number (-1.0)), ("/", Bool True), ("6", Number (0.0))])), ("l\990423')\1062179v`\ETB\164677f\EOT", Object (fromList [("G", Number (-400000.0)), ("\STX$=", Null), ("$d!\1068748", Bool True)])), ("CI\5695i}\CAN\137004f\DC2", Object (fromList [("h7{\985592 ", String ";p"), ("", Bool True), ("c\1053951\1028477\US", Number (0.0))])), ("r\61432\DLEL\1005146\&7_C\ETX\1003390/N", Bool False)], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList [], fromList []]))))

testObject_QueuedNotification_user_8 :: QueuedNotification
testObject_QueuedNotification_user_8 = (queuedNotification ((Id (fromJust (UUID.fromString "00000042-0000-004d-0000-001800000054")))) ((List1 (NonEmpty.fromList [fromList [], fromList [("", String "")], fromList [("", Array [Number (0.0), Bool False, String "", Number (0.0), Number (0.0), Bool False, String "", Number (0.0)])], fromList [("\GS", Array [])], fromList [("\1040105M", String ""), ("\144860\984284", String "\10922C")], fromList [("", Array [])], fromList []]))))

testObject_QueuedNotification_user_9 :: QueuedNotification
testObject_QueuedNotification_user_9 = (queuedNotification ((Id (fromJust (UUID.fromString "0000004e-0000-0040-0000-005c0000003b")))) ((List1 (NonEmpty.fromList [fromList [("", Array [Null, Bool True, Null, Number (0.0), Number (-10.0), Bool True, Bool False, Null, Bool False, Number (-10.0)]), ("q=-\3975We\34648\NAKl\996442m\133105", Array [Null, Number (-2.0e-2), String "\134259", Null]), ("e\ESCc\\\21868\&0\7566\EM\1071552\148802", Bool False)], fromList [(";", Number (1.0))], fromList [], fromList [], fromList [("J", Array [String ""])], fromList [], fromList [("J", Array [String ""])], fromList [("", Object (fromList [("", Bool False)]))], fromList [], fromList [("", Object (fromList [("", Bool False)]))], fromList [], fromList [("", Array [])], fromList [], fromList [], fromList [("", Bool False)]]))))

testObject_QueuedNotification_user_10 :: QueuedNotification
testObject_QueuedNotification_user_10 = (queuedNotification ((Id (fromJust (UUID.fromString "00000017-0000-0079-0000-007f0000000c")))) ((List1 (NonEmpty.fromList [fromList [("|\1027097", String "\SO'\110981\1091808\SYN>Y\DC4\n"), ("\US2g\GS\r", Object (fromList [("\DC3\DC3W", String "\f\28427"), ("h\983608\&5", String "{"), ("\144054<\1109056\ETB", String "\b")])), ("S\989804\1030868\NAK\v\a\STXFqb1E\178404", Number (-1.4e12))], fromList [("\18151\178612", Bool False), ("\SI\984754\1003875(", Array []), ("/", Object (fromList [("U\1029706>", Number (500000.0))])), ("/\DC3\SYN", Object (fromList [("w", Null), ("\ETB", Number (10.0)), ("", Number (0.0))]))], fromList [("\DC4\b{", Object (fromList [])), ("\1043377", Array [Bool True, Bool True, Null, Null, Null, Bool False, Null, Bool False, Null, Bool True]), ("|\RS\173007", Array []), ("Y\1746\1773\t\1024915", Array [Bool True, Null]), ("/1|~b", Array [])], fromList []]))))

testObject_QueuedNotification_user_11 :: QueuedNotification
testObject_QueuedNotification_user_11 = (queuedNotification ((Id (fromJust (UUID.fromString "00000006-0000-000a-0000-007500000067")))) ((List1 (NonEmpty.fromList [fromList [("\176843\15842\EM\SYN\12112\1024918\v<M", Object (fromList [("<", Number (0.0)), ("f{", Bool False), ("", Number (2.0e-2)), ("p", Bool False), ("\STX", Number (0.0)), ("Y", Bool True)]))], fromList [("", Object (fromList []))], fromList [], fromList [("\1044730", Number (0.0))], fromList [], fromList [("", Number (1.0))], fromList [], fromList [], fromList [("M", Object (fromList []))], fromList [("", Number (0.0))], fromList [("\189937", Object (fromList []))], fromList [("\US", Object (fromList []))], fromList [("", Object (fromList [("", Bool True)]))]]))))

testObject_QueuedNotification_user_12 :: QueuedNotification
testObject_QueuedNotification_user_12 = (queuedNotification ((Id (fromJust (UUID.fromString "00000034-0000-0039-0000-005200000071")))) ((List1 (NonEmpty.fromList [fromList [("2kv\fA\DLEI<", Number (-1.2e13))], fromList [(".", Bool True)], fromList [("", Null)], fromList [("s", Array [])], fromList [], fromList [], fromList [], fromList [], fromList []]))))

testObject_QueuedNotification_user_13 :: QueuedNotification
testObject_QueuedNotification_user_13 = (queuedNotification ((Id (fromJust (UUID.fromString "00000043-0000-0015-0000-006d00000070")))) ((List1 (NonEmpty.fromList [fromList [("7Ss\RSj\DC3", String "\1045345E\SYN\49550^)0<9\156141sG}\7560k"), ("", Bool False), ("\ETX\RS>\173266e\41899V\SOH\154850\STX\RS\1085751D", Null), ("U\CAN\1090465", Array [Null, Null, Number (-2.0e-2), Number (3.0), Number (-2000.0)])], fromList [("\1037326y", Bool True), ("4u\1037419\STX\a\1030932d2\1025082:\129058DN0", String "\ETBbUR()X\DC1K\ENQK\38510O\175835"), ("d%\66691\DLEv\167015~N\68494p-\NAK", Bool True), ("\DC4<s\997051\fv\ETBD\1096499", Object (fromList [("\1015845", Null), ("N\1087357\9312\ETX\2632\1066393", Bool False)])), ("\58695h}\160243", Bool True), ("\DEL/\DC4\30758", Number (1.5e-9)), ("\USD.n\SIz6f\EOT\SUBC7\ETXjF", Number (80000.0)), ("oW\164415", Null)]]))))

testObject_QueuedNotification_user_14 :: QueuedNotification
testObject_QueuedNotification_user_14 = (queuedNotification ((Id (fromJust (UUID.fromString "00000058-0000-0032-0000-00730000005c")))) ((List1 (NonEmpty.fromList [fromList [("L", Array [String "\1104725\GS", String "", Number (-0.1), String "\1005231", Bool True, Number (0.0)]), ("\ACKgEE\1023869j4", Number (4.0e-2))], fromList [], fromList [("T\38347", Array [String "5l+eq"])], fromList []]))))

testObject_QueuedNotification_user_15 :: QueuedNotification
testObject_QueuedNotification_user_15 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005d-0000-0059-0000-00340000007b")))) ((List1 (NonEmpty.fromList [fromList [("@", String "{V"), ("M\1007322", String "R7;\83094'"), ("}x\132968", Object (fromList [("", Null), ("\35448", String ""), ("T", Number (-0.2)), ("\17314", Bool False), ("u", Bool True), ("S", Bool False)])), ("$\1111@\SO\ETX?\EM8\DLE\1086312\66620\1029880V", Null), ("CpD\DLEVl3U\DEL\NAK\DC4W\97261", Object (fromList [("", Null), ("\18890=\ESC\tLp", String "\v")])), ("@-m k\131917IB", Object (fromList [])), ("w\DC3SW\DLE`\1018304\1039063\&5\1014900\1112845T\DC3\1042638", Object (fromList [("\v\141802\1003230\183861\NUL", Bool False), ("S8\"\SOH\t\187287w", String "\DC3\167256")])), ("kz\t\SUBB+Sr9n%\SIc\63483", String ">0\1100072\SOv\SUBH(\SUB\1097319/\155427")]]))))

testObject_QueuedNotification_user_16 :: QueuedNotification
testObject_QueuedNotification_user_16 = (queuedNotification ((Id (fromJust (UUID.fromString "00000009-0000-005f-0000-002d0000003a")))) ((List1 (NonEmpty.fromList [fromList [("\1085656Q\53349\998514l\40723E\DC2=I\ENQ<\1077146\nL", Object (fromList [("\1100391", Null), ("", Null), ("\DC2\1014614", Number (0.0)), ("\NAK", Null), ("\ETBY", String "|")])), ("\1068734I>\1089474\14594[\r\40315\141844", Number (9.0e10)), ("\FS1?12/~?D\558\1017851R", Number (8.0e-4)), ("st0\32150I]\52454\ACK\DLE\120566", Number (1.2e-2)), ("%\EOT5\1045277\ETX\95408n\SOH}\f2", Object (fromList [("\177635\GSJ", Null), ("|\r\1052037", Number (3000.0)), ("\EM\SYN\DLE", Null), (">\r", String "Q6\ETX")]))]]))))

testObject_QueuedNotification_user_17 :: QueuedNotification
testObject_QueuedNotification_user_17 = (queuedNotification ((Id (fromJust (UUID.fromString "0000003e-0000-0010-0000-00100000005d")))) ((List1 (NonEmpty.fromList [fromList [("\987647a\179847\t\buB\ACK0", Array [Null, Null, Number (400000.0)]), ("", Array [String "", String "z;\1085758", String "", Bool True, String ".H"]), ("\1034094\&6)", Object (fromList [("", Number (0.0))])), ("\152772vk]c+y2\DC1\20087:Isj", Number (70000.0)), ("O\111080\1009125 T\b\DLEx\DC1\1064318+", Bool False), ("\148637A\155972\999326a\185632j", Null), ("r\DC2Ny*~\46010|A\100540RC^Ng", Object (fromList [("{\134218W\1074027\SYN\a\192a\53882#i+`7h", String ">\136285\NAKo\191146\&8;\1031859\SYN\1012225\nf\NAK0=")]))], fromList [], fromList [], fromList [(">", Object (fromList [("", String "")]))], fromList [("YW\994503", Number (30.0)), ("", String "w\SI"), ("\a\44144\&8", Number (0.3))]]))))

testObject_QueuedNotification_user_18 :: QueuedNotification
testObject_QueuedNotification_user_18 = (queuedNotification ((Id (fromJust (UUID.fromString "00000019-0000-0012-0000-002e0000006e")))) ((List1 (NonEmpty.fromList [fromList [("`\41689\4612\1020636w\SO\GSS(E", Array []), ("[\GS\1047962A\SO<\1095693\151254I#\f\14088S\1049776\STX", Array [String "\1090348", Number (0.0), Null, Bool False, Null, Bool False, Number (0.0), Null]), (">ij\SOW!rDk Sl", Null), ("\1056572\1071084fMi\CANqx\169671^WK\983236A", Number (1.2e-3))], fromList [("", Object (fromList [("", Null), ("\r\1026636\1019729\1008827", Bool True)])), ("\1111807.\16622nD\184092y\998883\v\DC1C", Array []), ("\US9\65030\EM\ESC\EM\1094785", Object (fromList [])), ("?\163151\1009143\13711\1090298\DC3\DC2s9\993561`Q\DC3", String "\SUB\100159")]]))))

testObject_QueuedNotification_user_19 :: QueuedNotification
testObject_QueuedNotification_user_19 = (queuedNotification ((Id (fromJust (UUID.fromString "00000045-0000-005e-0000-007700000025")))) ((List1 (NonEmpty.fromList [fromList [], fromList [("cK<=Y(\"", Object (fromList [])), ("", Null), ("\1063218\141347\137560\&5r\30585s", Object (fromList [])), ("&", Array []), ("L4\DC2\78778`", Array [])], fromList [("O\165343\&5\127969", Bool False), ("0\SUB4\1017347=C", Array [Null, String "", Bool False, Bool True, Number (0.0), Bool False, Bool False, Null, String "", Bool False]), ("r\13278M\v", Null)]]))))

testObject_QueuedNotification_user_20 :: QueuedNotification
testObject_QueuedNotification_user_20 = (queuedNotification ((Id (fromJust (UUID.fromString "0000005c-0000-002f-0000-002800000002")))) ((List1 (NonEmpty.fromList [fromList [("", Null), ("@+S\vq", Number (1.2e-9))], fromList [], fromList [], fromList [("<", Array [Bool False])], fromList [("\FS", Array [])], fromList [("\996153", String "")], fromList [], fromList [], fromList [("", Object (fromList [("", Null)]))], fromList [], fromList [], fromList [], fromList [("7", Array [])], fromList [("", Array [Null])]]))))
