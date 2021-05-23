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

module Test.Wire.API.Golden.Generated.UserConnection_user where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Connection
  ( Message (Message, messageText),
    Relation_' (Accepted_', Blocked_', Cancelled_', Ignored_', Pending_', Sent_'),
    UserConnection (..),
  )

testObject_UserConnection_user_1 :: UserConnection
testObject_UserConnection_user_1 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000002"))), ucStatus = Pending_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T21:52:21.955Z")), ucMessage = Nothing, ucConvId = Nothing}

testObject_UserConnection_user_2 :: UserConnection
testObject_UserConnection_user_2 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000004"))), ucTo = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000100000000"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T10:43:38.227Z")), ucMessage = Just (Message {messageText = " D\ETB*\1111732\1045722\ACK&*`\bR\12652\177451\FS\143803\25129\EM\ENQ\n-yc&t\t3E\27186:\170285t\DEL\DC11\STXN\EOT\EM$:\"\EM\EM\SUB\162168\46118y\SOHe|\DLE\156824o[\78645<56Tm\146830&3\50454!x<y\"\161399|m88\159184N{\STX9\1038504kK\ESCa\NULr\EM\1007066+h\EOT(L\GS\bdD\ACKu\992141\\\21510\59850\156479K`9;\ESC\bx!\DEL/K,\SOH\26358\DC3\n/\1096058uU\ACK{\992363+qA\1072067z\ACK\SIf|s(\18260~^\171221P\STX*U\68767\15049J~\US\1111717i\9220\EM.\CAN\1001863\1080354Kl|B\ETX\1018680d\ESCg.6n \24343\NAK\111031s=\1065416J`#\1060577\78510{\159244h`\1072205O\NAK\1085355~\7378\139390t2t\78696B2\t\20659\&4"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000004")))}

testObject_UserConnection_user_3 :: UserConnection
testObject_UserConnection_user_3 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), ucStatus = Pending_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T19:25:59.201Z")), ucMessage = Just (Message {messageText = "=\150784N5\r@NT1d\1092153\167388\99461\1111339\127861\995254\NUL\1046197\n\1048626aSH\SOHB\ETBc\US\50502[E\160938\48230\n\178369\r\ENQ\43663\24872H\NUL`\1009984\9485\173731_:\v}\\|\1408\29765\CANm\EOT\STX\120550\b\SOH0\31634p'ap\DEL\vwo9\"z|hVZ\rQBV\ETX@\FSA+\182717C\1011224T\73008\EOTs\SYNP\37375\984849h\999741@<5\1054596Ql\164052\179986\EM\36999\t\SIV-\DC2@\17401\USwi\27376)\ETB\1082133\r\vYt\1077868OZ\1062261\36015\FSp\bPS\DELly3X\ESC\RSU\131841(\99574t\35738\NAK\8694`"}), ucConvId = Nothing}

testObject_UserConnection_user_4 :: UserConnection
testObject_UserConnection_user_4 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000001"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T14:39:50.322Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000000")))}

testObject_UserConnection_user_5 :: UserConnection
testObject_UserConnection_user_5 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000000"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000000"))), ucStatus = Blocked_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T06:10:28.560Z")), ucMessage = Just (Message {messageText = "A\188888 \DC3$\1056679\NUL\RS$7%\SOH;\1027563^O\ACK!\95262\189636\&8\990383X\13407c+\12459=:dct\ACK"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}

testObject_UserConnection_user_6 :: UserConnection
testObject_UserConnection_user_6 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), ucStatus = Sent_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T12:04:01.926Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000001")))}

testObject_UserConnection_user_7 :: UserConnection
testObject_UserConnection_user_7 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000002"))), ucStatus = Pending_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T17:21:11.586Z")), ucMessage = Nothing, ucConvId = Nothing}

testObject_UserConnection_user_8 :: UserConnection
testObject_UserConnection_user_8 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000000000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), ucStatus = Accepted_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-07T21:27:56.433Z")), ucMessage = Just (Message {messageText = "\194824\986678\EM\185672|D8q(cueA\991797g}U\74308\\\1092763\&2\btR\DC3\1017950.t\36063>\GS\SOH\1041549\121406\1091087G\az\185883A\1028764\EOT\1070585\1104678@O<{\20817XE\GS?\1099881f\120354\155685\120480\1097338\EOT\STXM\DEL\55149T\1102748'"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000003")))}

testObject_UserConnection_user_9 :: UserConnection
testObject_UserConnection_user_9 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000002"))), ucStatus = Accepted_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T01:59:33.405Z")), ucMessage = Just (Message {messageText = "\1073946\36516 \1089607< /\1012911f\SYN4\1108720>\1062360\SUB!\NUL\1086550\SI\181852'\996807g\1041377Ak\STXHI\992631\DC23EJ'>n\SI\n\SO\n\ETX\665\1063331n\2329\120541\EM_\DC3d\EOT'C\vYF\8711)\NUL\"[}u%\30648qc\SOH/#)\1006871\1027535I`\GS\119587\&2*%!u:\ESC\1041800\NAK\FSH\136125f9/\1096599\128449B\1001125J<\1056108`[u\1016181\&7\152911N\a>\521\158287i\4184\v\ETB67\EM2r\RS\996654\RS\US\a\NUL8&Ae\SIK\1085789\1101619\1041753\99530\ENQ\ACK>iTu\186918\188290LrY\173368\154276\EOT~=a3Q\SO\&H\1099938\&5lN\NUL\EOTE\\\CAN\64888\b\27421\ACKFuJ1\145195\1038200I)L\1054744\r\DC4\147656;g\1093593\1088508^7\SYN\FS>(\45890\1084891\145257\159097b,9\DC2\177325+&4,\CAN&\a\NAK6A\177995jR0\1052988\&9M\SUB"}), ucConvId = Nothing}

testObject_UserConnection_user_10 :: UserConnection
testObject_UserConnection_user_10 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000001"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000002"))), ucStatus = Ignored_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T23:20:11.508Z")), ucMessage = Just (Message {messageText = "B{rg\b=_&\1024093\SYNn58\18354f`\RSj\1109808\2817L\ESC\GSN\NAK\34635G\DC1\SI\1048723(\NAK15My\21584rN1<\EOT\1039277Ro5\vA\DLEK\120139\EM\151540&\1008102\50056\NUL\ENQyf2p,\1031274\1094062]\USs'\1076202,\ETXG\29564\SUB\149048\1042592\nS\1109138U\127060m\NAK?vX\999879@.\ETBfr\159399h\SOH\177670\1064176Hq\999963&\1089697;?\EM"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000002")))}

testObject_UserConnection_user_11 :: UserConnection
testObject_UserConnection_user_11 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000002"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000002"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T01:03:59.594Z")), ucMessage = Just (Message {messageText = "h\1051816\DC1\185085HH\RS\EOT\1019294o\149471\65937T\DC3WR\EOTf\178380\NAKa\59133wJ#\NULbRBN\ETX\DEL\60125r:\988826L\FSmXu\46977R\NAKE\DLE\SOH\DC3\GS\ETX:A\1030522\1032822\185220#Ki\12091\119635\SO\1110240#s!y\DC3B"}), ucConvId = Nothing}

testObject_UserConnection_user_12 :: UserConnection
testObject_UserConnection_user_12 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000000"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000000"))), ucStatus = Sent_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T04:35:47.647Z")), ucMessage = Just (Message {messageText = "D\RSj\45864a\1092668F52t8 ;Jt\1058663\DC4\SO\SYNse\SIX\1002089\DELQ3\1050069m\STXw\SYN/e\97642sG\1074243\74602`\35540\ACK\1016307L;\1064910\1061941@;\35482\12629~\78343M\\\USI\NUL\1026796{;=d\SUB\NAKJw\DC1?\137976\CAN0Kl#D\175531W\\^\1049138\153027\SOHp\997672\ESC!n\SO<}\8367`>(\1000749\f]\1100371\nl^\US\3659\DEL\ACK\SOH\142182\&8M\SYN\b\FS \ETB\1003073-\"K:3\rE4`\120177\ETB\EM\984863\&0!^a\1094483\US8\1092660n\1077279\RS\1013154\165388Q\990365\EOTq\188521As\SI\1103654\DC2\DELqJ$r]\1104340 \vk%*\t \1055351s\ACK\\y)+$ \1006569$?1\US\DELQ\1087252\r\1072918M\149834\160560A`\96015[)pJ/\1012118\bcI\13595)*\191107\157231~s\DLE;\1044875$:w\EM\1110848\ETX\167891\1016014\DEL\ACKz\NUL4\122882A72obm^X\FS\1084097c"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000003")))}

testObject_UserConnection_user_13 :: UserConnection
testObject_UserConnection_user_13 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000001"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-09T14:20:36.319Z")), ucMessage = Just (Message {messageText = "{\168289\EOTa\ETB\35015\1092855%"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004")))}

testObject_UserConnection_user_14 :: UserConnection
testObject_UserConnection_user_14 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002"))), ucTo = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000002"))), ucStatus = Ignored_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-06T15:50:31.413Z")), ucMessage = Just (Message {messageText = "YI97f\177582\1090316P5ra;\DC2d\125130\1031258\"cp\blU\1090133\1095282+\SUBQ,tWuX\1101900\176614\1053952u\ETXh\b`p"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000003")))}

testObject_UserConnection_user_15 :: UserConnection
testObject_UserConnection_user_15 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000003"))), ucStatus = Ignored_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T01:23:07.786Z")), ucMessage = Just (Message {messageText = "ABA`\1105648\DC4\a\161279DUz1\ESCsztI\68488\137727\DEL%Y9s0a\FS\SYN\1033572#\DC3\1035580\1090107?6\1013669G?4\1070751g:n\DLE\NUL\992906\162531-\RS)0}w\DLE\1063599\fMQ\ETB\r\1063073\67668\NAKB\NUL\161940f>!P\RSt\SI\74757\136503\SOH?v$j2\SO\US\144761@\993024\bz\20343\60956\SOD\STX\DC4_?d\97745\46338>\144784\&2+\GSH\USiN2\1071976\74954\1081176\&7\DLE;\1060692\1090776nJlO%\RSa-\151524\f+\61109f\158867\ENQ\SO\"\54924z\f\176230w\186430\1054863S#mH|\1068008C\1080668S!\STX\no\RS\1034986K\t\CAN\DC1h"}), ucConvId = Nothing}

testObject_UserConnection_user_16 :: UserConnection
testObject_UserConnection_user_16 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), ucTo = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000000"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-13T18:59:24.504Z")), ucMessage = Just (Message {messageText = "gZ9\ENQs-\ACKPu\DC3\ETByt\nD~\97072\SI\ar\1053572\172903q\166739^n\ESC*!\1081907C\1112059-)tm>7\FSc&\CAN)\186733\ETX\177450M\997742\&3\1007160xS\SUBRks\CAN\EMa\v\151355g\57713\49603\STX:A\1017614S38f*\1045521\1052407\63254\1094089\1064724_0w\"of;\ETX\65884\152974\1010820Yw\142146\US\187431\USYq\f\GS58\tO"}), ucConvId = Nothing}

testObject_UserConnection_user_17 :: UserConnection
testObject_UserConnection_user_17 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000003"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), ucStatus = Pending_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-08T23:56:52.951Z")), ucMessage = Nothing, ucConvId = Just (Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000300000000")))}

testObject_UserConnection_user_18 :: UserConnection
testObject_UserConnection_user_18 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000001"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000003"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-11T22:44:27.192Z")), ucMessage = Just (Message {messageText = "B\DC3:\"|\97044m?\DC2GA\ENQ/ n\SYN\1069097dz5\132085$'[$un\b{\vaK\b\GS\170798[\ENQ\996060\165773\989461\SUB\SYN\STX2_R\149935\96591\1092985P\1029686,\EM\1093608\FS!\995920'G\182421A \139308;p\121400\t\985347:\DC3j\176831\&4+\DC2+\ETX\194895wuBb\1087404*_\4944_[y=\ACK\1042791\64515\1087143\18031ly\143288ce=kaOy\f\164181>\US\66819|\52659!n6H\157652\24829F{M\154394i\\\44652;9q>?[\11262\DLE\n\r\SI\r|\"\1068128MxwjZ;^\1558A\1039463G;S\DC2uB\a(}_Z\18385\110843V'\STX#Aa\SUBM\1080726\GS\9719\&1_"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000000")))}

testObject_UserConnection_user_19 :: UserConnection
testObject_UserConnection_user_19 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000000"))), ucTo = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000002"))), ucStatus = Accepted_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-04T10:25:12.601Z")), ucMessage = Just (Message {messageText = "g\182853\1108760\1056585\993432\151849\134553\1077980h\DLE\998328\SUB\166789\1064260I\150170\1084112O\1082101\26629L\ETBZfM*\ESC\n\1040416Q\EOTf\USX\1051398m{cf\SOZ\1035788\997310}EJ\SO/fi\163344]\171918W<L.pK9H\177871\SO4\1087974(\1056365\&7SO\r\1105429,N\fy\1053085\EOT\CAN\179331\DEL\r\1042176q\a\1040491\EOT\SYN\985201RPy5:I$d\1057843\&2w"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000")))}

testObject_UserConnection_user_20 :: UserConnection
testObject_UserConnection_user_20 = UserConnection {ucFrom = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000004"))), ucTo = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), ucStatus = Cancelled_', ucLastUpdate = (fromJust (readUTCTimeMillis "1864-05-12T04:26:30.406Z")), ucMessage = Just (Message {messageText = "z@h\987054>\SI\998936`\US\r\SIB{sy\1102535\CAN\173513%\SO<\1002662)5SL\1050619\RS1\67401D\1063259\DC2d\3370,\1006997\NULSB6P\1063559Y\45085\153566\99660\31105\149405\DC3h\r@\31145d\"{i\"@=_\STX\n@\DC1L;oU~\STX`\tf\SOHNp\STX\46173(\ETX_\1047159S\DC1{e\1068612[\n\1091016a.?sq\GS\163218\12245\1039553\&4av2=\1048862\&4\51618\DC4%0\1039214\41227 9\1094194\DC2P\1005432\1097200\SYN4S\996693\1024956\52233y\DC40D1\1108505=2d\1072731\DC1|k\US\989599\62592\DEL\52613\1050002\RS4\983939w\SUBi.\1064004\74510=Su\1048545}\CAN\1003828\1059673{kg3(\1048808\30697"}), ucConvId = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000004")))}
