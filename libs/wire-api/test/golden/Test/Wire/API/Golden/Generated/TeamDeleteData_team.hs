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

module Test.Wire.API.Golden.Generated.TeamDeleteData_team where

import Data.Code
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
import Wire.API.Team (TeamDeleteData (..))

testObject_TeamDeleteData_team_1 :: TeamDeleteData
testObject_TeamDeleteData_team_1 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "i>Lx}$\\\RS?\1032425k\142215\1026011`8\NULY\48212|\ESC?\58058S|\EM6X-XK\62237\178988(\SYN@]\57844_\175989\1000844b1\b\82971\992121\1066140\1104485\GSs\155708\6542\1073453C\1070329:e9]x\145287\ETBPxh\51703G\9182h\148850\131171\DC3\DC4hbY\SI\1046696\&3\120879B0\a\53167\173045\995949\US-\ETBO\983699\&9\174970%\GS\95540W\US\ENQ+\NAK\b\CAN!,h@\DLER\1015765\32217\1047195\DEL,\12610}.{^\1090133K*\\\996909_X|9T\rM~\b\SOHKsC=\1010484w\1057801=\FS\ESC\RS\NULwFM%CXf\r@/NU\1054989\&5v\v\SUB2U\1053859K4\7249r\138577Q\1105780[Z\DC4#)\SYN\ETXsR!\vt\EM\US\1036001{\NAKz\1048398\1084558\1043080|\DEL@\47085\\\164262[\45446\1035221D];s\70019c\EM\1088115q\NUL\39248]F};f\DC1mz\1089294$\t\r&SuI!,\v[,\SUBc\57707\&8=\1083051 \DC3\140071v\120412J]\ESC\CAN-\GSF\ETB\DC4\SOHFD@\137590\1101727\SUB\994474<\DC4\164367\DC2\DC2U\1082404\&9\141435MQjmAb+\RS\129179\ETB\ENQK\1085702\2790v;\nQ\40363\97861[uD\1052274{\189293\ACKO\14870@7\RS:+:1\4216\172234Mr\58280(\34625f\1091318\1067121S\39579b\1040841\1071547\vF\35601\990171\r\b\1088916 \1087477m\9195E\EOT\137371\159298{\b=\DELS\NAK=\1009056\7723\8867\DC1\NUL\1028454\SOHh\DLE\SUB\1024764RPt\v7\1113500\159388\vD\1104573S\997271.\DC2}zP\47237E^?\27842\161895S\\S\1098500`\SO&\tU\111019\129639\181462{jj\1096914\SI\DEL|1\SOHR\a\f9\SYN,\1010156b\t^\1035824?V\n\GSz\RS?=@\35005\1103831\DC39'\b\EM8y\ETX&\1044131\1065694\NULu\1061927\SO,)\US\59053b&|h~\36591X\ETXD\987729\\~'\EOT\GS%\FS\DLE|\ETX\1041203\&3s\EOT\SO\r]|7J\1065338Jhi\38217\5537\148956\NUL\SUB%\985637\SO\DC2vjc|m\44638VDN4kW\1034646\119020$\EM\DC1r\21603]@\1086358K\158685\185187:u\1003863\EMG\94717\&8qN2;\DC4)X[p\f\SOH1\1031984\1031232\46840\1082621x.P\165688l\"s#n0\ETX~\US\t\74408[\1051014\1046406\14852`\1087777\DC4\1103137\&1L\135864\994377b\98392\ESC,\ENQ#\STX@A\SO\178614\ETX{\SO\27565\&1sX\19404}|EZJ\RS\GSxoSe\26956l:0g3\ACK"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_2 :: TeamDeleteData
testObject_TeamDeleteData_team_2 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "?\166974\1059823u\166070V\156206uk\DEL\1014216W\FS\SOH\STX\8328CP6\1080415p\vr\42868F>nX9\ETBI\1033277H\187336WIt]\NUL\1024225!i11p\ESC+'\187602\ESC\SI\ETXz\128844\EM\30393S\1004509`\68342\35440\SOH\"\153382O%\EOT\DLEl\1081113#FO\STXN3K\ACK\61872\DEL>\EM\vq\49514f\1047157\162431\tG\1071802iK\SOH^\147861O'\RS\DC4\96736G.\1102472k:J\ENQ\1030914\SUBAM\ETX'\36945\"6y~4<JU\63109\17240C\173934Il\CAN\SOHV\1012781AKcaw\ACKho\1069400Y\\(+\EOT7E\SI\SUB\53635\9166S]\r\160710%\EM\63998gn\DC1\DC3)\ENQotL\US.j\1029963\DC4\DEL\US{%80 \SOH$X\RSB8\NAK.\1094647\NAKS\1053443\SOf-\44715\NUL\183730| FvV\133938/\STXu\1021924_y\DC4\ACKM\1011033jNF?\24013\187624c\NUL\bZ,\GS\SO`\SO\58385O\ENQ\NAKic6F'\b\15952\DC4C\NUL\f)|\983678\&9G\r\1071869\&3\136349\154611\ETB(aN\USP\1038676\FS\1114l\1039964\EOTx_\GS\1049412K\996283\f\DC2M!8C\f\37230cc\ETB$Qx,=P0\RS\8818D\1043718`rx\SUB*`\ENQm-\SOH\58497\NAK\1026671kcY\ETXm\f\10181\143776\bf\83218\ACK\EM/\155827\1025494j\DC4^G\"\r`/{\NUL^@\SIv\190209U&`\ETX\1045998,/I\v\a\1092645C#Y_\nw,\SO\\\ETX4Q\SO$o4`\GS\CAN0\52606\1076165L\1056090?\SIi\FS\141334\DC4C(\RSX\1021912Q\ACK\n\SOHXK^y\133473)\ENQZ{q\121481\1002266.Qk\DLE?t\STXUQ%`V-~O\SYN\1094198\999527P\\\SYN\ESC/Fv5\SUByKI8s$X|\ESC\"7\EOT<s8}\fcy\123621\1079256\f\ENQ\ng,\175376\5514~\NULfw]\1090183\65605Rk\t\45926sgwm"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_3 :: TeamDeleteData
testObject_TeamDeleteData_team_3 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "D\159323\1063952\170572/>mI\158273(\CAN81cA\13746l\ETX\DC4\\\1025222\&4P\24624\1097175t?zd%s\NUL#I\44727\&31\141208\15975l+a7{\EOT<\1089133z\RS\1010747\161238/\48716\DLEjx\bB\DLE\US\15095&i\DC2\SOH\996246\DLEV\128680e\b~\1005062\1102177r\1006448sHW/L\6809xC2\153652M\1089024\ENQ,\SUB\118927\1041840\178027U\1057168\f\t\143120\ACK<\59653\n\DC1h\1058720#4N\CAN:\1044380\985702\&7\EOTP\1031894\SIu5\SYN\a\fuS\SYN\"Z\NUL?\1052908\99609\61972\DC2N\1072697\15914\v\EOTb^w\1063161tbt\35386uCg'\n\f\DEL\a\1047387x\GSSt\50443\1040666Zke$|~\1028617KixS\841\DC3\1095419j\995187A`Fa2\184680\41393\NAKuOy\DC2@\\fKr\vpnu[W\EOTvU\65546@\SOHx\19292V\"\143982\a\bsQl\DC3\CAN\97358D\1025141\ACK\tH\NAKQWPjJ^S+\986928\1014957\1050268\167552\1097122\129506\1072622\15892=\141574\EM\f\1045924\&6\ad'f\NUL\NUL\v\173465\26156%Vu4\1083260\1033045\&7\STX\DC4W\1069943\NUL\vY\166831f\53269\tb~W\161692\US\v\51528\44135\r*\DC4'{Js\1006163&6\95410c_9Yc\\z\187834\146677\SUB,\1028055\ETB\1051709\1072410\1036468\DC1fVI\NAK#i\1089557pi\1093510\&8\1080013\1050416\DC2\1081978L\1036631\t\74531\SUB<\1092486~;\50008\1055455{\1033009{3L\147152\SUBX[\EM\149325\\_\157906\EM\177995vv6\DEL\143831:xn@\100807X\1077293E)\r\985524q1\925>>t\14597say#\SIX\DC2\172468U9\180603Vc\996994{\DC1\SOH\1006305\STX^.\US`Ad\GS`n\"wI#}D|p)78+54$\a\13717\r\NAK\ETX1\138139H9JKpqyg;\984704\6818L@rY\4441\190106?W\f-S\SUBX\v\1079785TK\151860 "
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_4 :: TeamDeleteData
testObject_TeamDeleteData_team_4 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "\167486>(=Oz\RS.&\FS6\DEL\170257\EM\1002928j\DC1\97893'HsG\DLE\20059\GSzaJ\NUL\1080467XF$ \1060690\145809\r:)\49151\1073952F\GS3]\a\185029\"\51411_w)d\128946\15212\EOT9\1111179Z\rFs\1003507\v&h\47156\&6JaT= \1044714cy\1050480\ENQ\111284P!'\ENQ@>;G-|SY\1021564\1098567wM\SOH,\1055552f\4883\1076600\155845\68127\&5\1046930+^\990917O\1005927:0(#x4\RSuj\1078909\182583\r@n\"#]\1009578n\1062717j\NUL\ao0P\1091887qu\160610+\\d,\STX\169726q\DELtN\SYN\1099445\53742DvD'\172716\SI\129299\v[\EOT\1013802\\%\990907QS\SUB)\78338\1035121pg4\184481\1016554}\181939\SYN\1036974\SI\FSHu|,/\147470\1038677c\1069053\r|\DLE\1111179\1085354\SO\182406\1007665\74397\1011061\140405\EOT(\988948\1058753'X<\990426`]\47830 ?c\994989\af\r?X\179138&F\1060816\66176M\42801\1016345\DC3]\187040\99798@\b\NUL8\DC2TOT\163647\v\SOHV\RS\STX\DC3\121266\987299<\DC1\191387b\184415P_\NULZ.\1103781'\21496\&1\149873\1086160\DC3\160655\1080705\36096\1072090[~\95381Q~\1003807\985791\EOT<DX25O2-\DC29R}t\tkc\1110894 U:yJ\141267(\1029293id-DGr.\983940"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_5 :: TeamDeleteData
testObject_TeamDeleteData_team_5 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "\43464h\1032922~\1061936\b'N\1002992xh\ETB\ACK(].XO*\96569U\a@*M3\141591\45357\131590\&8X!\132118Q\ETB\r\137913\183490\DC3m|R\26008\&2>vd\1089936 \ETBh@.\ETB G4Q\1091026\&0I!?](\EM\1009092\rd\CAN\r\EM=\1046335\&8\1040668\8102\&5=\1105655\18286\35547>\983842E\DC3d&O\1155_\EOTDM\24125*\1011980<+9\f\111201\ENQ\DLE\ETBzU\DEL9Bq\bs9\188496MY5a\1040748!\45600H2o<W\1089138MJ\ESCA\r\GS52_\EOT7\SOH\151636\SOH\21773!\1044927Ys \NAKy\r\r\v3[J\16571)\SOH^?\US|)\RSk@@\65728B\1101294\DC3\\\16151\181660\&0\DC4\\$k~J\41872\144655Y~\SI\142499\SYNU`o\42333<\1020096\181026\a]ijw\100818Vj\"8\CANIL\fP'\983525\11612\ACK\STX'\SUB6Mm\GS\9703\18390\f+\168043f>\999564\SYN\DC3P|\47367\182203\&2\\\ETXe\ENQK\1045299\&8?\SUBq\127482,\99522;%\DLE\174777\FS\ENQ]\ACK\174055F3\169125\ETX\178467\US3Ph\ESC\134497\SI\1043316lJRL\t\60741&\DLEnU$)\129495\1060894\1039833\f\ESCK\EOT4\1096716:\156752\1027507\1079518U\FS\119638zz{g^\DELH\1019515O\ba5bo\STX\DC3i;:\14212\37940\1027439\RSJW>\987912\EMV\1097994B<\1033079\1002491*'\SOH\1059824\&1d;\1060391a\41718H\24770M\78258:li=r~`\23933zC\1084262s\v\1027415T~2\1059089X+I\DC4^BQ\1109659\SOHX\n\142087w\ENQ\1069109C\184166\1073464P\122897\177772\US\FS>\7449\1054606LI|?\SOH\1057802IE\127992\1108354\11244+\998617\52417I\63090\1054253)\DLE\DC1\a\149244>\26994(\DC25X\1110682\68647i{8V[i\190144v\CANj\GSRq\RS\701E|\155116\&3B/;9\39419\DC10\DLE\\g\153395cD\63464G\985591i<Z\131925x-\aWB\agN\1017923L\ENQ*\STX#\988009|j\152417\18563\SUB,b\160189\16235\b*<9\1007403JG\997370`\1104916+Dz$\USs\155558PG\46624?\186411\tds\GS\DC2\1003408n\1042993\DC2#%h\NUL\1109645c2\1002522\t2q\1037978\176449\ng\1058047b\ETXG\f\DC1\1099970KK\b\1107837?\1034726\DC3\b?\136664ub\154036\1051279\EOTQwCO\1041774o\1039375\1101904p~w\1038298Z\DC2]>1\FS$\NUL\1033716\"a?N\\\1102565c$W:\983079L\1044273a\1100761J)]x\DC23\SOB\v\71683\1042847Q\DC1Gg\a\NAK3\23269rI\ETX\1064632>I\984011zIX\DLE\v\984948\&4u{X\1078053\155024\\Xv@\n\147547\ESC\RS13A\13457W !\f\1104523\1108909\64188\&3yr\SUBC26\rU7\SOH/ E\98829}\v\USV\DC2O2;Z|F\1040501\STX7\183792\1100376L\US\991426\1023339]K:/-\1044621\985412U1l\41354\FSRn ]\8766\DEL"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_6 :: TeamDeleteData
testObject_TeamDeleteData_team_6 = TeamDeleteData {_tdAuthPassword = Nothing, _tdVerificationCode = Nothing}

testObject_TeamDeleteData_team_7 :: TeamDeleteData
testObject_TeamDeleteData_team_7 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "|\SYN\1053350R\ACK\14029\1100974\141152!\1112498Xw\1043175a(\RSX\989391B3\1009126J;?'\989522glvrF\178434ci\1040055\DC2hh-:g\141765\SUB\48247\1051934\SI>$\144167\988649L$\996662\NAK2sq>c%{\1061771zXX]\1062375\bd\160314 C#Y\RSw\GS\RS\1038222\1081158\EOT}\ETB(F\SIg:\1083021\&4+i\1011266>b_\ESC\191314\1056764\ACKm\1013162~c1\143978z7BM\n\EOT\f}bo\1096197'\991291\1007734&<5\SOU\SUB\DC1\131235+\1050870A\FSS-D$N\190895w\49045S-L\144414\1093889i\167808}EC\1081955\"\1034844\98599bq\1037627\SOH\153279C\33744Jh\1020874e\78082\1083389\&1%\STX_BD\1109230\NUL\144134g<.\167270\CAN\ENQ9:\182574\US:\1034863\EMT\SUBSH\"\1103704\DC1\ETBV\DC4{!\FSW\a\13340{\182394@A4!yV\f\EOTVY\ETBP#\1059240\1003701\1106905sysSo\1098350h1B\5570\"\9350!\DC2\1031344\NUL\1099868\ENQ\CAN\nZUk\183853\986232\DC4\SUBG\1107741jv\1040544\&35F\178531"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_8 :: TeamDeleteData
testObject_TeamDeleteData_team_8 = TeamDeleteData {_tdAuthPassword = Nothing, _tdVerificationCode = Nothing}

testObject_TeamDeleteData_team_9 :: TeamDeleteData
testObject_TeamDeleteData_team_9 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "5\tt\US\STX.-:\150454\1008817\1108150\7302\180616!z\ACK)\1036966,\36158cH|\ESC`\983356,\1056228\DLE_jb\SYN\DLE\999616\SI(Y\52758@\STX$\33211m!;P\SO\165645t\FS?l\1084281}Ui\\\ENQb\155094RJ\1036671\ACK\39953*W\1019548\DC1\986051\DC3Q\1086809J\DELh\SOHY5i-\142840\DC3K@W\1038530i\14430p\ESC2[,J\DC3\ACK\RS\f\f/\1048120!\5751-\SOHXBq;V\98370\1018087\SO)u\SO>(\128175\138077k\1092224\STXR\35799\ACK?1\aw<\SOH3\rH:\RSYA\SI|IOV\CANX\NAK6\tM\985927Z\1083464*\986212\\\fl\134144\&9\1087151\DEL\SUB\CAN\DC1\\\a\DC1\1088970H0\nk\EOT\DC2\DC4\SUB\1002532XO\171906|!\160319 \1088766\40807\1100379W\NULXd\993779L\140128\GS\DLE\98366c#s=\DELg\155615\r0$\r\vD|\GS\993376:H4\STXMg\27349Qf\43148/,z\62636i\a\1048347#\95511h\57479mF\1063847]h\1089472Z\989287\SUB*\1099020\&6\CAN\DC3Bq\169694l\1090008\1034040\ETX=-8QP\ENQ3\1083969\29219i\52068\USXG:\DELE3(\ESC/\1037295 \188038\&3\ACK\1037819\29071Y\163233\nn\1008010\&7SN\SUB4<\1019928E\aUDeBUIJL\42492w_\1008912hGI\DC1w:nJ\ACKfW\52528\994039\a2v\ts`\119066\1004985\b1'j:\1063674\ETX<'\64040\FS91i>T:XD\CAN\a\1078993!M\EMwc@\174048\f_|\CAN\DELM\50126tm\1047367\SYN\26017:+Xt\1016079\1028901\15823\17821\1008174^ )!\SO\42711\1029362y&\992585\52874k\996506\\\1066493-&\DC1\SUB\1002828\154321\r\47583\vS\1095338c,\1104404\&4\NUL0E^i\1081545\1015786\8631\t\100419]p\1005291\137798\SOq<E+^\5667\1060821%\r\47950\DC2\tZ\1100093\FS\n),Rb\998641S\vsd:\1028019o\GS\189575k;v}+\1009918\DC2v1\ENQ\1082779\1041328\GSD%\100448\1059422\1027846W&\GSTU-rjX\991592\1046697\ACK!*?\191007\47729^]\143630\"\USJ%Q\68772F\178252y\38102L\US\NAK\983553Q\DEL@\995322d\1102873et\984318+Z\DELjf^(\\\1049551\1024743\1060017g|\\'P\\\136819\988124a%\1066622\174969-\ENQGMY7P\DLE@J$\1109421\&8\1019116\1004345R\1113795}|l+\SYN'\1020293\48716`YIQ}\SYNs\EM\78826\163193}\1101221s$\r\1102434\f+_Oj\STX{(\t\SYN\1106943S\n\EOT\128831\990666y]rw\f\t\EOT8\173718}h\ESC=M8/U\ENQdLA\1016643WF$\ETB\121175\GSk\SUB:Z\54514\1005960s\37035+\63412\nDbJE\162618H\30886\1010880@\ACK\"a\141547umQ\36246V\1110753xPA38U)vj\\\175041\158178\rBF[\n\997241f\DC1v\190426\&8EX\190577>{Z8\1085622K\15273\36480]\\\SUB`a|\64088\GS\138266uk#E,z=+!/\SUB\162983\SI$b\4525\&5\tio\99777\SYN/\3242\140303<\1090896o\GS\ETX\EM\44779$U'\1037588\999481vuQr07\19473\t5\128968\CAN\983281\994806\CAND\131278<A\a&\1002170\1040817m\1076190m@\176752K\1104241uZEs#\1058554\1014664\1101451'\188189x\STX#80\DC2,1QW\ETB.;\1051589sR\99655\CANy@,\1097528cb\1013008\30891\ETBN\DC4\1060980DGU\1015425\SI\187757^\US1{p`p\SO.*\US@:\1057875\DC2037*\990694\ETX+\51117\993538\SOHL'\"8.Ne6V\156242\1897\&7\DC3\v!\ENQ);dCd\t\1079691plk`\40743\53904v[R(\137106\157902\165315J\CAN[R1\10609&\1031192\163220w\43311\1069866\&9\DC1FS ^e@[\1077043gmVy/r\RSn\FS\142856\3848 \1009922:\rsg\SYN\EMZT\161813[8O\DC1\39077+E\CANrSTb=Vnf\1074261\1087152\138410"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_10 :: TeamDeleteData
testObject_TeamDeleteData_team_10 = TeamDeleteData {_tdAuthPassword = Nothing, _tdVerificationCode = Nothing}

testObject_TeamDeleteData_team_11 :: TeamDeleteData
testObject_TeamDeleteData_team_11 =
  TeamDeleteData
    { _tdAuthPassword =
        Just (PlainTextPassword "\145748vO\37261A\1048309\44800\r\1095440\1044179R\FSVXd\DC3\FS\SYNzlz?4\DC3b"),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_12 :: TeamDeleteData
testObject_TeamDeleteData_team_12 = TeamDeleteData {_tdAuthPassword = Nothing, _tdVerificationCode = Nothing}

testObject_TeamDeleteData_team_13 :: TeamDeleteData
testObject_TeamDeleteData_team_13 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "\47332\ETX\"\159133;UJ1k\27996\14684\&9\EMc\SYN~\995204\&9\t\97023\EOT\"\ETX\39142\185543b\DC3.p\1061399yM\984576#f\t7\68813h-2\DLE\DC2\STX\ESC\SYN\1019193]hi/\17983X\169049\31763_)'\132516\1093204Y:hj1\155003X.rS~`{\17812\1071828"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_14 :: TeamDeleteData
testObject_TeamDeleteData_team_14 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "\FS<\14380D\a\28253|\54152\157603cl\156030E\EOT0\DC3\r\"5\1094906r{#[C\152194\1045015\31799u\27136/\186010E\23833\FSW\51412D\41239\ACKf\CANOzi\STXd\DC3c\DEL\154377\&9 k\DC2LL\1009859\1112477-\174707\ENQu\1027443tzSNZ\1062533K:fU\SOH \EM\41917\37109\13119C\ENQ<\1029899S\1087522\1024282\GSR$Fz\1058283gEM\72389\b\134548\1023234\52555R<t\US~\1033949\1035963\DEL\EM\1001175:\DLE@\t\ACKWw7\1028219yJzB\137644\1106193\24230pX\ETX\1014976\&9H\988999Rs\USH\f<C|\tI9\SO\DC4U\FSp\68403x"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_15 :: TeamDeleteData
testObject_TeamDeleteData_team_15 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "Pp\US\\S\999050\1014598bk\ENQK\1100817@\1003588L2\154937\1041254\184542\984751{#o\ACK|<Mv\1045730\171677\37473PX7NL\ETB9.\1066216:^\183270\1054464d\b\181117/\ACK\ESCf\992675V9q61\b\RS\SIo\RSd\\e,&\991637AQvJ\70058S\176734-E\986799N\1002215{?\DC1n\100821lpUL\ETX\45898E\40541B\26597\990773e\36090\30248EH{'\1104846%w\189654\1113206=\r0\1003222!<N\1045657\1108872\37380\1002229\153251rw\160022\\@\1104000d\f\1036574\&8WIf-\96119RvKD\13892\&1v%2\1017177+\EOTQ\1081825\1058081\67317HJ\1046816t\\_lF\STX'\vX9l\171223\1079460\DEL%\DC1"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_16 :: TeamDeleteData
testObject_TeamDeleteData_team_16 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "l\ETXc\ACK=1\GSf\ETXx\995960\147516\n\f036J\96246\&6\178094V{}t\GS?TR\"\CAN)\CAN6h:i\ETBZ9\\i\DC2b\GS\1034466\&6(IvY \SO:\49299\140528\ACK\DLE\FSed\1074866\EOT}OK\95864H\985202W9_u\1038723`\174726\1059907\994770\v\rIAy\t\STXy\1104194L;\ETBXz\EM+g\STX\ETB\1099149E\RS\bF\1040928DK\1019702_\ESC\ESC\179922xMah6*\1068469\1044158s\GS\65117J\157841)N7J\DLE\188212\SO<^\ACK\135336\&9\8019Z\989377#,E\1081018yNG\94414)~c\SOH5wC\134860:\n&5\1079759g\164598\NAK\101001w\\C\ETB\CAN\1054630.\169137}k\ETB~\1095617\6683\1055846\&9\1010883\1029800i\DLE\ETX\tDg\SYN\DELa\t\RS\151658\ESC\181262_C\DC2\61997\vGbu\FS[zo(\984637\1043834g\ESCfe\SO=\73747\31585M\v@\119571\991649\CANuq\SI8\54175EJ\1084681\b\72806;\SI,c\v\31238\169009.1K\CAN@\f\23230\165835W\vX\RSk\1030773\SI\993010%\ESC\1023471>\ENQI\1044258DXL6I\ESCB.P\3930\1090709\SI7\ETBPu\SIK:\RS\8017}S>\GS\997008\v\ENQr;~`Co,/,\148290\USU\32073\\ngr\997268i\149277\GS\1075609Y\1110379\v5FajE#!\aF\50300z+\GS~Ly\986342\1095807\SUBExbmgxU[\22230]b\ENQmo\983838k^h\992093\NAKD\173627\144512B\ENQ2\1006334f\132015PWU\f}\166557\&3d%y\165250\44801MN\38044r\159335?K\34409\ACK\EOT\44504\78068\DLEO\15676G~1 g\985974ne\13669\1075356U\1060554\"B\SUB\1016699\DLE\994930Bj\FS\SOH}wDG\179165\EM#</\1017185uCs!\DC4d.\tg\1081223Do\1096969\&7\ni\178452Tw\EM(mf\26560\70855\f\1060289\186235k>\DC4Vc\\\fz9'\ACK2\SUB\SOI\t\1102083\nb\70657\vR%\t\ENQ\24196DJA3>\RSD\986251V\CAN]\ETX\SI\985787R\42725\1105102dGFO\f\1027792\14140{\STX\161088'\1063310\1014846\183656\STX\SI\DELjb\tQf-9\n-\1012873r\78321m\180126pJlc \133719\988689'\US\ACK>c?sXEN3\1007843\DC2l\1029759\EOTJu'\74452y\v\996071oM[\3007QB&\28108\1053307\27606\&2\v\1072650\ESC<t\153311'Og\v#4\v\US\DC1bY\8716r\1056916m\65577\SYN0\DEL\1081880Y,\1037952{X\1014244\&1~)\SUB\GSyj?\1051623\DC4\32937}0.\95684\1002883\42679m$nuo\"Wf\NAK\DC3@EC:\RS\1018631\&4\21792:Y`tp/\b\15000\20323/\fe\ETB\b[=\149415\184777\1072429J\SYN\1099234\174588\&7g\168017}%\EM\1054083.z\1006953Z5\1113341n\5836O_\1049968j\n\DEL\1111188*_\25305S\SYN\100360\127766\154173\917842GA\96013\1019284\14238\EOT\DEL\1023963;\DC3>\132795\1001058C[O\DEL~h\\8*c\145008O \aDQZ\1031791yb \GSX\1004099`\989803\b\1096355\DLE\1085472\DC3\1056898w\997883)\DC1\EM.\11322\149106\158323K-G\1029431\DC2Hg3\"t\SOH\EOT\1060954b$@\176852\DC2OP\ESC\1038106Ip\28195\&9ty\1036676!"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_17 :: TeamDeleteData
testObject_TeamDeleteData_team_17 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "*\12110m,b\994288OTCM{x:'\SI,\1040212\DC12ffV\987602\1070011\f\1109675uf\SYN(-@ B<\50301*H9\1099931Qa.%`.6\DC2GQ\ENQ\USt\1058323Mge_gL\t#\CAN\ETX\1111199vG\ETXg\n:;x1\1035394n\SUB:<p6<\1092860(Na WM\181898G\EM\SYN\1110570\51309\DC1Y^Y\RS7\\\2280|\50729NF0\9829Q\1056874\b\1082805!+\ACKjWv\154147\1069533\&0cgP\1006408J)4:\NUL\DC3@\n<K55\SYNq6\DC1sZ\v\7678\SOH?E+SX\NUL\138569=)2)K\ETX\1009966\t[\t=\CANu\126497= /D\DLEE\ETB\41613\1001812\1073116Wi\ESC\"\1049021I\SOH\191214K\1042931>l`^\1045359\&73]IiN\1048742/\bOs"
          ),
      _tdVerificationCode = Nothing
    }

testObject_TeamDeleteData_team_18 :: TeamDeleteData
testObject_TeamDeleteData_team_18 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "\1063567;7/\DC2\29449\94019\DC3n\122882\31258S9\ACK]\3850\&8P\t\1084384\&2\73795#\181815\&7~`\1100343\175281\1028579~n\51679\EOT\FS\v4?nPy5\EM\EM\1095405;C\nW\78148\&4\SUBTd\147540\CANyR7\ESC/V\1089526\STX\v\1000358,\DC2bv\164290K\92285{mU\1060894)\DC1\73974.T\nB=\STXlsux\DC4\1077175\&1:\CANL\1110743Hh\NUL\174066n/\1102525\162921\993112\59381\DELq\46095*)}\1098746\1037636\1101970\1088021\NAK\CAN\CAN8\USU\b@jr\DC4&\991478\1075234_a\EOT+\SYNm\4275\SYN\DC4Kw\DC2m:P\25327\&6V'G5@\EM Z\DC1s\142323cq\SUBP\DC4.d\36321\NULK\27185\NAK\47378s\1031768j/\tu\158145\ETX\1010872\SOHT\48868f:C[\fJf\1087126\142737\DEL[&hi%)\136397\&2\184974j\\\1072975eM\1085470uvK\v\CAN\FSCw\v\1031529$\EOT7\STX\1027727\FS0\1199\&2L;N\1073075\"H[k\1073178\DC2\1009501f\EMwuSE^\1107505\vf\STXf\DC1\14113\EM`XY\21048\&5B\13496\1022663\20371w\51905Ot\44037\&2T3Wb\SO9!:\RS\FS\1006766\1097727j\ESC~DY\SOHaN\n\1065381Jw"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_19 :: TeamDeleteData
testObject_TeamDeleteData_team_19 =
  TeamDeleteData
    { _tdAuthPassword =
        Just
          ( PlainTextPassword
              "_\r}u\1075299Z!X3\160102\ACKKOM\1081288%\DC2\65730B\SO\180051L%w\"\ENQ q]\ACK'\a.Ox\1105498\1057171\\7c\r\33864\21114{\DC1o\1105122uO\CAN42n[k4\rn\152690O\SYNP\135580\1110329\n\1042613N\1061340\16437\\H\SYNbr^\1003766\1060894i\1109911\SUBy<\nO\ACKTp\SI\35591N`f\26658}!~\1082799\NAKcp*!8l\ACK\DC2 \37542\DC3\61149f\NUL1\40820\NAKT\24987\179326 _\94795\&0q%;\119094f8E0<\997746u\SO}\1031140h\35142+a\n\1008145N\1041221I`\DC1\1032664\191259\1113574\131171.3Kj\37035\RS\39573H\SIzI'\NAKH\69706\152434dN\176099\991214\990295`L\ttN\1013377\DELQjV\US1i#Ag\DLE6.\1112310eZ*r\EOT\1024019-b`\1102712\&8\f\SOH\1010355nK\170543fp\187486RW\FSs\994965mH\1045304\30604>\v\1049211r!}\53167'W\993809\1098296qE\EM\181344#y\fW?#\DC3\1072094G\\l5\1068018\986650\1038548\141195\1102837#vsV\1016098\&8PP\EOT\DC4TZm?\167010CC\ESCR;\USw\\Df[JjbN`X\95418\43924x\33016\&9^\98002\127310~\ETB\bzWl&\r\1032458K\996614\154337\b!\SUBL\a\1052800t-w2N\14407\b\EM\177903\37957iG\DEL\1014649e\ETX:q\f4qKRr\ETB48DJTS\1113548[\v\r\DC1\100102TF\1044374\&8s!}SS7\v/\165368)T\GS\ncjR\156817\1023594\NAK\v\167937-%&^\EOT\167120\994763I(\"B\EMP\DELbl\DC4\GSl\1105622\NUL\1096218&\40531&\60243\SO$\GSU\ETX:}l&j\bQ\1104300S1v\1049270%Q\181048^\994564\1042226\EM\ETX,%\"\161513s\SOrfB\STX\26259\US7i\DC3\ACK\26195\"G(\11669%\SUBn>\1063303\&4\995605c\SYNq@\12458*I\ENQ\60070\14051o;\ACK\1043958)\"B|\1009891\170532\1051466\22730\&3\FSg\DC3?\1100853}\SO8%Xl\DC3\16332[O,\125199g%S\160477l\ENQ\"(H;+\STX}J\DC2\1091067\SYN\RS?7\181974\v\RS\59550X~Y$\RS^\18330\1036144\DLE\144451"
          ),
      _tdVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_TeamDeleteData_team_20 :: TeamDeleteData
testObject_TeamDeleteData_team_20 = TeamDeleteData {_tdAuthPassword = Nothing, _tdVerificationCode = Nothing}
