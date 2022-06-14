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

module Test.Wire.API.Golden.Generated.TeamUpdateData_team where

import Data.ByteString.Conversion (fromByteString')
import Data.Range (unsafeRange)
import Imports (Maybe (Just, Nothing))
import Wire.API.Team (TeamUpdateData (..))

testObject_TeamUpdateData_team_1 :: TeamUpdateData
testObject_TeamUpdateData_team_1 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("@t\1104947K\1103008\v\34277\ETXe^\984496x~U;^\1086372\b\SYNwn\\aS\1022526g\CAN\1015468\ENQ'+\DC2~yJ\190623%y\110657!#3\CANtZ\1095609[&{?\SYNX`\50850f\FS\62969=j\US\1046631+d\ESC0\111091\50408Ft`U\97666g\158703\1072122\987428F\avEBjP\153147\94534c\142165\1041426e\176319\SIL\189459\1080869GW\995547I(XBV8\ETX\EOT\DEL\1017745C\38693\1075418\NUL,\190006/P\1000635[y\NAKZ \US\51607c\DC4X`%\1066586\&8@\tP>em\917813E\SOH")
          ),
      _iconUpdate = fromByteString' "3-1-47de4580-ae51-4650-acbb-d10c028cb0ac",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("\1022724d^\GS\ENQ\CAN\163966ey{\131853\1078784q8\989062^\GS\a(\NAK\26149\&1\143037(U!w\USqC\NUL#g\CAN=\1001510\1040448\SO\166655zEJ\GS\24481\162891\134036\STXe0\1001249D\ETX\b9x@`VN7\166384i\72099uq\SIdjL\FS\GS\SO\1082202<\\\1078204%.\v0K\19396;5\DELh\DLEflQr\EOT\DLE%\1031074x\f\FSL&:/IK\67131:\179222E\1110477n7~\988971*tOI3\SOH\RS\990034pp-\16356\21562\1038682@O\180973\t9]27\994976U\1068604\51662\&3[\1093765B9\183546+\NAKS\991710\CAN~\GS92sLo\1061755t\SI\127014\138452\&2\62505\120746,\\\132777\1112482\11321^Q\147229W\54723dY\194645\GS\133328N\74578;\SOH\1021417'\167765\165511|\150535(\1097341]\GS!\1112618$\US\153908)hloBRpT\1076445\f<")
          ),
      _splashScreenUpdate = fromByteString' "3-1-6fcf77a2-f58f-464c-a0bd-20fc4aa686ac"
    }

testObject_TeamUpdateData_team_2 :: TeamUpdateData
testObject_TeamUpdateData_team_2 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\189807ZV\ESC\1108470:RV\ACK0%U\r\ENQ\5305\vg\SUB\DC3\67160\v\17005\v\164969\DC3\CAN`\t\153326\t_\1030121\19120\US\r\"\182508\95642\1011430\SO@\39970\DLE\ACKy8*\134852P;o`53L\ACK\999693\t\16157e\50198\SYN$\SOH\1101935\1093304\NAK\1031461\100218\b\FSeW,\1082547#\DELUU\DC1%\23739dF\69383Z^\993333u\182995\62551v\1026012gb\1087967\USR \49133\SOHm\ENQ+\RSdHcX\1043456\SYN1\41562\t.r^\n\DC3\25500^\EMp\23943h>\1008252%\1065685#:\20208\DC1EB.\996292\&0H6\174124\190683\19272\1012708o>L\6289_\ETX\988770\&7.9\1073238\DC1WQ\vr3\1014429g\US\178828dZ\DC4\987183\\\1033879\998865~\30943R\tl3Fz\GS\DC3\SUB\ACKD\1032087Kj<W\EM\ESC^d\44752\DLE7^")
          ),
      _iconUpdate = fromByteString' "3-1-43f5979a-b8b3-4a6d-86f8-532445d025dc",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("8Q(\995440\"\24914\1083052]1'N\172704?7&\n\1082773\a\f\187381K\NAK2\58591\997862\78864e7\998731)Gm\1102551ueG\1013007\1051103^\SYN\EOTi\USXKLxz\1105002\EM\1018599I\CAN@p\146418\1097021 4|D\rg\132917K\RS\1054553bHr\1105965\991873\SOHR\155079\1027295\STXr)\ETBt\NULy>\1086123lE\"\1068000\ENQ\97499v'T\1021675(VJn\DC4\50699feOI\1009582D&saC\f)k73\SOHCp9o\SYN\97923\40491\1109035\1019461IO\1082545\1036802\1094798K*w`xc8%\5428\bQ\1108643\1026166\DC3\NAK\t\186580\bd\1029714~\1044113\STX\61177p\378F\990904\1048094-\STX\v\169217!\1086602\ETB\167313+\DC1&A\v\US$\FS4\1103959\184039C\SI\1096634W6{BNO<\58455\DC1\b@\DEL!a\1002905r.-\1001694\175413\1046218\9086b")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_3 :: TeamUpdateData
testObject_TeamUpdateData_team_3 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\65759Z(\1086736\49591h\1037022\ENQSy~F\GS[\ETX\ESC3q\1097010\ENQ\1078669#\21679\v\172979\33156\NAK\181412'\1052188[\1069799\132404\ESCJXn$\SUB<\f\NAK\NAK@'c\n\95819%\10649nO\1048297\14805b\44502WW`5\rA2C\NAKF)\CANB\141169\101090*:\ESC\DLE>M\FS^\SI,\39922Rjve\NAK$\DC4!\\\SI/13xE\176873\41996X:B\DC1h\38384\&0\15928>\1084065\v5\GS'\1028874\ETX\SOHgj*\181871#P")
          ),
      _iconUpdate = fromByteString' "3-1-2ed18927-755c-4197-996a-7076baa23923",
      _iconKeyUpdate = Just (unsafeRange ("`m\DEL$\1032324\44660`\152159b\1052163\"\FST\SYNiA%ZnO_\b\DEL\NULb")),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_4 :: TeamUpdateData
testObject_TeamUpdateData_team_4 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\128134e\DLE\SUBcs\2387\1075156|\US\SUBu\FS\52294\153736&@\EOT\998980,D,<\147898\1023755RVpe}[\148576\52157xN\tz,T-d*\159171\EM\GSa\1086147\188771\STX|>]\v\191414:7\190399\1002509b\1012517|c\131827,\44613\1016960O\120010l=\1026976QA\21240\48375E\1048133#J\SIocH&\SI\191295\DELh\SUB^\t\NUL\58166\986951\1040859n[\1099585o\118936\DC2\43837\41993,\SUB\a\ENQ\EM\20127*m\1060088&\171058\1057983\EOTE#W'\a;+I\SYN\FSg\n^i[\1044417\STX.\CAN]\21346b\\\1106355\1004766\DC4J\1010071\1109900s:D\FS\SOBwpPF\ESC\\\f\1043258\157327\32653H\1038564\1018956j\1068498\19386\119144\SOH\\\785NE\51900\72110\DC1\rU\t\149777j\SYNX\n\1042182}\1041865\1047029\1069576\vS\1022749{\1063362\72135\USi\1043163\DC2\1098488^8\78341\ACK\SObG\6333X\1107580\SUB!R\59730^\DEL\SYNqjf}|\STX$\ESC0Q\SI\ACK\1025203\a\46015F\173556~\DC4T\1110827\135066j+I\EM\RSWK\DC2f")
          ),
      _iconUpdate = fromByteString' "3-1-2f002dda-74de-457b-aabe-831229662e4d",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("ch=\DC3zyXV\1454\1101200\128701[4N\97150\1113651(sN\1094602\59751\59442=J\CAN\ACKuZ\1025534[L\SYNf-\1043969S+zKX\DC3 L\NAK\EOT\159717,\DC4\168372'\1098967#b<q\CANOS\SOS\vz\EM\f\ETX\DC4*\5249\STX\DELn\1003044[(\8408 \92309\DC3\170639\&6\54117\GS\ENQx\30667\&5\STX)\1055711]f4\r\1013137X%\19501TE\1066195\GS\ENQ[d\ENQx\1037802c\9103\42477\998882\&4\DC3\tfT,6v\2116,d@{1s\"Ya\991140c\ENQXX5#\179447\187638;!\40003x\1039893TZ'4\33957)\147971n\159464\CANuL\1083827\STXZ\999710")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_5 :: TeamUpdateData
testObject_TeamUpdateData_team_5 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("c\DC4\21957\1083082Am\SOH,0\5634\1011802\DLE\DC1\1048597\ESC>\DC1rj5Hd\1061313\DELI'$.\98215\DC2}\ENQ\DC2\1009633\158711\100133|\FS\r\DLE^\8538t\190283\1060031vf\1047172`d' '={4\48912\b5][T\165195\&7A1\32515\NAKY\\frek5$f6b_4%\129513\DC2\1047616!\DC16\f\ETXK[SQH\n\35821\1017522\1088735\EMd\\@RQB\1113466\75066A^ l\1085060\1033719X^i\1014199\SOH\1042929\176179I\1107945\US3\1044762xIC\DEL#C-\1054562\SUB\136101r\35811\f,\SYN=\SOHJ\40558I=\987545")
          ),
      _iconUpdate = fromByteString' "3-1-1ab65a15-a0de-4e3a-b5cf-533b43df652e",
      _iconKeyUpdate = Nothing,
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_6 :: TeamUpdateData
testObject_TeamUpdateData_team_6 =
  TeamUpdateData
    { _nameUpdate = Nothing,
      _iconUpdate = Nothing,
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("\165611\1021466!itB3\1058831D\ro\120187xW'\147774-\187218XM\1068926\58137\vGM.`3\DC4\a#)R*\994856\1053602By^Dh\1093091!")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_7 :: TeamUpdateData
testObject_TeamUpdateData_team_7 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("n{\1057261oZn\DC1\ESCJt kj\ACK\r\1009375\"'{\SUBX\183635?@\1072481Ly\1034079\ENQ@$\126078W\182880\152533mW\1031829\DC1\DLE^c)\185735\987874\168851\44285\&9\1026256\1081073\1088339\ETB\DC4\DC2My\EM\998884\CAN\155753gmi\18003\SIy:r[\1028859i.\\\SOH\1013999\ETB5\184553H#\DC2\100088#l\SI@\149391@\NAK)\155671Jg\16061c\ACKV\EOT\1052115\166619\1106254\DC3\7348\1014585\1039214fQ\36540\1014874\1099704|Ik\DC1X\SYN\FS}ii\1044665M&.)\163680\SYNL\1006642\ESCk\a!\DEL \SUB\1083653\150892+\RSRW\\x\US\GSt\988142\1060379\33437\CAN\STX\51186+\DC2\1051428,\\F%,w\174606a\\\DEL]\RS\141663~X;f\134482 \1065664p\DC1d8mhY7w\RSe\ETX\DC1\1112177l\ETB{3&\49028\ACK\DC4V=D\NUL\ENQ\SI\93957\aK_di=,")
          ),
      _iconUpdate = fromByteString' "3-1-b5aa5007-2939-4c53-874b-aecfbb6244fd",
      _iconKeyUpdate = Nothing,
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_8 :: TeamUpdateData
testObject_TeamUpdateData_team_8 =
  TeamUpdateData
    { _nameUpdate = Nothing,
      _iconUpdate = Nothing,
      _iconKeyUpdate = Just (unsafeRange (")\USi6V\175058>F9>\DLE\bOqU\DC4\67882l\1026522")),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_9 :: TeamUpdateData
testObject_TeamUpdateData_team_9 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("QJ\1097031G\"g\SI~jG%\DC3o\SI2Zb\30604\1005260\145682vb\US,N9@\1044946\ENQ\DC1\151830:\23929H\v\EOT\66778\ETB/1\3753:\ESC\188539\&3X\146473\&5g)3xq7\38571\140250o%PvWF\vF-vF|\a\1071563k }\1008775\120687!\NULZ$md\97106\119012'\1035663\131295*Tj\ACKh\\TK$~ *\1658\19623*P/=W\GS\29550\1019406~_$~\99885:\ESC=\153783\1005174r\65190\\/o<rzSpw\46822\a\985957Q\1095193B\170449a*H\48202\SYN\SOi+fV)+\SI\DC2m\53573\1009347\ETXH\1109078\57508\STX]\93031\&4Hj,\1024600w&(W4\1033171\1002889\1067890\ENQ-\FS>RB\v\EOTK\1073165\18061$\17338\EM~-}S\996372ipLl\190933IJ\GS\SYN\bu\28200\CANkq@1m\126546\&5|\DC2O1<'<U\60431\1037614?CE\48396\b\STX,lHL\1109533!lja3?\STX\EOT\ak\1029715fq\1046971M\14646\&1a\19425")
          ),
      _iconUpdate = fromByteString' "3-1-3f6de95d-a973-4652-93c4-0ffb3fa381fb",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("F;\ETXO\rTm\DC1\164377\&0\1035562\DC4\1059095\1053068\47858\&7\EOT\188212![\STX\1033259\1047034\DC2\SOH\b\n\SOmV\1033077\1112321\ACKB\188155z&\144324\DC4HP\3176WS\182549\SOHQT\RS\ETX$b_\186241\64092\NUL\1081667\1109728@~\DLE.~\141150sl\CAN\ENQ\EM\1079555\16908V(^|\190897\GS\18132\SOr\1113585\EMeb2(#*!\184596B\a^\983731\SUB{\DC2t\\\110623\fu\"\ETB\SO\np\1049207[\SOHlB\161213l\43878L\45439\SI\138287/\n\GS2Gw~y\143044u\4467\&1\998784\t\120970l8CE\DC1L\188694\998926CB\GS{Xl\148655\991418\DEL,\2349#{^\1055214S=.g\SYN\ETX%\STXb\1113290\181647\&4:\49720\1048192\1107038u\1010360\GS\ETX!\121055q\GS.J\1102032\1076353*2\1069991\STX\1068656x,.\DC3\fR\ESC\b\1004144/=2n^f\US<xYz\1089741kW\DLE\25945EU0c5s")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_10 :: TeamUpdateData
testObject_TeamUpdateData_team_10 =
  TeamUpdateData
    { _nameUpdate = Just (unsafeRange ("s\1025937D\1093955\FS@?\ESCZ\RS[\DC1w*!?p\DC3\1025380L\n\bGI\GS")),
      _iconUpdate = fromByteString' "3-1-6065a84c-2b9b-4c6c-bcda-35c0cd7b41bc",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("8\1021447\STX.\1002730.G\27632\1052631\ACK\22312\RS\ACK2h\27721c(\1112108S\993484j\1014633\US]\1048807Vrh%:\ACK\EMu9z\51503!\26510\DC1\GS\ETBQ:\137985\1111453\1081343\3422\EM\1025434\"$q\"\1086393\f1OcD\152663\DLE\144575\173054pc\DC1\182364\&7\SOHTE<\1027414cVC7})9n|2es\ESC7I=h\78030[$\v#\aX\SI;\992748Rr\61252\1091351\1105023\SYN\STXUP\50587\&9\EOT\181904F7$\vs\DC1\1049243\165519\164968\54311o\178522\t\NAKu\23146,\1107645\1096120@|@uJ\EOT\\TT\SIveg\62939-\189304H")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_11 :: TeamUpdateData
testObject_TeamUpdateData_team_11 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\fu^\f\SUB\EOT\DLE]\1103654\1073077B\ACK\SO\1073433\&4cS\142526\DEL:\DC4`\25718\ESC9\DC3\SOF??Ev1,\ETB~M\RSz{\9729\r?4\184571s\19216\1060220\8074Q\f\r}N)\DC417G\SIS\1104513\63502\511+A\984709\1078087M;7\99865P\1090541)v!_&\DC1\141974\120023\20983\1096711*b\1050523~-\21096\NAKw\1078534(\ETB\1876\998248\ESCm\49309\"b\78325r&\DC3i\36117\DC15\f&\US\1057502\&5\DC1\t\36126\SYN-(^XI\\\DC4p]}\STXrL\1102558gW`\1051194\a|\DLEP)iA\41441\ETB\"`\b")
          ),
      _iconUpdate = fromByteString' "3-1-e6d874c6-dcb0-46e6-94fd-4ec8d0b780f4",
      _iconKeyUpdate = Nothing,
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_12 :: TeamUpdateData
testObject_TeamUpdateData_team_12 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("v\1072163!\USc\NULF\ETX\DC4\DLE>\ENQ)\1004070;\1045448\SUBF\v\987260I\SYNg\fb\nXB37\DC2HHhO\DC3\aD\ETX\FSmm\65705W*\1045560d\v\SUB^\1037116ow\166819&9\185716B\1015997\nK!i\DC1\1103398\\\137045\1044022\95353\&4\1041203J8g\ACK\1076662\163809\1074446N\51814|^\1097868@\1071814\1095356,Wi\54749\&4\SI\NUL`\SI;\SI=<?5\991330\1079578,^f\61693\SYN(\n\1051843e\rn\10566\176067\GS|Ryy\DELz= (lTvJ\61924[7k\v\170007\&4G0\172459<\ENQ=\1062324RM^0\US<9\1111503k\1005272\1070193\ETX\21225\1039121@&I\DEL\FSg\DC1\DELL'*\SO2\SO\RStYZCdY\SUB~\SOHp<#.\na&G\SOJteDn\ACK1^\v0Z\1014907)HF\154822rcu\167525\1042755Y\146978\&12\110672\SIW!\1018541?0Zy\SUB:\1043819\1038648\1036044\tTg\DC2#b;OzD\51146J\ACK\1081019Xo\1018201hJ\1001570P$8@")
          ),
      _iconUpdate = fromByteString' "3-1-8ec4acbf-fb81-4caa-beec-e765ea3d8398",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("i\59711f\DELk\v\1071702;uNl(\EM3W\ETX;g\n\1007122\1092907\\'t@#U\FSpqA$vL\5830\73791\NAKC\\\1082247T\RSR\ESC{\131157\63613;\DC2\1033640\52644\1011095\EMp<=\t\132117c\1066631&\\\EM\1084915\&7\ETBT\31537\1077730L\1058797IqY\1842\1033754>PK,\SOH\1069865<(3\ACK\USp\1058835e\NAKi^%c\SOh\1079603%o\NAK\82986\1084487\FSE40u \aaR\1030565\&0<'~\"\ESC/\1059815\159224[\1044979<\EMH$LLV{\nfze\ETXj\f\ESC\36400\ACKxoi\DC2~\1026287\970y\rv\SIM\24717^X\b!\38182\STX*G\STXm\vU\DC4\CAN")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_13 :: TeamUpdateData
testObject_TeamUpdateData_team_13 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\DELL\\\SO7W\1038011\RS\DC4S}\vj\f0,\a\1052446cz9\998266\162436tw\190024x\33291\176303\DC1\180635o+r;ntv\SOHt\GS\r6\1112383G|\999961S\b\1041427~eR\62992\&9\1024664\EOT.9<\1081249\DLE\DC4\DEL\25486\1017400HF\43904s\53589\96330i\1046457\DELD$9M\50857\39401'S\1077209\&5\ENQn\GS\1035684:j(w(d\r\1068431\GSO\b\RS\170156f\US\991795Zj\171048\1109679\SUB\41722\EM8c\1081627\DC4`\151059w*Cj-\1017028\&8{l\ETB0#\40960)\1074229\v\1070208w!k\135781\1022126\6951\&7\917972\187920x<\37552\&0\1001075\ACK)\1041224( \1014424K\36098\&5ijLj\SI\183472zK\166729c\ar\1050492\1025241{U+\DLE#\187499\&2\rsH]\\'\1081587\12560#\1060646F>}jY\993753:\182678f:M\991209\1103492\995417#5\172275\DLE\139206*\99381U\155843\ETX\DC2U\983347\50942PU\v\60676\STX1\b")
          ),
      _iconUpdate = fromByteString' "3-1-fb2b55d0-becc-449a-9755-ed72366c4e24",
      _iconKeyUpdate =
        Just
          (unsafeRange ("i\165439\1084715\70744\984960r\143191\FSiL\SOH)I\EM\n9l>\SOHPu]\NUL\34711Q##\ETX\185628\DC2")),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_14 :: TeamUpdateData
testObject_TeamUpdateData_team_14 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("W\v\128126\49287\DC4\STX\SOH\1071632\1089152\&0\177175\1020380\1097825N\1096909j\1551\&7]v\DLE\ETX\1064358\1088228do\59527]?\"\39129\&4\NUL^I5\8990{\153487{\f>\SOTx\n!dR8%\1008955N5\vI\139104R\DC4\DC4uo\993229\&2\172393[V\DLE\GS\SI\bn\121117J\177399\EOT^x<\131581W\1080876H#oF(t\tQ\38424\1075412V3\180074\155485\SI*T]r5G\1091385\158397>\30986\99439\1029421qwVi|\1018658\163652^b^/\\%\DC4\142529A\ETXgL\46741Zt^Y\NUL \ACKcv4\189064f\181439\DC35\135778u\31202\nOI\48512\1102654~\1093814\178360\&4\NUL)%X\992245Ar p\1078684\1014480y{DB|]lbI:3$\17570\&1bX+ \1032696N\1021333\SO\984213\r\51699\f~%\"{&\93818j\57610ME\USmg{\DLE\28913Q/\96067XW")
          ),
      _iconUpdate = fromByteString' "3-1-bc1b2714-64a1-4cd5-bf28-769f2726c204",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("yG\164154|\1050498c\1017018\"N\STXj\SIb\SUBH\GSNt\156151\1069016?p,l!\19573o\47847),k\a\991553\52599-\SOHqT@\992203S\EM\144078\&7\38954\r^T:=\66478\154889,DV\DC4\148156\1085560i\8145\SI\171841Kx\ETB\177238\SItLw\1023225\1080752\1062386\STXAf\33665\1048974\20100\1050342*}0\1011133\SO\1035494\1100843en[\133158\SIG\CANi\111137U.\f@\NULCr\13027\ACK\FS@\1027274PX\CAN\EM\SOH\16012\&0\42068\NULA\133179\a5\65016M\1069862")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_15 :: TeamUpdateData
testObject_TeamUpdateData_team_15 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("p\ETX\47602\GS\NUL_\127910n4\1075628\&6V\65148\STX\156622a\1080314\&97\55267X\36536)Y\65341\1035712t\1064872\DC1\990797\1072225\20887G`\tV\ESC0&\DC2t\4414&\984777oq\DLEM\182922)+>Dh7\1011725c\157347\21358R\175842a\991848H\992285\1098926\r_pU\r\ACKXP-\raR8P\EMT5RD\1075743j>\RS<G\EOT\GSt\ENQTu")
          ),
      _iconUpdate = fromByteString' "3-1-f7d6739b-9dd7-4dff-acbf-972b0864158f",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("`Ai(se\1064157q\1013082O\\w\41530z(.\ACK@g\68654\r@\1060564\v\SUB8\SOHc\18063;\FSq\121213\DC3\1008626\STX\177191w#\\<\SOH\94390v\164787\298K\37906AB\SI\16236\1036842k(\1059022\DLE<\1093484\SUBe[\ACKdHU'^=rX\32340Gv\DC34\159768o\141580\&3*\998145)")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_16 :: TeamUpdateData
testObject_TeamUpdateData_team_16 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("{,W\113725j\66867VW,|\DC1-\EM\92324\52301\1085991\ENQSdM\183964~\187744\166807\v\46661\1012290\1008523\11770m\1001938{R\\\21218\184105(\DLE@\1105928Eiw\181379\989957\&7\1088623\53157\vc[L[\NAK\9325_\CAN](H\aOj\993741\FSgdV\179455")
          ),
      _iconUpdate = fromByteString' "3-1-81c16343-0d4a-4330-aae9-ca264c0bf7eb",
      _iconKeyUpdate =
        Just
          ( unsafeRange
              ("\1038370\1000350\DC1X5rX\DC1+j\141026[Jt)\14003\987536pm\1000899\1001410\ESC%y\STX\ETX>\179680")
          ),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_17 :: TeamUpdateData
testObject_TeamUpdateData_team_17 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\ETX\FSS\SIf\100932\&6,/\1002474\132486,\97457\165667D\167566~\152771\127189av-W\n\1083763s(y`D;\17019n\SOHG\"]\SI\157483\"''A\170309G\ETBeu\146845\1100251 pM\DEL!4r\1075090\SUBK\1031588R\10916\DC1\FS\ENQ\vNu\r|{\SOH\ACK\140365v-\147660\30720} 6]\ACKp\992664>\ETX?\170592[\US8\1098891jT1\139047y\CAN)")
          ),
      _iconUpdate = fromByteString' "3-1-8a5fd50d-9c32-494f-83ff-69db6d290fca",
      _iconKeyUpdate = Nothing,
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_18 :: TeamUpdateData
testObject_TeamUpdateData_team_18 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("\1079724g^\SI4\SYN\1058518(\1009158dn\15153\5338\1106457\&8\3255\FS\NULd}W\1077482\1112219a\1045348\&8.DV\1112683\DC2Q!\SUB\1015114\NUL\165488|k\141351Y\b&]P\NAKM\23995{\SOH\US\1084668\8678fEL5\1099186^xy.\1081341\1097387ZD2EOw\1067991\1103136Z\990193v\SUB\17778:U,yu3)*\31312]\61413\&6t:Q\nQ\70111\DC3\ETXCd\983894&\165641p\1107770u|\1097560wh:%KJQB>I\20517W\169935\11540\135417\vIP+|9C\43303XBM\1070327$FR\68308J5d\GSK\DEL\167980\CAN\1107001\EMt'\RS[zmz\ESC-\1090175\1053386{o\153401/\DLE\NAK\1071487\DLE\DC2\DELz~>iz\1035567j`\156674G\rat{\b&\1091867\175116,W\1102256\1102670\1041725\180873G\1032893\1051388Q\SI\32211\RSg^&>\EOT&BB]\SUB\183680^^n\83211\1056047\DC3\33295\RS2\120638^I>^e\1088165\&2\1060054$+\1099972\&2\DC3>&4%4\1049880\DC1\985577M\95025\99763\&0\10709\ESCM\GSu")
          ),
      _iconUpdate = fromByteString' "3-1-a55ba42a-1fff-4720-ab1f-404ac449a8c4",
      _iconKeyUpdate = Nothing,
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_19 :: TeamUpdateData
testObject_TeamUpdateData_team_19 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("{ag\147194<-\41002\"\1080393Ad%\30025\1023746U>\28518<>g\bt\29617:\1083297^=6\1076845\1001362\95768\DC1\1083749\r\ESCIu%b\DC2\b`/-+`\1071102\\\ETB^\ETBw\DC1L\USb?'\1004489\ETX\DLE\ESC\v\1089138\161384}\1078506\\\10356\DEL$\DC4OE\ETB\RS\GS)Vej\1072959\174859!\DC1W*s\DC2U%-\140833KC`B\\k\1048017\RS:\DC4\1095557\USN\DC3\ESC:ns\GSj\DC2&-\ETX.h\SUBJN\1030050x1c\NAK\ACK\646+\SIb\DC2mnp\1075229\ETX\996854)\EOT ;u\169592\&5\EM;\f \6592")
          ),
      _iconUpdate = fromByteString' "3-1-03de6b21-0d74-4107-95cd-94808ece38a4",
      _iconKeyUpdate = Just (unsafeRange ("\133920RaQ")),
      _splashScreenUpdate = Nothing
    }

testObject_TeamUpdateData_team_20 :: TeamUpdateData
testObject_TeamUpdateData_team_20 =
  TeamUpdateData
    { _nameUpdate =
        Just
          ( unsafeRange
              ("C-\187649k\SOO\1110183{\1073314Qv\1084755fQ:\5820X7\1031889H{\SIRmi\SOHZ\1063875:\1026588p\69234@!\1047114l\GS6\178911\1067979\ESC:\DC3\1104493\169362\DC3p=\175710\26656\SOH\139854B+\18241;u\1076014q(1a\1106792DJ\17605\160683\&7DC\1033041J\DLE\ESC+\1004313sg\1036363\vZ\RSK]")
          ),
      _iconUpdate = fromByteString' "3-1-0646b16b-0732-4b69-909e-1bcafc3b7baa",
      _iconKeyUpdate =
        Just (unsafeRange ("~ndy\a\1057180\&1H2\185151\1005778\ETB\n$3w\164759fsE\24634\9638jjG\1079112")),
      _splashScreenUpdate = Nothing
    }
