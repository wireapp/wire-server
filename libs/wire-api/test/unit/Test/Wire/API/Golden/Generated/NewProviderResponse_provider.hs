{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.NewProviderResponse_provider where

import Data.Id (Id (Id))
import Data.Misc (PlainTextPassword (PlainTextPassword))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Provider (NewProviderResponse (..))

testObject_NewProviderResponse_provider_1 :: NewProviderResponse
testObject_NewProviderResponse_provider_1 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000001d-0000-0013-0000-00210000002c"))), rsNewProviderPassword = Just (PlainTextPassword "\138212[\1082070\DC1p8pHZ\29723M=\1043373(\SO\131480`x^\33407\184340\70093\t\19287p1\GS8B\1057223nFIuy\1068405t\1021942$\25186W\\o\142655\RS1\1084828j\1032739\133945\STX\1042020h\96420wi1\1070943\&7\12671\1084658`\126472T=)-6)%f1Q\SYN\1023011KD&*\SUBJ\986931\985888\STX\160818\69797\DEL4I;J\1062600,\159971\an\3275\13542e}\134953\&3h\DLE\DC2n6\1054550\ETB\ETB\DC1\1078866\RS\bc\EMKt|w_\f\1093382\187749\&0l\DEL\988035z\47132{\SI8\1005551\178976xqL\DC22>KVNuWk\\*,C\CAN(\188830\1054405)\1041136ywK\184067\166077\1098197I\23817>\1030026/\1043223\1041775\DLE\CAN\v:\136297\1060327\&5\FS\60000\1067017\FSYcK\1084187\143092T\1022344~kZH\30896V\1053398%\1025175R\t\1036947kR7:1\DC4\r~\46163;\1064061c\150551\1066250T\18604\1050741TN\137473H\GS:qE\n_\19012\DC27\ETX[M3\ETB0\1007017b\29148\1023744ZaZ\133404\1076592\DC3C\31899\v\187154y\1039076t-\1067572\&8VZ\v\186206A}\DLE\1107535\USL%J\1021982|\ENQA\1056579OY\DC4\FS\USa\120625\96272\1103955\&1/\51238/yV!b:\SO\DC2\"w\67699o\DC4\34388\DC3\DC2G*\1059727\191235\24912\EM\10540\DEL\144258\1054269\175926\ETX\"\SUBv\1082956;\989491y\44695\119661L\1017538u\v,\SYN\159965t=\SIR{>\NUL\19349\1070542\60751\999120;]\1053198\1044506\1057260\CANP\67366\1001748coG=\CAN\US\1014511\a\180084\162281\ETB*\177541\EM\20803\a8\fC\CAN\188800\rG\ETB\"]\1108090\1042950V\1014992\DLE:\9239\144437NC\171536z\fGgD\DLE@\ACKu':\ACK.0\1064350\NAK\SYN4\1073143\74776\r\DC2S|tY\40210\1070961:\100205\n4\134018Q \18123D\62039\989813B\142958,\1004136\190489\SOt|U\23856\&4\GSs\1080418*A?![\1063357\ACK\1079431>\\q\21961\CANn\120341c\29668IT\NUL\ETXBN;\9933\&3\52688\GS\DC3\1008367\SYNd\186705\&1t\DC2W\DC2\ACK\186433Q\NUL=6r\7836J\EOT\1079466@_\1087887\1038981l[xQv\NAKIb\r>/Td\GS\SYN:\998371\1094244\t\SYN\USDI_J\SYN\992565S\aC\5486\v\189566Z;\SO\194666m$\SUB4x~8\DC1\1022653\a\EMhhd\184558juGj!@ML\1088412-/X\1044395(\ACK\1055896X\37186\SUB|\f}S\163604!\1093825.DZQ\984192\151502Fo:4\26045B}`q&\1029492\1105700\45469m!wS[\1080960*$\45686\&8\987971\184543\1068840\39210m&X>\1059492\53035\1108711\1075852#\NAKRD~\US\1093122\&7\1052020\127964\180226\3559\&7'b\n\125080\v=1(\1068369\SI~\1104211\45179\STX#'a.X]\SYNtOW\39046'|e\35527\nG\147096I\f\150806\ENQ_\1100915_\ENQ\995095\163296\DC3\1060897N\5391\1023349\&4\n\53249\v=_\1101321K\1090586b_\171219+\ACK{\NAK*\1042558m\178486\&2aF \NAK$m\66289[::\ACK4\SOHG^\SOH9\148570\184675BvhE\n\1085245\61886q\EM\DC4U[\1082739\&5Um\131793L\54268\1015\n1\59799\r\DC4\165398\3088\&2sM\171691_B`d\132349\ETB}\to<\28373\&0_l\1013283\DLE'c!$2\SYN\133150\bFX\STX\RS\27526\1014674\1062904Q1H#F\121131&\1024058\147528\ENQ%=\137887_\166376\a2LHu\ACK\149657BX|lT0\994969\rY7\1017402Q6f\984562\168756\48627\185931*kU\EM n!\DC4\94586\DC4\rY\v\172116O\NUL\"-B\999538\&7\34674L\149633JqB_HFq\STX\ACKgtf\18120\&47\SYN0\ACK\DC2\1079614(>lOos\NUL9A^Mw='\RS\SYN\1007442\&9\v\DC4\156813\1009598\27686[\ESCE\30314\"\1046242\DC2.m7n\f\ACKJv}\137548LI:Ct\tNO\f>\ETX\ENQ!\t\1111513|-\2792{H[\1016313q~!\EM\CAN\\or1\ACK_0.\1006891->\3608A\SUB\25940M1BN~\b\1035987Sg\b=\ACK\1061834\1040713\DLE\DC1\1143'6\FS\1036205.]5=\995135y\1088921>zw;\1049155\34930d\63777\68484\1073559\1007539\1059519\11491'?(1.,\SYN\t")}

testObject_NewProviderResponse_provider_2 :: NewProviderResponse
testObject_NewProviderResponse_provider_2 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000060-0000-0001-0000-00660000000a"))), rsNewProviderPassword = Nothing}

testObject_NewProviderResponse_provider_3 :: NewProviderResponse
testObject_NewProviderResponse_provider_3 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000001a-0000-003f-0000-003500000069"))), rsNewProviderPassword = Just (PlainTextPassword "U\DC3<\FS:\1055837\f\NUL\1030423\21688x\63687\185235\172632lw.W\\W\NAK\SO\\Xk\EOT\1106631G\1057402&\1018546\DELd\33650w\DC1p9\148200f\1033632\ENQ-]\157035\NAK\156753\182277\147513Lm\1060882Epp[\42118fq\1094307\172614\1112918\SYNM\NAK,!S\54308\GS\1058361\ETB9\r\1070070\ETX? \1087043:T{\52641C\1022936\12060\ETX\167155\fd'v\1010270N5\1103279\SUBe\fph7*\RS\1044638/1Br\NAK_\173\132738K\73060w<[\97377v8\1035947wY){\1096914\159315\37970\1076780\1044951\136701\ETB< \ACK\DC4Bi\139268G\1001751\FS\"\142742\63162\1023974\1009763m\1079549\1018524\1109082\&5\NAK\ENQo0C[e;\997701Xh\133384\12354\&1\1044141U(I5\1071453\987926A\142352V\1003510\STX#\\Fn3\1047889\DC4\ACK\ACK5#/oGCZ>2\";i_\136456\983895q\n\bwe3\9438\ESCr<\1030309SMx\v\r\ENQYz{\DC4\v\NAKk\b\1105881\1062614\175129@gF\1018445\1036543\1011732j|\863W\10189-\RS\1093095N(\SOH\1033043v{\SOI\ESCat*\EM&\1086808ob\48726\nI\1034239\t7N\17443\SUBG\1112408'\1104782a4\1049666K?\984578\a3k\FS\EOT\182555\bN:'$Z\1036469h\1061943\b\1070954\1064837FCd\FSy\52431\ENQ\FS\183488?W\DLE\CANp7h=x\NUL\281U!\157347\147744\t\1102817<Jau3\NULS\NAK\983758@\SYN\1004792\&2=\DC4\989861Z\v\78362\DLEp\137251\ESC\1092679u]baX8\166990\1032721\DC4:U24:\13062")}

testObject_NewProviderResponse_provider_4 :: NewProviderResponse
testObject_NewProviderResponse_provider_4 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000014-0000-001e-0000-004f0000007d"))), rsNewProviderPassword = Just (PlainTextPassword "v^y\1081881\1068628]\USp7\96196\&4|\DC1\EM\NAK\145943-Ds\SYNC\176987\DC4Zl\16087\SYNuN\4779RteV7\77988\52531\1025345\1946\SUB8\GS|\190979}^h+W\aO\25232\t\59678%\t$\ESCWyt\136005,\21927`9\ETXT\ETXX\\\DC20\1088212s\175766e\156465\fX\60660\ENQ\SYN%\146138\&6\1024970y\ACKoY\SYN|g\40820\&0\1098322o\1092251M\EOTe\1060932l\1011293\1075150POJL^\1022443\1040611T,\SI8drM\146275\r\194676\1095963|,f:\33125w8HtB3bjk\ESC\66794iGt\DC4B\992132JD\152691\CAN\ETB\986054Q\1042875W\181879\1068990EKF\STX1\38612\30706}\184664L6U*\1102372Q\1090186\FS\nn?61}\SUBg$g<W\139281\&4\186677\t[I\145168\RS\99464Sw\1086220Ya\SUB\ACKS\1033334\DC3&\FSo-\1103189'Xe`y:m\v\100744\41565\ENQ\SOU)D`\SO\139860\DC2'skw>")}

testObject_NewProviderResponse_provider_5 :: NewProviderResponse
testObject_NewProviderResponse_provider_5 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000030-0000-007c-0000-003800000004"))), rsNewProviderPassword = Just (PlainTextPassword "K`I\1032122\v\v9CR\vCB\44032\n\ETB\1080971-\184952A\DC3E{\23976\1108295\31480>w>s\174419\&6F\ETBEj\DC2\1112922\5700\22504\1051572\SYN\48694~\1098632\33937\EM]6~\1042505\1080067\SUBG\992754@\144281 \";%e\US\a\986429\173782\DC2)r}\RSz\STXu:8\2013\ETB\1106259w\1060131\127318\SYN>\SUB\ESCfLC\t\1042789GO\1017427a\t\15131\US'6\70130\38312\181760)\143993\996555Qv\29719:$o\128256IN'4\32506\&7\24568\DC1\\\NAK\f>6\1055019\"6K\ENQ8\19156\&38\CAN\SYNJ\DC4\1067119\1058023 E\DC4\35353hk\1004971\ETX\ESCw\42692ed@c\1022322\CANo#\6275\1057346r\152854Di\1062244\1019013d7\18410\DLE+%\1095623K\78676Wh}*\f\157471*V/*\ETBcl\34514\986569\19161k4\DC1D\DC4\54645\159498\1088536T\US\35130\f_]\ENQ#\b\RS\164933\143980~\28418\DEL\20140s\n\1037109BIa3\997264\&12h\nZ,~6\1051611N\EM\3776S8\v\126483*Z\153582WaYm\169836S\ESCCO\60320\55044\"Nr\1108281q9+6\1083317\SI\DC4x\SI(<U\r+\ENQ\1020752\\g\v\EOT\fY 3\67986\EM\179156\1035353z.+`Wr$\SI%k\180665\120707|\f$o^t7\tPWU\RSa\ESC\1102863\154038HXi5\159229EY!\US\183164\1003506\171874\RS4\1021317\1011448.\129656\1108991@\26619|'@\1110849u+R\RS\1096361\&6O\1047882M\44162[MF5\1066747W\1074746\1047867(\ETX(3\ETX\989844\DC3_9h.J&\rKk5\999907\a;J\143473\14714\CANC\GS\143896(4I\1042785E8\1105318[N\139643\926\1007137#\1091732\EOT#\SOH\49197\SOHgcsa\70476H\SUB5>&BJ\DC2\1015336\1082932Z\SI\1086314+\1059098n'\ENQ\FS\SUBC\FSK`F\146519<)Rl\FS\SOH\1089\989681a\1008472N\SOI\ESC`j^\RS\1088348\&8lx\NAK\SUBZ\DC1\46121\&1\SO\DC3\DC2\137204\ESCs\SUB\1095365\1096550\30228Mu\1036279\1059875\1025297Y\\b_\1058884j\156794\n\991844#\176907BpC`\1090453\DLE\SUBh=2V\1031360I1^V\7737\FSw\RSN\NAK,|?PE\nnS\66455\&9\1070635w\1104327Mc{NbuQHTeA 2\141584]\14488\&8\DLE~^\f>lA\GS\15373_\165\138717\154295D\158320J\1019877\148490\&42\1112182%O\RSk\v.\DC3v\1072504\20020y@)\989449\983903\&5\t\64937l:B\SOH<\1011816\SYNr'x\1011365\&8v\GS\NAK\SI\61835\24965vd{\1007773Z\t\139836Evah{\\|V\NULWh\1075431\US)~$\183370\993019#\r\1112685\5314\US\DLEh\1072923\50190\n\US\984257;Y\SUB\USc0*z|\bU-\1060937pgrepF\EM+TMCL\DC4!w\149591\DC1\"q4\DC1r\1099732A\NUL\ESC\RS\tdP:C\DC1|m\1021538\1051724=\STX\96764iMkN/\1021634+x?\EOT3N,\140980\&7\167789\&9\SI29ch\73983F,#`q.V&jF\DC2\38222|\1091027q\59326AFe\1108954]\v\8527\68247\EOT^_?\ENQ]\ENQ|y.\FS\985814f\179165\t\1086909\SUB[o7J\44822\DC4\abTt\998335\SOH\US\1061559\NAKvl{\EOT\SUB\176132\1007737\&0p\185453r0\bj\1078215~7mFE\"|l_0\CANOt\1065399\&2\t\"h[\1029990\RS4k\STX3\63898\1022953;\n\FSX\STX[#\996056\SYNF\f@\1056757-3\47636\1092415~\163138\&1\ETBt2Z4\DC2\32681>\EOT|/\CAN)\n?<x\188164R\EM\EOT<\100042]gG\US\1107754\1088616Ae[/%\152124)\US@)\10272\1019040VR\1021580\\\1102853Te\66184\DC1Y\"j\1027677\1061332")}

testObject_NewProviderResponse_provider_6 :: NewProviderResponse
testObject_NewProviderResponse_provider_6 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000080-0000-0040-0000-003400000011"))), rsNewProviderPassword = Just (PlainTextPassword ")\984271\20599\SYNs>g>\984781\STXB|S\FS\EOT\b1N7k\"jg3A/\ENQ`|-t\f\1098225ZCb\38163\98012\1100210@pN\EOT{\SYNpz\STXAmAn\CANx\1094784\SI\EM\985088{JA\155838\140449k\1010379o\1066863\146988\4403\146168\985629\ETX\DC1w-%\US5wg;/\1015621\RS#dZFHtV+\t9\1112260\ESC\42755C\\e\96608f\vs\NUL\145260\169629o\1004991\68610#*,\1048482\ENQe;\47663\DC3\t_\168590'\b<0\28931+\1056885\1076420\t\1085492\1047003i*%k\\3m\999742$b\1075285b\US\bHikq\83524\"+S\US%\163377\SUBc\GS\NUL.\DC3\1100315\ETXw\7845EeF/W\188253\v\998044\&8\42528(i\49282 \DLEL\983821\1042779] -\NAK\ENQ\1074685UK\1093335f\tu_?3\ACK\DEL\vl\1066716n%\168928IW\119103\&3\ae\988596jZ\26730yU1]9\n3%M\\\152553\FS\983270)\DELP(\53079\US\"\1011331\171767>\984664\vkJS\SOv\NUL\1070508\1001694\984041\SO\DC1,)\DC3O0!VIJ\42024fxRa\44520\&6C5\1016544l\18451|\ACK\1049870\SI\ENQ`\bD\28847&\EM\1036800t\US?}\DC4\137694,\161747\51197UM{j\1005584\SOHZ3\157339\&0v\DC1 =s<\1010946b\141570-V\1056895\&7\178850N\160477,\1091265\1049627\SO>0dV;Yg\20502\SI_\\JVY1\1104658/|\1044099+9w<O{\733Rn\1056312HrD#\1040639z\STX\48492J5\133451M\EM\SI\DC3\1003369\US\27187xX|+\99733Z?P\EM\"z\DLE\GS\141615\DELE\STXxK\SUB\50174Ep\144426\FS\SYN?L\23725\NAK\b8\34360\1095284\996977\ETBgW\10782\ACK\aC;e\1098496\50807\&8\1090127\SYNUO@\8740'\33657gg\178950n\1024917\EOT\NAK\tz8\SUBU\GS3\DC2,4()y\187341u\US5-\STX@\\\1051926d5[qq5T\NULIvr\62683\1062669\CAN\1105070KOY\997527\1075701\ESC\f)\ESCG_(dT\"\135727)\FS\CAN;\22722\RS5\a+X\STX\1068396\GS\1010144oW\1039320#W\66835\173858\52671SFK\taU\1397\DC1v.\984526\&1\29174\181765Q\99877ba\n*\988885DNPk\1078313\SI\DC1N5\47965P\1042522(\"A[^R\CAN~'\983109h7\GS\1041211\SOH4YUHy\991441Zq\1083568\GSo7]\RSQ\137971\1011819;A*\"z/\983102,c'\ESCY\2891\1048616|2bO\1064185s\SO\1111630\RS6x\b\1000907yL\DC2M\1020740,t\161844xdB\US^7l4\1042073};\NAK\917900\SOp?\SI]g\1089505\EMk}@@d?\ACK\SI)j\ESCAk\14989\1055229_{PE\47647>r\SUB*!EV\"7fuL\ACKf\94000\1050214:4u\t?\US\a'dh/\STX)L")}

testObject_NewProviderResponse_provider_7 :: NewProviderResponse
testObject_NewProviderResponse_provider_7 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000023-0000-001a-0000-001900000007"))), rsNewProviderPassword = Just (PlainTextPassword "5\1092933o\20001\ACK8U\1095864\&6f\1034622\EM@\189365M'M\1007596\95653\171599#H?\994372D6Md\FS\DC1%\1010873\a\DC4;Y%Hb\SO-\SIt\ESC\SYNIX06\r+7Lq=l\96279M1_?\SO\1002400\SOH5\1090065J\DEL\1084934\ETXCS\190331\1095730z,2\EOTB\ENQ,\RS\SOEAS_\SOOs\n0:n\51887.8\28383]\\")}

testObject_NewProviderResponse_provider_8 :: NewProviderResponse
testObject_NewProviderResponse_provider_8 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000005b-0000-002c-0000-006f00000060"))), rsNewProviderPassword = Just (PlainTextPassword "w\a0|bL\1041357>pq\14334a6f\50385cE~\ACK\DC3]u^\31448\992480VE\155770pCtGC\FSe'\1000113\8681\SO\NUL\38836\1046332C$ck\1113411q7\RS#IK\a\1902\1068299:\1037089Y,\DLE\1010276\NUL\SO\SI0U7\162670\SO\DLE>\62663c+;r\DLE\1011919]^\n\155935\&7cmhg?\1055991f\1033834M03,3#N7F\GSEC3O9A. \CANK\DC4k@\1100987\59889!/oKv\35560j\b\23513<2V)\63885\&6\190852\1025230{u,\NAK>\145163u\16221%\1099463odLyX\1086528z\1934\"\DC3\43017\157283^sjT\176583R\1109666e8C 1z\DC4\a\ESC$`\\5\DLE K.@\141310$Vki\33498J\993401\STXT\157592iy\SUBF3,\US?\1032412,\NUL\44298<7\194563\1007500xY&]}6\1100863NsX8/\DC2m! \ETB\ETX\9857:@>\1001703>-Qc~\US\DLE\GS4+U\EM\1105092\153785a\1038694n\148671\SO\tU#S\EOT\45479\141125A\FS:\23376GCK\ACKe\30922|,\ACK\1107048K\ETXUkS")}

testObject_NewProviderResponse_provider_9 :: NewProviderResponse
testObject_NewProviderResponse_provider_9 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000002f-0000-0043-0000-00590000002f"))), rsNewProviderPassword = Just (PlainTextPassword "\1017623-\170417I\1073880\1079299:H7\73769\&1\DEL\1011215P\1111716\191385\DLE\USc\147301ERN0/\1007222-$Jo|\1113499a*\DELd\GS\47078+\1112334\1090143\&1*9j\1060438A\1097559\NAK\1074913\SUBMy\DC2cD9\1022888\1063555\DC4^Jjw\NAKvA\ETX\NUL%\1069500\NAKv\NAKu\143421\1033237\62936/|7\ENQ\176376\1039199\1090687Vm)\748*\DC1\1037993-C\1096341x\NAK\v\STX 9?\1027486Hxbh\24647\&2O\FSRwLw\FS\59665\140292\"L\188953+`\16674\14828+\DC1\ETBb+\v\1020111X?\1068966\1108715\165032\ACK\US;f\26706j\ETX\SYN\27846!;~\\q-\1101499C>\r\13051)\EOT\DLE\ETBVH*\1022877>)=\a\NULp\187391?_3yA\1000130}Uj8)\RS\SI\190547\1000959\CANt5 bBI\DC40gs\1079998\&0,+\176371\NUL^e`1\SOH3\"C`n\US\988424sM\1068774\181834\EOTi)<`Q3\36378|\1036759\NUL,z1\95394\&8u\b\37396p3\40124ty\987708\23678\158073\1077193F\183075s\SYNdnr\EM~\1031005;\176121\DLE-\US\1056632\37044\EOTS)1d\DC1s\1081234=\ENQ\139383\45860\\qQ\1025253\1063416\1011241id0\1096482^/\SO\1049155\EOT\STXz\NAKQ\991965}m\1032905u_\SUB#\37646'|+fW\142886\&2db\SYN\1059598\SYN\58628K\a5x?\175246\141042\25637\50193\&8q\39942j,(\165690W|,\SOH\f`\1091911\RS\1081342``i;1R\1057606\1075652OkM \ETB<\STX\ENQ\1054951\58357Y1\fPPLa+\DC3e*\RSP\EM7s\66725P\1095199O\148627}}\1033919\US\DC4e\988074\&6\993604\15117\141567\SOT\f\1066032&VWrL\1014145vW\ACKW\1043861'4\1074359\f8f$\1044125\59097\1065470\1073021?\ETX<\DC4'2VX\f\1031453i\SYN\58680A^<4uZvD$RRuz@\GSb/\DLE`Z\48797t'\SO\DC4K\1095174\SI\1101267\r}\994070\145225\STX\"\ETB-\1066978?x\174128TF\CAN\\f\78660\7986K>'\CAN?5_b")}

testObject_NewProviderResponse_provider_10 :: NewProviderResponse
testObject_NewProviderResponse_provider_10 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000071-0000-007c-0000-00680000001d"))), rsNewProviderPassword = Just (PlainTextPassword "\1048701m9\NAK!-=\f\1061646\96960\SYNg]\ACKC\DC4%\1074261f\SOH\46137:q\986397\&5\r\ACK\CAN\50922\FS\SOH\EM\CAN\SI\DC4\148027\10570\DC3\b\DC2V\SUBE\1113465)\\\987516\1112790\1052602`\32710\61505_@9\141069\CAN\ACK<\nnt\CAN$\ETB&Ty\1009812>\1080244\119172\138090\99297W\1014152%Yw\r\18504\RS\ENQo\SO'Yi\11499z\1042745=8o\SYN3\"'&9Cr\SO\NAK?dW\DLE'e0m#\92409\\`4h\137612\DC1,\1079782\1038558#\SI,epa\1080883\158772\94806::L<l\1074937d6\ENQ\1042767\ENQ+a\RS\169139\GS\173277\11923I\1088614dr\CANE\9442*\1070161\&4I\RSv}s\30090\1079851\SOO\176473}\1032837\SIWK\61453+\ETB\1100670/\2860Vu\"+\917925J\1102399\988541\985586\DC3\DC3\vRy| \NULq2L\1073654\33429wP\SI\1100599\&0>&3\NUL\1005587P2,9\153294\EM)\30474\&8\1106873\ETB/-\DC1\DLE\US\994231n\DC1\34565\1081971h,Y\3266f\CANxN\aJ`#u\1080006!p\1104595BPM\64345\987805\1063126(I'\145201\1027533^!]\54361\r@yK\186407\1025019\RSF\1083246\1027354\37049/\CAN\NUL\b\SYN\b\1053210\&0\1044038/\NUL5>m\\\1007670AV\SUB>'\ESC8z>tzm\1039869\DC3!\NAK\SUB\SO\139294\146437sH_\SI\1028114\DLE@O\CANO+\121271\ETX\329F\RS\ENQ\DC2r\190233\40286~\1027557\CAN\DC3\twej\987756l\58928\SOH\EOT\1048137i\52393Vn\187739\&5\1070931\1093930V\DC2\US\n\63582\1068693\1098294H7b\133809{0Gm%Jl\n\992688\98100\\g>\1028133E\71478EG\SOrX\1012985\32794-\ESC!9p;\EM\45343H%\FS\DC3\DC4H\1046506-}\EOT\rm\DLEa\ACK_O:Ch}\178361\1023498R\137757\1073369C\"+lN\NULRC\78540\47971\139193>\DEL]\1028169+\163752\18298W\EOT{\a\"(J\140114+\1056956&\17359\1090368b\DC2\4828\152968+4\DELgO)CG\1093523\1007885tt\a\CAN\1071475oo\r3]\1060308\1084026l\ACKeJI\n\DC2/\62523>\SO\SUB@\38154cOX\ETBq`\n\DEL\1077276<^.\168043v\987250\142611di\1028462\ENQ0-,neX\DC4m\132418\&9)\a'3\190678\v&\19684@@v\1085276DL\1004211+\\52e\v'gK\1029348\NUL\29568\121198\1019231\1084180\&0\12067s\135906R\EOT9-\ETB{@\1108545B\SOH\43195\681q\173037\46382\SUB/\1008864Ga'ep\21847\72250\149851\1069409\&3\t9\t\992917*\"s4e1\1080399\&5u@x\SI\1092505T\a\FS\SOH3\GSdQ)\1055250;*v\1074731J\CANq\20976\1075946\1004990Pls\51000%\1051701L\182762q\21932N\1038559&\v\FSKgzh\1094884\&5t\SYN\1009006;\48207\27988\&7\DC4,\EOT\1113125\r}gD\DC3\1063350\rg\CAN1F\US:\NAK[S\1078198\EM|\EOT1nE7jeq\136364\992933\142920\65285)E\f\b\ESC\1027040\f\a\STX9,\SI\US\1032618~Y\ESC\992913hJ\1056354N{qm=W/+fZ\1097767\b\157383>H\160989\&6S>>\1091581lD\1081514\995028j8\SYN\1098166\DC3M\1059316\EOT\rO\1034516\DEL C9\548-\RS8\176289TR\fihwWTkCZs\ESCl\ACK[\21520\ESCO-\23136/@`\n\29582\SUB\SOH?\1012556<Y\174647:\1070818\&1\fn\63658\1094310\r\DC2D\NUL\1022767\&5\1032799;S0\SYN\1109235fv./\1005067M)\59900k7l5\"\1004788e\DC4$0\54950a|\1075031\1107372 \150613\ENQQ(\SYN\123164\FS\SI|\SIC\998623\&4\RS(\v\ACKr\187861?+SnM\DC39\993315El\GS\\},<}nV\1058978\GS\1098031Hm\"\anZ\FS/Ul\r\139407\\\1053529\1076881l \95056\989820|Dv8H\ESC\DELPwzVXdN_S\1022596\48361WQIA3/\1082008n5\35045")}

testObject_NewProviderResponse_provider_11 :: NewProviderResponse
testObject_NewProviderResponse_provider_11 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000050-0000-004f-0000-004000000075"))), rsNewProviderPassword = Nothing}

testObject_NewProviderResponse_provider_12 :: NewProviderResponse
testObject_NewProviderResponse_provider_12 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000079-0000-007b-0000-005b0000003f"))), rsNewProviderPassword = Just (PlainTextPassword "\170771`Amo\GS+y\1050809\1105987+;\ETBx^1D\34691\bcXW\51918\CAN\1056098!fZ=\74380g\t08x\1080643:\1082797E\148141\149217\1023008\ETBy5\DC4\180913\rMXH\GSF\DC3f\147582T9^X2,\aeS\"\ESC9NMkI<_\1077295\988670\19055\51464\DELto\a\DEL%\120795c\1027208\32364\ESC\1041058w\a\1093857\1033589r\175822\&6\1102032Scx\r\1071627*psUU\nao\171935\NUL\186267-+3!=?s63\64074si\FS\1050566r\1092820g\1041225!\1003716\ESC\US\r\1026580+(\ESC\US,{\1113294u\153315]c\73876\DC4\54372TL\\\16208WEFL\a\134138&I.\48643\&7\n\ACK\50316\995975F\v\DC2\EOTQjq\NULP\DELbTu\134957\987684c\1069034\FS\128056.\1050421\137267\DC1L\STX\996868zN\15850.\991408\143969-@\DC3'\ACK\NAK\41148\&2\SOH\1009627\&0d\n\1108308\&0\b\45428\&6\191392\1041537FeXp6%\101082MP\DC3\1092841\1056694\US\1026337\140374\149992\ENQj;\174560\1033218\&6\113799\"u\FS\165610\&0h4L\GS\74854m\RS\ESCc4\1018269B(l\FS9\1006756Y\137665\&0vi\SIKZ\158952$A\25672m\EOT\141730o\1024725/\50056\162152\DEL\98773\ACKp\SOH\137163\6820y\NAK\1079334\78718-Y\n\2763\ESCHb /W\ETX\DC3B>mmPX\NAK\1057622g\CAN\SUB\190853\184963\&5\142823R\1027339\GS`5}\172782\FS\99600\f\FS\SYNK\GSxIt5D2\FS8Hg\RS\rW\146535\DC3\20417\1076627Ro\48821\SON[GAUQ\22020Q\988184\"&\176085\1069113\1055338@\SOH\4900&O\60553Q(B)\148878\53902NU<*;\DLE\f{x\171325%\137622u\1102622S\ETB<W&<mg \16960W\189221\&4\FS+\1082959a,\995239X@4\1089388\"\1031499\1078767S\1065104\EOT3\1017260\USxA;:f_%,i\62939\GS\177362h\141629t\1077097,\1053173A\134768<Qt\156470u\1051454\989270,\143561\ESC*\DC3+m<r\1075887\NAK+>ng+\189490HU\40443\&9\1098608+\v\a\axr,'%\EOT\147569\r`\SO\NAK\n!2\ETBk\1049271bW\42873e\78605\1069671{\149832 \a\1015719u\DC2\47781fh\1075203\ETX\98198\NUL6\182414<MS]$\SOYA{\b37`\152804\1041226\42702\1056583\988390\180451\1003431\1069497\1039268\&9\b\DELnE;\DC20n\6039\94387\151091B.\DC3G'C\\\EM^\51681[\rSK\11852Qqn\DC1d\ETB\n\985801\1057124\1078980J\t\1081446\1072378\FSDN\36314k4\NAK+li\126604\1012063\1000139\1100258N[8![\53517\DC3dj7^mm\179628S\ETB\EOT\n\165355\EOT\49097B\1031286LY\RS\ETX\1051023\&82L\47121\EMx_\43218`r\54426\DC3\1029918\1055463\v\184790\ETXW\135816e&^g@\SIFf\177898\1089300\US\FSyx\7236H\t\14393\NUL\blz'\1045974\NAKg\FS\DC3\NUL\2274\1006615q\"i\1057782!&a)\ACKgfk\136627B")}

testObject_NewProviderResponse_provider_13 :: NewProviderResponse
testObject_NewProviderResponse_provider_13 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000007-0000-007c-0000-001d00000074"))), rsNewProviderPassword = Just (PlainTextPassword "\17715\SI7'\1082998\&7\ETX.\166186\7424\40537\43646V)/_\SYN%R\r6\DELJ\\\48440\vA\DC4l\SYN&-\149507)^\NUL[\171400\1056233\a2\ENQ\STX\1033949\SI]\46999i\38342=\t\1027939 2\31585\1109608K\DC1S\DLE\f`c,nJ\45090\168045.\1083422\1009005}\1062182\1059160BVI\1031432wo\988705\\)/\1011701\&1\STX\1098100\a1U@\1010007o\1096202\29485(<:\FS]\1113782 B\1087025!F\154333\DC37LM\ETBTGK\STXl\DC2a\CANE)\1041531\1096597E7\n(vs\24354\v\45902]\170937B~.h(`\1043080K{\SOH\1053584/z\1070849\1018534BAr\NAKL\14249\1071766^\SOH\54336\1014750f\DC2\DC350\SUB\SO7\FS\v}\v\SI;\ESCN\ESCK{q^6$\ACKu\164551\62896\1036395l\63513<\174794D\STX\999719\DC2r\ETX~&\1057930'Q\1084916\&4eWZw\1073803\t=\CANk\1107565=-\1026507\46904c[<e;\1066213\FS-af\CAN\EOT\FSq\150331\n\30727\143585QBp\995419\141410\74851;*>tb\1061469AKkyu\NAKd\RS\172284U\US\a\132925\FS\996923@x,w\NUL]Hv\185497\n\SYN\139641\1083937='@L8\28425\997304\GSTvF\NAKcF\tb\ni\47348p\1045000`\1053503K\1047025\1065259zOS/\1091190#^fE?d\1031273.\SUB*0\US\120731\137529\65724\127541u\1066706\NUL\DC1\CANP{8l\173947@{\SYN\1092993Tx\23842X\161315\1029837\23654@>\SUBSf>5K\72389\"*.\42366\132149\SUB2z\DLEc\1036641P\ETB\NAK\ETB;\144113J")}

testObject_NewProviderResponse_provider_14 :: NewProviderResponse
testObject_NewProviderResponse_provider_14 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000001a-0000-001a-0000-003000000051"))), rsNewProviderPassword = Just (PlainTextPassword "724=\34304\992526&f\SOi\DELez\SO:\1024059W\134230d\46325\141726O\SOH\185105\1050604cs\ENQ\68828dpspp#\1100148_\b\bY\1050723&\1004106\ETX\STX\989453\US\1009575\11840$s\RS0\f\1030707\167487aV\1084346_(#)\DC2N\f\158415\178760\SO&\189209)l]X;\128037\57713\ETX\1098191\32267\GSA3\EM\a\32135_\DC4\ETXaM-\GS-\1058649\160113\&8\ESC\51962}q\\\DELW^\120936Dm\127366\1013089V3\DC4\f$VJ\SUB\r&D;Hb \DC4\1009918I&x\SI\1046375\STX\63934U\DC1x\43692\1089418\DLE\1017551R\173377\133432y\185740,>\1049170\"\NUL\ETB\57794j)\174031\USFP\162987X\1058411\&8!<W\FS~~?M\162605V\SOH{:\28534k*U\DC1Nx|\SUB\NAKrn\1081435\1086409\EOT\SI4R\1044922\171650\35080\n\1050733tLv12d\18095\"67\185257\990171r\1087315\1045987fSE\40018\151402\154743\SO}G\SIb\1007449a\1093056\174367\1022130m\NUL\\\26915f\1044232[\SO\DLE>lQ\EM}\25577I\FS\FS")}

testObject_NewProviderResponse_provider_15 :: NewProviderResponse
testObject_NewProviderResponse_provider_15 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000010-0000-0058-0000-00560000003f"))), rsNewProviderPassword = Just (PlainTextPassword "\1091642om\132322ry\1056061B1\"\DC2`(Z\ETXk\30043W\ETXO\1112372\&2\1085084\SYNX\97307L\DC1\vt\1096512\100044\&1}Q\SOH%oCv\7377]g\13125O0\SI*K \1073473\984176\STX\47770\DELbW\163314\19913V`1v\ETB\177150\v\t#[HT\1101777~v\140600-'xVUw\1083894,yR\SOm\92516M\EM\aYD\175755K\999758\DLE&\9169\DC3\119610=\164640>\1100386\SO4|\1080989K=\64562lD\1567>V\1085709oP\1066359A}A\144421\FS|\1058126s}}12\50438\1063104h2\NAK\DLE\"&sc\an\ETBm\ETX\n\NAK\NUL9\ACK\SInOuk\1073566Pc\999541\EM\SO{\GS^-\RSI{\ACK.pg\31455}\1079199\tyU\188276\175045\RS\1016175\ETX\DC3\SUBa\1055256\bh5!\183396\&5\989379n\1056705xd\100307X]\188498\42434\987030\&6\STX9n\162457e1-\19416<u:+M\1035389V\48387@hk\CAN_/\10465\1074904w\61879J\1059037\162800\&9\1079972=\172843\157456\183675\14458\ENQ\127750\988962\CAN LF\144831\1098705u\188157D\SI.uN[)\1004236Hc3B\DC4\1093590l\14903\SYN\141958\51435A\v)=\1110579\EOT\1105978\&63\8369$\DC2\1023438~\38872U\78231\10108^\1105025.F3a^\183250\78647\1031350d\34037v\139580\n0p^\US#\41895$\GS\\Z%7b\140662uf")}

testObject_NewProviderResponse_provider_16 :: NewProviderResponse
testObject_NewProviderResponse_provider_16 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "0000007f-0000-0076-0000-005500000044"))), rsNewProviderPassword = Just (PlainTextPassword "\ETBnU'2\40753\61291\38238)\DC19\NAK\DC3\DC2\ENQ\GS}98Ul\15228R\SYN\NAK\US2z\n\994540eY\ENQ^}` \ACK\1082818\DEL\173704\30345?\1108692_\vIjM\1005137uJ3\23990\994591\DLEf\119833a\NULF,f\98574\145339b'\983250H\1101994q\984653\64166^A6\8296\171330[?\234C\DEL]Il\EOTd\rFC\f>\28346\1090880>\ETB2\bb\9645\DC3\DLED\GS;BM\DC3\1095146`<\1087719\RS\1018604\DC2\179844\EMs\167380\13627?vn\1105003\1009932#\SYN=\US\16448:1-\1107923C\"\174396JyPB'\NAKmuu/\983586\a\1048248\&9\a\EOT&\983584*'8\132864Cv=s\tP\1008677F\SI\1073061y6\191043\1008473\1008543\1649\95882\78034\1071083\988548\1054714&\97077cB\n\DELKh-\1095311\129323\&1Q\1213\1084070|\GSpcv\1002430\1051352P\139193VJ\DC2\SYN\1083098\DC1\5465\31330\83353~\1105633\120870\"_\1058706LQ\1093459\SYN\19540\&1[\b\1010436v3\SII&\FSn\US\154643\27086@=\66693~\EM(>m)\1034607\994772\tcS\1018902\1008902FE\USc\DC1VPEa\ACKe\1060808\1091838oH\b\DC2n.\1062323:\1097774E;\NUL\4333\150525P{\991902\1032220\1006459J>\CAN\42032N\1087225\184669d\1090264\FSPqG\\\169555\DEL\EOT\1012977\1056480!=/3\\\170244Ot>hF \1009843\1089883\NAK\25782`\RSw\SUBt\SYNa\1005192")}

testObject_NewProviderResponse_provider_17 :: NewProviderResponse
testObject_NewProviderResponse_provider_17 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000060-0000-004a-0000-001f00000040"))), rsNewProviderPassword = Just (PlainTextPassword "`\EM\113749\1031501B\DLE\1061644M'\1006456\EOTs\"\994582AW\1024774\995360\1057395\4635;|Ua\988675\DLE\ETB]\1054510\45394z\STXDR\1034286h9-\DC1;\GSyR)I.1XQ)js`n~\1017027\r1\989672 $iJ\RS\DC4\47918\1042259{\1059804\132487\1083033hI\SUB\1095148{\132528\&8\DLEg\GSc\NULa{|4\ACKm\\%\63350\1032001r\ETXh[(3k\1077565\&9</@\7079\1045184{\US\18140\2902\1056199.B\152922\DC1\13281\STX\28073y\48214'\nhUh\984733+\1077931.\\wQ\1088795`\DC2T4sg>\1113958T\96762\&5_-T\\_\11287\179256@\a\22773x9\173193M\SUBD\141109a\1035380]ajO\DC3-m~\1043103\FS]\n\CAN\1103364\&7\DC2k\b/m\US\EM\RS&\1034709\US-\US!E~k\1070052\133625\1106268wL_By\DC4<#KDn\69382:d'#)\DEL\FS\EM\1075474G-\v-\131607\GS\ETX\GS\149409\96444bH^%5p\990495\173756_1;x\STX\ENQH\46603$\DELZD\ETB\992103?G!\SYNeoY\1089956a\vb\1040891]\990176\CAN\n\1083485\70082,\177295=\nDo\CAN|\ACK")}

testObject_NewProviderResponse_provider_18 :: NewProviderResponse
testObject_NewProviderResponse_provider_18 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000025-0000-0042-0000-00710000002e"))), rsNewProviderPassword = Just (PlainTextPassword "\ETX\SI\SO#\20994\&9F\DC4\DC1\b\RS\1038287\NAK^\SYN>q\1064789r*~\CAN\1078817\1091992_q\DC4\ng ;H(j\ESC\b\US3*\989954\SO\NUL\DC4tez\STX\24881uGQGgh\NAK_j\1017134\CAN-\1020321NT%}v7\SYNYQw-@\ESC7qqVR*\DLEC\1076304[ InEyD5-\NAKW3B\1092906\ENQ~\1075196\ACKT*\1052273\9838J\18029\1028663@'w\t\1036823\\\1004590J\136317>c\1075087q\EOT\140145C\1005212\40815A\SO\SOH b\t\r_\1058766\&1g'\173330\1050951)d \1002671\1107873PF\DC2k@\39807#\ENQ2\r\n\1103694\8053\155924\176984R]rIS\993395\23030\1003223q\179266cR%|\USxI#p57 @\EOTR},7DL\133968a\SYN3\1057427\ETBw\1044594\1059078T\190786V\NAKwq\EME6\169555\SYN\1106251\STXS~\1077630\1037308\160082\1003234znW\1064190\GS\16633u\SO#\v5\DC3n8\v]AQ\30358\169782#x\DEL\1002450\1099525\t\FS-a\SOhxy\33298\&9uAu# !\189724v8}\97431\169998\1064028,<31=5e\67392Cd\165859s%(d{';\1057621\RS\194682\1066872\998845:H^6#/?\157782b\166636#\\_\1006503|\1061856\USd\FS#\fx\21701a\131086w\SO\b-0v\991756\SI\RS\66415{l\185772w^\1072628\1070986\144315\48115e\1113976r\1170I\SYN=\185280\SO\52729\\#8\1039520\996341=\ENQ\19279a\ETX\SO~\13412\NUL\\%}\\9^c\1085832\1100766\131949B_\1097503:\1070445{3Zh\US\174948~?_\154630\DC4-\EMmX'\1081292\186651F\11150\135815;\DC3\NUL\1089609k1I\1063656\SI\GS\1025160\96522~\SYN3\163717]\139965\"\19808\EMoJ\DC2\1047448\DC2Lgc\66900<X#&\SYN\SOH<a\65243\1084133\58461A\45603\FS\1079812!")}

testObject_NewProviderResponse_provider_19 :: NewProviderResponse
testObject_NewProviderResponse_provider_19 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000000-0000-0062-0000-005b00000067"))), rsNewProviderPassword = Just (PlainTextPassword "\SOH\bWG\986784\1101128\EM%+F2>NFl\1064494UG\135239\r\37895\133238-\1075428\b0J\1043496\NAKi\n$\DC4ta\EOT>\35509B\157829`\1097802QQ\ETXT\f\a\1093522\1084487\SOW\EOT\1008444\134699e\992469\GS\156212a\1048619|2\999471>`b\r\ENQ$2dG\2451\SUB96{y\993171)S7G\1027170\&2Y?/\t\DC2k\SI#+\EOT4\ETB\1100073\1084657\146998]\1078697aMVd\CAN\ETX\1633\1034776^=;\12517\f\t\SO\ENQDFX\n\1020374!\DC20\12964\25037[=z\STX\ENQnY4\bLQyH\NUL\94684o\SI\1064434[Ro.\ACK1\ACK\DC4G\186332\1097423)T\30543\143448\126071\&07Gg~~`#I\1008483R\136262S.''0\57452\t\n\bfq\"\987491\&3UE\1033013|\DC2\RS7\b|\ENQ1#\DLEP;-\140504wKs\ETXpx\996531\189463\1066741\157855j\38454dY\988641trN+d\ENQn%`\1051604\1106330'#\ACKv\STX<\SYN&x5\DC1\CAN\tg\SYNq\154184\EOT\1031634\145658*\33025m\NAKJY\1069693n\ETBX\US\n\1064842k\a-\1093451\1017060\aK\120920\180904jjm/*J\ACKuHO7s\1051455D\"4\41295Q\FS\1063290,.Pi\159694\177428\1098504v/\ETX\"3(\DC4\1029505t:(:Mp\ESCb\\;Tyn#1\DC4\54968!\GS\DC2\991758M\29055\1083849A>E\n\SO\DC4u\147867U\1099918\1025765b\1027348je\61622CpD}@\SOU\17748k\"s\183751\\FNI1\ENQGH-t\CAN\28981\ESC\1068139\r7<\ESCiL\1003515\US\SIL34m\42015BFV^$BjE,8o1M<V\b\152035\ETXNN}`f^\SOH\183436\&4!;4/T\DC4F]Iz\1022972\1031784\999773\9236\184786:K+J \1039963K1\173404U\1049417e6\a\129066b:^wo\52462\USH\42689\24141\138743\SYN\SUB\ACK<\1105776L\USi\1034204\GS.\GS\\\ACK9[*;\67133J\36273Lx\1051491r\1007587t%w!gU\986635\1027046\1000387\SI*m\ETX_\1000552\t\"\SOO~!X\824C|7MK\v\SOHH\1024806\DC2IW\EM\985790VWl\185999N\168920\151485B\1069415\&5\1094260[\1028076g\1040729\990055;j#1B\17387\10461\1044926\1050789HM\59583b\nA\1090519V\4216\1006591\ETXHf\154305A7\r\SO}G\"K\\\DELez\ACKn\1018925n^sZ-wj\18017\ACKr+\ENQ\ENQRk<W\1072781\1097397{6\EM /h\999816\988738\SYN\171327d\SI\1096569\STX\b\133715uE\EOT>9w*D%T\41964\t2\159457P\DLE\ENQ\ETX+W\ETX{\b\CAN\1100458\156489$\40574\CAN;\175154tD\DC3\1083785\39085\ESC\\-l\\9\SYN\119573\RS\92594\CAN\n/\SO\30422\ETX\1019633/2\STX\78820z\60183\&1\1081243\38950c\b\1102296\1041365T\1067898KZ\SYNV\171073@\17636\23222\1044591\1011231<\20778JE\EM`\NUL(XK\156049\EMKv/k;\ru\SO@VUEd]T\STX!\100766I\1084773\DC4&Z: \1021345\&0\DC1}\70723\SOH/d8\ETB\163270I\DLE9\149783^=:\18877-\NUL\ETBW~n\DLEw\EMs\CANB\EOT\SYNn\42044Bo\15614\DC4\1089962  1nER`b\EMbM\DC4\bd,< [\163704_\v\47338\1068094\988206\&8\36737g\1068927\NULB2\SI4S>);\175899y7\50162Wbw8C`_J\1114031\1094412._\35364\SYN%\1056737\67621\1004020\1036767f\ETX:\133669L\194963R\1096622~T\25511\RS\1002799}\DC1J}\18224`|")}

testObject_NewProviderResponse_provider_20 :: NewProviderResponse
testObject_NewProviderResponse_provider_20 = NewProviderResponse {rsNewProviderId = (Id (fromJust (UUID.fromString "00000053-0000-0048-0000-001f0000007f"))), rsNewProviderPassword = Nothing}
