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

module Test.Wire.API.Golden.Generated.NewClient_user where

import Data.Code
import Data.Map qualified as Map
import Data.Misc (plainTextPassword6Unsafe)
import Data.Range (unsafeRange)
import Data.Set qualified as Set
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, mempty, undefined)
import Wire.API.MLS.CipherSuite
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

testObject_NewClient_user_1 :: NewClient
testObject_NewClient_user_1 =
  NewClient
    { newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\r"}],
      newClientLastKey = lastPrekey "\EM",
      newClientType = TemporaryClientType,
      newClientLabel = Just "",
      newClientClass = Nothing,
      newClientCookie = Nothing,
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "]?S`bO\DEL,%\foV\1058249\\+\1085138\&1\188945Hc\a\STX\34506\vH\STX<^e\1069270(\FS(UsaJ\74984\EOT\ETXJ\NUL~K\174557\53413\1102650\CANB\21894wP\RSb?^\152318!\ACK\989862z\EOTD\57439\DLEV\1014962n\5537/W\93061\"\ESC\44216mnq_t\ESCCR\1099860\SICq\1032826\1113138\DC3\a\CAN\n\14612\177521\GSr'\tA\1105228\DC2\1008406\1015229\28799VW\ETBK&#~\SYN>\DC1J\SO;UFO\ACKd\DC4\ESC\SO\992594\53528\ACK\SI`\156081RxHJVQ]:s\a\188379IY\54049~H6,80\1061902\1030969\SYNwx_\34144\97145q\ENQ:N\150078\1101779\1108114\157366\NULF\ETXAs\156773\993458bf\t\151541|\ACK\1027577\DC2{\"\DLE\9157\1046357>\187020,i.L\SOH\146676>\131459g\32728\1027802\188157\1055365\&6\983541\t+\166512\&8\SOv\f\NUL\b\RSv)\38280\DC1J\n\STX\1076642\36253\DC4&R\ESC:~\92995\nO\37177\&5\GS!3V\1080913\147100,\1069133n\CAN\986564\1045245^\881\CAN\17703\GS!\SUB\30722\172478\1013987;8{\ttB[A\GSH\1092438\nv\24007\1006563z|\CANS^.@\FS\140955#P\1108759V6:%7|\DC2o\99270\1060103)V\161010H\37080e\NAK\43917/\1084962\aSv\121274?\CANW7\ETBIOc~E\1063561\1028266b\ETB\155485\92278?\26363\1039530e\EM.|DXlNA-\1081928\178499E\ESC+\1041616SZ\97929\989365hRm/^{\64076\1090370\1036645=*)\1087615\1080072\1018318HjR\1015837\14527Vg\1041636!g\169151\147136\121037\DC3PY[=<z\DC3\20524+6DDO\STXz\1036509\1065649i\NAKW7dO!$\"\STX\1005295\DLE\53909q\"a7Q#D;\179671'Z|\64933\DC4\DLE\SYN\33476,4;/SX\5942H\999386\1001210n\1050473\DEL\994125J\1084142j&\RSGlY\159922\&1mt\1031248\&4EP^\1054740F\a_.E)\13384\CAN\EOTL\NAK\EMv\73128W\161926\FSh\SYN>2\DC2!0\1005141\SYN\"\1113389\1021455-<2]`6:\1044580'&8\ETX\"LL\97181*^:\ENQ\EM\1047774 T\1036505e[8%J#~u\1104342\32511j=hX66)+\SYN\aU\SO\SOH\1070386j\1085132\1090312\166954Td^\1078796\983350\ESC:\GS\98354\1075395@\1100827X\DC3\SO\49895\EM%`\38791\1108632\65179\1086075\STX\a-\EM+_\163560:H4o\DC3HS6`\ENQ\181063M\61286\ETBV?.>\1007053CO-\182201\1034506bf}\127013ZM\1034664\rNp\1095318\1036312\DEL\172193%\ACK/WNZ\62756t\39633HnO\1078611=xm\1095149\44536@$Z\1041048\DELdEZ_1\NUL\fw\983252\DC1Iq\EMa&\1105414\1092447\ESCe\DC2:\1104318#\STXe\STX\993655\&1S\1082850v=A\994881\NUL.\3929\1107033<1 \135839\132170$>M6\23716\1096562\1004254&t_6\EM\138609q\NAK\44158ZTX\\\DC3\1051437\19070a:ip\\]\145930y\135361\32810:b\DC2\28839\txB;!\1036389\1094377\57346HmV\ETBv\95479v\SOH6(/ph\186487.\18345\ETB\NULC\140113\1063283)R5\1107380a\DLE%^\1081815X\164772$D9i\1069264\DC26wM\1045336\SOI%x\1101120+\DELYT\145956\170673\142731H\185687L\rU3\v.+<\rT\1009564\996048_v\94297]H(\"\NULJ\21218C\ru\DC1-\1096638Q\EM\"\"\169384r\EOT7P\164640 \FS\1054047R\1032087\1053518\133154dz\1019877\1061911D\148027Z\ETB\1045958\54215Sb7\152587CL\\"
          ),
      newClientModel = Just "",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_2 :: NewClient
testObject_NewClient_user_2 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "\DC1\142248\13922",
      newClientType = PermanentClientType,
      newClientLabel = Nothing,
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = "&h"}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "I\1065423\995547oIC\by\1045956\&1\13659&w>S~z\35967\a{2Dj\v|Z\"\f\1060612*[\65357V\1086491kS\145031A\1106044\1056321(2\DLE\48205\SOi\SI(\1032525\168748f?q\SO5\146557d\1068952^nI\1103535_?\1019210H\119099\SUBf\995865\n\1004095x\ACKdZ\1053945^N\fa\SYN\SUBb=\1112183SP\128516aTd\EM\186127\DC3\ACK\ETB!\1011808\142127o{uoN\CANqL\NAK\ESCc=\v@o2\1043826\EOT\142486\US\1079334\&5v\STX\GS_k,\DC3mAV>$\1029013\1061276\RS\1089843\n\8980-\60552ea}G`r? \DEL\1004551\SOH\US\132757\&9\brl\155069}u\120967\1080794\1062392@M6M\155107\98552\167588|E5Ud\1051152tLjQ\1022837\6734\RS\v\DC1jE\ACK'~f\SIR\1010717\NAKd}}\1059960q\1031766\DC1\151174\&9\160469\RS\100592\ETX\186780\DEL\r\FS\US\36812\14285\NAK/\GS\25526\1090814\61061\NUL(:\1054313n#m9x \1078109\183480}\1052622\54486\GS\991929\b`\1087609G#T\DC2-8\NAK\18310\134655\tp/!\STX4C\SUB'DP'.\a\1110090\&8<9\SYN\NAKEq\168018Ep]\ajZ%\1025589\4170O\35069>\CAN\ACKw*f<\1102303\SOjzpjY\US\SUB\19086\DC1\DC1\ACK|\SO\1064500;\135633F!f\19971b%\1048714t9\DC2\f\121106X! \133247C\RS\1029038\162320C!\20923H(/\GSV)e\SYN2\NUL#H$BAJy\ETB\162654X\137014\FS\SUB\DEL~\f\ESC;\n<\GSf~{\b_"
          ),
      newClientModel = Just "om",
      newClientCapabilities = Just (ClientCapabilityList (Set.fromList [ClientSupportsLegalholdImplicitConsent])),
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_3 :: NewClient
testObject_NewClient_user_3 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "v7",
      newClientType = PermanentClientType,
      newClientLabel = Just "\1107729\DLE",
      newClientClass = Just TabletClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "\fr"}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\1052368\1027374`7\1059315\1007093\1113589ml\140833\"\SOH\SO\DC1*\DC3Q\RS\14805.I\f\1049837\DLEXHyy\155377%\NAK\STXygG`H;\95385\SOH}U\177626\137231\EOTcX\1040316X\34265!|v@\SO\CAN\fp\996755\127817d\1054362e\135350\vr`c\1007164\1023752\142611 \DC2\58317h\ENQOS\FS_X$\181753\GS\74118g\1066468x%\b\1015412YzZ\1069885\&2`^h^R\1101423\62761b\1095153BOyj\1040477!|E \58547\US\62210!Bu`5I$\EM,Jt\DC37\78371L)\70459Z\SYN/pl\172834Xb\DC3\a\STX\1091299\SO!\1078114\SUB#\170440G6\162069m\CAN\1029459RI\187903\983334_\996859\1000036W^\ACKj\1070150\172043x\EM\1040352\&5BV\bVU\1001763\142747\rPt\1108970\36507C\78096\f\158701F>\vkqOC\n-_q\DLEc"
          ),
      newClientModel = Just "\1016506\DC3\134041",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_4 :: NewClient
testObject_NewClient_user_4 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "i",
      newClientType = PermanentClientType,
      newClientLabel = Nothing,
      newClientClass = Just LegalHoldClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "\FS\rz"}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\1039017\EM\188473SyAf\RS\1015867X\64605~RZ}\1062576\&6}\1040947\DELke\1039536v$By\137108\1063620,\STXy\138845~\1099492\&9sr\1072529\r_\1069502\ACK\50260\1078888\1006226\&7\1100806l<c\1034035)B1\NUL\1045145W/>\1078895\50212|\1009874\163356\n\1052573\ETB)u\1101000\1104322%\32265\ETB\991708P)e\DELE:sR\1084361\SOH@\1038853y\US\20921v\165504\1017085\DC1[9b|\ETXHNQ|=\1029089\&3Q7\40623\1103209\DLEnoh9\59006)J\FS\186747P\54437cN\SOV\41980\160251O4S\SO\GSR\SOH\USBn\"D\156034\&3c]Npy\94751\70717\174343k\157787<\ETB\1110191H\t\1061523\164945\DC1o\f/w\EM>%\119897\SOH\144896,\92174i\vZ[I\45897\SUBN1mZ\29435\32128H\1059019Imv#JR/;\ACKK\DLE\22913\9036{ZR\a96G\ENQy\152237+\1076894\65774=!\EM\v\DC4\GS\1027536\1041750<\1050841\78770\186899\146197$\1025274\CAN-;\ETX4v\RS\1084606\r\1075881q\1082834\&4_C\160530\NAKU.PD\DC4NYI\DLEv\b\r\FSxH\1013670%mxC\1046045\SUB\1048930\CAN#cE\1084200;j`\136585I[ABy\1009976f0\138223t\1014439\SI\153598?\SYN~(Z\v\DC3E!\EOT\1028681[TW\993095\USn\SO\21854+&\STX\183016\FS+\131838\FSv\RS\1069777D\1037076$<@\ETB\fCEb\152380&\1024980e\DC1\CANgN\DC2]\RS\\A:*\ACKtV\ajD3\1109955zBA4\EM5\SUB\SYN\993240\CAN:%\1006879\51029\SO\184148{\vT_~0\GS\DC2\EOTV0\1066531K7xI/\1034400\DEL\54219\STX\1060450\EM\132009\8421.m\75044\DLE<A\DC3\163365>\1020963a\44058|\SUB\6647R\ESCr~\191257\EOT\DLEFrvb\59948\111197\ETXz\bI\\\DELo+'Q\EOTl :?\71309r\US\146650\1086696o+\169525Jn\1042987.*\162403_7:\1042422\NAK3T\ENQ\t\155471b/q\SYN@OmA\97170xZ\52646\CAN\RSh\USg\aE_K\SI{\1052531\1053068&\1112242N )\157576zB\ETB\SI\1048564F\1082705\"\ACKB\66649\ENQ\182827S\GSX\27274\1060500yHAi\156511 \DC3\23109F8A/e.8$\99020\DLEOu-Mw\1008926\&4zb\165386BC\1078129\1070383\STXY%Bg\RS37v~2\ESC\CAN8\CAN\DC1\FSz#q?ADm4\vBK&Z\CANK\1112860f!*/\ACK\DC2iVTs\141472\1059086\1089418\154356OQ\154020-\46297[J\1041865\18234\1047221H1K\1102038\US\121032\1065005\&1\SIk+EB?\997749\1030694\1110639y,\CAN\NUL\USfT\DLE{\NAK\1032571\SYN\35342E,\SOHy\35156?\1031354$ll%\1046167\SOH`)\74445\1078638u\152196\1102759\148712A\SYNpDoz]L\19874/\1019344\1067340&f`AX=RA\v\1103061%,O\1049934\NAKB\119918@\EM\ENQ\1047236\1081280-\997855T\998310l6\NAK'\172576^\1053906('2AP&?\US\DLE\SI0f\2012\ESC$\DC2\31866\&5v_\1051689\ENQ\153046> [3h)?\50999\1039306\137233m\983891cBD\2027\1016974\1014463\SUB%X\NUL:Pib\\s\2743t\156273\1005692{Sz+O\GS\DC4\DLE\DC3.\EM;2S\SIF\1102512\DC4ftF}3G\NUL$wR]\USYpY\f\SO\141464;\ETXe\28203\38494\57722$\DC3(\SIw\ETBBp\128488\ENQ\DLE\154640\1040605\v:\51261V\983830.\SOHN<\60444$\tj3\51990\US\18582E,$\a\n\SYN\SUB%\1022574P\983622,\1034053k\NUL\134010E\82971\ENQ'\CANwrhj?9\1090612\1017377\CAN#\181249\58693\ENQ\DC3I\f\"\ENQ\53481^\144621}T\164393<d\1026943\\"
          ),
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Just (Value {asciiValue = unsafeRange (fromRight undefined (validate "123456"))})
    }

testObject_NewClient_user_5 :: NewClient
testObject_NewClient_user_5 =
  NewClient
    { newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = "\1093219"}],
      newClientLastKey = lastPrekey "?&#",
      newClientType = TemporaryClientType,
      newClientLabel = Just "A\170327)",
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword = Nothing,
      newClientModel = Just "Y5>",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_6 :: NewClient
testObject_NewClient_user_6 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "\1103895",
      newClientType = TemporaryClientType,
      newClientLabel = Just "{\ETB",
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword = Nothing,
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_7 :: NewClient
testObject_NewClient_user_7 =
  NewClient
    { newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = "a"}],
      newClientLastKey = lastPrekey "%V[",
      newClientType = TemporaryClientType,
      newClientLabel = Just "",
      newClientClass = Just TabletClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\60526\139445l\988963\ESCQ\DLE8SH\f\GS\ETB-\SYN5\FSPy\1061623r/=\986816N3\1035659&N\15316w\67714X/Q*\150730\13744X\1000479+9=U\1024020\SOH\ENQ\STX}i\b\1097422\1069734\SUB\ENQZ(\168168\1009772o#Xm\CANM\1023654\&3\172872^Pn\7760crQc\DEL\986307\SOp\tr\\\986121t}\EOT`\177281M\66289\"\4716S\SI\1037221\SIQA\1106589\RSb\DEL\1021792\73060\&0\990076\68641 +~\991677\SUB8,b\162479g\1077282%\CAN\10999\SUB\ENQ)e\SOH\1015562\f9\1094666!yW\1033160${\1059712}\1110303\1078145\144829+q{\f\164278\1032027|g\ACK\SOH/`L~\1047083\1084997\158108\ETX!\ETX8\n\1025179\984212p\RS&\RS\32948\1096125+\1052271uft \1037257D\1055020\160376'\SOH\999953\NAKV\SYNn8x\29075\DLE\40118\CANP~l$f\7823\DC1\ENQ\1000273\8729\RSLx<@\DC2\EM\1027640H\181765s\1072018\&18\152448\137948\ETB \DC1I\f=\CAN=[V\1045990'\SOH\46563\995907G`6'\SUB!*;\EMJ\1075591\STXy\DC1\\\"!\aE\28556\146573B\1033154\178164AL\RS!\ENQ]\1050549?_t/\137535a\64278O G\RS\1102157\1087399{=\DC2<<\1046051[o\1021278Lz\a\ACK\1108792\n>.\DEL\ENQI|hlT;h\US\SOI\NAK\SO\SYN\1102001\1016492\DC2\1038033\FS\\P\FS\171319\v{Y\DLE ' q!\1045478g\1015953T\1017164^Lg:\1043756)~\183956&R{\1077936\188232X\ETB\1096513\1012477z\5029s.8n\n\58208\DC2|\SYNRP&\b\ETX7\ENQ\aU\1003317{\DLE0%v5 \18048\SO\94341EU\RS\3357\&8\SUB/\4793W['`J\1067364uv\163012\1062281\DC3\DLE\EM\1083526+P7\161606P_X\100493\1036346\&2\1060605\&54\184580xB\1054288g\180383\"\DC1\SOHrC)0^\SOHrQ\fpd\46566\1106204\59787\18367B\NAK\162313\42281s\1005088\&6gi6m7\144814\EOT\ENQL\DEL\f\100019f?\CAN0$\145372\ESC]>CV\nVPb:^e6\7528\ENQ\1049417\1094717$!L\1052468v\60090v\SO/:\SUB\"\1052989o\170175\fe{:\1002279\6755\DC4\SOs\a\DELz~\62543%H.xF\1041929\165988h\ETB\985392\ESC\1061053E\ESC\170298\&7\DEL\SOH\v8\97125:PS: \1092223:\1039879\&6c0\167650\1088174$\1102815{)|\GS\1053757K\f\RSa#GK\1046896\ETXm\STXbr@\6481\r'bv\b\996462\SYN\1018321dv\1000550\"\NAK\171594\ESC\1034597\DLE>\70305\b\164202\9087S/[\1093372];\10230A$\n5S\156734_y\188649\ESCP]e\CAN\180901\1110553\160289-\GS\1078851\GS\1098241\1048062&P$\GST\120646TLX[\49486\129523)\ETBP;\45799\SOm\1113854D\1061956\1043723>EI\NUL\NAKI\CAN\1058606\t[AtgxM\SYN\1038293^\1101184\b^V\143698+l)n\1029632\NUL\"\1104565y\a.\DLE"
          ),
      newClientModel = Just "\150744",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_8 :: NewClient
testObject_NewClient_user_8 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "\DC3,\US",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "d,",
      newClientClass = Just DesktopClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\b\RS\1083911F\v\"p\184881\161833(\CAN\STX\SUB{1.\STXSh1\EOT\141013J\68124\159527\52361?\16802\DC4hg^h\1058009&\SOH\a^\9060\1109232V\74979\r\DEL~\nHD\"\ETBTC#\152775\57858\DC1\1029658\f\9672D\SI\ACKX\CAN?T\STXQa\v:}\ACK\1043380\FSv\ENQ\EOT;F{\69424\168419P\24246SNm9yl\ACK\1039741zm\r\\ym\GS\NULVYm&H\ESC*t\181898p\988616Sb\46454\1041292od\175159\&3L\58851\1048155\SO$3\1079098\RS\r\12927\fy\173674B]\aj\DC3g:\GS\147498\&7\ESC\NUL1y79R\ETX\124968\NAKo\1030373\GS:C\EOT&3g\ETXPonO#u<Rk\1111096 \986764^\US1\fD\36617H\SI\1059620Z\NUL\47916]}\998418?\48964\31642\1063013$I/0^n*\1025119\&7U\DC21!VB\RS\139118yu\DC1F\1006826\1082084\18986;\19168}\DEL\DC2\25722b\1019058\158678\188844\1089928v\177690l\ESC\v\986572\ETXM\ESC\SO\ETB\EOT{^\172359\EOT\SOHU\21309B\1055188 \155011\FS\DC1yB<\FS\CAN\172493\1038994'(\DC4.Dnv\68008(ys\1022468\180274\EOTVsa\1088265]8D\26062\155836x{Vzv\1026812\1054920\RS\17852\37216\v3xNAi\30473nPQ\1085732\DC4\177666\1036966_\13645\1073711\n\1098829\1046571n\NAKa\CAN\194844.\1044430\1009786\37924f*2\37822AI\144147\SUB\43050Ot/\1107459W\1030725yvD\156801W\GS\1104332\190771\CAN85|5\NUL \STX'\FS\131667dY@a-\SUBY\113715\EOT\131465\120175\72324\1033568\1086513D\ESC'\990915\DC1\1101540\154908=\DEL\155372}~'\141421\ESC=Z\DC4P%@;\1051276F\39040wh>\DC4A5=z#7>+R\181936\34025Qk1\42728\FSF\DC12MY\1045697\1056260oqk\t/\fWI+1!]}r\DC3\ESC\NAKK\DC1*M\STX\1071403\SOH\EOTqe\CAN\134249\&1]bj/t\127036,\DC1jk\EM\1096906\DC4\US/\1018712\2562\SO3\119865k\DC3\1089523\FS~\EOT3\1017007{\46838Hf(x\SI/Q\1052752\nzgdyT\1112482\&4CE\1061698\FS\b\f=v\EM\1052769+C\142840\642f\1012060x8b\DLE/\ENQ\RS\rQ;Ec\181947\984650\SYNQM.)\EMk\EOT^\"&~EQmQ[\1109601GR\SYNz,\1112207kdR\SUB\DC1M\EOT!Nfn)\994438,oFb~\ETB\DC1#\DLE\\%\FS\EOTC\997002\1005097\8330\60433.\171514G\1104943,{\EOTR\174171=\1108508\135699\1049776g\DLE\142921\\r\DLEF~V\35187\47168\ACKP\10702&hC#R#z\DLE_\DC2\1080508\1089113dK\156671\22090\&2:M\1003119t\DC4\120237\\\995393:\v\ENQX\\\174898,\"\1018585:\28181l\FS\145440X\1061109hv,V$'\EOT0zk["
          ),
      newClientModel = Just "",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_9 :: NewClient
testObject_NewClient_user_9 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "n",
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = "\SUB"}),
      newClientPassword = Nothing,
      newClientModel = Just "m{",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_10 :: NewClient
testObject_NewClient_user_10 =
  NewClient
    { newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""}],
      newClientLastKey = lastPrekey "\STX",
      newClientType = TemporaryClientType,
      newClientLabel = Just ";*",
      newClientClass = Just LegalHoldClient,
      newClientCookie = Nothing,
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\SIxXs\STX\1049513\&2\1102168\59074\54772\53296X\v\1090222\EOTIk\176588E?ZQ\132276O3\63616X\992817!#B_\152795}\1084694y\1054376PM\ENQ{\DC3\157254Ln\ACKF\DC2\NUL\11009\&6g\36110\991920\25715$S\DC4\SI\1004033\&4'?z\1081305\&0\39720A\21773\RS\DLE:pg\ETB,_V\34799\27560\1040999U=S^$\1041670u:X\63822~/*\50548\DEL2\ENQd7pJ-\DEL^\NUL\a\67607\&6a\158943:%\1062864$\1077297\&69R\SUB/G?C\ab\SOHl$/K\ETX\DC3\142946g 4J3\NUL#\31582\986721\DLE2\ETB\175407vV2\EM6\96746\v\36097W\54750\176689+4tk!+Y\152620\1104329\NAKQ\1023294\1105747GHQU\bx;c_\SUB^\34594]S7\ru]\r\1034086!\1067469\150060\&2\ESCMg\1074803QE\NULm\NUL\STX\DC1P\155051\v(H\1011822\1031529|g\1082510\SYNb\DEL\FS\ESC?=f\1018763\54754\DC3\NUL\ACKg\US6\172308Q\52355q\t\"\190130b\DC2\138877\&3K\138660\&9IxGW(\t:\f$9\74183\SIU?(\177077\fgI;4\EM\147733\ENQ\"RGbgCf9\DEL\64069Io\1105067\51831\&8wb(]\US\1061721!\1061150\1078273u)t\DLEkO\NUL_\DC4\ETB-\RS 4\a,\f\19767Qgd\1078960\22989\1062074\DC3;\986351~\1092523Ui\FSH\1047632\\\1040094\DC2vmU\983357D\134769\EM\984607\ETB2D\20960\40111\ACK\SUBJ\SOI,\1085675TgXb\SOH\FS\1094376\ETX\1096029C2\127781\&7\1032517P \SUB\CAN(8htD\DC1\30721\162821\1067602WzoM\GS8\v.[zIG`T\164362<$s\1076321\165910\SYN0=f(\ENQ?v^?_\7818X>7\21617|EQz\1039534q3\tr\1080514nq$yq\137821\&5\987933i\1096583h3\ENQ\1079332\19161Ac\1003179s\155333\34595X,\EM\1089910>\47776\DEL2\ENQ}\\M\nd<lX\ACKO\70425W:\EOT\nHVP\v*\DC2\ETX\RS\32603\151426w\NULx\1047507\39591\136728!2+RF\1106068\&8\139868\&5BV}\1000110<w\1066871[\1700\DEL5\1049894y[]a\1053022;u68\182556F\37299{g\2045N9\27827\GS\995886e_\1102367>3%`>\24917?\rv\1053845\35041d>\twE\188031G\1031126\&7\1023454\ESC{G\1085555\SO\SIv\1055004,cP\1060712\1019814\"4\DLEM\SYN\178587\1021477\&8u\1078424n\SOw\ETXV\US\DLE\DC2\fXf,\1036282b\1101047\">&0xFqd]^\DC2\ENQ4@9\NUL\94493B\73906}L\v\n9S\EOT\SOH5im\DC2-\95629}\SUB'\98821#,\1034274c\ACK\ETXP$xc\137033tB\DC3\ETB\35422\SYN0\ESCu\\\1048883b\23147\"\EOT\1006915bwE:\b\DC4\1065071vp8\1022622\SOH\DC178$E\DC2JE>{?\6021I\NAKJ*7sI\ESCeu\DEL\1077673\10421\&1a\NAK)R\185144|PJ\1111255at\SUB)\ai2\1009821mY\46293~gdde>\1010509yG5b\653 \132576\1001031zJ\984798\DEL\NAKT?i\f\1064196F,]9\GS\46035\DEL%hB\128429h\34802\&9\168858=K\NAKrFVWO\CANVc\65362\1065989\1092750\NAK\1021958\ETB83\ESC4\"\1035406\DC2R\1073182\113709\&1UD\1055011Fi\1059797\&8\EM?j5j\998683\SOHW)\1057415fJ*m\1074569d\127187l\DC42\1086873\"Xgp\95800v\"\ETB6U\22131w\SO2*\174690\21425vl\CAN0\ACK;jh\3871IC\SO\1026262L;55\CAN\1031309\ACK`m3*. C)\70284\156404x>y<\1104362+>*\ESC\vs\SOH\1088826NSChj9\49235\"#M`?\54746i\ENQ\US\STX\GSlcS\1047710\EMC\ENQ\SOH_/u?/\38071\997094g\67263\175233*\STX_v\1060509\DC2C\DC3\ETB\174166J~637\DLE\t\DC1P\DLEy\66836'Wz\SYN8\1008920j4\1374d^7\22270JF&,\SUB\47147"
          ),
      newClientModel = Just "9FO",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_11 :: NewClient
testObject_NewClient_user_11 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "",
      newClientClass = Just PhoneClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "oq["}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\ENQWU#*\b\169584\ETXp\3616\DC4c\1009690e\47037m\1102567\r\t\1012852\7852\NAKz7\ENQ\r?I~\1054524;>$]I\SO\42203\DC1\164569T\1112610\48142\ACK\FS\179049y71,h\12763{\31082`\n\r\173936+N\1096448\GSlqmi\1099779\SOH\EM=T#%\1022504\ACK\fZ_UCe\SIk\"\n'%\1076023I\US\RSZR.n%\NUL'`u5NI\ETBx\ACK+\65807j\170243(x\DC1;\181398zyUe\f\191269\37010\DC2ow\1044794?-Wz\992114Z+\SOH\n\1083159\ESCly\1047059\&6l\SINn<f\1002741\SYN\US ye\1011963K^}!+RyZX\1095114}\ACKFEGl\985515\61674f\18135\&7q;0W^w\RS\43303'\DC3Ms+\DC2o\1022250\95106\1028851\SUB$Pr\62215r\1022626<\NAK_Q\1091631\SUB\NUL\DC4R8\SO/Mmw\139369cS>\SIT\ESC\EM*\1083747\&8&E.\b\DEL\148620\1070806\EM_\n{\188784\RS\158747\SO`\1019622\46413\990554^1\139313=3X\DC1K\NAK\RSc\SOHB\1078866B\GSm1$\FSDve\142150g\SI\1087970D<\SUBKo~X\SOH\1094699\1105344-3\1058387\"\9645\39804e6G\ETB\1011308\1111485\53599\&3eM\EM31[L\SO\DC4@\132573F\US,b\199\999413\v\STX\986195D\DC2V\vX\SI~\1097246 nS/\57971\169887RTD7cMd\DC1\EOTOw\994792yO[8\1082860\60631\100663[\185643>\99795\nA<=PE]'\DC2\986494\&7I Q\1069408\17420E=\SYN\ETX;v\CAN\SUB\163809!ek\DC2B4\STX\EOT\1100350\178438\13392b\t@{0\1052374\181553\151609\"\ETX\ETX9/R\v\133430n\DEL\1028100\ACKA\DC3\n:\126546\46116\45354\189672%Z\US\r<K\rwHW3bF\SYNF\189514\bMuPn\rMc,\15539\SI$ \RSY3.>[EYo\DEL0N\RS\ENQKR\1033145\1099889~c?\EMJ?`\DC1\1024319\99332\t\1037670h.n|\SUB2\987386{'\144962\1081004/^ut\100480\134584--\DEL\1054725\SI\52605\1034550T?\1000376\177692\1007643?\DLE\1065650\&1\179681*6\SOti6\134585_Z7\DLE\NAK\ESC\SUB\a/M,\r\1013726\27638\1049228lg\GSjH.uC$\50698^1h)\b,\ETB\166565a\1092454R/\1035274_\SI.tx\ETX >\40781\984980%B\ETB\1009320\14711\&5\STX\153557\f\121255\136884\57749}-j\ESCyO\CAN\47414^\1051627F\51571J\EM\SUBE}&9w\78649u\r\19329nY\369q\195010\DEL*3\b\FS\140122\1051712~Rx\SI\n\ACK\n\62186a?S3\25573\bn\"gf\1113756\57969F&\16122V\ETB\1024468W\1015855\SYNN\ACK\SYN|r,;.w$\FS\b\DEL.ro[V\987558\1015707&\SO\17069+\16921=\ACK\\M\EMYpXZ|Jnnr\1052032\\\1004404\23525A}H\164247\ETB:_\DC3[\SOH2%\rh\FS\ESC]\92372\DC3\190770;\f\1105269#\r*0\92455Z\58095\141267\153723\1031644\1084692\&2\148115\\ 7\25709\176908\NAK\49305\37904\179314p\1099243]P\93039q\v\RSi\NAK3FX\74130\&8\SUB\33013J\DC3i\22841#\239K\1037511%5j\179793\&6\40198\175557\1113225\82980]B\US\1045028\&3\1024025-o;9>\1009214A3~\ACK\FSR\41975\140632\SOHa\fiFV\1002037e\b\1035521\96860n*\1008115\ENQ\149608\ETX\GSnp\DC4m,\r\1058496hb\DC3\1069244\34144,&d"
          ),
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_12 :: NewClient
testObject_NewClient_user_12 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "\ENQ",
      newClientType = PermanentClientType,
      newClientLabel = Just "\ETB",
      newClientClass = Just PhoneClient,
      newClientCookie = Nothing,
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "T\166327fq\DELdA\18130k3\1105137[\rK\n\f^\1090853\&0\98095\DC1\ETX'\97298M+\ETX\a\ETB\nw\SOH\1797K\161329\11176z~qp\153477\ETB\GSW4m@CYG(\49998ob:\CANP~My\ESC\175657\v\135887\"\34075&\71209X\1070912r\DC1\131932\1022615v-p+))\ENQ\STX9i {&p\164508[\r\US&;b>N\ESC~bu\DC3\v:W:S9\DLE1L|\\S+b0Q\STX\31885rs~;\USG;\986684o\1008621\3277%\STXm\DC25UTR\n\DC3X\ETB\1058418~\RS\62657\DLE;\SO/\US-\\mPc<:.HmK\175151o\f\SOH\1094091&B2\aR\24795\&51\GS&-\179688c\183928\988858t~rT\175316H?\60066\&6\155512~J\1111920q,\STX\RS#\1085689W.=\52405L\1073572-\1077186\ACK\173126\152012\&75*Oq!\33433{x\DLEt\1077477\993588X(w.\EOT\r\60577\1111873<g\1067652,\126518\1042949\1046591W\RSHF\177656\ACKV\4843ah\vV\1093384\DC2\1035774\SUB~+\SYNN)T(%v\ETX\av\99731&PB!\USv\SOn-\1069745\ACK8Y\NAK\t\b]\41266\SI\DC3d-u\nln\SI\DC2\1093454@s=\1026623\1074969\&9{\1061582\1003483\v(dyw$\1113627cZ\92231\STX\164573\1097384\142434\EM\1078237!\174662\&3JR\143296\SYN\161817GWD\148342tU\2428X]\40961\998341?0\1100801-\SOHu\DC4\5236cx@\994731[Y\990002\\\CAN\30353Ed.\fa\SIv=\1078644\SIGBs\1083053\1050753J\1023066o\24519W\nJ\149812s}2\161680\72241\1065720~\SO{\ENQ}\5580&M\20451\22909\1060155\SO/%\171159r\1054365Z\186105\GS7\ACKU\v`\1094898\48352\155431.\121425\1035497M\134777\1036948\t]\34223\SYNL\f^I\992198|\NUL\1008174\US@\169381G8\1009196,S\US5\1021000\DEL\22333\39922X\r\994770\GSiM\131770\SUB\DLE\f\\^\5471yD\11965yU,j\999268\139322\985475\1059433\44478\23685|o\1079700\DEL\r\195095\SYN\NULS]\100598d|\SOH\ACKCp\FS\nYH\FS\1099357/ixF \CAN[m<r\135674 `\1075120Ks-v<a\1094526u\134002\52349\74147z\1062584P\RS~=\DC2'h<\55168\63231\68613\DC1W\65886uZ\70659Z\24561\DC2C\DC2\ETBWVb}]Po76\39692\998814\&5y:\ESC\1079927\1013138\1032649\ETXu\DC40\1076425>}kp/\th,z\\\27305\ACKn+=\23672jL~F\NAK\32287\&7\a\44660 ,0\517b\ESC\DC1\USoA/\ACK\v\1005276\EOT[o(z+c\SOH\35973s\35678C\31719\\W6\NAK'\STX\1065586\&6Xb~TO\1106854\1078560U<Z\ENQ\156731\7392\&2\54129H-\27785\SI\13042/*Hy\140127R\1094687?\1085149z\"\USCZg\1097893\CAN%p\EOTuIp\152059\15387\6724\65894\1101113!\DLE.\1064291(\3539\DEL*\1007532=\40985f\FS]k\27780\DC3ky\1064026Y^\994817\SOHuI]G\DC1\34673vk\1067509\USe\b3Ym=$M0BD(\19406\1010372mb\v\1004994=:#Q\1050514cE\134262g\166838\GSp+^6\ETX\1041157C\NULA\FS\1403l\bQ1H:W[~+3t\v\1043027x\173663'tg<\988208\DC2\994035\997407\ETB\150118h\4009Y&9\1052503\1103498!\USPh\1072684\DC47\1085315E\DC1\n)\1105943\DC3YD.n9&\SUB6\993897\120683\STX\1067786\1043843\1076525]\1088405A\NULu\1020886\CANPB\rf}\US\13180\1071360O\vn%\178761\78715d\EOT\50791\STX\172034xI\21241vn.k\1071669 \DC43T s|4Hzy\2984\1056711l%td\GSg\DLE\146245=MY\DC3*M4\143306\DC4\22852\&5$.\158831Zqw7c\1045421h*'\60993\1013472\&7\SO*s\984102\"UOI\985374m\1061984}\ETX\1077690\19087\SI\DC2,\ETB#\CAN\DLE\162616\1012813dQ\1096827\STXz\71316yDm\1086378\18451\FS\SOH\DC43N\bS\32995\22450\1090108\n\DC1\r\62696\&8bF)N\189028\73824Cd\SUB\SOJy7\1033822\SOH'\v\182332;VrZ\190360"
          ),
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_13 :: NewClient
testObject_NewClient_user_13 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "\DC1\DC3",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "",
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\DLEF\1034128\&7\1031631&L\24275\148172\137212%\149468\DEL7\f\1080700/\1101432I'-A\998257B\34190\1079095h\160611Vq\988784\1031840\RS\CAN\26303S\a\CANAG\1041327\127546\1076544GE\1080981\NAK\b+v1^\154744\986594\&9n\164089Krr<\n\1101960E]\1000111\1065128\vE\ESC$Vi~o=\990252\NULY+*\r\1021245\1007759\&0\DC4\"lxU\SI\645\169748%4.R[\ACK\136528\\\US91b\STX\STXJFHcoRp\44381BvB\ENQ\143651@\57825jA\1006035dF\1061704 \60170\&0Sw`=Hh\24564\160196s)\163654\1006719EBK\DC2\988725\993160fr\NAK\RS\tN;ym\1000448\nF\1061983D\ENQ\FS\1059546\187154\SI\1013960Z5X\ESC'%A\183297\t\5848(yQK\1932{\taL\40398\999384}j!F\1098359\ETX\1017409\&0P|\10680+\22277tI\1371\ETX\177781\DC4\1014772\1045863\SUB\DC1\SYNC[4v\GS}_9b\"n\DC3` _\1060194\EOT(E\1049311\&6\178536V\137323\EOT\DC4?j\51994\DLEM\RSL\158335@:j\158207r8\83020\ESC\1112905\a\USY\n\CAN\92445+75\EOTk\n\".\990872\99391\f<DD<q\1081197%\61729\&2\\pZd-<Z\15355\1063004\1020083{\21225-`\1044570\&8G\146407\985804 <i>\36772\SOH\"H2\1071196\DC1\1111474\1070801O#\SIu\SI\70804\198B,4\917939\1103645L\1098719Mt>I)U?p\SIrQ\991975\1024380\&7\170637\1058486\DC4\ESC\n\3616\n$\1019615\abhXT\DC3w\36477 x\7606\ESC?p\b\ACK\1019692N\1047942=j<\156592=)\n6\25048ZF=*`(\DC3\SI\1084532\ESC\FS\172082\ETXyR},\1088502r\GS\US hY_\1059030\1053557\31965eHsWj\1065305N\51163H6dd\DC1%g\1020583\&6\166285=KX:V5l\1011535\&0\r6K6?\1031425\CAN\1016917:,x\994043_{rgc\bI\SUB\65378\1094330`\\\EMl/`\t\US\DLE`r\1030112+E\2755\93034L\991483\EM\2608d\1049231\153892O\SYN\1095453.\GSom\28655v\1086471\DEL\154279d@\1076849.O,*\151457K\NUL[)H|Y\ape\ax\\P\77938 N\1012329\1083848|\f\1026650c\SUB\n\\+\SOH\SUBWCk\SOHe\ETXG.B\NUL&g@L\ETX?B\SOC\1035168\ESC.\r\GSv(uz)\EM\19888\1027956Y?/R\DLE`]g1o1\NAK:\US5\1018150\1027769\SYNKS\CAN?\110635\1069040\998938\1073365#bf\ESC\1047091mc\54492Q"
          ),
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_14 :: NewClient
testObject_NewClient_user_14 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "\35793\48115",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "\SO\1054082\985803",
      newClientClass = Just PhoneClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "\185625 "}),
      newClientPassword = Nothing,
      newClientModel = Just "\1108879",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_15 :: NewClient
testObject_NewClient_user_15 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "\100417\113707",
      newClientType = TemporaryClientType,
      newClientLabel = Just "",
      newClientClass = Just DesktopClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "\1095013\176877"}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "0\EMM~K\162154\45005\164305\1004413\172280y\1003918*\174546\1042834\rE\DC1(\1088983\1005968+\176459\DC1\1007952\DC2\1078947\n:\1032500Ig\SUB\74639l\EM\43295p\EOTx\1094422#\b\1043804~\1003046f\SO\168069L\14137\&5\1072066x[#&\DC31qH\132531eA\b\36117\1000509\1112836U\DEL\tb'\125239\&0\DC4\1099298\46462iELNQ\ba\RSb\v=\7552G}s\GSN\1025484$L\DC1\ETB\\\1082834\98732\151628\"\998010\&4\CAN\DC1H\SUBt\1105766|\FS6\137171\SIqu/k/b\1002207e4/\FSD\1084290\v\1047428R\38606tL)\1018496\&6Q\60194\RSd\46542\146946qzqbC\f\EOTpS\DLEY/\990314\1108440T\1044302\21442,tw~)k6\b\USZc\1000679\ETB\1053814<Y-]W\61543\&3/&y/:]q\CAN+f\22994\149885^\137583\&8\16557\ESC\1105009\150876={'\1087783\EM\150568\DC1\"L\119564\1048464'K\187367%L\r\CAN\r\1031250s\SO\ETX!J\984440-\SYNZw\140870\&7\b\1086365sS\1091540\DEL\tRN\v\DC1e\59225\SI\1069742&\33315_hJ?\DC3\1017437\FSt<[\EOT\SO/\1035982\143822a\NAKh\1079284\34032*\\\ETB\ETXSW\164519\rCUH\f\1016697=)\1031594\37958\ETXwKwP\152609?\ENQsUF|\1081243-K\DC1>\FS\142098Z;6'\9971\DC4e8)4YT\1049079\&7gm@AD\178053\b{\175557\SUB'x5\US\n\DEL\nv\138741\1106818L[\35688\46936N\979N \61464\36923O\50044\&8\1023723\33945\DC1\GS\EM\STX\1011210\b\DEL\180649\990944yX)\139946WmBo\t\ETXFj=5n\1088694\US\170101*\NULWuDo7\vLt]\1041373ff\52366O\1109893\DLE4%SqD\136650][v \DLE\NAK\SUB\SI\1107671\171196\r.Z\RS\54261\US\FS\1032146"
          ),
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_16 :: NewClient
testObject_NewClient_user_16 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "\1078202\37369",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "]\FS",
      newClientClass = Just LegalHoldClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "q\43234\185884"}),
      newClientPassword = Nothing,
      newClientModel = Just "\ACK;\143320",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_17 :: NewClient
testObject_NewClient_user_17 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "\138278",
      newClientType = TemporaryClientType,
      newClientLabel = Nothing,
      newClientClass = Just PhoneClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = "k"}),
      newClientPassword = Nothing,
      newClientModel = Just "\ESC\15411c",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_18 :: NewClient
testObject_NewClient_user_18 =
  NewClient
    { newClientPrekeys = [],
      newClientLastKey = lastPrekey "z\178029",
      newClientType = TemporaryClientType,
      newClientLabel = Just "Q,",
      newClientClass = Just DesktopClient,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "\"5\RS\160225\60188\9236\1112830Y\1031107]\DLEqDD\1045338g7G\992982\75047\DEL<o\ENQT ul\170115\1013929\1086245\t\t\1081038a\1025366\SYNa\1102309=u\DEL\\W\SI[\152148L\"\ENQu\62491\&2I\FSQ\ESCt#:D\STX\51757\1076084\SOHc\DC2b{\ENQ\145100r\32825X b\148209-2B\1004357)\DLE\149331\133045\rJ>\39442\133937ceE\189589 Nx}\172612\tF\21563a\ACKN\f\DLE\1040194)U\58059\1021976v\"\SO#\1063353Cb/Dc`;k:qA\USQcsw[uG~\29313\&2i\ETB\157089RB\t\1078526\r(\f\10102\1060258&\SOHd\1105089\155519f>e2~\999553u=\1059210\10136/\49832\1099898Q\65153qT\1092626\148106Sa@k#(\ETB\a>!m=5vN~_#\1060173B\184540N\a\94462\64032.K4\152591s\156905zT\172441vv7/\EM\"#1*'q| \157195\DC2R`\ACKO>P\152635Ga \rQ\DC1s\1083048nh+q\150871 N\GS\CAN\RS\1054551\\;\12308\vMyY\\w\ETB+8bJ&2?D\1069890Y\1008350#x\162454\&6A\1106082\&85\64170\183343\NAK\145787Chh\22172VJ^R\CAN'F\160371[\ACK\n:%\1087606~}\ESCEAS@Wn5@\1096877&a2\1088377`\120586\8472\&5\CAN^\EOT5W\EM\SUB\DC1R\DC1D\RSL2m5XofV?Y\155649\SOH\DC3\1019336g\ENQq;\1001052\12738oP\SYN,\DC1\1073001\994800>\14799\7333\165461\1051770W&lHVm\1008163B\vl@%\1039130U#\NUL\1093381$XbM%q\1045263\FS0z\1088980\DEL\ETXr%.\1073307\32341\SYNZ0\fB+_@\DC1tcO\185752\1093870|\186862&~\136183N\45192v\155081\137844`\44461\&1\1014483\1108323\1093550\9737vf\SYNEJb\984223\DC3\nG}\SUB#_$D\1066925\1020860/\RS\vf\DC1\160209\DC3 \v8'GP\rb\DC4\48192\984278\30001?r \135911\1100614\n\160781d\bVStN\"\SYN[G\1586\ESC\US\1011464g\135744_\SOHY1?\USdu\1059402\74951\995455=\1074262})J\1072682$X]Y\DC3\1061380h\DC1H0\STX\66830\1027029\19815\b\11503C\DC4\DEL\a\1074263\1024496\RS|oTc!eU\992641Pr\138670\NAK\RS4\185988DG\136753x\995479FG0P\50986/F\RS\STX\ETX\171779*\ETB\SYN\17831Gw]\1015631\1080624\1069774\1061830m}tg6\166134\SI*\173230\\Wi\EMOa'\FS\1063048\1000576sh\1107838\143056lE+&\ng`\1004945\&3kY\GSY=\DC14`\1062494l\RS\1086111!r\20132\NULw\120334\1077517?\ETB\STX\1082825I\SYN\ngWcrN_)7f+#\ESC\178930\1076598jt\DLE\71890hAx\\-{\92491^4rf\DLE\189136\&6MJb__\48310`n\70443\&4oZA\43460\92533H.-Y {\a\FS\USu[4@6\1045998\94370\3461\SYN\DC2/|,]\NAK\1012422E!\DLE\rZ\178451Q\181143\1090192\&8.X\fwZ\DC4\DEL-{KWit\53903 >\175517L_Y5\GS\SYNaE\40086\&0Bf'M\DC4\1015266\35448\ESC\1009511\&2tF)\1092249&xf9|>K\EM%z\a\1015215\fH\120884\RS\b\1063192\vJCmoJ\t$\r\40361S0\134532k\ETB\DEL\995717z\1023854\b7\59291\DC2ACx\1092749p\45751-e\128647\GS&? \SOHSt}S'\1027538qm\1092436\54380\EM\\c\ESC\40780;\1004615\31215Y8mz3\STXROJbY\\\6422\1067862w\147906 in\SI\NAKnm\189428r\1066116\"\DEL\1004774%*U\66603\DC1C\1106018h\a\DC1MIQWC\172322{%\SI\118975\54512<_$\\d=\SOH)-4~x\179098Ov\NUL\DC4\185898p\1052353\24161c\999358,\23825o\SI\1027216N\STX\1037726\158923B'd\1006564`\1061268W%\1067374\1106444@hx\SYN.\GS\164392cJ\10971M\1033250\983450,I\DC2\GS\ACK!\18551\163976\564\EOT\DC4:\t\DC4\v\\\1109669]\1075069\139170r\CAN\155314%\STX\SIp\96570r\3758\DELR\FS3X\fsW\23719]L\155502\&1`J\ESC\SI\19051\&9gM"
          ),
      newClientModel = Just "\1077465\1056032S",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_19 :: NewClient
testObject_NewClient_user_19 =
  NewClient
    { newClientPrekeys =
        [ Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 1}, prekeyKey = ""},
          Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}
        ],
      newClientLastKey = lastPrekey "",
      newClientType = PermanentClientType,
      newClientLabel = Just "",
      newClientClass = Just TabletClient,
      newClientCookie = Nothing,
      newClientPassword =
        Just
          ( plainTextPassword6Unsafe
              "#\1113416~\nk\1060135J\188666POZ\f\SUB\ACKJJ\RS$\11618\EOT\42121lL[\FS]h,Xf\1042350\CANd\165029\US\65203\1079326t\ETX]\195015\155339e\195099e\DC30q\44188\189121[WSGO{\b=Hs\1089312;6\SUB\19036\"\SO\134697&\1112323]Xmx\143526\SOHi\9442j\v\SYN\NAK\1040391.\f\ETB\SO8E\55091i\1044526P`o\1033279i\137855\SUB\ESC\71171\1104725}8\143576\1001544\39043\113800\ESC\1016284,v\1025287\t\169739^\n\190789\1032336yOX@F \EOT17uq\58778G\10534\SI\GS9?8+O(\1034807v4,[k \14191--Qk\159337\ETXA\EM\164962\&9\138800\984565@\18866\1099020\147346\134891Q\US4;\DLE\r\f0\92231fz\aDC\157549\&3\1045725|Sn&i\EOTj\1051374\128703KS7\146137\fAx\n\ACK\vx\1111954\&5\129488MWIp$m\1060695\118857m\991735\185401\118823m\16484\DC4\SO9SDb-b\a=F\78227\25535\DC47/{\1063538d#X9\SYN\44105\45323/{=\1020413Z\DLE\164180xh\38176A\t\STXJ\a\1022997\&9Bk_\1091831H^\166229g[38&\ACK\150410\1081436\&74\96537Dwdu~r\70508\1013335\1071132&\184387Z^\rl|\f\ETBA\STX~2r>\EOTsM\171720iX~`\ACKzK\EOTW\1099149\1028596ezCh6O\SO%\SUB\SUB=\149997:\1070391DQ\NULc`\SI\aDmG)M)Th\190731\NUL[u\ETB\SYN\"?\EM03\188014xi{h\fy~??)w\NULy-IaG\v\NUL\52252\157648J\1047554g(\n\DC1\SO\t+p\ENQSl\78666\1079369\1018476\"N=Q:/\9007[\161661\r\NAK\147739B\37332@kA\USwi\NAK,p=\40491l6\66318'\132639\120855t\RSWq\985403W\SUB\STX\ETX\r\74459\156671C!j\DLE\SI>\\W\1004807\ETX\152436q+\"\10840|o\990028\RS\1031899\162664$\1033161\43937B_\NUL\1076842DV\DC3\177090W?rH5Xi\v\SO\ACK\187975N-\164527PQ2`\174057r\ETBga\156\FShT#ay}^\ESCP\1089292yp{\DC2#^/5X-D\NUL\ACK\171851\RS\60072\1033438\"}Z`x{\1055488TT_ Z0\20103\1039639\30357k\DC3Q.NT2/\1095308O\SOHP^G\SYN\60717/%d.\172353\986193P\f2iYS@\t\t\DC4.o\11544'?-0a]\97289)f9w4\136279X\182987\181688\DC1\a=\1087084T\142663g\RSb7\GS@\1070005V/,\1029412K\189653\DC3MP\n?W\1008141\&5/z\1046740\ACK\62886u\98299nKB\US\36186\DC3U\NAKh\USr#\1078423\"\1072189-\142719(K\183599\SYN6\170953\1098238+b\1088379s\1111208\STXk\1077908\149688\1103504~Rl\SUB\RS\f\"X3\163579\SYNOQRpJ\1037359\SOHWvE~\48192I5\993324\22741j\v=PK\135321%#\ETB\fN2\19120\181456vz9\1012476nY\DLE\SYN=F\STX\EOT\3416\&9W\13458\DC2^C)ZX\ETBJN2P\1003841\SI|\\\1004102h}P1V\1113257\&7\ESC\v\DLEl\181234\FSz\EM\DC2\1093528IM\993293\SOH/NBiM\170360~;1x\49216Md\EM1\52727\14564e#\"\US\94465eV?i{\1112978\1104388\1010293\151662&\FS\SYN\3436\153277#"
          ),
      newClientModel = Just "\CAN\1030222g",
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = mempty,
      newClientVerificationCode = Nothing
    }

testObject_NewClient_user_20 :: NewClient
testObject_NewClient_user_20 =
  NewClient
    { newClientPrekeys = [Prekey {prekeyId = PrekeyId {keyId = 0}, prekeyKey = ""}],
      newClientLastKey = lastPrekey "<",
      newClientType = LegalHoldClientType,
      newClientLabel = Just "+\FS",
      newClientClass = Nothing,
      newClientCookie = Just (CookieLabel {cookieLabelText = ""}),
      newClientPassword = Nothing,
      newClientModel = Nothing,
      newClientCapabilities = Nothing,
      newClientMLSPublicKeys = Map.fromList [(Ed25519, "GY+t1EQu0Zsm0r/zrm6zz9UpjPcAPyT5i8L1iaY3ypM=")],
      newClientVerificationCode = Nothing
    }
