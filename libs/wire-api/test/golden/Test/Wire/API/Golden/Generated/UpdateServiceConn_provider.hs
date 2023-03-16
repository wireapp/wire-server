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

module Test.Wire.API.Golden.Generated.UpdateServiceConn_provider where

import Data.Coerce (coerce)
import Data.Misc (HttpsUrl (HttpsUrl), plainTextPassword6Unsafe)
import Data.PEM (PEM (PEM, pemContent, pemHeader, pemName))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Bool (False, True), Maybe (Just, Nothing), fromRight, undefined)
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
import Wire.API.Provider (ServiceToken (ServiceToken))
import Wire.API.Provider.Service (ServiceKeyPEM (ServiceKeyPEM, unServiceKeyPEM), UpdateServiceConn (..))

testObject_UpdateServiceConn_provider_1 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_1 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\48023\US\EOT-)]~A\6084X\158541\1085038\&5\49967:=\CAN\1042311\1110226\98388zHH\94299[Bn\9081\151207I87\DLEbJ\ETB]\158065\1042093Xx\167446\&5q\194776WjV.\141689xX\4761A\131712\4959\1043857m\27816\1066578tf\98275Q]\162246v[$\1041185&7\SUB+\60975\SO\1022130\60565\RS$~\176589\SOH\a+\US\47262\995553\ENQ\984394$1]\139626\995152\FS_\9559\1112532^M:\SYNnj\EOT\1053023\12419O3\SYN`j3\NAK=\1027692\&8\t\1023383\27247\RS9pY~+\1060011\3990\v8vx'Sf{\EOTUu\1003780\STXoJ\"~E\EMpo\FS=\STX\151702&U\STX\SOhM\135675f\RSr\DEL`F/&WR-\ETXP~V:\NUL\155119\fl.\135176\DC2\1020429Y\1779\b&Z<x\NULw7\1077823~\1047042X\b\DELH}\984335;f\187779rE\52326gw\ACK\EOT\EM\44813\44048\ENQo\1097258t\132417Ty\1049899\&7\141334\&1\5166@S\DC4\"Rm\NAK@|\63239\133286g\ETXZ\1006895\n/^\DC32\EOT\74646Ojn_fiz0=\ESC\t\EOT\1030725f\v<p6.6\EOT0\1089527zp\1016599\am\46660x\997256\EM'RF|#C\ETBW\52692^)(or.\121444O]4x\1069122\RS}\1005052\917621y\DC3O ]\29946>X\v\1011849\DC2\51384\t]\983559\":\7506\GS\"\182388\&5$\1002096i\160424\1101600&6\127976S\30272\SYN\SUBC\1012663\EM\994623V\47942a\1041770]\r\EOTk/#\f{\159982\1022881a\150434\&8\DC2m\1011420\n,\ETB\1037975\61278<9\1052021<L#\147965\ETX_\n`\1027079yS\36427\STX\t-\1040509\1003488d`\174763\&5\1082484C\RSP\DC1\"0\72859\149290/\26633\119271C\DC4\n\NAKH G\1067348&\SI\176067H\DLEU\vp\1042885\ETB\FSyh\FSl\1107018^\1078487\"\1070915y\160579\1020600\FS\1031902\FS0r\92320\&4v\a47\1094721h\DC3p\74608\1113436\ESC56\RS-\aa\25768\&6\1064313@Af\47470\1000665\CAN\74076@Z\STX\60306\1050675\54777\1096893\1060256m\1104739\134689\983394\31784\49958\DEL\SOH\FS\1074239\138046Yo\ENQ\1054951:wF\DEL,I\SO\183773\131781\DELzE\ETX8\99801\v\1068988\SUBY\DLE\162330}\ETX\131692\1005066X\USn>\138859\1103888\EMl9iQ#y&\1045035+\162880\SUB\157158\186690mtb\FS\ENQF.\1044807\ETB\US/X{\GS\DEL:^)_\EM\"\SUB\180660*u\127154qn\t7P\CAN|b\32170\10673$\"SQ?E\992071\988250\NUL<\\\188234\&5.0\1044422g6d\NULA=n\tx-Hi3DU\1042619\179566=Yo5,\163525S\167821$/>\SYN\174673\b8z\1054067\1057469\&6!IG\DC3\ETX_m\182211Q\178659\bm\GS\5667l=0 \50133tA%\DEL\139117[}P\SYN\163285\GSb\"hw\34294\ACK\vJ1}\1037364$%\1089500C\138271?(\v\57736\v\154898\1048679\SO)Bj\ESCi\52062i\RS\1110207\EM\33516)\1013786V\121251)BM\ETX\30148\&3a\191006\&2\1051182\DLE>l\1012313\v\DC4\26436\1106068\DEL\ETB\44487\6721!#\SO\992108\70057\38800q\NULX\DC1F8\RS*7mPn\ENQW\SOif,\146459\68801\1081967g\atWo\SI|\166891\1095803W&/)\SYNb\1083839<\CANC\RS\55229a#\1027399\&3\1023861\983662wR\DC1 \1029712|/e\1041457\1078751\"&\ESCV\9896fA~\21012\GS\66884@\ETB\DC2\ESC!\vTJt\NUL\138082\NAK}s\SO,\FSy\SIEnElBS[-\149460lN\152753o\GS0jj\DEL?",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "QA=="))]),
      updateServiceConnEnabled = Just True
    }

testObject_UpdateServiceConn_provider_2 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_2 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "+)| \ENQu\132121[|G`|\1040791\15047\&95GpkVm\179149\&9'\1095291y\DC4N\1090395\&2;f%\ESC\163769\6676\&9}\US\23801)\1004419\DC2\174995w\DEL\v82^\1113829jtZ\881b\ENQ\181571T\167461\993132\EM\1058779\66324\ESC)\ESC\GS\145456D\SOH2\"\78054G\1034108\1007724p\SUBV \156796[\176190\&9t\EM_\39958\1066046\a*DVE\131211'&Ls\990176\tt|\992680kk8\FS\97637e\1082040\aTi\145584q\DC4\1015584kR<f>4\1051046\DC1vZBp)%)7\1049932\1067472m\NUL\61327I\ETX\1059016u\DEL\1042762\NAKi\1107524\1081325\ACK\CAN\1097686w,\DEL\DC4\"\44527\&9y\ENQ2A\DLE@\184152O~/rQ#\a\95564\19393MZ?\40205\161527\n\1052423\176558dHa\bS[Pg\DEL\182722}\t\n\164475\190962{\53676,\US\1004610\&3=f]p\1071518&\RS\STX&\8086\1054341:,\DLE\ETBx\1049389\&2\\\991260\SI\1043333\NAK8R,?6\DC4\65761\SUB\989022\DC1_HHSk\SUBopnH\tE\1076132\43655\&5/\STX\45409\a{\ENQ\ETX\1083721M~Y\144193\1073005\142836\&1\988121\1048654\992897\&1|l<\1031839\rPi\133054\1101047ABh7\27814\96862{N:uw3\151854p(h\DELwN\SI\NAKUf\1102463\150103P\ENQ\1074920q\NAK_HJ\1034658\1101595\v\EM\16883|K|\SO\ACK6yS\1019630k\20733\t+Nx;\1017121r\SYNQ/;s\GS\1045420:*G\164017C\ETXm\ACK#\1000114\12877V\169274\&7,\"r;\58557z\SUB7!YYI\61386A-wC\1086129c\1010103\28026&rKJ\r#7Nq4o\1006018\n\1055756}\EOTQ\ayIwA\111034\SYN\1075090\1003496'Y\47832}\SYNYc\47414#\27767Y\SI\16751\164771t,Zc\30393\ETBXP5-\NAK\1091008H\ESC2\1105144\185806\62391b+u.+\1002917,^s\ESC+\v\998922MTe\141056\DC1`\1100336~s4r^\EOT\1090306\rnEW\1007431X\1095464D\1108330 \141831\DEL\163685$\NUL\152132BS\1094612n<\GS!,fL\SYN\1019156\1089303\162030\184646xu\tVR\10264SvgXL_\1006409\&43\68768$>\SYN\NUL\RS:\171701\8999\1096643\GS\"U\"_\54854:\ACK\29845f\ETX)\9816yBK[KJ\DC2\1060909\a\7287l\1025318M[\DC4:EGBo\DELflD4\1011645L\ranZiv&'- ]\2070\fB[\v\1028002\1088988d{(&6\1091108O\SI\DC4\1080293\SOH\1089060`\12769\1101797\47171Hhq\160300H2r\39026\15463\US\r8\92242\1002459\&1\r0\ENQ\a\1078486\SYN\995748QQ\ENQdt\1093632\1086005#'\ETXK^\1064639(\SUB\990804N)M\11804\1092898\1002195cB}\1948\1095791\SYN\1046504JZt\NUL\1018901w\t=\164602ya}(\SO\b\996327e\94822\DC3~\1044914\29528)9\1080009\1099690nI1\23611#\9881\&1\1007335qFG\6500X9zM[t\44727ii\RS\r6JQ\SI3Q\FS\1063991Nq\26275\DLE\172731\141475[\1111927k\142278\ETX+nbs\RS0W3\US\1019367\NUL'\al\aIbN\DC1\NAK\ESC9\15908\155439?(\1027259oT[w\163780\66760.\177719\ENQ IlW\17013H\n\r81\v\EMAC\1111637oN\25386Dtg\191292\&3'\1037882S{\ACK\1071846l\998294\1020722oi\ESC!,\f\1073852\1034280\&0\184139{a\1060324\145065#p-\GSX\EOT\bI\DELq\DLEzo9U\DC4t\r",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys = Nothing,
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "5KalsQ==")),
                ServiceToken (fromRight undefined (validate "el0K2cA="))
              ]
          ),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_3 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_3 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          ",,\ESC\DC2i\40982F\DC4O8\164360P\1110158`\EM-{A}3\144146\EM\59157K\60476QK~+,X\28979sTF\RSCF.i#\1110927K\1037977Q\185888~'b\DEL)k\vJp\1013700B\ACK\164756\1026430",
      updateServiceConnUrl =
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
      updateServiceConnKeys = Nothing,
      updateServiceConnTokens = Nothing,
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_4 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_4 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\998823X\51416x:L\1108503~\1081065\179896^u\147090`\v=\EM\17195\SI6\1084185\1110421\1014174Q\NAKp\1005116\191234\1072050\186595\1110889c\129596W\1053917g\175490/\DLEc?\13917a\1033729%=0@\GS?\ETXs'\1018967\168225cQFfA\1020720\ACK`6Vyu\1087659\SYN\SIQ\165109\1073798\62456o`\1072757\GS\r\\}y;\984161jCk\fg$\a}\SI\USh5,\ENQc\1048050m\10195\&1\59237\&7@2\f,-U(Z\1086790&\16311\164166\STXR$\UScB?\1027375\vs\SO&\t*;\1099821aj5\1011812\28555\162408\SI\DLEI\985837\1059736\GS|\95430\&5\DC2\aXO`\185053):\169868\NULl\b\1087087\&9\SUBz\5115\&1\NAKC\1070536c\DC4$ZY\151608:\atq \ESC&>\EOTdlu~\140630\98361^-\n\ab\RS\20775g\NUL\ENQ\1001283 qy;<\24769\SI\EM\DLE?\125026m54\NUL2)y1\SO\ETX\1106368\1076724\"V[\1035849\SOH]\988558i\121137O\v;\5801\CANB\180951\EM\1110465\NUL\1070697P\110997\996463\1098272\&8L\nC\1058911D\US~\EOT)\1106263\49650a\ENQWn\9909*\137125\1107951\EOT{\CANbCzzGW\DLE\1007282\131870r(\33868\&5T\SOH\39403\US\NULa\1030299DS}r\"yz\DLEs\ETB\1097590,dP\96305\1090751\ACK[\179037e\1076353mD!T\1008638\v^\70675T\EM>}A61\ETXm\DLE_7f\SYN\DC3a\16741\SYN]\1101678\1018543\&9s\DC2O~a*:O\SO\36905\26464?\NAK\1006010Lp\120936XI\127258o=e\fp\GS-\"\1078156\&9h\1089507?|~\SIa/FX\USl\US\1014043\190432uX\1059318%]qlfXxxLP1g\DC2r(Jjm\37174\134955_v\1022678j\SO\33008\&3\52949SY\SUB\n\48504G\DLE\ETXn\145113\f\1001617J\NAKD(ns.M\1046950A\94402\992891XI\996351\987337\36063\&6m\994039\FSS\1057973+!\183589\144687\ENQ\SYNY\f-e\ACK\EOT\EOTW\1094735\DC1,?3\STX+\1103278\38508.\167813\&2\"\1052642\ACKI\SOH\GS#\RStJ \11809\DC4,}.wo\1016501",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "yXM="))]),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_5 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_5 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "]\ETBN`w}I\1077225\1063207\194886\ESCb\RSw\1009412\45450\1037598\RSTlqD\51158\161489x\b\1073534\991958\US\SOH6BZ \DC2\1111505\1041340\74089n]v|\1001629\ENQ+\b\1068414K\1096643H\SI\FS-9Of\42179ST=\STX\160642%\1026333i\ACK\1092593\155629,\SUB8;2\DLEF{\DEL\147933/5g\1061459\1108739T\EM6{l,#\SIVG\SOH\1019450e#u~#=\161137]6\1081794t \DC2/q1\NUL!\1015690\&6]g@o/\fR\DLE\1016108\27347VY\1091689R^\48943\35925Tqj(}\156901\ACKem\99629\SI\1017747\136120\121040\\\1092184q\ESCbxFQY\US\1106578!C3V>|\1095264\NAK\1045860h\RS\182757wl'\1067837I\1028704\a<:\182006(9E0j\35838~\14622f\\\DC38a1[N\ACK\RS\b\GSE\\^K\SOH(\166682\&6\tf\61599\DEL\ETX\999448Y<\22136Q\ENQY(%$Iy\fE\GS@G#\180989\171711\DC3\1034013\1035014\20714\SOK\1095577wS\44294\36694{\DC3Am\28623C\1083349\ne\179359p\1065578N\9086F\FSxfT\n\172966j\1046025\DLESk\1110958\1031038xar2\160384\&1\173990\141065\1037577\SOH\51109Y>\SOH\151803Sr\f\994611\1025721f\1013214\DC1\12375\67110<w\ESC$_$\EOT\\g8\GS\65261E9\ETB\SOH\994046.y\180637\6488\1087734\174986G\48790R\SUB28@e=Z_\f\rv\STX\SI\\k~t\1002061F\97312\1110620\f\158703V\189621\169711\1095607\&0\DC2{\1025430\100195\121158\&9\DC3}.b\FS68$e\FSWm\ACK@w\f*F\1000253N2\138986\32856P\1094381Kh~q\ENQ1s8\30069\1058282ah\134322\173157\SOH6\9425K*v\ENQ\SYN\69454\ETB\DELr_\1047169\&8\a8_\1113660'#\"\ETX\37991\EM\1046117\1017291ay6\SOH-&|\DC3PG4{=F\1032591(ln\988686\59705B%$l\DC1qI\1060413w\70721c<\ACK\FS[Wq\ETX\ESC\ESCf7\EM.\r\1097718\1064310O\148957jUp$\19209\rx\1025938z{\1085418GQH\ENQn4\1094930\36549w\STX98*/\ETX@F\5264\&2c\993986=\182000\&8\CAN\100636(\SI\fK\63761\1073204\1055824\190910=J\1112735\SI\n\52573g\1098012\999486\f\NUL\1053221\n&A\SYN\ETX<%Jo\SOQ^\DC3L\ETX\a\"\128872PgJ\1005097gkr6iI\143991\NUL\121065\RS\138404\SUBfO\4382\1111161r}\1023712\1096253\1111305\SOH-\47337J\129432\DC1p`R\n\119160\a\"\ENQ{`\SOH\131539\44442\1100101\74037x&\CAN\DC1\DC2qU\ENQl\ACK\ESCb,R\169072\NULV G.\ACK\1096110d8Y\1008827|\30487\1074384\STX\DC2{\20853c\f-8=1\SOH\EM\NAKY\1043993\STX~\DC3n'RI\1082205\1079793E/\US\ETBMS\46963\1016970\&20\1035089\SI\94765&O\164545#P\US4\145711X\158211\1073257\1112951U\184912\183826\NAK@-\146619\&6\40796\1077798SV[c:\1043469+\1027308\SYNW\173615\CAN'L\1105962:\SOHXhW[5\ACK\27841\153628\1034887\127292dj\DC1\27970\&7<\1017435ZaDPP4P\SYN\CAN\n,\rI9n\NAK\1066821\1020273IP\14525y\nY\fp\45423\10389\r\EOTZ\992202AqW\140085\v\SUB\99669C+\1035417U3\"\bSE\1036108X\174459\ACK\a&c}\b+\1086610\US\SUB&\r\1048894#?\r?\1082207\1058836\a\39190b\SOH;S^4\1113043~\180358\60266\ESC\188485N\NULw\177745\1108305\983812F\13297\44779;PY\1036195-\22600\DC4\172853c\78258lNUlt9}L\DEL\1050262~\b\CAN:,\\V\GS\1026576)AK\US\r\DC1i\FS\DELO)\189912\48221\1006250\"\SO\v\1061770g\SOHk:.\1049957T\STX\40674\&2\98600\&7\1088366KQ\FSH\169989m\DC3D\a\137742w`:r\52568\DC4:U\SI\16084\ACKS\DC31\SOH\EOT3\32611'\ENQ==\nyLW-|\a\125079\CAN\EM\DC4\rWux\b|\EM\100670\DC1\\@\1076339\1110606:\1020426O\78002$`)v\119556\39652\&3\22778\RS<\1039392`\1057033`\1075542`\EM<@\STXg)\v<\1039649rov&\153157\1099134TzVW[\SYNy\FSF|\1085932\&2w1\142461E;\70738\FS\1034142c\997202X\172676\EOT\167798",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "")),
                ServiceToken (fromRight undefined (validate "25QkmfM="))
              ]
          ),
      updateServiceConnEnabled = Just True
    }

testObject_UpdateServiceConn_provider_6 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_6 =
  UpdateServiceConn
    { updateServiceConnPassword = plainTextPassword6Unsafe "n\EM\1097578J\154077\136250S(G\1099243}\ENQa\SO\1048917Cbmi",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Nothing,
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_7 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_7 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\US\100069\DC4Q\40084Y:\169212\1022244\ETB\SUBM\RS\DC3t\996115\SI\EMD\DC3\EM\44581\50401\1103830\20577R,Ql\156956\&6}\DC35Fg\66420~\131804}*G\FS 0,E}w\NAK~\49716\26599\EM\SYNn\11379\ESCB\95781\98621\&4\43607:H\63038\b\1008248zhyc",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Nothing,
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_8 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_8 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "Y\SO\1017069\&36\EOT\111351bOlW\167871@\"l[9%\10565\68863c;9\DC1\ESC\ESCy\GS\1095664Cp\STXhYJ}\DC4\SOH\1101832\1109183\&45F\57999\&4=6f+\134075#\120471\13182\RS\STX\1105226}\DEL\SYN='\DEL\137232l(=\RS3\RSAK\138764M\vT=\1056251Z&`\40981\60743,|\1053502\a\1104352\57977Z2\EOT 1cc\1061591\DC4\33282C\US&U\DLE\DC3y\1006769ki\ACK\nw`E\CAN_\ENQR\169074\SIa\1046147\\P&\25420L\t~|i3\178403);e\132049_\158218\"5\1002123V3\DC3&6\1019524\190305E\1061301\DEL\162919\151745\&4\NUL\161153#Fb\94509UD\1006997\1056155d*i\f*o+?\a\EM3\1096922\\}f\ETB\990968\1006894Y\f5\1049494\&7]P\993489\DC3&W(lZ7?\SO\48757\10058)2x\NAKN$u\66809y\33818\62164|\b\\K`",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "IRIB"))]),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_9 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_9 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\t\1004295\162055{\DC1Q\rU\ETX\1093429\141292\NULhij\145889\34900\CAN\140329_\SUBE\NAKH&M*M\ACK\163606Pc\1005914'Y5\DLE}1u\94365=9\1048473\SOHp\1028956\45250Yc>L\NAKS\DC4\9494'aLM\GSl\ENQ5Q\r\b{+\1010407\54694\STX\3170E/\1038169\1101732\163194\1071944%O\t\50278E&\1097597aVEdM\1031226B#Sk\1063346,`=o\61550U\SYNP%$\ESCq\1106926U\t\127854n\58958<I\SO,M\174837 \157974I\SYN\NULqO\\V\991869J\US\20823\179423@kcvt_2\93995\&4\21502;GN+\1049624lr}'+\154587\ESCQ\1084211+\54698\94255\1006145\&3\DEL8\DC1k\n\171862.\ACK$5[#NyO\1113886n\nr]2IP\DC4m)\142352\188268\FS~\1012994,N*G\1093524\&3\SI\GS\DEL@%qvt\f\SUB{8!\DELd\r\DC39\18508\181322\v\DEL_R\ETB:\50468\995126\1107824\ENQ\1024437+\SO\STXK\137215\SYNbz\SO\"\1025281\187378\&4\1074004\STXIb\ETX@\ETX3\153632\27153\1081833\1110177\b6:\SI\1056723jYp\DC4C\USz\1072441$\t:\1068234vh\1034872\159005\CAN\STX\DC4\50838\NAKR\186266\ETB\1089493&^.,\1055696JO\1000381Wn\SO\ETX(\1017668(g\1048173\"\rH\167792\ACKbX\1088829V\1027239\172621>\ETB\ETBWhVB\SOb\127121[T\155401\187876\169584+yf\1003534LJ\FS\SOHnn%\58734-\vM\NAKz\186535\174616jF\1112890-j\FSy\1056822ee\59349\RS\RS\DC3L*grCi'l#h|\1004844i0H?\164702&5\1002827nlD\25298\993777Z`\SOH\RS\SUBY{0\1054005\GS-4[W~?\DC36\1011105\&9\ETXb!mB\ESC\\.P\1087523igO\DC49\SYNF\131796\1040687}\4110q8\NAKYMS\1002659\2652p\1065434@f/\1099324\DC4\187209\1051638x\47542K\ENQ\998157t1`\54485\1017782\&9%&\SOHb\DLE]\181021&\25645u\1051933\1060980\EM}\32354smg?x\1048733\39344o\154541&\1053210\&1'\DC1\ETBO\SUBv8\161106\987513Pe9mK\33543B\1010759!j\1067279\186235\RS4c@M\DEL?\a\ETBc\1100803\5649#\994290\ESC(\1099246\1012906\DC4\111062@s",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "UNqTIw==")),
                ServiceToken (fromRight undefined (validate "TcxpLPQ="))
              ]
          ),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_10 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_10 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\997769\&5y\SOH\1077253-\STXv\ENQ'\1013305\187122\157714S{\rL\1081690]\n/\157912\58428aHB\24264WD\66619\&7>&\bP\"\1017643\b\1089515q\a\183348\r\SOR\1014552\176079\ETX\"*\828\DC2\1043428\1000500N_\1005066uF\NUL\"\DLE\tRIL\1078390\1098873[\EM\ETB.u\10586\1006943\"UiJ;Kd8\1039008\&0\30306&\f\SI\162744\SUBY\1008806LK\61093\DC2\DC4z\SOHva\EOT\12884!pV#\1104879+=\1100776\18104\CANm3\ETX\9066\63172||<j\1100811\DC1u4Tp\61720\&55`2\FS)x\EOTh\US8\179442cXCOj\1004161]H\"4)%\47793g\\2\994468\162412\fj\a~:\FSzR{\DC3u\169628jZ9\DC4\29921\\\a\1029851J\176500(|)\1050940x\14784\176874\RS]u\EOT\1016839TE\DC3\\!%@-yd\179791a :n\f>\169448^\96706k\1023330Z#la\59350 '9b\1113666\&7mX]2iWb\n\190991\1086837*2%\1021942Rqs$z>J\1015846B\1059046\1014472\&5|\n\ETX\1083565Y\133520\151004f\EMN\1008112`\151361\&9CE\1004364\ENQiU$%\1108721<\1051653;\1052829\1018452!\aF\SIv\172482\&1fx\1084389pz\NUL\SOH4m\158767&m\SOHzU\STXai\DEL\r\EOT{L^\1069351I)\EM0^T'pV\189557\142219{\33681?'\t1\b\ETB\1003846k2N\CAN]}\DC1\DLE,\164970\1071435>\11135U\190941u\vZ\th\SOI@\ETBe\f)2mg6}:s0\NULa\132591m1\EM[/-$\169856\CAN/-6\NUL\\P\f\fq\8201D~fp\1014825\SIRz\1026058\ESC\170772}c\vh\\\NUL\1005676:CY.+\150506\1018750$\r\77874\48956/\CANi\SI\"!m[\"M\1110323*.u\8922\NUL\ESC5C:\96606j\SO+6\64030x:J\GS\142277\"P~\v\1106653\1031178 azK\1045557C9\SUBH\EM\132709\1106185\EMD\59381\DEL\ETBa\a\"w!\vO[\1002646 c\DC3\152706\&8\DC1\ACKu\147193\r\FS{!\SUB\44738\ETBmM\1054254o\ESC\DC4\f\1501E\SYN'N\137549#\1079995\DEL\1040911\DC1\SOH\169691\&1nizru_\1080817\DEL\174475\SYN61\1075510\SUB\DLE\SUBxQ\6157-*zD\183523\GS\1271\ACKIx\DC1F\41942\1016837~\bq^\DC1G\59001E\24917\1017983n$\168123-j*\92680h6%^F\CAN_u4Ef\58125\1113047\&9\bV578\33478\142522F>\20387y\39307\SYNrW\DC36\n\991819`BmCl\t\1055055\&1\184705\131098\1054689\f\DC1.N\GSD\t\190261",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Nothing,
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_11 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_11 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\NUL\1039410+\1068094\f\32165=\26661\&6\96912uix\SOFI\1074896;Oy5|*sB`\rz\SUB\39196#il~3p\SO\136450\STXR1\1035394\&1y!\n:\FS\1030524%wbKet\986807\32629\ETXh\39291}Y\127366\EM,lSI\1047988$\171754X\184873X\GS&\SI\ETB\v\1034870\1008812cCR0i\163769m\ETB\1045128wIC\SYN\n\DC4\ACKY,S\1098786m\16798\a\187665\USpF~\CAN#\1103408\bAh\1046849\"\181489\ENQ\EOT~s\ENQ\DC3\t\992102I_xZ\ETX\988002s(\42396DA\1005736\1094958\133185\DLEL\31943(\b6i\DC4j\160392>\b\1092152\&8\ETX=3\rFz\50418{,g`\CAN.\GSC>s$\DC1cV }@O\995276\129551\tb\164051\ETX\1090390\166063\&6<OXW<#\46320t\ETBw.Yb\1027648u\NUL\DC2\139767*\1073033JB\DC2mb\1231A>\SI\17512\&8lR|\ESC9c\NAK\1067118[\1084738\1082491j\1028113\SYN\rSR\1065825\GSg\EOT|\SUB\ETX\99553\1025396R>\RS\95055b(\1001611zP\1049004{NF\1110583gq\NUL\1061911s\DEL<\1098832!?\r\CAN5\1048092\1004099\DC4jW\STX\1062849Ib\CAN\149511\DELef\SYN&z\5327\186881l\CAN\175815x\n\SOHiE\1086555\157602zw\DC1\1073863|\1056621`*e\SI\SOH\n\1095029\&9\22631\1017717TgHL*4KU\29116\1038790d{\5770\1008429aF}c\29509lAV\SYN\SUBo\60764\GS\v \DLENnJmz\7285\v\98968X\n\1062559/)\tV!\151950#xH`qG\FS+\1022894v\1112591A+}pK\NUL\18200\DLE\1014161\39367@XB\1022649\fd`\r}HA\1098736O6F\b\1106094\176048\DC2E$5\CAN\SI\STX\ENQI\110776l\125049\1038537-\181021P\1008889\NAK\b<v*,OFw\1022575(\118862\1038733\US\1022658\&9\1010204\STX\998667LI\1018985\DLE\987709(\40684\ETX\t_\1027403\&8e/NL\DLE\GS|-\155215\987306\52696\SO\GS\tMu8\1009448aD8\ETB3>4&Y6k\1049678;\1113712l\18726\1027540[\139508\"\CANW\110623P\STX\1011964\989283GMC\186990#\1016158_DD\STX\DC1\ACK\58642\1021046\175312\16600\SUB\8585\&4\EMI!1\FS|\t\r\DC1)\26943\DC10@",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys = Nothing,
      updateServiceConnTokens = Nothing,
      updateServiceConnEnabled = Just True
    }

testObject_UpdateServiceConn_provider_12 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_12 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\993220\1038660JXV''\GS+-'\SUB\170581\US-7\43896Xr&D\184991\NAK!l\DC4\180616\&37T2K\DC30\165699\1028198\FSf\1034228\78643a0\1040973\166882g\EOT5C\41427\18581\SI\DLEi\172589|\1023099|j|Z\1027919Hc\1090518 \1062911q\\\ACKF\ESC\25422\CAN\SO c>\74396ca\96458\&1\142138\35173\1004117\DC23G\"v7\CAN\EOT\1099295g\f\1107486\ACK\SUBEhi\GS\EOT9\SOHv\1080551e\"7\DC4\43597E\98124\r)\1102009\rw/Iis\1025536\SOH\97931\&4E^\27334m\1048941\1007679O'\48945A\1079964\19956\n~SSEJ\165849\DLE\EOTcL\1045161c\DC3\1016438`8\DC4Es2\RS(6lDMD~\NUL^\121204\1025259\1050222\SUBw3x\ENQ/g!}~\vR]\67993\37327\SO\ETX.X1\983377z\136253\CAN\CAN\144168+\1071342V5\165416\1054183\183010XQ\187880\7622\1077469I!kea\1097869\DEL4@\"\t\1078208\1099149\43628M\SYN\1065348dp5\1001583Hz+\1022080\83262\DEL8\STX\ENQ\SI\19782V\61880w\194717\170930\NUL(\176178$9\DC3g\25394\1046505\&2S;\t\ACK\DC3\EOT\SO\NULj\CAN)z|\SYN/\1041123%\t\181144\72411^D`0\DC2\1067402\1107058\984800\1005844\120958\149529\1049220\1002522\NULfgh\SYN4Td\129587J\1109052\FS\37807caG,Si\140100W\1091163E6\1066725\FSC2\8707+\n\144629fn}\1068169\17347\1014616\SUBV1%\SYN\157558R\990269\14875+F\984275?7\126233a\")G/=\vRx\1080985\63164\13794\1011824\NUL\EM\DC27jQ1S*XcO\17051\1107557;)ls`6\DELe\SI\1033603U\111261\96008RMf~q\140619`v<vg\1019707\51571\1001225j\187852\16622\SO\1041564<\53657hD\985881\ETB#\160604R>\ACK\1053032\&0\GS/y@oF\1013954\RSr\1051855\CANA\ACK($'~\1100152x\EM4(\n~quJP\1110016\1014656^2D\tw\EOT\999641t\1007432W\1028093\ESCSJ.\DC2}\RS\1035745\97267~\ACK8M\146611\1051882tz3[Yv\27460~CC\ETXc\28165Of\1112868\GSn\1109968TNm\SIxU\ESC",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "_b76")),
                ServiceToken (fromRight undefined (validate "uA=="))
              ]
          ),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_13 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_13 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "t\GSC\1000753\bX\164154kx?\1013629L\36559\&7Is\DLE\1095203\986589\&1\28485Jd\95666\993761\n\36454&=\DC2\189436\r,2\EM\SOHpH\CAN0C#\\/\1056247O\SYNdDG-E\SO\142275\SO&\DC4(L\fZ8\1006244\1000574f\190213\1112952@\SOH(&\186075L\31730f\DC2Pn\73894h\1002020 vjw\SYN\1008529:znpI\SUBu\DELE\1065996\187117\146380f\1065951y:_\1094517d\EMr@#\194907\&06\1039784\SIaS<g\98711pC3E\DEL/\187792\1856\&6d\STX\159560\51838\&1\1101752;H\1054895\60571\EOT\t\147850\72276\r\161824|\EOT,\1029001P\41682\EOTE\1082925\1075118,5\\f \1006113\31285\1096458\r.\DEL\141135\DC3\155488\51983s\SUB1Y_9y\DLEq@\1048683k\RSQ\185499I;Ww\1112040<\1091342~i1?mk\r1j\1013717\128958\DLE]8\54261\n\1001968\1053141\&7\DC2^\1091148\STX\131200\1062316\ENQp)\DC3\DC2\43855$Fh\DC3Ndp\1095639\1105657F\77842Zb\1039666`@_\ACK}reco\DLE\37746\1098282a\1011210G\SOH\n:\154903\DC2!\SUB\FS\DEL\ACK,c\1023146;\26228Y\ETX7v.age\GS\SYN8\1033923Sf7B{^\1058414|\"\ETBG\1113523y\RSG\156933m\4341\EMC\nv\ar~\995880g\1058014\995348\SIq\992707E\USSIo\985560\&3\DC2<yJf=rB5\re\t\186080qu*\ETB?xfx\997571\CAN\61076M/\FSnAf\20048\166764Hv(x~\1099997\15182?AyL|\b~d\152166\1039118\&8=\npM\NAKr\GS`F\ESC\ENQ\1032220\DC3&\7170B$\179387asgsLz\1010934W6i\170857\ETXn\SI\65751JQ\RStXL\10043)\v\b\157368\172933k[`\140853`B\984290\rv\ETX^{>q\169253=jJ\ENQWTB\42831<P\78198{>\ENQ\b\a{e\12482\1130,\133300\1049410\1054859\US",
      updateServiceConnUrl =
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
      updateServiceConnKeys = Nothing,
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "afay7A=="))]),
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_14 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_14 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\190394h\SUB\1109797\DC4*\RS\EM;\NUL\"0\EMUoX\1029877\ETX9L\1052240\ACKm\30939\169455\&7yc]l9N\1008545'\1102625oHl\1105755\1019260]xQR\707\1101356\143488__@7\33574\47923m\983514U9\42346-)\n/\EMT-R.\FSR\GShI,J\149880\ETBo.\21840\ENQ\1058903YP\1056152X\1100531l\48771\13939T\1015540q\SOH?~_|\121117\39498\1057936\NUL\175881\&2^=hBye$Gr4\EOT\61957S\\R\DC1\DC2,\ETB]\1045067\RS,]\46572{A\DC3]B[\999576f8s\ESC+VGgmTkj\US\"I{W\29261;/ 'N\168508\1092432\70364\1042873$L\EMno-\SOH\5386\1037350\1052214\CANO!OHH=7\DC1Kcj\36365T5899\DC3+3\152617PTHk/\1052286\1109078?@\\uDf0\DC19\FS&N\1040430nkE\SOHE*\27176\1029316\1002801\1034060d}\1022512FX`FD\DC3\1095997C5d:g\177379\1085981\by5{'\DC2\v8M:\DC4\19403\157453\&6Js0!\ESCbT;g\b\141132\&3^6\DC2U\1070466!z\1054801H\1079152Dr\ESCIV\166596L\CANlh6D-p9\SI\ETXrvV\ETB\"\SI\a\ETX\989243l%{c\1054177\987256\1018036\1050434R\\\1039005\STX\159894H1+\15160\&0\SOXqM\10186\&1c\GSo{Q\SI7{Zb\151593\&3\1021654\183743\t\136248}\NAK\GS\95886\1092115\997138\&0Nij\ETX\t\92506\1021352C\13748\35262[\1049660\SI\1000937\SO9\1013277\t\1032553\DC4Z(\63140\ACKB\128501\&0d1\26793\&2uhz}\987497\SO4,>L$\1060453aUv\1043860B7!\132218u\176663\USQM'\ESCFI|\991412\1061444&A\STXO\CAN%ga\NUL.h\ETX\SYNp\987112\993913u\ETB\986350u\1007673y\1080137>\1003299a1%\b-E*\97670uh-_!T\40834\166613*\ETB\DC1\1023495\32162k\74053D\985690\32642%J\95157H16\119596\\U\170700\1030522\61957l8M\1086340G\30550]\146680\171952\b%S\RS\1036496\1064001Q|BQ\1069432\92302:KO9z97l\SUB\158540\SO\1082542]c\ETX\140799o\1083227c2\n\DLE\RSF\1027349b\1050948\SOH\vp]\\o-\1021196c\ETB3]\DC4\SYNt\SUB4\1049581\10708Os|!fmz\63956>\2632N>\24775G\1086284\178948#\11371E#,\128740\NUL}\180512M\1030210j\1025092\nV\1086401\98223\&3D`7\EM\NAKv9$8Y\DC2\994529\1034217\ETB\150192b^\986967\53183Truf0b^x;:\11795\1084517\39347\26525\SO C:i\1023504\b<}\1053280)B\1050491\DLE\52672sD\1063444*+Gz>\1052360)4R\ti.\SUB\ESCp\ESC\DC4(I\16719\1034269W_\1017734\1075210Y\fg`P\DC3n\157709w0m\DC3ec#<8)\DC4|#\SOn$\38394\NUL|ejd\ENQ\1108747\58097EA\\Cz\1102504\GS&G\GS\170391OwH\50355r\1003495\188221G(\ACKuN\ESC\1097964k-\1065205\DC1Vz\US\ACK\153795\ESC\1080576&\990206D\1018960X\ENQw/sx\54555P@EP\1069026c:\"\134166N2|M\FS\ETX-|\39506})Htm}\NAK\SYN\ESC<\1032423\1055241\\\1012449\168010\&0@7??AkY\1096614Z4\1053341&\68619\&6\v|\21375E\vDR\998672\&5\DLE^\NUL\163478L\n?0a]*mQBV\1017677\NAK9\EM\SUB\57722\SYN1JfI'\DC1}\1034409\EOT_D\171988\1043457\DC4\18796\"hU)on5\27639\SOH0s<{`\NAKl\f9\\\NAK\8614x\bN{\1027748\1023446\US\US\66723#_",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "67zOOg==")),
                ServiceToken (fromRight undefined (validate "dA=="))
              ]
          ),
      updateServiceConnEnabled = Just True
    }

testObject_UpdateServiceConn_provider_15 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_15 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\SOH)1l\987683\148155[y6\\ht4;\DC37\1054742P\992335GZ\SOH`-\1084634\168490xF\ENQF\DC3\1049610iP*\20000)5\NAK\DC3q#4t+$#j\DC4{1\1097854\&7\126236\GS{\1056548a\1107816\1054171\STX{\48079z\1054195/5O\1081905\27127&(o\39424~\984292\24987\ESCU&\ESC\1088756\v\998764\ACK\t\RSgFXNig\DC1@8\SO\ETXqp\EOT2\r\1077703h\1090197\1037670y\65729\1094478\1078657\1055314$N\DC3\35281r\DEL\534\v\SOH\1005065~J1a&\156371Lz0Y-\a\ESC56\39613\1018854{:0\tCG]68a\ESC\1093341\77856\FSh6\FSM>rU\1015613s\DC3_3<\181722\170960E\1103690\&7|\162612k\SI*c6\DLEd\1009741s\1007391w\42177)\"\1103677,-k2\45021!`4l\1102141\1085344\3180\160568$s\65124l\1016531O-hb\1113375Wk`E\36192\173301Fzl\DC2\1091888\SIj\SI\SI\SOQk\tyb\ETX#B\SUB\1034586\1075342[\1090619b#\GS\1111268\33422\1098425\&0\1081669,Azi+$\33444J\ESCUD\176210Ml\SOHg)\EM:\ESCL\983478\NAKX\t\EMA\1063521Q\45205=Ol:\151007r\ETB\DC3\110789\US\n\168042+F\1049002~\DLEp\1006119&X~\46361\1044213\DLE\USa9<\1073068v*\1025840\tF\1000262c}\1069962o)\b\172315\37902}<g;\77971\1027666P\20298\DC4\1086663Q:4\136812\31055cN\vk\DLE16EG\174557\94605\1051854g^0\60707\182391H\173128\143527#s\a\NAKn\173358W\147242{\53284eU\NAK\142062iC\SIo\"-\SUBpX)\23868szx\DC4\aa3-l\fI\ETBB\1060613\165446\&4t8-CJL\24078p\ESC=\ENQ\29062\1083438]\STX\1004671\DC2\r8)\n\n\1018086\fF\DC3_t\92660A\1046008\150808&\127148\1008237\94604mR\ENQs'\42069K\171886s\NUL\DC3r\DC4\1043587kM\1067726oQ\155453%\40674\n\1013850e|v\DC1a\1105031fiiS.\28153\SO\r\fva6S2^\1097837f!#\DC2\RS_(6\1065224\vW#t1#\CAN_w[M\1077580+\178576C,`yLx\1022772\DC1]\ESC\1003783\1022846{[\a\62513\41927\b\nC@7wwc\DC4\1040277'\993843\DC3\58655\9383\n\194756\39958ma\20864\&7<~\1073988]>D\1068546:3\167728\1009034\&7 \1047970\SUB]8\b\\j\DC1\993234\146703)\1016109\1027454w\171333\SOH",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "EvaJ")),
                ServiceToken (fromRight undefined (validate "BfDGJsM="))
              ]
          ),
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_16 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_16 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\644'\n\7167q\EM;\1032724BLAvpn\SI\1002860\1023507<\49854\STX\1013153\137209\EOTs\b\b%7\DLE#X1\161745\ESC\59873v\SUBl!eh\1021505\v\35441\993107o0I^:\DLE\1086270\1069220tn\DC1C\1019637jX\NAKm\188269LTK\SI'\ESCF3]nr<?y\RSR\1016239He$\175827\"q\FS\DC1\SUB$\60407H=\EM'w\US\bh\EOTdjU\1013067\ETB\60534TK@Fj\SOD\1076714:)\DELq<\123153\NAKP,}]tTH\SOH\1108863\&6\t\"\\@\996592t<x\987632q\134083#q\NAKP>/\168452\\\1088205bB\SUB'\b\157100\1014790\&3\SYNg\n}{pq\NAK\1113906?\995672\190804\CAN:\175546\\\1069654ZMs\t\1068874\&6S\1024467\1093547nO:Xy\1064925\173331\1044605\164489\"ry\DC2\NULT#\1077621M3\DELS[\127107\48973K\1104211WE<\1018102\&1Y\b\53326\1051138Z\1038689\FSU\993629R\175863\DC4FN%\ESCi\DLE\EMy +m\RSk*Txd\19948ji\189084\1074062\1081201!6N\DLE!_!\1026215\&3a?s>\EOT\STX\1041788\31864@\129112\&6\f\DC3\996985.\SYNz+8\NUL\1077938\1069477\EOTQ\ETXtV\\`;c\fo\50816\120881\&0=\1065656\EM[\ENQ{\1052186B,\37696'\48642\157636\162832\98083\120030PBx\998172?6I]X`W\158572\ENQ\49963\986583\DLE'\a\ETX\1074659\&7U\59933\135290\1008696\9082\ESC<^__\100688B;\1099451%K\150128\78399E\989825\DC1#\26616_\DC4\1061882k\1059333]3.6\92298\14451%\US2\143989'p\DC3\EOT#j.\31151@\1054758\&1\155144\&0;NR'\1048341\60816O\1032754\1094257\DC3\abJ/\v\1010244V\1047548 \SOHo,2[\RS]4\f\nGb\179257|\1048501\1048359b\SOH2C^x\DLE\ETB%L\FSQEF.X%\SYN\1076692\1019419\\ze6\ACK",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "pnY="))]),
      updateServiceConnEnabled = Just True
    }

testObject_UpdateServiceConn_provider_17 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_17 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "\EOT\1045806t\97003\SUB8<\ETB)\EMa2\NAKI\NULc=\1108345(r\SO/\148273.&\147705\&5\SOHfH\1035927\163968['\991226T\997928\&2\RS\83083\vy\150182\1096305\144065juC\94678",
      updateServiceConnUrl = Nothing,
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  },
                ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "AjoN")),
                ServiceToken (fromRight undefined (validate "c_WGcw=="))
              ]
          ),
      updateServiceConnEnabled = Nothing
    }

testObject_UpdateServiceConn_provider_18 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_18 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "BK Q\1014125\bn0$T!r\CAN\150725m\SUB\NAK$\142935\1090156\1040023\1110772b\DELn)\1057067E\97334\&0)ZK$z?.\SOH\167550[:\1105660r\43928(H\43310\bU2\179051\&3oS16<\1105969Z\57930\CAN]\47876\&7/\120262JUI\24934d\1080925\1009915\SUB\19033\DC1\1061392Z\EOT\EOT\54639z\SOR\1086710\&7\DLEs;m\37477\28396>\998226p)\\`15\DC3q\1095343l\SO\152478b\fy\14607\1075202$>D\152059X|\182419\1110462\1091179\ENQ\54447\&0Up2]j~St\EOT\159824w.T\t\1068686\f\DC2\186530\145288&W(\ENQ\1024967h\SI \991230\ETB4o\v\FS\1096193\EOTB\1021960\&2!\DLE+\98373\vpK\CAN\163478b2]h@ei\143476\DC3\47232vG+ytaV\100000\&6\1033181\nE\n\1050459d@jI\168771\&7\18408\44309F&\1037698\134204(?#P\1102778R\1040710<\94302\1052687~c\GS\1079831RC7f'\NULf\1064876{_Z:Cl\SO\1103911\GS\74318\1062883&\101025\37781\1004774\1019853\&2ux\n MN\176144`\SYNp\EM\ACK\40602\1075473\1071332\991026bl\DC4\ETB_\US\ETX[U\SOH\NUL\995920\7454\RSZ-}e\991160V:L\46179 Xh\1032551io\1039546@\175935\NAK\STXF\SOH*/D\172325\&6\DC1\100415\12730OjPQU1K\1080043\SYN\994399O\RS\75043\&1\1092605\65399/[\182411-\a\1112589\&8\EOT\SYNcE\190631\1027179\1028700\1055026u\EMs\1042923tLzgD\184376y]o\DEL\59030\67658\bv\EM\11200\1009731nW\1058051[\GS\n\98337l*\1020276\149362*ca\1043242&\991049\GS>\DC3)\ESC\r\ETB=Q\149426\EOT\ESC\1110189\178428vGL{\1096339+\1068305\1097108\183886{+&\1032994\21683(\SI{',\987672@\98096\ETXT5\131519\140923\1009789J s[\\^2cSJ&g\155812\CAN\1110385 6\998376\1038801Q\159855\ETB7\\L\995599n\DC2\1103386\aO\1078070\1023853\1027079X:8YA7`\r\92176\176851dX$\1347\"\148822YYU\144717\987220#i#`\20260\1083835V\US)|\14405\&2\rN>\1107993\189621\DELM_&t\96315p\n\DC2\f\1105846\177556Vw\1015372\EOT\1063032\&2\RS$\69697SEF\SOo\53638\ACK\44500\1001122qYL93:5{b\120606\&3\991440\DLE\990212\SOI\bRT:\1065174vM\a\52129'?\EOT\1060415^\1013803\RS^\STXfY\163783F\42249q\b\vh?\1076636^\DC1l9<^)p\42648tmQBN#3,\DC3\119977\SYN\148346_\SOvAB\DLE42\1007350T!V\FS|\1054203\994976Y5gy\ETX=\132355p%e\46874\v\DC4\DC2\1018287ms'a\SO\990743\62031\DLED\RS\175516\993474[\1100496$y\DEL2+\"\24584\139851\155402\65081\FSe5\99380\&8iB*k;\SUB<\19304\1111933\t\STXz\DC3s88((+E\r!O\ENQ:\ENQ\94087\DC4T-CF\1093660KP\24961[f\66588fx\1104991$G\1099775\52417\r\990169\&9\991666\146083e\fs2\US",
      updateServiceConnUrl =
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
      updateServiceConnKeys = Nothing,
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "FX2Rjg==")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_19 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_19 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          " &n\160619\34154\&8\43937U\1039397>*\993401=\35882\171390\&3v\1059159Hs\26704o}\40618l\1014576\FS\148314m\NAKj8\987797\167591]/\14092K(:\DC4\68870\STX\SOHK\b\32766J\1065304\1087197\\\RS\NAK\1008251\1078793\155825%\ETXd\1077641qS\134619y*#\77898\SI\SI\ENQ\45021\42114\171376:{\nP\144997lC\1048175>z\1076881\1074716v\1105912\1001528\&1|g\2134\DLE\1058580UT\1092924)vUVM\CANS\1011756\n\DC1\fy\DC4\1087227c\STX}Tll\GS\"G9\EOT-\1056541\1048887\SI2]\178229\f\171206|}g\1045301\161759e\182773\STX\73448dY\1037520F\t#{;mK\137787\36684\29082\n\SO\71211\1019153\1018611\SORn\CAN\n(\1092530>FCD\154433-\1104128n\ETXI\94364\93819\ETBN94\178422?k\ACK?|,\1097051c\1040341Qkp\ETBm\13083\28246\STX\21644XpZ\1028843#~T\rB}@\EMB\ESC%\1056576\184331,\185280m\1000086\n)HlK{PE\138393_/[H=\22492\&4\RS\ACK\49640^+MX\99139\1094111{D>\1058311V\DC4).HX\1020816x\EM\4869E-i:,?{H\n\1032654^Xt\1075783\EOT\RSfC%\1036350X\992457\120586k_\rl8v\1059490O\132715\f[CJD\148581*\44858\31446~0lSdA\DLE\SUB\988261\68042|#vCkB4\DC1zwq\5448K\1109392\1071549\1094223aU\1046318\18272F\NULED\1016313\v\26976Su\vmB\1019120\US+\1011430\1090687\DLE\SI\1097286>\121221\CANUQ\162002p\37165\1019838(I\1077362\r\DC3\157492\96158\1110610>\1020297\CAN\170247hF^<7\ACKIn\152012\SYNw\83193\DELH\154839\189257\bF\170249\&8\1063107\11763\73876dR\1093883>D\1102005D\1079913\EOTT\GSP7nw\42408\DC3\147829m8*R\DC4\DLE\DC4a\998793\b&\153797\a\1064029\ENQ\ACK\26632\62173\a\NAKheaM\1103290\165755\992228m5\ro\154059\DC2\132686\&9/~&$c\EM\3111]0BuH\DC1\151251S\7591p\57398\28319\169436\16438\DELS PUYy\v-\1076742\DC16\995339\1102710\148982\138245\1105664\DC1t9\FS\27132z-\140909U\FS^c\147694!\194919x\98811\1016231{6\r\rG\fe\1060938\r\1022210\ETB\989852\bv2YN\DC4\ETB$-\1081630*\137645\&4",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens = Just (unsafeRange [ServiceToken (fromRight undefined (validate "1LoyRg=="))]),
      updateServiceConnEnabled = Just False
    }

testObject_UpdateServiceConn_provider_20 :: UpdateServiceConn
testObject_UpdateServiceConn_provider_20 =
  UpdateServiceConn
    { updateServiceConnPassword =
        plainTextPassword6Unsafe
          "4\70367\1069671\141726\&4\ESC\126570\SUB\FS#7e%Lj;\SOHe1\152448\1038592\CAN<T_\NAKtn5\ETBM\EOTp\40849\1031635wm#L\119044wOL\US\1047615\USkj+tsh;\STX\35711,9\40206\1086753\1107106\"F\1042105\RS~\NAK\1012388\1010085\GS595MESht\1023825Y\DC4wNG(!\CAN@\rH\1023688\1070213\vYN\1077438d\1026498C/v\DC2\1002782x\158395\152909Z\SUB~(\t}\DLE\135266\154944Bj6HNOqk*|&F&A\68006m\FSq\73753\45805C\SYN\166626D\EOT#~2i?\n\986589\t\8371,:(;\SOH\28829+\SIh\1086283\ETX'3\6474\SI:\194583\SI\vu\991145qZ\1099315[H'.\CANa4\996648\120801@l0jG&ihNp?\DC2\1011034\SYNiB\65608La\991812\SO\1094480w\161826\30745\f{KR\52619\1032877\142671d2\STX 0`\144858O)\SI\175306\DLE]\1030691\b\990281\1059422Cw\1027962X.\61794\&7\1073391AD\1102643&P<Sz4Tiz\990394\&3}<R6\1085319!\DC2\132778#\26177p\37823usUI[z.8\57478ff+66\1110594(:v\r\"KD&5\1046428\8093aW^\f\SUB\DC4\165735cE\46862\189333J\NUL\GS\SUB\DC3d}\SOH\4927\1101985j\ENQ\96575b\ETBpCSul&~l9/0\DLE\\.3\ESCF\36079G\NULYR%-\36368;\SOH\53760/N:",
      updateServiceConnUrl =
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
      updateServiceConnKeys =
        Just
          ( unsafeRange
              [ ServiceKeyPEM
                  { unServiceKeyPEM =
                      PEM
                        { pemName = "PUBLIC KEY",
                          pemHeader = [],
                          pemContent =
                            "0\130\SOH\"0\r\ACK\t*\134H\134\247\r\SOH\SOH\SOH\ENQ\NUL\ETX\130\SOH\SI\NUL0\130\SOH\n\STX\130\SOH\SOH\NUL\187\226\160\252\241\199Sv\173^\181\ESC*|4\ESCN\133\150%\220\&6\221\229\&3\tv\162\206m\192@\220<\241p\253\247\134\136\STX\178\155\SUB~\236\154\153\SO\187\RSK\144\253Lq\171[\227\144D\131\199Z\245\SOHv\"\223\SUB\182j$\237\182\220\&0z\SI\194\182J\239\232vi\227d\157\179\219z\225^\129\NUL\173:e\187\224\224\244\175\156\216\181^]2\149T\243\154;8-\NUL\GS\181\\\164bC\135\171\154\168\"\223\249\227\175M\235_*\191\168\217.5\222\173\&5\200>\FS\a\198\197\241\175\188$\152\ENQ\248\146mB\171\252\ETB\128\173\132\\\143:\255\135\153\181\"~\159\ESC\248\159\244a\b\234o\GS\196t\253%\182\&9\223\b\164\178\140\&2\233\168\194\186\171$ X<\237\DEL<\220\DEL\139\ETX\247z_\144\147\136\251\245T\204Wt\NAK\"\CAN\251\130\244\132\255\232#P\215\242\197\183C\247\237\172y\243\226\198bV\133\163\185Z\157\STX\ETX\SOH\NUL\SOH"
                        }
                  }
              ]
          ),
      updateServiceConnTokens =
        Just
          ( unsafeRange
              [ ServiceToken (fromRight undefined (validate "NA==")),
                ServiceToken (fromRight undefined (validate ""))
              ]
          ),
      updateServiceConnEnabled = Nothing
    }
