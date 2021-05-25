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
module Test.Wire.API.Golden.Generated.BindingNewTeamUser_user where

import Data.Currency
  ( Alpha
      ( AOA,
        AUD,
        BZD,
        GNF,
        GYD,
        KRW,
        MXV,
        MZN,
        NPR,
        QAR,
        UAH,
        UZS,
        XUA
      ),
  )
import Data.Range (unsafeRange)
import Imports (Maybe (Just, Nothing))
import Wire.API.Team
  ( BindingNewTeam (BindingNewTeam),
    NewTeam
      ( NewTeam,
        _newTeamIcon,
        _newTeamIconKey,
        _newTeamMembers,
        _newTeamName
      ),
  )
import Wire.API.User (BindingNewTeamUser (..))

testObject_BindingNewTeamUser_user_1 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_1 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\fe\ENQ\1011760zm\166331\&6+)g;5\989956Z\8196\&41\DC1\n\STX\ETX%|\NULM\996272S=`I\59956UK1\1003466]X\r\SUBa\EM!\74407+\ETXepRw\ACK\ENQ#\127835\1061771\1036174\1018930UX\66821]>i&r\137805\1055913Z\1070413\&6\DC4\DC4\1024114\1058863\1044802\ESC\SYNa4\NUL\1059602\1015948\123628\tLZ\ACKw$=\SYNu\ETXE1\63200C'\ENQ\151764\47003\134542$\100516\1112326\&9;#\1044763\1015439&\ESC\1026916k/\tu\\pk\NUL\STX\1083510)\FS/Lni]Q\NUL\SIZ|=\DC1V]]\FS5\156475U6>(\17233'\CAN\179678%'I1-D\"\1098303\n\78699\npkHY#\NUL\1014868u]\1078674\147414\STX\USj'\993967'\CAN\1042144&\35396E\37802=\135058Da\STX\v\1100351=\1083565V#\993183\RS\FSN#`uny\1003178\1094898\&53#\DEL/|,+\243pW\44721i4j")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("Coq\52427\v\182208\&7\SYN\\N\134130\8419h3 \30278;X\STX\a\a$|D\NUL\SOHh'\62853\&3-m7\1078900\SOp\22214`\1093812\&6QF\CAN\SOH9\1062958\ETB\15747FP;lm\1075533\173111\134845\22570n:\rf\1044997\\:\35041\GS\GS\26754\EM\22764i\991235\ETXjj}\1010340~\989348{; \119085\1006542\SUBL&%2\170880;@\\2`gA\984195\&0\162341\&2\163058h\FSuF\DC4\17376\ESC\GS\SO\vYnKy?v\129546H\fcLdBy\170730\&4I\1108995i\1017125\ETBc6f\v\SOH\DC3\1018708ce\1083597\SOs3L&")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("\ACKc\151665L ,\STX\NAK[\SUB\DC1\63043\GSxe\1000559c\US\DC4<`|\29113\147003Q\1028347\987929<{\NUL^\FST\141040J\1071963U\EOT\SYN\65033\DC3G\1003198+\EM\181213xr\v\32449\ESCyTD@>Ou\70496j\43574E\STX6e\983711\SO\ESC\135327\&34\1063210\41000\1018151\&8\1057958\163400uxW\41951\1080957Y\ACK\141633(\CAN\FS$D\1055410\148196\36291\SI3\1082544#\SYN?\ETX\ACK0*W3\ACK\1085759i\35231h\NAK-\42529\1034909\ACKH?\\Tv\1098776\54330Q\46933\DLE-@k%{=4\SUB!w&\1042435D\DC2cuT^\DC4\GSH\b\137953^]\985924jXA\1010085\133569@fV,OA\185077\38677F\154006Az^g7\177712),C\1020911}.\72736\996321~V\1077077\1024186(9^z\1014725\67354\&3}Gj\1078379\fd>\57781\1088153Y\177269p#^\1054503L`S~\1101440\DC23\EOT\145319\24591\92747\13418as:F\ETX")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just XUA
    }

testObject_BindingNewTeamUser_user_2 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_2 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("G\EOT\DC47\1030077bCy\83226&5\"\96437B$\STX\DC2QJb_\15727\1104659Y \156055\1044397Y\1004994g\v\991186xkJUi\1028168.=-\1054839\&2\1113630U\ESC]\SUB\1091929\DLE}R\157290\DC1\1111740\1096562+R/\1083774\170894p(M\ENQ5Fw<\144133E\1005699R\DLE44\1060383\SO%@FPG\986135JJ\vE\GSz\RS_\tb]0t_Ax}\rt\1057458h\DC3O\ACK\991050`\1038022vm-?$!)~\152722bh\RS\1011653\1007510\&0x \1092001\1078327+)A&mRfL\1109449\ENQ\1049319>K@\US\1006511\ab\vPDWG,\1062888/J~)%7?aRr\989765\&4*^\1035118K*\996771\EM\"\SO\987994\186383l\n\tE\136474\1037228\NAK\a\n\78251c?\\\ENQj\"\ESCpe\98450\NUL=\EM>J")
                  ),
                _newTeamIcon = (unsafeRange ("\SUB4\NAKF")),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("-\ACK\59597v^\SOH_>p\13939\ETX\SYN\EOT\ENQ\2922\1080262]\45888\917616\SI;v}q\47502\190968\a\SI\1113366&~\51980<\GS\1024632`,\1033586sn\2651H\160130\1100746\176758:qNi]\1051932'\1000100#\a#T\171243}\990743\DC2\1008291M_\FS\DC4\988716\1091854\EM,\SO\CAN^]\77867\&9\1112574-\a\SOHID. FAp\EOT\1033411\1004852(S\1052010\68416\129120\DLEsI\ETXe|Mv-\"q\49103zM\14348$H\SOH\139130\1004399D]\SUB\1056469\ESC\151220qW2\ENQ\1104272\RSy\1018323gg\1018839 /\1079527\98975\18928~&y\b\ACK\1084334\1047493\36198\SO\FS\SYN\RSt\\a.V\SO\&Hy8k\US$O\699Xu/=")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }

testObject_BindingNewTeamUser_user_3 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_3 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("Y\\73\1000020R`h\STXp\33328\b:'\1027731Uq$N\1082599\1001197G*<?s\68000\190946\GS\ACKBZ`\1024690\16734Yf~\STX<\6927\NUL^\1096878\DEL\47055Q\vUV_\NUL\162524n<\19839C#\1026408\133185\DC1\1056546\&6,\1064179\t\DC3GU\FS]! \ETX\128227\EOTdQoQ#\99603\9777\63351J\a0\ETBG\v[@\5416\1004252+\SUB\137188\1019659'\DEL,n)c!\1022975t\NAKTw\STX\USLf?4\63165f@\1033065\US\63296,c\amx\t]\992539\51082Qhv\1057376P\NULs\DC4w@+\1101906``/|\GSj)\26050\"\997228\1050873tWb00M")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\ETB$\164154\48491;\170142E\68300\131111\39687\SOHc\ETBod^.\STX$V7Q?\ACK\DC4L\\\n\12747(?\183089\GSc\1048413aZH\1029526\&03\1057795LA@\ACK\nX\ENQJz\1113786\&1\1008767\SYN\STXl\DC3\ACKh\1103270!]#\140274\US\1017434V\46223\182498\STX\NUL\NUL\44898]D6mR[\1022305kV\n\n\68096,\\\989346,aY\SO\986655[<[\1092680K\SI!o\19465#wC\GS2\59357^\DLE$\1061129\78617^|4\STX\38516f(,Q\52469\")\1037873\"\141030\NUL]\135158\&9(r\1106610f/\146987\1054241%0ihW\1045932&]\1015198mi2g\1106291I\US")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("Bw\1005727G\146683\n>\62847\SOf\186815,FU-\92666\ACK+\171367\fz}\1110732\DC1}\ACKt\f#\1112868\f\1039\SI\NUL\ESC\41467i$\GS\33394\&1A\999853(\r")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just UZS
    }

testObject_BindingNewTeamUser_user_4 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_4 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\182517M}\34971vE\1040495Q\ENQ{t\\\1067121I\1055789\ACK\1015032\\\95540]\1080160%A\f\44921u.`^#\1048408\9898\29781\1071878\1105997\SYN\b\1050002?K\v\\\1088814\US\1066940\1095557^}\1023619\1003580\NAK\1096134-_89z:.9z\SO*W@\67347\176076wb\1048896\166578\USW\1011757d\138689:WGs-%(\5303\1108372C\ETX$0RPh\148147%\DELC9\f4O\1064448Hv3\NAK\160792h$\FS/\996780\FS\175099\1030295\1048043\138937P&\NUL\1104523du0@\1050427\SUBG\SIlSw<yJUq[\46058\a-\176983[\1066895~D\26875o")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("H\1018746\NUL^p&3/ \1082976!v\ACK\USD\1083159\t\ACKg8:\EOT\38236\b9\bA0D6f\1105927\STX\ACK\CAN,\DC3w/\SYNh|\SO\DEL>@Ss\SOH\GS\43065\SI\3650c\72789\21393\t>a\SOH\1002922L\1098595R^\32537:K{>\37828Y1\NUL4h\23073Q@\"\164114DR\132814\47778v\EM\135514x\DC4\ETB\DC3\ACKA\NAKv\121377\136197\t[aVgL\CAN@kL-uP\SOH3$~4\37309K.c\b\v0B\992595+\r20}\SUB^g\9021N\SUB\1073393\1041445\&2\1003353\ESCHP\US\DC2p\1110885(\1100822{\t%Gry\FS'^Y1\1101933bM&(A\1078466\1092282\ETB\f\47782\&3\1044974\1107052o\1113466\&5\92955oly\39470SE\DEL]\22205\1082988\190649\b2\CAN>h'G\1017338(0\1034452\6723\&8\12192\ACK\119266\1113941C8\v~#\vev\169940D60\164441$\DC3x\10113\73706\155270\ETXA\tD`w\"Qp")
                  ),
                _newTeamIconKey =
                  Just (unsafeRange ("\DC30'J\1023170\&9\40151\5667k\1099047JW.\EOTu^0ro1y-\144892\166522\STX%!Q\48508")),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }

testObject_BindingNewTeamUser_user_5 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_5 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("U;\a\1108087I\1089486\&8tx.J^o]7\DELP6#Lr0`\EOT\DC4\1053251\ETB\1048623\b0\1060677d |UY\4546\28421\&7J\1084630\94811\rQX\133250\185655vW\59837\1441'\37139 \GS\SI\SUBoVj=\nVDq'4\22639B\\;6^\ACK-\NAK\1041995\99761`.r'5n~g X\991432\SYN#\1017567S0'\1014026\&1-pY\1048393\16180\\1P\121450\70127\42207\993292!6\63549Jw\182952\ETX/_\SUBy\1085242\&2Rv\1017742v\NUL\996793{\994743.l\RS\DELq/\159354\1007711.\92216~ri5\DEL\EM\13134>c\SOH\1017030\EOTf\1078294\146896TR\GS[r\167219,2\RSM,\1105004\1022538\179367JivP\78721In?ec#s\1046450\FSg\DC4+\60535eeAa\rA^X.\97936J\ETX\DC1")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\DC2\RSd\44888\82946\1015273Kg\168358\60534\bJ.BoJ%\DC38z~!#|@\17904uQj}\RS\133043\1108513\1082985\175914\n\1064502F\9690oN{_:67\1037008\rO7d\1093250\178638-`\9253aRB\ACK\151421sx\64878P@\1064552:\SOHb\190489\NULNuL\RS\NAK\137507!\fb\SOH-\1071296a8BM?kX\EOTc;WIeG\NAK\1097814\EMr\1089652\CANtNv\DEL\996402\996965\EMYU\bsT\62496-\32143\986088\1034207\aXG$5\STXHi\1002699\46837=\1020133")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("v\1053051\&3\SO\1075972N\1040747\4197h_\ETB[AS\98479\NUL{\DC3\f\1058390\"V\fS\1109927\DC2\r* \NULi\NUL\1061172}\38778ck\157296n\EM\52953iRGA!\\\CANWKI1\1050639\SOg\45944`6Q\68177\EOTsY\1088598T\3174\29369\1053819\68000VF8xA\37760,\ESCt\98082\ETX#\1073850\15498\1011311A\1082386\34173\167070\SO\&Hl]d\1030151\DC2\1091436\1031340(\NUL<\62433D;gb\b\ETX\58840'\1074090\US\1077526|\1050393\1015265>\FS\50596\1057737\ETX.N Pp\EOT")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just GYD
    }

testObject_BindingNewTeamUser_user_6 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_6 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("M\FS\148019\96606\FS\1100702Bzq\CANb\ETB\FS\51538\1102552 \158766\&0\RS\72787\\F\SI\NAK\ETXPs\DC1\DELPg$)r\1008935\171681\DEL\NUL@n(\25789yf\DC4\\I-\nOok\DEL\1006510@wb^uM\NUL\RS\DEL5\1108464m~K\1000820lQ\SYN\176049\1004567\DC4f\141993\SO\STX\CANaJ?\\yg%AH6R\1068361Ax\986978\&1\DEL[6]mV(\1037512U;\1059091\GS,Y\DEL^\18067DXP\165016b\1070465\1001647=b\\\41060\1052544k\\oM.v^L\1058306/cF\ETX/Dk\DEL\DLE\1013320\996138b)\1095767\985263L\ETX\1084505}b$s\ah\58600-.\15149Nh\31648Je\1025856\51222;\1022921\174521\DC1#\STX/U\GS\5598T|\t\15463\99045\991928")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("$^\149118\160734*\FST1t\ACKt\140597w\ETB'\1042165\DEL5z\186274\1086367mmEaf\FS\19965g+\11132<\DC3\184610\DEL\134869-\1016966\NAK\1104140\SYNpy\1011675\&2a\45298zPS|_\DEL\1090933a\STX\USmo\19067T&-$&\998793\51567xX$\ENQo.A\1038189;kz\NAK^$cGD<\EOT_\DC4\47519\1036320\1109909\1025894\1085168\&0{\RS\169770\&1\1060955O\33831WY.\96279\11375M8u5\DC2\GS'4D\b\1007938\1012915ZQ\1903Z\n\52515/\1055598^51\1082703\SO\1072261\RS6\12794T\98885X[\1084450Ng#|2VjG\SYNU<\20737\&3\1049104\4259\1088825N\1044794M\r3\\gp\1045813Vx\r$7N4W\DLE\ETB\159861\12542\n\1020713qZ\CAN1F\ENQM;u\NUL\1032689V\fj\36970<]/U\ESC\1102104~)j\185388<U0u\DC4z/n\a\r\SO\135448\&1j7aeA,,w|")
                  ),
                _newTeamIconKey = Nothing,
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }

testObject_BindingNewTeamUser_user_7 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_7 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\EOT]C)\STXq6Vkf)2\SUBg$w}S\DC2\nL1UN\38598\1101792\20207r%PK\DC2\17695AIn\SIRsFx\54034D-w\19113b\1045666'\DC1\US\GSYlH\b\25996\991873\&0\33140X\175101\ACKP@\1089871\189246a\996923F\166466\&6A\74083rH\DLE\NAK_%ya\1022082v\NAK\137185\171119{")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\999293\189216\160876\40358\t\1031018x\135156\37864\b#\983359iJM$!\"\1082773A2\EOT7=L\32705#\59545\CAN>\NUL\"\25482\"\DELNsd\STXm!j/:I\EM\1068817Z\ajac\ENQ:e<e*\65365\EM\1018295\59136oEO\f@\66186c3\996913\FS%z\DLEX]$[\1057128An\1017534-\US\1028580:\f\992929\145396_\DLE\176338\&8^\DLEA\991738<.R6\SO\ENQM\111291Y\ACK\NAKsD\154677]8c\ESCoO;~9\STX\1306B#\38792=\NAKX\1110367?s#;\995687k\ACK\a\1034971qt\EOTw\45995^Q\28940.\DLEU\a\1043125\16342\DC4\151861\n\r\rn\ETX\b\STX\SOH\EMFN\131394\14294\rMXXy\1038459\164574\nPeX`\156316\\\1053274\21734!\SOH\SUBz5=BG\155664\"\ETB.sD\DC1T\SOCjBQg\EOTD&~\143511\US\FSk\NUL\1012301)\1110121A*QFK\SOv\187155g\992873\94741BC\120704Gx\DEL\tz\172064j\CAN\DC2\DC2\145388cn")
                  ),
                _newTeamIconKey = Nothing,
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just BZD
    }

testObject_BindingNewTeamUser_user_8 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_8 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("x\FS\1039000h\1017310u\RS\ao\1088702|W{M%\ETB\NAKp)\1101758\1051521\&6\"\47741\tV@Uc\1046847K6Nf\\\156546\1008152e6fZ81\US!<\NUL\\V\21225\1106571Z\70364H\CAN\EMH\145656#~5\SOH\1019011/^\1043248\rDo&K\133880v#a\134224Ao&\EOT]:-\1013601]\1025037F\DC1\US*\vHv'd\999605vrb\993851\&4\1067604YIL\DEL[(\ESC+C\SYN\\\GS\1038285\&7,$\155812\&8j\1029494\159415X\1057418K\1095302\188934\&5\144653>xU8Dlu\29230\43566\52951\v\994928\&1&9\64056\1103374\1040843\ETB6\DC1\1013532!C1W=j\FS;\1034595<#w5Sl\SUB\1048927\DC1\CANO\38868\&7\SI\162365]dX\r\171232\ACKD\ENQ8s\DC3\189474@\"*h~\42718Q\155730q\1063830\SUB \RSE\SO^\1102626Fa@\b\r%+W\STX\1028795\1020794{HR\DC4 \DC3\DC4dg\SYN\NUL\144054\n\1089496~\120234j\1062769K3uK\1011439\&8")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("{\6407\1045496\162593 }<J\159456\&2Yji6#,_}P\b_N\\\152450\1092082tv\ESC\166133\EOT)8H\ETB\b+\ESC]cdSa\1044497K\b\ENQ:\44096O@hcC\EOT>\ENQ\SO<\"\61180\n\1056882d{\1050887\DC2\134394s\DC3O\1011012g\RS\n2-\NAK2,\ACK\48216\n\ENQ<pB\NAK\a\EOT\ACK\153796c`r\171192\&4\994147\151077\986270\DC1?\\\DC1#o\SYN\63696\DC4&\148798\&6\1015172\1019703rD\US-&P6 +w\1052303\EM!\159326\ESC\63297\&9\ETBub\rA")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("J/\t\1052300\&6FjdZ\ESC\\4\39673w\DLE4VB'\tL\v\1052552PEg\183222\fp\155919\987264\1106430U\1057515\998006qSw\1016903Q0\\\1047888)J\1072800zo\SUBu\127076\&3\95493#\ACK\"\DC2\CAN\129539#l5bBw\ETB:yPn\167470\133265\134473Y\tk\1029505\ETB\188533WwR+\1059134%\40486,-\ETB:q5\bk\1071538\161749\132854\aO\74251H\"+\45079R/C_/\"\NUL'\r!\CANE\987140#[\EOT\83089\189867{#*ul[\t4#<n\STX\189914\7761N\DC2\21655\&5C\135585I\DLEP,\SO\SOw\NUL5:\144222\1038864.\137897c?\36606qM\GS,K\1045759\158109y}-F\EMx\74185\DLE}-\175768|2\1110004\&3\1101851\CAN8\FS\1045442")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just MXV
    }

testObject_BindingNewTeamUser_user_9 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_9 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\ESC\SI\35580b+\ACKGR\177701\&5*\b=x\b\r]pP\47501\1061959\10398F=Tm\35155\CAN\1038496\SI7\NUL\185684;\1073540l )m\133268\1053194\20466\19892\EOT\52370ak%JP\1090109P\120232M\DC33sT\1043234\29034ml\1078505\4459un|Obt \1022838\1069861/:5\1111662j")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      (">a&7\1021610\&0\ENQ\1054469h\64406\SO\78032n\DC3 k^\v\1047582\&2OOz\139572X2\1003924\1051705bu\1071407C\1093019\GS\65399\58155\&2;)\180646\53270\aBY\1031479\RSn4\NAK\ETXv")
                  ),
                _newTeamIconKey = Just (unsafeRange ("\1048793f\31251\1106466")),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just MZN
    }

testObject_BindingNewTeamUser_user_10 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_10 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\1049073\168559\987351\NAK8u;\ETB\1063929\NAK\rA$#,\159447\EM\FS\990410Y\ETX\70730\NUL(7\999642\&1\1082313\t\132777\SOH:?$s]1U\119670\\:3\144821n?K~")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("+Y\SOH\n\171682_{\25671\23150hFn\1101809-\37324o\ETXK6\1050439b4#~f\EM\129595\15408\FS\EOT\v\142114\&9-[n\1006769\1008911z*\1110193\v\ETBX\ACK\1041891\ETBz\SUB\1089909\78036s1 [\1004363\DC4|\98659MoY6\DC4d\141826E\ENQ\SOH\71350Oh#\1084622u!\168019[\173222(\48983$}Y\166556<\ESC*P1A\188556*\162377G4dx\147381\1009215@;x\1055442d\DC2\fp\SOH\27702\&4WY\ENQ\NUL\1113786D^\26624-I\1069722kC_\26032S\ETB>\1075495\&8\DC4Wb}$+\1104271y\NULm\nR\1051310!(\1007896\1062798\t\181190KF1[\\(N\153483(U\STXN\1896\&3\1012754z{$\24188Nv3NY\DC4\61735f\DC1nDfh\36158S\NUL\985284\DLE%e\1027534o\1038814\CANqq[w\1092640\f9:\2365Z|+3o\n/\988347SM|J[,\1059055\&2\1000072z\DC4\US79}\EOT\170925%V\1026633l_\992878)\993061I")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("\GSnlrE\1069158H\GS\NAK~\US\ESC|-\DEL\158753Zo\\$\ETB\1023645\GSC\1014314\SYN\ETXFj{<7\1072221m'\ESC\CAN\59710Bn_N{\16970\&4\\g\v\162503\vsg\1031998\\\156790\&8\DC2F@t\SOH\NAKXy+\FS\SI\1009740$\1047633\152050\1018475)\fC\1030422_r\SUB\DEL\1047697\ESC&4Pm\a-wS\1009483n\31481]\SYN\1010618:\r\nO\1028492\SI+9q\EOT]RyY\RS\168945#6'mk\133798\48513/\ETBZZ\1046307|L\135076o\1103335m_`;,\1103745D\27083\&4\1036685\NUL\53658\GS8\47277d\95743\148115\983191\&3#\1111531\96828O\167214\&0(F\EOT")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just QAR
    }

testObject_BindingNewTeamUser_user_11 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_11 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\SI\95458\f5h\151753\&1G\173466\ETBa\1098048V\GSe+SH|\fe\1065118\169528%8\183035jWZ:\1052418O)\1006809\b\EM\1108918U\1079280j0V3U\17171\1016407i\a\992344\1033718\992713<\58600n\b\21764]Mo\1012929y\ENQBS7Qh#\ESCI`=\1039942\998772\&5\"*i\8268\1100641\98267\1102214v\997521&\164378)<d\STX\b\GS\1052068=R\b\162021\"\15590p\1026794\DC4\1082320\"8*CP\180955+\ETX\US\144017[/\ACK7G/\989055\\\SOxwx\DC3<R\14649e\US\161031\1027690>~\FS\991237,\n\20701\176571\1003331\1051745\NAK\FS\1054277~VWB(jD&i\1037532\1094662{\FS|\DEL\SO8\1015427\65428\1000624\1008187L\1024084\&2,|E\31595\1087844g\1028452\ACK(8?\EM\aL@")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("!5\512\1050477\17628I\1105565q__i\119621V\176513\1034860\f%T'\924\8084'%5y\SIj\1013150qK\ETXVJ\1097190Q\1084135x8\DC4\t\33502r\NUL\r\8876\10482\1094496\1090965,;\f\1014738^\1016624m\FS~L:\986452\DEL\ETX\1018311i\148795\NAK&\ENQBFR\NAK\1067140\1008316v\1048407j\19255b'd2D\128850\&9o\78052\EM\13077!`{\1047528/]\FS)\SUBq\28514F\GSS\995957\119110\DC1<\SOH\GS01`\22800\"Qzt\1096237\ESC|\151058w\FS\1103524\990407+ =Xhl P\1095890\1032018\148690oPb8$\1056472\DC2\1049747\&5\DC3j\DEL\1106720sU\"#@u{Y\64608w\r\EM\DLEAUae\67078;\"uP{\v\nm\1023816}\ENQL\SI7\157473\165846:\RS= Yt5Nj\15061cYf\ESC\1009616,\bZCPn\1019754\NAKU\180892|\a2\46892z\26490\NUL\30026RaO[\DC1WW\1026040\SUB\37673\&2\a\35961Fjs\DC4(\1034668\168590\118825\987066H\7929l\136503\987355\41918g8\DC1\EOT\156787rC\SOHx")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("5;\CAN'BN\64596:j\EOTr\DC1\27899\30930\40021\NUL\t\129184\176976~Tq&k\SOX6cP\NUL\188778\1005491\&7\1085132\SYN\RSz\16834\19917\991570)%}$O'.\5935#g")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just NPR
    }

testObject_BindingNewTeamUser_user_12 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_12 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("Y<i\NUL\1084568'yJ\1004042d\SYNh\996289;e\DC3\1027092 \20270\a\STX\156181F\t\SI\v%\1003629F\25539)?\1040303eW\153905\DC1\NUL\183806m\DC26\1009172\ESC\150984\&6W\r\1081533t\177340\n}\37309\nnZ\22305\1066153Q@n\b\998378\54845>\aUh=:\1051152?\1008009V~\fX?)\DC3R\1088934\n1\1041465^#\1082033WepJ\FS\SI\ETB[ =-\ETBpu\1081924`\SYNo!\54226\985251\CANk\999213\1004979\f\1009811\&8NY\SUB_G&\1068566\NULY\EOT\52584\EOT2\1101608\\\GSa5\DC1g\a\1050452\1020014No\13133\2414\DC1(\121130\CAN\ACK\1108717_\182967\f{! X \NAK\SOH\19467\GS\97131 H\190009\&4*\f/\149119X\1021775\n|.\14201rd\17432`.j:\CAN&\1068603\23474\1026077\\F>*\GS\1074682.Y\DC1\a\"\STX\155381/\993320\&7\151625a\1046770\5214#?LQwzpeF\t\1078600\1035290\ESCOj\\P\34954\36395vP2mh$(\999835eaUw\SUB\b] 60\ETB\1089879S\159439`\1042176\&2")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("l\DC4]\a`N\DC2\1085917\64042\1112824\1025631\17693\STX\NAKXw\1021176)\ENQ4\96680\1046103\CANuV\146642\158725T\146421\30196P9\bP?J\1025903m|=pk\GS\a-9@J\tP\1068743\1030242\1076014\1040997\155277,\CAN\129071\RS\1110051g#\1004400E9\1029404ky8\ETB\92251e(v\CAN\52300\&0\988855\2402c!\EOTQGtp")
                  ),
                _newTeamIconKey = Nothing,
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just GNF
    }

testObject_BindingNewTeamUser_user_13 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_13 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("V\SOH8(4\"z\43146AM\1011377/\"\ESC\SOH\SOH\1024288V\189607\1070754\161051P\58886\141963^\USH\8835I*\DC3,\12013:\ns`%}H3%\DEL\RSe\78392\42031\98909A\rY)':!B\ACK\1032049\t\ETXrc\SOH2h=I6A7g\987060YT\988319\173685\1050792l\1015588D\ENQV`\SIE\FSE{cnx\1048660ej'{\DELv#u8\166711\149914\r\NUL\65211^T\NUL*\149083\1038716U\28222q==\US\168170\&6\21483\&2F\n")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\DC1\61392\&2>\41950G)+\"L\161601dZQ\988371XGMY\r~c7U\1101728i\169682\US?M# \SYN\fEweI`\7648\156246\DC2\986316\nCO\FS\1055148I\NAK\1020258w8\61013\1056373\\\1025878t2\127185u\EM\r\60003[\\fk\US:Yp*?P\3743ul\SYN\1089177\1018795\996155\STX+\1060552\NAK=\45793\1029690\1029986\&2b,<y+wF\1091289(\t\t\1003345[\1007467F\US\12695\GS2R\ba}\1029051\149855N\1052014>N\r\1069922\SI`\ETBZD\126578:\39424\152739\92971\1007563!\21587\&6\ETB=DA\1062109\EOT\12467\&6\SYN\177806\1078102\1084689%\SUBZ\NULKNmJqr\176398\USF\174456\&0i5\147118X\GSKD>b\1076133E3T")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("\DC1IS\RS=xd%\EOT}T_7 \1049411qdLtTL\ruiu\SUBd\1060256\100699\v\DLEMn@+R]\1048851G\CAN\29890/H\ESC\1075972\1100097up$b2q\1108780!\47435\t\994235D\24029.#\6116m\159042\1075860\ENQ=m\1041695\FS\DC4\174246\&6hD\FS\SOH\DLEAsL\v(\1053429?u\188274\1018247\&3%\3611Y1p\1004873\DEL\DC4Lhj%\1074536]b\ETXpsL\a\1094624G\118961\CANw_")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just AUD
    }

testObject_BindingNewTeamUser_user_14 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_14 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("HUZo!3i\159431\DC10XJ\153368_m\ENQcc?Y>\SOH\bbd\ETX0p&f\1104396Ct\18649\1061087Z\133783R\10670\a\STX\NAKa\tC*eet\1029011\\OHkGN\"CD7Ch\NAK\120783\1026017tcS\ETB\"\58037\ETX\EM\999311\DLE=_\13107~\1072003Lve^\t\1106810H5s\1108105<\1007375\37572i<s\1018069*Lm=$q\1002288J")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\995009v=x\7681\DLEB?{\1045432\155281\139997y\182573Z\fP\1099297\63710eC\160237\EM\r!(x\a\FS\DEL|azC$\119873\&8\4304Mv\rV^4Z01\DEL\ak\92726\NAK \a|\EOTOcxw\58394a\135463x \DLE[\994962\1075502K\NUL\1029443=50tg\DC1\1092051\EOTO\1049597`\77924\v\64667\CANoPQ\1059653v\169199\SOHt;\RS7\1031212\1109443v%\NUL]U@\\:\EOT3\138205%]P\1110998\SI}\131194\SUBW\175840\SYNi\ACK\DC1\n=xtyY\ENQ!\t4~t\SOH\DC1~\CAN\1023383\1054166\DEL\1072148\GS2\12517\917576+X\ACKE7.\r)`f L)\NUL\1008273\992207q\EM\1012916\12073\131183\&79\vh\92379\99803(\EM\173634e\1028963^\"\ETB\1113946e\1113044fHZj\1021058\&2O\1102009`<\9499\1033123]\SOHl\1060722qx")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("r\1030445\&1M>=,:\1052570\57604Of#\18551%?p\83128c\1064044~\t\1056058E\f\1033507\EOT0H%\3235\1034553\SI\29108)\FSs\999576\acBw\1051189\EMd\SOH\1027598\&4\DC3G\166007\&8\164281\NULi\1021721P~\ENQ\v\60662\NULVr@vR\96899\1020932\1078411B2+\NAKBDMo(\1017787;\47792t\1010438U<-\NUL\DC4Mo\1087139U\180083'\DLE>)\ETB\189133@qO\DLE\SO\ETB\99100\45493+)M\rD\1023643\32194\STX-$\1070170T\CAN4M\164178\&3V\a[y9\\' {E\164327\1088663z+\12155\US\t;*zUvU!\SOH7\38057\1106255xF=sqy")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }

testObject_BindingNewTeamUser_user_15 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_15 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      (")\SOI\SI\n3\CAN2\1093803\156479\32728\1082366\v\147610\&1,|.=\1094050\1091883\fH0d\1027540\b\ESC\DLE\ab\1034699\CAN\GS\95766\1044787\&9|\1074820\95646\167502\1095215C\SI\fT`\SOH\1020498w\1112382Fn[9\1003685\DC4\1068849N\35430\v\20748-rC4\"{^@\a\110799RG\r!]#]'Z\1061236n\1038640-\1053831\154857\181092n\DELv#Fwiz\1344W\DC1i\47961.\v\64101\138177\1081686\t=\RS\CAN,j/E\36364\1029033\ETByt\999568.9Mk\\\"i5M{T\1110224\989485RyrD%3_\1017028*o\984061d\ESC\121297h\fe\1003889\1013351\161393,=r\12844\161365\21711o\35771\82975Yt\1084118\n*if\DC3\1100917T\1044803I\ETXo)\180260\GS\ETX\FS)\fw\DC4\ACKEk\167349[\SOH;\1058234p\EM\1066363T\GS\DC3Hx")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      (",3\DC2BJD^A*eH\DC3i3\EM2J@\a\1073979\DC3W\129616Ecd')}\162724+S~\14075=\186797ANFU\1080161\&5kg&\\\1038406\1022727\t\ETXJ\4769\ao\1109192\CAN*$\988647hM\148122\DLEl\ENQ!&=BN\ETB3\991458)8\EOT\15362;-k\6712\ETBF*\127889V#7\98929);\v\160873\SYN_\DC1c.X\ACKyL\SIQH\SYN|3\GS:JX\195009fD|,\SYN|MC\176485&\60957J\143856g~\ESC\DEL)K\1113676\1106497W\1069036\GS\a)\98434J\ETB.!D\STX\156884[%\FS&H\ESC62X\141111\&7r\180262HoMs\1026205\\\72333\nqmR\984002D\RS.{ S\SUB\SOH^\171040RC\RS}\DLErzN\vN\DEL\1019401\CANI\DLE\SYN\66010\18966`!Q\1111295Wm9cbFWz\RS\78132\24816eD\46135D\126978UI^eOM\1065465\EM\177862\36329\49916Q'7\40709K")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("+#s:\83505\128803\1026731o\1010649\EM\ENQ\41143\153592\&8E\1009150\&16-gJ-\ACKAG\983421Vv,>>Yd$\ESC11\158776Ea\36589!O\NULQ\ACK\"mlc\DC3\170484\96618y\CAN\"(Wu4\20264rY_\r\1067208]-\DC4r\150513}7t#*)\167224m\v\1042944[\135624\US\47305+\SYN\1082953\1034731a\38522\td\SUBQ\DC4_\1006333mI\1065629S\EOTRK\NULp5uf \fz\1023561c\1038977\NAKU\STXYu\95490$\34609\NAK\DC2%jB\ESCNHxd`J9z\nyg\172169Z\8554\&5A\STX\181034\1026223\1093572\vn\US\173694'SNtE[\ESC\r_De\1110917'G\165278\ENQ[I.\987304\&3\1003155\bv=R\1110424\146961\NAKG\ETBR\31059)\1080786\EM\182482\ETBT\STXdGW\DC2YBV0_%\188754\43426w\NAK9\rL%\1057563k<\a,?\184822U2C\172924")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just KRW
    }

testObject_BindingNewTeamUser_user_16 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_16 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\DC1\1038979` \CAN\"\122902\1063633\EMa*@*U\173927=p|=:y\101049\DLED.9\nt~\39377\161110@,B\ESC]$\30129\&1\1057585\fdcU[JM4\ETX9_\53434\1097597w~\ESCkI\72134Am\992355<vg\ESC\NAK\1071991\DC1\ETB\DLE;+\DLE:NE")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("i[\1081378\&9\RSzKKVf,KD\"\995029\&7\54737??\35818\tt}E!\1112721\99602\1087928\134418A\ETB\1079151C\SO\DEL\1065392u\ETXe\NAK\41524;u\1032072\&60T\1043185\1099222\SI\ESC\11429\FSY\SYN\1056816x\16400h\EOT\b9\1045988]\\\SUB\137365e\1102214\28201Y>\aT\DC1\t/k\39709s\v)h\CAN\1034546UH\ENQ\168530~\SI\149629\DLE.\DELq\165542\52321^6&2sUN\v\1060209\989949\"=\SObh\"\DC3\27323{1<T\1041515\67401\1045819ZX\46072[\98336)t\15546\100919\&6\172762\78404\63583QA\r\DC3\1029979\994762R\DC1\181129<\ESC\78731@t\125216NP9K\999171\DC4\DC1\EM\SYNfT2x\1026973\DC2Z\DELm\FSM\ETX}Xk\SO\1057210")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("\EOTH\1034498\vW\CANj\73821@\DEL\1006826:+j\35306g\158975L\na5\ENQE\n\132605\&8\1038640x\159016(2OA\1031407H\ETBh \GS5zP\45864A\988290\US\166355VMvw\NUL\21690,+\988935U\DELpj3s\139817\EMn\127480\&9~6\134064[\23592\168274L\1022710\168295\&1=[XP-\DC2\13301\70083o\\Q\83351\151683t +\DC3\162022\&7\1039071A^\990386Y\1107726F\NAK$-\1046145\ETX^\r6OdQLB>\134259\CANK]\9934a\1091961m\34218z\STX\1027027I\ETX\1093906\1055514\1112798\96003\1050872x?\1071365\1029983<v;V\991034H\180353\18984\DC3/\1097725\ETXFk\t\r\11366g!6\1018742w\995258op\58081\5990")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just XUA
    }

testObject_BindingNewTeamUser_user_17 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_17 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  (unsafeRange ("8^\DLE\1095167A\ESC(\61065\1009133P|C{\ETX\73121s\1029827\1008690F(\ae2\NUL)\ENQN\RS")),
                _newTeamIcon =
                  ( unsafeRange
                      ("-\181020&7\1024573\171389\SO{\165658Gw\DLE\f\1071982\133600,\148267\b{k\95948\42063\SUB8\nu\46597t5\994668\996171\US\SUB,\SO\SUB\1090766\SYN\1028679\52922\1028474\EOTHQ\SOH\1017591\&0\t-*U\DC3lOh\DC2%9y\ETX}d\\l\1000362\63872_\65026\&9N\74420Vl\1100867\ETB\buElfzP^o\\\42569\181449\989926\1097752\SYNQ\9021s\31056q~\a\32822\15835\987602\45610C\43320\EM\ACK\1524J)a0\59127o'\ETX\1070270T\ENQg\160121x\1051740d\STXD{\bA\1113775\38869d[^#\SOHd_&d_H\SOH\EM\12176+\173290\1101445\GSM\4360\\\1081744n\165858h\41480\DLEa\RSO<\1111159o\SOHXzYAF8)\1078816+\US\172359\SOH\1093596\&88fs\175609hZ\38508\ESCs\1073053\ENQ|\ETB>\1097902\52895\&1\f\1109541h\1077444\a\ACK/mX5$C?!")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("\f\1061655`HS&\1068216\&8Il\EM\1113259\tQJ\SUB\SOH9\SUB\a\1066663\120011L\12791\41370\134059&\STX\DEL\1054533w\1087783\&4\1041272\US\138765\ETX\DC2~\1110206\EOTB[Ya\ACK\STX\45769n\78142\988360k\1047843V(\1095279\97663\1058771@\474N=/.\987157`m\ETBd6\72996'od2J7[\1072723Z:\49502\EOT\148040l\t\1077406\161293\ENQM\1087599y\1105784\63793Y\38102\37856\&5\NUL\ETXRl\DC4 \DC4|:\DC3\1048792&\71326\SYNX8\983194`nmFZw\153337[Dg\ESC\SUB\b 7kN\1042049N\31579Pl\43166X\DC4\96591?B1\\jM\"b\\x_\DEL\ETX\1058594:\DC4\50294\&6\1025098\176655G\190007\1050448{\SYN\SOo\CAN%EB\146689\r@\1052853\48854Em\1045971\1100364\177383\160220v.")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just AOA
    }

testObject_BindingNewTeamUser_user_18 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_18 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("r!ef\DC2~[2v&j\v\169592-m\14496\ACK\1026600X\SUBlDSOHrN\1111331J.\142961\175421j\1060446\21905>\190844^\164752yz\ACK\70115\&6G\\\1095501m\SYNEi\DC1co!^/\SUB?\SO\DC3\ACKa\1030543\RS\1000989\SUB\1109471\36537C\rt\f&zN\20219+\SOH\SIF\1071230\SO\119161\DC1\1014459tJ_,\FSq\1006918G\ETB\160437\158350a(P\14610\63560<@F\1107755\1044959, \1014349xt\ETX\177197\78612\&6e\68355\63114NS+1#\ENQ\94592\"j\49402\1082412\&0G\DC2\DC4\f\1020944\1030980\94227F\170594o/j:I\1030148VD\48051?Kz\"]f#\13170M\1096460\996288Ed\t\1104795s\986159\n{MKM\30751\&0\r\6002Di)\64750`\NUL:i\44185\125195r-\173274\NUL(x\GS\SOH\12902\ETBxB?\rK\SO\ffm\61899\167110\137002\1069966\&5V\SUB\67291\171430\\\1011813b\tj\1105352\137223\1108413\&9t1\25744b+[N\CAN\43931x")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("Y\120720\1107885?iT\98262\1104262`3\94511\STX\r\1054664t\1038827{@`@f\186222(~\1018549\USE0\STXD-\"\1057454A=_a\EM\136795Nn/x")
                  ),
                _newTeamIconKey = Nothing,
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }

testObject_BindingNewTeamUser_user_19 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_19 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\1110951C\26749\&4\DC2f-\1017780;+b2rMM!ck\DC1\60720\1073435M\SOi$K\1088397\147462\CAN\18721ur\1030183D\147829v>\DLEIz\b\EOT3=+io\119012j\1079557\n&<\EM\95116b\62430\n]]7\EOTw\69615q\1107694e)A\DC3-O,\12743\1035555\&1'\r\1038262\DC3\1058511\17815\&1\SUB6\ACKRuY\156664<!I',\1081416\DC3\n\\\983717\&7^D#\1082283\STX\DC11\1077909^Wm\DC3K\151997S\41455M\22158@D\1085656\"\EOT{\ACK|hD~")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("\1024252\DLEy\1017306{\1065850\\\22198:)*Mb\NULw.\1041701o\1031193\DEL\1088971yd%\16972\1093098\nu\1083000\fj\"v`2-\b\17161P6\a\46689w\a9a+\1022323Q\NULAm\14076(8K%>CU\GSQ\1033630\17811\1056890\189786\36319>")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("~6u|\121168\ENQ}\183480a\DLE\ACK\1013935?v$\1028477F\1045549#\59393w@P\1001058p\EM\ACK\1070303Ok\1000351\a\990590\1019167\120539\SUB\1045521\&7`\183868\44862BL\US\990859\16418\v\ETX\28704\DC1{\ACKsv4\DC1I8'<d\ENQ\ENQS\r\34826T\45239\EOTG\1110085\67659\988454%~:_\DC3X\186973\"!k\158260\t^\187721\&7P\b4%C\161407\1013609\NAK\1068695{(9\SYNg\185935\DC1!\144297\SYN<0V\1105469\25230\ETX\GSn\11451U$HMF\136596\f\DELb\151006M\DLE\DEL\9324~L/\NAKJY'\DC2OVo\1008952\&7H\ESC\157713\&1\175827p,\1072246aQ\USC\FSkl\NUL)\1068004h\1025463G.\168192B\DEL\\FH\\eW\NAK\1047627\51367\\0\a^}\1044358\1054396\1108235\US^\161149\STX\1040506*R\1030962\1011916;\23034\&7]\1039976\&9\1031462=\63769\63651\95035t\1063507xh'\10428\&0pf*\n\rkE\1032217b\nt\\SLcTT\SYN\NAK5\r\998150BP\GS*j'e\1072402\1103330o\EOT\1071383nv8AN\1102729\1019659Q~")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Just UAH
    }

testObject_BindingNewTeamUser_user_20 :: BindingNewTeamUser
testObject_BindingNewTeamUser_user_20 =
  BindingNewTeamUser
    { bnuTeam =
        BindingNewTeam
          ( NewTeam
              { _newTeamName =
                  ( unsafeRange
                      ("\SUBl|>41*\190695\NAKTLx~}\1043142\GSNI\133566o\38308T\1000421DrQ\EOTq\FSD\DC1\GS=|<\68060]Y'{u8%\74323\ETX\120997\vc\RSj)\21686%\96011DE\1047083&+!l\1030782D_\"\SOr\1042277\1036522\1029837\1051035\987428\n_5*F/\181012$,\1037244\ACK\1059145Pd~\1045128!\1077426\SOa\1014402_\1024119g\SOHl^jz\RS\1065019\1047568\162014\171398\45236\988466F\41652\&2a8'\"n0bH\DC40v\DC3E\61478~\f\SUBs)5b\1055098\100208?GzN=\138756\&3c<,\f&+\67278\147133&af\137280FqV\992330\988290\DC2k9\SOH\FSOZ\1009435\36504\1054749H<\63734Iei=\1024367\1060904\SOg\CAN\DC2\EM0UO\ESC 'I\DC2)\170683'\134775\b\EOT\GS\DC3\EOTy]c\n\185883\DC2l4\DC2)\ETB6\984595?~\t\SI")
                  ),
                _newTeamIcon =
                  ( unsafeRange
                      ("C\146291\178505\&9=$Z4\1089687`J#^e:\1059521\1028975|\1011817$6\DC3te[EP3)\986627\169769&\96698\1043\DELHk`\1055905\vo-\ACK\140378\&1\SO\19105AtI\42662\94688&\NAK\GS\1080086\120323I%GLA\1093605I -\1090047\468\1079066pt6)<.\"\1085035")
                  ),
                _newTeamIconKey =
                  Just
                    ( unsafeRange
                        ("y\154212X/y\135232E\STX5}THt\b\SI6h\1080596\1070995F\988357\&8t\12802\995577\1027242\1006410(t\1082625\1088232DRn\993619~ifa\n\30271\DC4L\65281w\\g\DLERQwPRiyB>}3\GS`j\fq\RS\1079379)y\SUBwq\fR0\STX(JJ\DC1|\n\RS\1022103\&66d\NAKz\SOw\"\DEL[~\1107073u\991708\EOT\1023418\SYN\1023561\SOH\987451\14377\ACKD\DC1g0\ESC']q\142823\989620\6369Z\DC3lp\SOH\DC4\USi\EML)[?\rf\2489N-4\1048293(\STX\ETB<,!\1060373\DC4f\100484P\1035702s\97823r3\b{jzp(\GSN&\\1\137848c\998833\RS{hh$=i\1023918-0~\1058482\173539\132149\1013243lq5\999142\rclO\DC3)px\1073810\&9\vI\1107068\DLE\v}\123634+,l\998440o(\1010995\917985\10802c]\DLE\174962[]\999995\NUL")
                    ),
                _newTeamMembers = Nothing
              }
          ),
      bnuCurrency = Nothing
    }
