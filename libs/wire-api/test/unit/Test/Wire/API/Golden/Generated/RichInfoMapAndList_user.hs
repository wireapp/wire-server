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
module Test.Wire.API.Golden.Generated.RichInfoMapAndList_user where

import GHC.Exts (IsList (fromList))
import Wire.API.User.RichInfo (RichField (RichField, richFieldType, richFieldValue), RichInfoMapAndList (..))

testObject_RichInfoMapAndList_user_1 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_1 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\r\EOT-\1027344\132677g\51390\177008(d|\1020377[\DC4", ""),
            ( "\DC1f\28550\1078890qj\183448\t\1054443C\DC2V\24519ZnY",
              "\DC4\58409|\1067617g\"]#S\95247\DC2\SYN/[\SI\59274H\52762\120353\1024435K+\176372S\138337N\1069051"
            ),
            ("\DC4\148086<8g\ACK", "\a\1029966\1075110\191375P'[\1079123'\SYN>\\\1013784\EOT\57961"),
            ( "\EM)\142171R\63132\1101329@_z,l",
              "'\fS?\DLEk'\1084074\DELa />|Fk\SO\1079075x\983605\1032313K\1107277\29483kp\38343"
            ),
            ("\FS\FSPXw\33268&\NAK\27507Kr\50572\&7", "(e=z\"\178691pLmg\1027675}2j\165223OA\1000797_q\DC1\1008864"),
            ( "\"{^\ETB\DC2\CAN>\174235\NUL\49449w\DC3e4\STX\SI\\`\nJ\ENQ[m\14485vd",
              "\USW[\760A+h}\1011578zQ\51735\128295\ETX"
            ),
            (")\EOTdQ\985392\1063326\1049404\1090403\&6_\167322.|\176523_", "I"),
            ("4 X", "\ETBW\1005903m\1012077\FSXA\185451N`\1028930B\1004479"),
            ( "Fl\170211\SI*uBgcwKo\b\NAK\184082\SOH\187476\r%\188549\&4~'\NULilE\1022528",
              "\ETX\ACK\RS*\1052117\1002981\&8\1040461_\GS\1069714\6066H\1095762Jmw\SUB,A"
            ),
            ("k7\1112800e)\DEL[*\1025387\169659\CAN", "\ETX\fZS\NUL\DEL\CAN \194647^"),
            ("P.\ACK\22701=\36639A\168932|c", "Q\983856\165599h\1088153}~\a?:Xq2q3\1361"),
            ("t\1717\1015694U\189831/o$\fc", "\a\59273\&1\12942\1053396H]#\986844\135653\STX*LYqs["),
            ( "XN\RS\ACK<(4\97236k1ON\999401\186725]\STX\136667\157264d\SUB\8094",
              "n\998069\126643\&0y{\188179zH\n\DC1Cs\ENQ"
            ),
            ("\96434e\DEL", "O\2113p"),
            ("\156884\12840'5q<\178248", "@\1106532M\50269m\\\189498a1B\3886[S#=,.|S0\NULj\r%"),
            ( "\1023907\aS!\154358\&0:",
              "(]\1052314sJ\DC1\1037662\1059212\59724C\190354\&9\SO\1107665\\\EOTWu\1029094J\6803\1080372x\f."
            ),
            ("\1101303\19525\ESC_\1083068\&5\FS", "\1113659\t%b\180632\FS\8793B#\RS\190167\1028742\ENQ")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\DLE\1025518A\RS\DC4nUXi<\128195S<WE0\v{\SI=LG\155651aI\990277",
              richFieldValue =
                "\171511NMJ\SUB\72436\&6e\99663\FSf\b\74375\157525-\173712mg\1094212\FS\12648\1030123\1098771#\19047b:\128878("
            },
          RichField
            { richFieldType =
                "\21962'^I$Au\1104909\62791\1084478!\1015858x\DC4\135430\150073\1040471 \177055U\155329\1077971aL\174744\SYN\STX\DLE",
              richFieldValue = "&:\CAN\ACKQ=\DLE\CANYC\987265-\"{Ke\133417\ETXv\a?\983595\ENQ'\ENQ]"
            },
          RichField
            { richFieldType = "F4\SIm\1054888L\131997.Wr\189389)\DC2\132664",
              richFieldValue = "\132061\1032765m\DC1\v\ETBH\v\EOT@\1053652Yl\STXF]%\"\EOT\ETBOl"
            },
          RichField {richFieldType = "\\\133209lRJ", richFieldValue = "\DEL]#\ETBb\40545\156838R2\b\98801kv=.g0\nF+-"},
          RichField
            { richFieldType = "5i\3219\SYNbXL}\8536\NAK\191160R2I\173406c",
              richFieldValue = "\NUL\1041102\1028286|\16362\NUL*6s1?\GS\EMq%2\t\1092273"
            },
          RichField
            { richFieldType = "\"L+KR\990508og\USo\1093594+\16832o\tw\1008259\&0\64649\SYN\ETXy\1000604",
              richFieldValue = "67\68458#h\987795P\1041069J"
            },
          RichField
            { richFieldType = "!Nv\SYN\vQ\a\1114044`wC}'X",
              richFieldValue = "\94459Tl\n\nh.f\ETXq\175509\23629\EOTXC\npo\1002280>@\\0\DLE"
            },
          RichField
            { richFieldType = "p\CAN\1109970s\1060071\n\1008896\&0vH;.wkeD}[ecN;Hp:\1061953+\10353\STX",
              richFieldValue = "\45697:\US\1089108\163167\1074799\FS1)\180532`\ENQ"
            },
          RichField
            { richFieldType = "\145030=^j\"\1046417\SYN\1069802\33875`\1076411\1044721\ETB\ETXq\987060R^6",
              richFieldValue = "W\RS`%\ETB\DC4y\b\133135$\FS"
            },
          RichField
            { richFieldType = "u\SI~\1037877l\165807.~@\165769\1065291\163931\10201\61554\SOHe\t6\ESC\RS\25362i\tLbX;P~",
              richFieldValue = "\\\65801'\f%n"
            },
          RichField
            { richFieldType = "\NULm\SYN:I\1109371\&62\137158pV16\ETX\999821\&6\19731\DELk",
              richFieldValue = "6\1049834?h\a\1066300\97929d\1101759"
            },
          RichField
            { richFieldType = "a\DLEG\DC4\RSEJ\129541i\57628m\143290qZ\SYN/`>\DLER\996362",
              richFieldValue =
                "\ESC\62393\1034586U\EMr\ETB3m~\7998\164694u[xV<y\173644\150747\DC4M\1001248\35084\48744\SUB\1067694^b\27297"
            },
          RichField
            { richFieldType =
                "US`W\1050467\43203\1047990\SUBu\67240\v\996971\171077(X&D\1030347\139631\1106675Y>\FS\1005668\6739",
              richFieldValue = "\1035571\b`+|+eu\23981(Z\186019D\1028214\1015577\&6Y\DEL\"U"
            },
          RichField
            { richFieldType =
                "\28705UVd\146470e&\1044374!P\60067\ENQ\70448\SOH\1047197\&8\157873jZd\1067565\1028925\997058",
              richFieldValue =
                "\DC4\1096824\DC2&\1070187\v'\DC3aM\17345\US\165296\SYN \r\ESC\"\SI\47365\ETXoEhQ\ESC\1002938f\ESC\1037898"
            },
          RichField
            { richFieldType = "\983787hk\1078924\1037397Etb?+\f,)d4\999135gM!\989834\9737qJ",
              richFieldValue = "f@SKbEo>@i\66330~$\ESC\53435\1106699\SOH{`2B\1030374"
            },
          RichField
            { richFieldType = "M\62113e\96191\997145\36092<u\ETB\t~k",
              richFieldValue = "\64114\ENQ\bYSzQh\USJ\69718Z\RS\b\33109a\vv\1101587C\30329\1083839rl"
            },
          RichField
            { richFieldType = "YD\t\1026432N\DC4\EOT\1058042+v\1111873\ETX\94768J\1063569>",
              richFieldValue =
                "\161708\1075141\19310\171344\21818\1032723\RS(J\EOT>n\44231\ENQ\47764v\1053822C\999391D.P\19299\68478\991411HS@"
            },
          RichField {richFieldType = "D\DEL1o", richFieldValue = "\SYN\DC4\RS\1028505C?"},
          RichField
            { richFieldType = "\NULp\1103876\ETB}8\DEL#\1104464i)\SO7J\1094446E^~\1086789*h>\1060646\r+l2",
              richFieldValue = "HV\DC3M\ACK\SYNZ\"\1020372\nFs\DC1-\n;"
            },
          RichField
            { richFieldType = "T<a_[Ugv\EML\SOH\USpM\r\1112983jJ\NAK%:*m\1030425\US\110961~nF",
              richFieldValue = "{<\1107928^96v\DLEA`\v"
            },
          RichField {richFieldType = "z4\156009P", richFieldValue = "{~\EOT\21103\ACK"},
          RichField
            { richFieldType = "1\DC4\ENQ\1036662\16348\ENQ\b\US<g\USW",
              richFieldValue = "#\RS\1100008F\FS\1049172\DC475\US\27637"
            },
          RichField
            { richFieldType =
                "~v\984494\ENQ\996335\ESC\52943\39567ibo\USbX\\'&\97649:\US\1039082\1049695\NULT\147629\1088054^",
              richFieldValue = "\176989(\DC1\\&0-"
            },
          RichField
            { richFieldType = "\ENQ[\FSb\1019838\18165\DC3\50934<\1074044\ENQf*[\ENQf\132599\1054165w\178992N\bh \8854",
              richFieldValue = "\22317\151381l&0\STX4\1035154r-E\ACK\1012285"
            },
          RichField
            { richFieldType = "\159992SyC\175776JR\1068294\DC2\187776\f\184828N\1065545",
              richFieldValue = "\136594\1044760_\DC1m\CAN\1003727\ESC}\985889|\51757\ESCv\1081706\&4\DC34\1070333%"
            },
          RichField {richFieldType = ".", richFieldValue = "8SOe"},
          RichField
            { richFieldType = "\1018493i\1068666\175210\DLE\63374q\\O\1084143\SI\140285\DC2\33226\1086558p\t",
              richFieldValue = "\161378\ENQ"
            }
        ]
    }

testObject_RichInfoMapAndList_user_2 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_2 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\GS}+C", "2\"`$^\RSw\ESC\1076801&)c\rH"),
            ( "\US\USF\138168{\DC3\br\STX%$%Q\1018986\50448v![V\t\bB=\GS",
              "\a\1076760{\1088298~\36247Va~QG\1105190^c\NULUk~{\DC1S"
            ),
            ( "\"\5832\DC1](\1111008",
              "\fNL\nW\140863\160695g7\a\"n>\1060709\SOH\1024519\&6\150331\1064434\983600zj5\1020200]\30039o"
            ),
            ("%\"hn%'Ls^\"Ej", "M\145285\&2h6\""),
            (".r\NULASiNn", "@(A)\158438\1039824\"^\RS\153092nT )\NUL\136003go%7i\1021480"),
            ("1Dj\DC2n\74494\DEL$#\a\ETB", "\SUB&\DC4\10781\ETXP\f["),
            ( "98\nJ\176662s\ACK\SIG\57736\1028516",
              "^\42880\167708\133306<1y:D\157231\EOTG\ENQ^\120231+\94324 \21330\SI\162748y"
            ),
            (">hI\STXs\aK3_\NULfO", "\r+"),
            (">\33141!]\1050626A7~\1050406<5Qom\rn\1098028ZKZHL\v", "rh\1089466\194951\1013243\1007763j"),
            ("cB)#", "\CAN:AjO\DC1\ETBc#{\r\DEL\492X\NUL\37340"),
            ("K\bN\SYN\170192\&4\ACKi", "m\ETB~\1066084\1099683\ENQ\1051199"),
            ("Mc\US\1088313J;", "V*IL\STX\9060W\CAN\SOfL(xbD\1095599Au@(U;"),
            ("TI\1099712l8\r\f", "Bxe\DC2\1004042}L1\DEL#Z"),
            ( "T\157570+A:\STX\FS\DC4@\1088081\1011374ri\\\185696\DC1A\"5\DEL\1076k\1074026\1021933q",
              "u\ni\1027707\n=yf!V\RS\134243\1105451iq"
            ),
            ( "U}\1060635;Q\1054239\&4\RS=\13874w",
              "<9\987997\DC2\ETB\172739\34051\1027611U\1000940f\138407>\988127\1022180\US"
            ),
            ( "W\US\RS/\58721\94746zPM\139597\&6\a\39956\NUL\vA\1033790\&2\169481\"I",
              "q\1050092\1089565\3404}C~l\997188\SOg?\41244]l5\r\SOHbr\1095249tMk"
            ),
            ("\43991\\~\ETB+/A\ETX", "T\12606V\1103784Tcb\"\DEL\DC3\1028869"),
            ("\83374~K\168125\&1[\ETB\1022301\ESCAx\DC1_r`p>?\74396\998441K_\1086915WqQzW9", "C\1061136.~\\^1)\26116T"),
            ("\96772*", "4\NAKU\17943W\DC2ea\99552\SOH\992891\1078365\SYN\137088\9775\62016"),
            ( "\162552\"^>:tx\1050599\1065772\69977\&2*\ESCL\nd;n%}",
              "!\1089182\\j\1070298\145738(3\12859\\tytD\284V\78186NU\US`Q\95330I\ETXCI\ACK\165900"
            ),
            ("\177141Ly\989538\ETB}\135536ZH\SO\1040094\155314o}\f\1084906w\FSf3\DC2]J\SUB\SYN", "xi\1005583^\SOHX\FS"),
            ("\184688qB\ETB\DC1\"\991311\1092587+\8522\USL", "r\1049699\61728\EM^-\70289J\DC4fY>\ap s\SOz"),
            ("\1004294", "\ENQ\EOTB\1107876\FS\1011360'3p9\1094076\ESCl\34791\a\SOH\1072226w"),
            ("\1020747OH6", "ml\1036052\97233\1111356\153702'"),
            ( "\1021699e\1038505q?vY\175539R\27964U\ACK>5rr\RS\ETB[\131335\139139P\f\SYN\r?\78705",
              "\SOH\1027571Q-\SO\DELb(rT\1099049"
            ),
            ( "\1029599\vh\SUB\GSY\NAK\142498c\177003t~\1047416L\ETX\tEH&\1049285yT y\ETBS\DLE",
              "N~F\24384Z<\ENQ\1060768a8Y"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "v\1049163vq\138760F\161731W\1083734\r8\29264.C;",
              richFieldValue = "\"\EOThc1K\30246"
            },
          RichField
            { richFieldType = "v\47754\r\v\DC2k511}Xp\1058564\187282q507\ETX3",
              richFieldValue = "\DC2\1099825\US\DC4b\52763\EMT\ETXE\bE\66848\EM\ETB\SO\1071731"
            },
          RichField
            { richFieldType = "Cr\US\141552\54986\16964x\997072\1044606t\n\16745z; F\184220r\151313\151309-\SOH",
              richFieldValue = "\1027732\1113624\GSD\1053194rN\1091428N\1047827\25358;|q7"
            },
          RichField
            { richFieldType = "kzXo[1\RSI\t\986353T\a\DLE\1028560U\188623\SOo\rJ\1213b\1026797\990632K~",
              richFieldValue = "\ETB\ETBc\185617LY\DC3\STX\1035095\95040Q oEo"
            },
          RichField
            { richFieldType = "\ESC\1042653\69709\NUL\NAK\fc\1075705\1045034Q%\DC4ID\605YLlMRu",
              richFieldValue = "\EOT\16552+_\\1\SYN\ENQ\95636GyIl&\DLEk\NAK\1109582.Vy\ETB\19162(8"
            },
          RichField
            { richFieldType = "5^s\44680\43077\1094978\ETX_\DLE\NUL\NAK\49852\19166V <",
              richFieldValue = "\134869E\FS\\Oy9M\NAKbK"
            },
          RichField
            { richFieldType = "\1203\v\"j\GSE\SYNYVm\141839\DC2\\{\ah\1057173\134711",
              richFieldValue =
                "qg\SOH\175454\154798.:Sa\985531\&1\ENQs5!\48353\&1:&z7x\111146\1003333\f\US\45791\1066900\1059251"
            },
          RichField
            { richFieldType = "N\STX\1097188\a\1002511e\157855Aw",
              richFieldValue = "?W\1086682\997092\&4;\131126)\DC3-Z?\FSeUjw\175237h"
            },
          RichField
            { richFieldType = "\SO\147377\1613\ETX\143260\1065343\&1up\DC4\bW_P,mu",
              richFieldValue = "\1092122T/I+8Q\25328\&6m\1079511\94749;\1020886;5\1020429E\1021611|\t\t\71712\CAN"
            },
          RichField
            { richFieldType = "ow\1037062\ESCLe`\ETX-\DLEen*\7912!\1046844\1002090\1048552\1004821[D4{\SOH\EOT\EOTH6",
              richFieldValue =
                "U+\24310\SYNa\998483m\\,n\DC4D\139849\&5\100485mY\986584\SOH0<w\173871\n\ENQ\1101187\&2\150445\164698\DC4"
            }
        ]
    }

testObject_RichInfoMapAndList_user_3 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_3 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ( "\NULiy\STX^@\DC11qF\1034433d\USa>\NAK\STX$]-=\995943\DLE\EOT",
              "\DC4!\1019690\138674i%$m?\48724K=\184479Z,\1092674"
            ),
            ( "\SOH[z\DC4\n&:15\t\1035689\30739\n\170466&\1075249w\1037270z$K\1039936\DLEB\991933/=\1001737\&0",
              "\US\DC2y\54642Vh\RSx\42879m\147018L\SOH\1057776/#\133396"
            ),
            ( "\EM,Ky\an-N\EMb\43760",
              "G$O^\1021021\144603w6\1093784\DLE\13779\SOH\1067406C\15160d\24616\NAKln|!o\64905;\DLE[\169381J"
            ),
            ("51\1080609&->", "\992690\38139H\2487\1054005y2\t\EOTp\a2\182032\1034377Gm})"),
            ( "\\\GS|\DC3hy\139452\DC3\21784W\NUL\GSXHq\ETBD\DC1I\SYN\1063233\rK0b\25332\1055376\RS",
              "}o\26866\"\GS\1019475-`!\156911QIn\1055097\&7uI\SI\EM\US\94072F"
            ),
            ( "`\2589Q8\15072W%\1050166U\1064919\&9",
              "\CANU\120173>\STXJcZ\STX\GSt\RSb\SOH\ETB\1074358\52221*\DLE\123604b\GST\3513\59817"
            ),
            ("B5\172538u\1084781\SI=h\f\b\n?pv,\ETX\ACKN4\143402", "-\1042068\185433\1025442vY\SI\1103("),
            ( "k\GS0*\DC4\USG\70325?p\SYNa:&\DLEvN\GSt\SUB/B,\1065709z_W",
              "I\f\1092940\&9g\ETB]\r\162816\32545-X7\41077U=K\988807,\EM\1015494$=\999086\FS"
            ),
            ("L\ENQ\1084856", "\ETX\1002509y"),
            ( "n4\128915\19213K1\ETX7\2423\1103031\1047665PE\DC2\NULCU\STX\DC4\1074147\1071387\1039210\672\&4i~b",
              "rc\DC2\1112746\DLE\1097373?\DC4\917551D\32439k\1057859\1077680Y\1096345\983223hK\172740\992509|\1104742\STX@\SYN\RSb\1111824"
            ),
            ("Y\DEL", "=\1059355\1095788(|\67272xb\135230\DLE\1085545u*\1076101]1\145602\US\1107488\65452\46177"),
            ("{*\f", "|S|=\v"),
            ( "\165633\SUBoP\10206@p\ETX!\176361\DEL\SYN$1\1021342\DLE [\131860\64780<\1057929\998740\164495\28367Q\NAK",
              "\ETX\1008801\48743pC9\146555N\1049688\30274\&7-#\DC4\1108575"
            ),
            ( "\996651`$C\1033243+\EM8(m_t\52980<*%\SUB\1021526\1039234[\NAK!\1014068'\1052160",
              "\SUB#z.\1080449\STX\EM\n"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "B\1100836?w\1131{\185475i#?~}x\DC3\r\USc4\95821C",
              richFieldValue = "[!h\52548\23411Nz\r\998793\1070715\153058ibz?"
            },
          RichField
            { richFieldType = "-\DC4x\1072167\1071702\1001928",
              richFieldValue = "\139251\ACK\ESC\1068809HB\1098861\na\159921=>e\b"
            },
          RichField {richFieldType = "x", richFieldValue = ":v\12213@U-\STX_Op;Y\t@\1101077I2[\v\166807"},
          RichField
            { richFieldType = "&\n\17455gg\94039\SOH\NULZu\DEL\DC3\1005498\ENQl4yv`n\40755\a`\r4\41011`:",
              richFieldValue =
                "\DEL7YA\SO\DC2kv\42911\19464\179440\16088\1079584:\903#*pm7\34123\SI\SUB\1038299\24981u\ENQ!\NAK"
            },
          RichField
            { richFieldType = "C\SOH\74163\127251'lT\169297`\179213!I\48221\1107718TK\174395b)\1056902\r",
              richFieldValue = "\ACK\156361H"
            },
          RichField
            { richFieldType = "\1047629o\NUL\t\998215%f\n><\no`+\997254v\"Y\1042326",
              richFieldValue = "\USx\SOH\68079<S\DLE\SYNj<l\66831/D"
            },
          RichField
            { richFieldType = "aJ\ESC\RS*\171240\&8K\157244\SUBI\"|)=0o\US\22682lY\NUL9",
              richFieldValue = "}*\STX\1087155\a\94945\&4(aY\98349q\1012949"
            },
          RichField
            { richFieldType =
                "\141255\EOT5\1097316K\36586\1051633\&2Q\1025204=f^g\1011891\992139\135122\984251<u$\1049348\187937\&8|",
              richFieldValue = "h:M{\1053828\ETB \SO\170087*8\1089426V\RS\ETX{<\1002944{\bQ"
            },
          RichField
            { richFieldType = "S\188630\1071727\1015092v\1011107wE2\"\f\a\a\"\NAK\4413/0}k,g#.",
              richFieldValue = "-Q\31969l>T8~%)0m8\148853Z\1021218QD\DC2w\1111875vT\9363k`C"
            },
          RichField
            { richFieldType = "D.\38053C\1102792\995677\61318\SOf\136867t\187027G;Q\60585p\15824\1083987\b",
              richFieldValue = "\DC1b.h\SUB_\1014471\n\DLE\184275!JH\7274n\1071731\15788\&6oQ\1039115"
            },
          RichField
            { richFieldType = "#\92413\ENQq\189845\55081\992026\EM\CAN@A5U\185891Aj\1102796\64257\174554\&2I5\1048824s",
              richFieldValue = "iOv\aj|tl#lu*UF\1101413$\"H"
            },
          RichField
            { richFieldType = "\1105664\1006677u\77963\996721\145058d",
              richFieldValue = "\33511eFEhD$\SOHL\SI &tu"
            },
          RichField
            { richFieldType = "w_% XL}\DEL\25153\DC1",
              richFieldValue = "\160225\1000025z`\1028939\USED<1\1028889+\984875mE7\1052871#"
            },
          RichField
            { richFieldType = "\1107407vu/\NUL\3205K\15153\&8;;\ETX)\171573M\EOT-qM\DC1\v\60882\83250",
              richFieldValue = "Kh;\170954\177327)Y\31033\ACK"
            },
          RichField
            { richFieldType = "`\DEL@\135298\1005129p\STX\1104846r0& Wk\DEL\DC1\US\17611M\1016595g?T\al\DC4r",
              richFieldValue = "1\72098x2"
            },
          RichField
            { richFieldType = "),\1064123u\DC2\1011050l#\999491w\36788\SI\152588I@a\68811GpzB8",
              richFieldValue = "d Y\99386\138173\120814as{\n+\1006645I\127095U\ETXn"
            },
          RichField {richFieldType = "", richFieldValue = "_E_\1110920\1099374\ESCY"},
          RichField
            { richFieldType = "O \t\26216a|,-\SIuXwiL\44658\176749c&\CANBDuG\t2",
              richFieldValue = "\49158\fuXTC;\995691u(\21426\\\39863\DEL\33831\1089974Y \1006692\118864\DC4"
            },
          RichField
            { richFieldType = "^\45304\986731\161125m\"\EOTB\1095651`\40624\994746l#\155085\NUL<x\26185b\\2J%",
              richFieldValue = "&\SI\v]\1099160\DLEE3\96143\51744\FS\46345\GS\SI\1071700a\ACK\1062545\DLEc\1015649Tbo"
            },
          RichField
            { richFieldType =
                "\1072914L18Cu\1006300\1089920i\DC1>\1079044\ESC\1001016?\1085130\60126\SO,\ENQ\1024324p\FS/\52943w",
              richFieldValue = "D5\1081664\129572Q\1029923*@\1050341G\DC3\1103345\151403\26256"
            },
          RichField
            { richFieldType = "\160638g",
              richFieldValue = "y\987196\ENQ\153254\SO\49670\&5]\SYN=j\SIVv$}!\1014078?e\17957q"
            }
        ]
    }

testObject_RichInfoMapAndList_user_4 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_4 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("!B\1071043(\1046230^", "o|~2V\8316-\12106>\DLE\rh\r\151511\148325^so\137986\1009802"),
            ( "=[3kw\1089151\3425\1084229V\141022]\"h\94355K= V\az7\150776x\\\178967\SO\1006917\t,",
              "P\110781,\DLE\994481\&8\1067195S\22736\1034878Ja2<9i\SO\NUL]\1088388\DC2\180157"
            ),
            ("\75044\CAN RS\NUL\STX\996303_\vubE\NAK:x:U6dj\ve\1036386MS+V\ENQX", "M$.\1003659d\rB{Y"),
            ( "\131171\SUB\NUL\SOHo=U\1036682Cf\174535\1112672\1086669\DELlf\34736\DC4X3>Sdb\1077202",
              "[=h{H\"\1076873\46124\&3jd@\1087950{"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\987942\147791`Z\23807b",
              richFieldValue = "$\NUL]J\ETB\ETBLg\1014833\160465\1036902\&96I/2K"
            },
          RichField
            { richFieldType = "v*:\12646t\ETX\DEL\DC4*\EM\985293\128174\111229\137078\992210",
              richFieldValue = "@\SOx\139548B\1092218_ I\DC3\t\DC4\16425\DC1%"
            },
          RichField
            { richFieldType = "\DC2*1A:)\134970\r}q7~\95100\NUL#Ze!\1108733\DEL\6413\v}(",
              richFieldValue = "p\29286\35927K\ETX\ETBDu\131704FAE\917966]-M\NUL"
            },
          RichField
            { richFieldType = "A\993024\154927<},\USzf}K8+\144607\148584N\1010701zI\51456o\37507A\92321\DLE\156647\US",
              richFieldValue =
                "~\1099787Y\1111583\51220>X\1091654\152044\DC4\CAN`\DEL\ESC\164425\DLE\"45\NUL\ACKz\EM\1068301\RS"
            },
          RichField
            { richFieldType = "\b\1004306\1089704L9=.r\65784)/\SOHPB.fr=Kh\24622I\1095737Y\23042l\1062366~U",
              richFieldValue = "\DEL"
            },
          RichField {richFieldType = "", richFieldValue = "\171417\1113813A"},
          RichField
            { richFieldType = "\1067266m\DC4\990224w\"\ETB6_",
              richFieldValue = "\EM\EOT\1087675y\NAK\31702fr\180439\143940\1076041*Nq\DC1x.:]0\NUL"
            },
          RichField
            { richFieldType =
                "R>\46518\63305)\bd$\\nH\1082857\185930\181424\FS|\167720-\1072367 \DLEC\1019450\&0\DC1\1047631UP~",
              richFieldValue = "\FSZB\18643\134281\"D;\RSaG\1075507Pr\1015475CI\1063206\ETX"
            },
          RichField
            { richFieldType = ".XV\987830\162631\NAK\EMo\54497\vq\1034154WB\989134\1045982(\ESC\983345B\1031387*",
              richFieldValue = "LJ\984449'\DC2M|(\990807XS\EM!i04"
            },
          RichField
            { richFieldType = "?-\SUB\1070019\174290\ACKD.&y2=\NUL\1093985M\1072534\43477+\r+\f",
              richFieldValue = "\150583\176077\ENQ9\994880\t"
            },
          RichField
            { richFieldType = "\1027457\b(k\NAK]",
              richFieldValue =
                "Q\987304\995175Kf\FS\ETX\177309^\GS\EOT\1049360<\168778\140181\987603Hb@r\SI0N;\148934kX>"
            },
          RichField
            { richFieldType = "\160115\ENQ+\f:\ACK<",
              richFieldValue = "x;9?Q(d6\SYN\141622&\998166s\DELmp\tkDn\SO\984047\SUB\SOD"
            }
        ]
    }

testObject_RichInfoMapAndList_user_5 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_5 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ( "\SUB=3%w\1011617\99879 u~\1028041t\64133y+-\1009569Q[\1044634",
              "O'(\NUL\1077813\ETX'08\97370b\51950ya\a\996702\1039882c\1053793\SUB\STX\10893\46842\SUB\EOTl"
            ),
            ( "#\FSmPU]\26394g\95117U\fWMoHJG>7b\EOT\48986\1056824\SO\EOT{Y\ENQ$",
              "\1097552zI\140419\987722Dp\170986\DC4'g\ab_VC+0v\ac\RS\1108789y\SI\SOHK](U"
            ),
            ("2\185555^", "\30694\\\1006114Uw\EOTu3\152196\&2Kn"),
            ( "mL\NAK[\162072\111106\DEL\23644\7866\133562K\ESC\1020965C\ACKws\39440}z\ETX\SOH\EOT\1058134\19670\DC3",
              "\SI8t#\ESCr5\GS\b\SOHAJj\48050rQnkU\1072170o\7527(/<\ESC\187964z\1103687\&7"
            ),
            ( "nz\128256\a\166004/;'I\985259]\119938(\SYNin@45\DC2",
              "\153998\CAN\ESC\143590\r\1110571\&8\158341\59577\&8\SOH\GS6l\ty\SOH\1078906\GS!2T<N\EOT\98165O"
            ),
            ( "\DEL\991797:L\1020398\&0\1059066\138421f\162570o\1001229\151926\1103487S\t",
              ">0H\f\ENQ>\181756OmK\SUBF@\1832\178698e\DC3P\aJ\186483M\SYN\1086254]6\57491"
            ),
            ("\50089", "\11992\GS6\n\128243zv5t\25183\1081926\180495m\DC1\ACK9\180332\r\983614"),
            ("\153659w\DEL\989887!\SO7U\t|7\169534a\95808\181171", "\SOH\1096987\1021324*Q\fHH6"),
            ("\189188Z\1078061\&7Vo\71862\1063403}", "\NULP\a\164102\33757\1029041\1011812\1025156\CANY_.Y\DELO"),
            ("\1096032\97635G\a:\13696$+\GS|", "fs5)\27616;\v\DELr#\a6&\EOT\ACK\GS\1695y\CAN")
          ],
      richInfoAssocList = []
    }

testObject_RichInfoMapAndList_user_6 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_6 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\a\176690E\1017778\1103374];\ETB&A(& \165355\60311\1012427p\985415\SI\188034\DC3Ak}\ESC.-\"'", "\r"),
            ("\v8\67075\STX", "\1083399x4\nam@\48393\fd'\25202\&4g%ngl\\g3d\21789\DLE.\1050581a"),
            ("\CAN", "\SOS\61865\988723\1101769\72252\n-s"),
            ("0\ENQ\"u\RSS%", "\"\ETB\998898\ETBsI\1113419\990022\ESC\t\EOTwA\FS?RUi\1060951_\DC44\27969/\1113617"),
            ("K\"rO", "\v\FS\ETB\EMb\156408s\171987\SI?\1098788\&2\")\36126)"),
            ("Q\RS.\1012674\1102164\986191\DEL\a\DEL\ETBFEj\DC2\1022184?j >Cv82vY~ mqy", "l>rV\DEL\ETBYz\83318pJF\SI|,"),
            ("X\DC4\1011458\1052511\148563?A\99070\43007\68322@\158252\a\1023501N@62\EOTr#d\1102274b\DLE", "\1062082t7"),
            ( "\189342\1036382y\999704--DG.D",
              "t}l\46821B\SYN9\DC3D\1113382T\1108830!K|\ENQ|:KU\EM\1105198@\73749+\ESC\SO\29306"
            ),
            ( "\988901\&8\r!\24330R[\DC3G\17751\SYN\SOH\SI?>LPKE\r\21128\ETB\1067860",
              "\1050442@)R2I\1096562\174002\999586m6n8\177225R\183296\163443\&9J1\190770\983764\986340\SOHLRw\SYN\1050284"
            ),
            ( "\1038100\1066346\&0\29703\1097218\1006964\983165ib\\RI\156345Bb4\ETB\1098848&bTVv\SI\68806t\43546\1085334\&9\DC3",
              "\1057783\23147\1053386H\1028525\DC2\94911\&6HE5\1038476\ETB\95433\1099384\7983"
            )
          ],
      richInfoAssocList =
        [ RichField {richFieldType = "", richFieldValue = "a\EOT\US\990379th\174671\1004957"},
          RichField
            { richFieldType = "\1084720\&1\111239[\\y",
              richFieldValue = "\nq{A\SYN\1104064\8053(}\ESC\1087325K2K\b\DC1Cit\173313"
            },
          RichField
            { richFieldType = "cf|\SYN{\ETBd\1034470\1074120JoLS\1011229S\SO|\156132|eE<",
              richFieldValue = "\ETX\1069228\ESC\74770\46177\1043093i\DC4.d"
            },
          RichField {richFieldType = "\1023997\1106991", richFieldValue = "z\54313\SOH9[\ESC9f4\2209"},
          RichField
            { richFieldType =
                "^#\1072101\57352!\SOB\ACK Q\1066051\1000366O\t\167759X@\GS\33915L\DC1g\ENQU\CAN\1016249R|",
              richFieldValue = "\141801\1113010Y,\1022133\984371\1110036\100637"
            },
          RichField
            { richFieldType = "{\998053\13016\1005789 \985019",
              richFieldValue = "TH`\1064567\1015273\ESC\DC2\60656"
            },
          RichField
            { richFieldType = "gIcTc\CAN;b\18097\DLE~\t\986477mWU~_)avv",
              richFieldValue = "r\SOHb\1033353:\1098734\161297\35845%\1030189o]\16288\1037928h]N{\SYN3H&\73834x\DC4"
            },
          RichField
            { richFieldType = "$\EM\94651_\119998[\a1(\139256\62509\DC3\SOH",
              richFieldValue =
                "&\CAN\1023849'\179633f!\1056824BF\NAK\141841b\161257P\52739h\1067768%\3657c\2275\1076613("
            }
        ]
    }

testObject_RichInfoMapAndList_user_7 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_7 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\ENQJy\58686\46921acIXK4\ACK\153028", "yk_\998162"),
            ( "\v\STXj,$\1004659(\ENQ\992342\179735I8\22559\39519\EM\RS\138941F",
              "q\52986\STXd\47044p\35044\SYN\1100089[\176474\1073824tl\1024631hF?"
            ),
            ("\DC2\"\SYN\NAK)\95970\154328\DC3#Yf3\f", "0&UI+\1085405O\68888Hl\"P\FS\CANX"),
            ( "\GS\1024731-TdC/\\\NULomf/\65096\12221\141012_*\ETX\1014476\49440",
              "\36414*\DEL\129605\1066701G\ETX'\182921~\160253KOI\EM0\983070_'\ETB\42323\917826\68073:Q=9U"
            ),
            ("4\DC2\r\1049659\DLE", "\DC1B\r\r\EOTB\61644\184587\21115\&8\1046686NR'\9945S%\DELy@o\ESCPG"),
            ("=b\60170@\DC46Z/m[", "vF\25441J\1025545\ETB\DC46F\1010187\"\CAN4tB\EM\fRhbt`H\EMH+\1048366\995903"),
            ("fM1_e%", "\f\990983\24597\ENQ\DC1\1108890\1086336\1039220K/y"),
            ("p\fj\13862u\n\rr\38028hL4\DC4\42079ph\DLEF-", "\1096142\&69u~\\04\48676\SUB9\ETBp"),
            ("u\157281%\1039198\a[!\DLE\190722\t:M%", "b\148497jb\1025904\v~y\ESC"),
            ("YL\1033543\r", ""),
            ( "}p*\EM1n\1035188\STXI\120023\1083881Z\19021m",
              "\r\ESC\ESC\f!\1032505<\61397\1020151\ETXk\62979\1014647h\68440g\NUL Y\55251o"
            ),
            ( "\DEL\1097952|\ACK.\\VO\t\bO",
              "\f @\FS\157089\1085972\EOTxU\NAK\1064654C^O\ESC4\ETX\DC3a6c\a-\FS\1019123\&7e\166420"
            ),
            ( "\42819f&\NUL\1044603\145954\156779~q\f\f)M0\25163D&tu\a\v",
              "BH\f'\41294}a\STX\SOJ=V'\153541\1108488t!9\185173.\1096543\SO"
            ),
            ( "\134249;\NAKSOm\64823b\SUBBq\SYN\t\\\119908\\\1063965\DLEx \1034768",
              "5v\DC3!\28952+P\12898\96310\NUL\132902"
            ),
            ("\143939cz\151072(0\ETB/\RSiKwut\RS\EOT}\1048670\b\f\1005845\n", "#"),
            ( "\173347$\1013271#E\171209(\1032692N['\148001o~wL\19715\ESC r:7\11128dmA",
              "{M\164240rud`\1008412]v\67072\1090405\1091224]\US\EOT(\ETXAah\135204Z.\DC2\SYN "
            ),
            ("\1007835C.IE\CAN", "mI%*O\1050793>!;D4`h\DLET_v\1051579\&4e."),
            ("\1031846\&8i-m", "{GUBA&\1014120+\ACKR&\ESCSsDVk1"),
            ("\1064339B->", "Q\151071K\163816\1094737\138798\1016820g"),
            ("\1105263U", "\\u\r\21997\74078\1094141\1098949sWJ#\136200d\ETBe.\am\1092241tz<\a")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "yVr\1070553'\1069999\178919[\70436W!\179079\121209x'!d",
              richFieldValue = "\1066404"
            },
          RichField
            { richFieldType = "\167907\98319",
              richFieldValue = "\983580\EOT\ETX\1081304N\DC1=\DEL\170497\1045717\DEL!K\GS}"
            },
          RichField
            { richFieldType = "C\5689?L\EOT\n\1017425\r3\997957",
              richFieldValue = "\44277\152011\1037822\15380mr1\RS\135378@\ESC \152867BCm1\DC1Pw\1095940Vs\\"
            },
          RichField
            { richFieldType = "\EM2&\GS\65457c9-\SYN\b-Z\38199\ENQl>",
              richFieldValue = "W>\ENQ-\na=,jm\1070873\FS\1050317D\185060M"
            },
          RichField
            { richFieldType = "_\175447\1039978zK\r}",
              richFieldValue = "p4\1051406&oh\DC4X\995132&o\59772HE;'eNj\nI@"
            },
          RichField
            { richFieldType = "\ACK\SOHT\1084069\1100918\f\FSOr,\99101Kh\5381\47691\833*r\ETBYo)L$\SIdGH",
              richFieldValue = "!\36363Wg\\\42303$\148610<6pb!\ETX\1072329BH\DC3\1085976:+\EM\STXF\CAN=\NUL"
            },
          RichField {richFieldType = "W\f7|%", richFieldValue = "6\ETX\DLEXq\43873\ENQ"},
          RichField {richFieldType = "\1054196\&4f", richFieldValue = "$\21287X{};c\CAN\176923R{"},
          RichField
            { richFieldType = "\CAN1=\1112874t\1064394\54291c\NAK;\33800P\173520%\1022737\128040Ug\181182@o\\ao\DC2\FS/",
              richFieldValue = "4\168384\135625\97942\ESC\160766<>d~H\NAKVQ"
            },
          RichField
            { richFieldType = "\167692\ETX\DLE\46872\996241;,cn$W^60\25496i\ESC?f\1027656\4631Qnf6\1088314@?",
              richFieldValue = "\1068831\NAKS\v\1034582~\1036986 \154074\1079904!\1017472\SO\NUL\148458NJ6$H"
            },
          RichField
            { richFieldType = "\ACK,bOfoZ*+*\127773nd4\ENQ\179237V]\92570\&3",
              richFieldValue = "\ESC,\DC2\1048312W]Y\ESCE\1009012vDiw\156939\aw\23869\RS\27634\1058290\fD\tUY\1054152Y"
            },
          RichField
            { richFieldType = "\149138\ETXI\EOT",
              richFieldValue = "\1085030\45494O\NAKwa\SUB\1064114\147901k.p\ETX"
            },
          RichField
            { richFieldType = "+\ESCA9z\1042385E\DC4\138580|Jk\54852\SO\1111039s;~yPY\1013727)\fw)",
              richFieldValue = "\ESCl\7678\1065306\169339\18038\f\EOT?szC\185520u\CAN\DC4\NUL\131789I\142165"
            },
          RichField
            { richFieldType = "tK1R\119669 \1003469\1010598\SOH",
              richFieldValue = "\984258\azmw\rJ\42327u\SOc9\GS\STX\1085970\1045411"
            },
          RichField
            { richFieldType = "Wb\20169U\EOT+7\164348\1059589\NUL\FS4\1031161]eM\53509=\27826\6673\b\\4\FS\1088938",
              richFieldValue = "\1098170\151564\990266~sg\1076582?\177687w\RS\177697\178277>7"
            },
          RichField
            { richFieldType = "~2+\1075449qp)\185719\DEL&\1380\f<W",
              richFieldValue = "\164178Xse\ACK\1106417^\22848t\STXm\1063292\STXBrM\1057600\&2(mj:.\1065459HD05e"
            },
          RichField
            { richFieldType = "\b]\US\183100+t\147916\tt\"(\35923|@",
              richFieldValue = "Z<\CAN%D\\FV|\162190qS`@\DC3"
            },
          RichField
            { richFieldType = "z<WXchW\SOH9a'\CANI\RS\DC3F\1074319\132122\CAN>t?\STX\SOH",
              richFieldValue = "\ACK\1023191^r\58137\nLX]\992619"
            },
          RichField
            { richFieldType = "jZ\1107214\&4P\798{B\1046594N\1032160\74927aZ",
              richFieldValue = "D\97344\990361\DC3\FSI1"
            },
          RichField
            { richFieldType =
                "B\SOH\STXV\EMErnb\180698\DC1\22574\190483\170702\997178U\993006\DC2p?\SI\EMTl\NAKOH\174078",
              richFieldValue = "\SUB&="
            },
          RichField {richFieldType = "R51\140325}\bV3\EOT=M}", richFieldValue = "*9\""},
          RichField
            { richFieldType =
                "\1063696\DC2\1112699O##.4h0\170622\CAN\171670\SO\164722\134879e\19855\&0\134134\ENQ\SYN3*\CAN/",
              richFieldValue = "!>\1046063\FS\135771\b7\183788\SIO%?\1107711Z7i"
            }
        ]
    }

testObject_RichInfoMapAndList_user_8 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_8 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "\168734"),
            ( ".1\1017900\SOH2.?K>p\ACKGG\65734U~",
              "\DC3\SUB\DC27\DC1S\EM\145842\FS\1103663\FS\ACK\169296n\1042453x,H\1069717ZFrrU\n"
            ),
            ("=\SYNbD\155035`8>\1032487'\1009948", "\\[\EOT\"\tg*~\ETB\148396{\SYN2%7_X\38235\DC3bC\niE"),
            ( "A\984792<\ETBu(ZbM\2326\186992UeS!\1107326<\SOH;@\DC4\STX+fwF\SOa\1052278",
              "\1087471\&0\DLE4\a\140290(k\ac\127282\18274-\1021422W\132842v\EOT\v&5J"
            ),
            ("p\1033750AclBh\t\FStf\1075770", "\ACK\178575\t1F\t\5310"),
            ("Xpr3&D\1079765\129368^\136014d\EOT\ESC{?\STXOd\36589\v\ETB\n\1049596,", "\DLE,[q\DC1B\1014186\92380"),
            ("yi\DC1:\"\57429v\32129b)\DC4.@E{\189972\1032385\171339YO", "`_h\167346"),
            ("\164042\164600\DEL_\41466\ESC^p", "B@\vE\n\r]P\STX"),
            ("\1075545\1009037\fU:", "Q%:t\DC2\ENQ(\2810:\NULj\41149\&8r>\DC2kmu\95110"),
            ( "\1101735\6627$\40648@\1061550\&7hQ8\164683*\EOT-'I\GS\150556\US^?Oe\STX\42442j",
              "\153585\t\119634^oG\DC3T\ETB+\SO\DC4g\1082103O<\983519n,mcPi2%= "
            )
          ],
      richInfoAssocList =
        [ RichField {richFieldType = "\f&3\4306$ur\177822oQ\1020175o\EOT", richFieldValue = "~\ETB\1084126\1113613LR"},
          RichField
            { richFieldType = "D\SUB\"\95144d\STX\NUL",
              richFieldValue =
                "\r\DC2\ESC9\32611C\96044\DC1H\151316\96727\ETB\991002wZ\1067986\16822\138867\SOH}x>Fd\SO&\4911"
            },
          RichField
            { richFieldType = "\1081957\129553\v9\ACKyXg\1110443NU\ENQ\67721\RS\66779\&4e6\1017278\a$\95933~",
              richFieldValue = "E/V+~p\1087990\DC3\11405a\60204\ETX\78290\v\f\1025599A[n6^N\t\59898\&7"
            },
          RichField
            { richFieldType = "CV\DEL\1026446/\DC4C\1027356EB*\1073139r\1024961\b\1030783\989999\151414m5\144580i",
              richFieldValue = "\NAKIk\DC4"
            },
          RichField
            { richFieldType = "1=D=\NULv\183554jD\ENQO(",
              richFieldValue = "\vAlLMb\CANvvn\DC3\ESCM\188913X`\168429"
            },
          RichField
            { richFieldType = "3\NUL\158775\SOH\STX\1071447`\144149P\USKEV\1104776\&4U\30610Ox",
              richFieldValue =
                "}83i\174615\1088090\1108364\NAK\1058962\144833\&6h\b\139235I\1058230S\DC4\DC3OW%_\DC2*\139154"
            }
        ]
    }

testObject_RichInfoMapAndList_user_9 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_9 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ( "\DC1C=D\1092975\&6m\183322\1030244^\1094152]\1113682I]x\n\SI\ESC\ESCt$\US\NULiX\DLE\SUB\1024319",
              "H\NUL4\tE\DEL(\1097719[\159865l\ETXGm\ETX0y\1038520]\146730\1096471)\DC3z?"
            ),
            ("\NAK\1080970R\78608s", "\DC2\f\178260\&4\1100188\US\r\DC3\165138x"),
            (" \189377\1100428b", "^L!\181405?~i^\DC2W\ETBg\986734\1101323*\72331\1014492\ETB\"8\1073966j"),
            ( "55K\40405\1008412\r\17921\1060113\1091673\1009671\t\983367R",
              "*H\168094\bg\1073008$oU!9qDd;\4720;z\146960R\1001826"
            ),
            ( ";`\62882)O\1097338\15981\1108054D(4l\1068400w\b25",
              "\1016965\EOTo\NUL\1025588\1013620$\16383\1092882DH\1075666\1087589\tk(\CAN\ENQ1M\1104067\173309\ETXcJ'"
            ),
            ( "H\47124\1038678\1089458%\1069920\43588H\ESC;]\1005211\ENQ\177765\\x",
              "T\24061\1050025\GS\185345mA8XI\ENQ"
            ),
            ( "Md6DS\1102384\983103#\SYN9e\US(\\\1024729L{\139901\1075502\DC4\98402\USp\168330`v\41799\&8\NAK",
              "B8S\985354;Tc\DC4\34011\37027\983124\1059709\NULhp\\+\992960]p1|M9RD\176534n-\CAN"
            ),
            ("w\15907\46077\SI\1026142S&\1113616\180599n;\7438po\b\ACK\1073265]I.8\1041840Nd\1102809\f\ETBU\aA", "\SO>"),
            ( "}S{;\17158j|\1074873\1020995",
              "\n1\58224+\166151\1016174!Ix\1032921L\ETX\2637\178561z?\37010\&5\DC4\v\"U%=\64279e\31156U"
            ),
            ( "\163183\EM*\1054724\\t\164022K\171461x\9054\1040150\24867\&5\1093083\RS\1019810\6424",
              "nSq`\t\vr\RSQ\"Qj\USyE\171450\&58e2\DC3:D\DC1\163636Un\GS"
            ),
            ( "\184513\v\CAN\1100721\5529H\12836\EOT<\\\996700\1092557W(Y\NUL\1107019",
              "\tb\DLEA\17694;\19219S\23988\1046617\46792%\1010606!/\1095332k\1100060"
            ),
            ( "\190207)\1023504is\171644\36126\ACKjJ*{p\1062831\&1\163252zm6g^\176808\68056",
              "=y=\FS\186577K\RS\GSy8\DLE"
            ),
            ( "\1049369wz\1046030\22352\SO\1048558\EM+oqC\173089^a[l\1020681Z\119990\&7&\RS\DC4r}",
              "Ihel\CAN\DC4\37046\1012506\f2^*h`u\ACK\FSu\171153\1016971\DC4"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType =
                "\1051215e4\RS\23439h^x^\EM\61905v\ETX+XJ\134982[X\1092473\EOT\1077911'\DLEK\1093610\155900",
              richFieldValue = "T5\11971\ETB\1047874](}{\ETX\1043337'\1081171Mq5\1020468\SUB\ENQ`s\46654\DC2Zw"
            },
          RichField
            { richFieldType = "^:;\CAN\FSy4",
              richFieldValue = "\1059457(0\DC2\ETX3\34133U\178634\&2\1068820\3182F+=pd\rp\1109245\28693o"
            },
          RichField {richFieldType = "", richFieldValue = "6cP\DEL\6080R"},
          RichField {richFieldType = "?\1047538~d\DC3;U\1106640P\995958zJ*{*T", richFieldValue = ")|\DC3\fK"},
          RichField
            { richFieldType = "\1015119<^\1999\ESC\184113lIdb\1072838\DC1^t\DC2\174936\1100963\182884Pb",
              richFieldValue = "\ETXd-\188647\RS\180191 z);],nC\1022457\1068377\180238D\999368\SYN\r2\FSD"
            },
          RichField
            { richFieldType = "\CAN\60004\a\DC3H[s\ACK93",
              richFieldValue = "\1041236\32361H\NAK\1096623\129058K\1075562\DC3(\SYN\181142{X\FS\189569wV\1034882"
            },
          RichField
            { richFieldType = "$\36819T\1105580nf\SOHT\133740{z\1026264Goz\RS_@[V\EM\1031481\&8C6D",
              richFieldValue =
                "F\DC3\73440=k\DC4\990834\GS\1060856i\163960\&1\1062637J\98269m'P\1027260plO\188080\1055753"
            },
          RichField
            { richFieldType = "\DC4v-\STX\EOT?\EOTQ[0\146988mnFN\t/>\ETX\1113899V\1000937B\ACKF\175446",
              richFieldValue = "o\ETB?}7H\146313Z\168011\&7\984607%4\173083\3879\167358"
            },
          RichField
            { richFieldType = "\FS@$'\7020LR\1058824w~o\1007673'\b",
              richFieldValue = "Ms\1025378\1034881B\1022931M"
            },
          RichField {richFieldType = "m9d5:O.4\1101624\DC3m", richFieldValue = "V\r"},
          RichField
            { richFieldType = "4fJ%*\US}Y\1046694\ACKV\1012548\DC3O\1062399\ESC2:{H*c\1005890\189579z\t\1021171W",
              richFieldValue = "G\171123\1090504"
            },
          RichField
            { richFieldType = "2\1023994\b\"f}+ \ENQv\r\1030394\ETX]F@\1069254\&0R\16066",
              richFieldValue = ",\ACKR\1053242!g\186623"
            },
          RichField
            { richFieldType =
                "C\1086563}\"j2\138736\vWi\1050956\61878\2267\1033370\SIDn\121030\1081299\1112031\20632\&9\a\a\153143A\SYN\57533",
              richFieldValue = "&,61\SOHC\986476uj>_"
            },
          RichField
            { richFieldType = "\150181",
              richFieldValue = "\DC2I\27369\DC1&T\159506x\1044600i\ENQ\19979\159274\8229\32065ZoH~P{4"
            },
          RichField
            { richFieldType = "K\v`=7NH\t\48484\1045014\EOTH",
              richFieldValue = "^_\\\\*\153688\177860\ESC\a\1031904\1040165AW\ACK'"
            },
          RichField
            { richFieldType = "RxS\CAN}\ENQi5\40088*Z\1038420\1026632",
              richFieldValue = "{%\ETXIEz\DEL\EM$O\DEL9\21968 \1034484RLt\131300\t\162365C\ETB\1021346["
            }
        ]
    }

testObject_RichInfoMapAndList_user_10 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_10 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\rw\52584f\DLE\98073\1089144\1041363{4\137629*\146352\121258H\US\995063%\EOTH\991375", "\DC1\1020499\STX"),
            ("\ETBD\1073349\SUB!\131245\1056613\1012908z", "\"e\26354q\t\1043638C\179332\50826qd$H\ACKn\SUB&s%5"),
            ( "\CAN\1095837+\988206\&5\17296h\n\1031439@\10137\SOHX\GSH\46038E",
              "0\NUL,@\121173\69854\SID}\f!MLXPZ5H\1090104Aa-\190975L\153407"
            ),
            ("&\1003117G$Dh\1110276;b\STX-~\158859W\174383\1100999\137868I\DLEi4\1055984", "A \159575<"),
            ( "`;\SI]\31840\1024538!67:O_\STX",
              "\vl\24815\r\ESC\989873\&6](\1067224Ps\ENQ\GSy\148469\SO\1089601\a\1076f\DLE\24031w\\"
            ),
            ( "Kt\17489\SOHoT\993262x2)<r\1016382j$K(;\1062721fWW5A\"\ENQ\179752",
              "\NAK5\STX\990688\&0#q\1081130\140907h\1086154#"
            ),
            ( "l\f\bU\ESC\SYN+:\1014585\"\1080151%$lP\69810\"\DEL\ETX\52798\EM\SIz]",
              "d#\\\ETXv,\1078854\1068169D\989806{\\\128339\&2sQO-J\1092669:\1024070:<\1069837\FS\SYN>^"
            ),
            ( "M)\167132piIg\49115<\1058710!\1022340\DLE\1060344kV_m5\8227\&6",
              "e8{S\ETX/\92379\985844\SUB'\DELQ_\186813>\1077258K\DLE\66822r\171348\1062036"
            ),
            ("Rs\5156s\49023A1\184182}\RS\16047rNA$\1025919\SI", "0\EOTd98X\EOT7"),
            ("\DEL#N?\1087521\19433\ETX\ETXZ\1045059WPs\GS(A", "|L2\DC4Iuf,?\a\DC4\51799S'\STX\1024742b\3779"),
            ("\131510Vb\DEL\179551y>\EOT\1100761\STX\1098928nQY\1093746\998555,XY\1067157\1006253", "")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\162562\98041\GS7\CAN'\v]U\EOT{rs0'",
              richFieldValue = "'/\136814\1032804LL\USv\121440_W\73866\12178e"
            },
          RichField
            { richFieldType = "\99860\n\GSr\ESC,>{\1103403l#9*\au\EM\1075299\NAK",
              richFieldValue = "H\1105517\DLE;\1044506`|;G\CAN2LD\128169l*"
            },
          RichField
            { richFieldType = ">\ESC\1036493\1079877\1091428\1055465l\ETB",
              richFieldValue = "J\EM\v\1051091\CAN?\v\SYNH5Rcb\149915"
            },
          RichField
            { richFieldType = "K\1081792\1068788w/\191158d \EMs\37229\SOHw\1014069\1063075\&5Z\35772m\1058616LQ}:r\GS",
              richFieldValue = "\1053421`oi"
            },
          RichField
            { richFieldType = "\bN/\1006005:3\1087462%[\1061611B\516{\no\68053\EM`%D(\"\EM\168355\1063458\1065708",
              richFieldValue = "\50897<Cs*6\178656\SOH2"
            },
          RichField
            { richFieldType = "\b%\ETB\983174\59639e\DC3^\DC3B\156838",
              richFieldValue = "qa)\STXl/\997210\SYN\DLE\137060+\STXztSih\RS5e"
            }
        ]
    }

testObject_RichInfoMapAndList_user_11 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_11 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "o!\128659AZn\FS\a\""),
            ("\ENQ\984805y", "\EOT"),
            ( "\DLE\vw!FZa#j\8776o\149945\1075650\DEL\rL\DEL\1097155rQ\SYN3\v\5693\132595\&1",
              "Qz)\1057950\NULAAWZ\STXH\ETX\1095220O-%\1007103\\"
            ),
            ( " \DC2\DC3\n\1084357\984798V\50209S\63357r.lH\129379\DC3D\1101480\1066327\1000699\no\148989'yc3\998004\1021611\ETB",
              "\1062658\EOT\DLE~\SICx\SYNk\DC3Q\1044082\1062981\rZr\39782tX["
            ),
            ("(#\DC20\52173R\r\DLE\DC1VJk\DC1\f:zTi", "\ENQ\997085/\65469Q\110599\65470Gp\97507=~A\SYN\995783w\DC2\b"),
            ("7l\155233!q", "!Wa9o\78005^"),
            ("7yF\DC1\NULOye\166219\SOH\SOH", "G\STX\v.\EOT;\146503\STX5Q"),
            ("8Z$\SYN\49070O\140985\1006540dDV\a", "\52660\34184\59767y\f="),
            (":\SIE\ESC\ACK\DC2t", "}\n\EM\1013835"),
            ( ":#_\143302x\NAK\1092136y\52084$N\SO_\nO\r{]\93990\41639\1061866_",
              "\SUB~\28334t)\SOm\1003634\126256\&1\ETB\1093032\1063444"
            ),
            ("`%\1094710d", "l|\ESC+l\SYN\146331\148\1091026\NUL\SUB\"$\1109052\999421<\SYNQ7\152685FH\5444\"*"),
            ("`k4^{\DC2\194581X\1108049N}%lI9P\SOHz)\DC1\DC2\49189$", "\136537~"),
            ("GJ\ah\FSjd\DC18<ip\SYN\47239\NULQ.\DC2\12869\1025427\1067080!.7P\vMe", "\1078481\DC1i\NUL&\1061560\ESCf^"),
            ( "I\GSt\1064339\1039059gA\1086005\1054377\96293\10725\USv\1064012\t)I\DC3\au\152624\1003241_4\SO5\20485\20695\\\1003826",
              "F9\1074535\EOT\FS.\70293\&58\1009412\ETB\160087\DC2\EOT-\DC1]7O\ESC22\nl\39238"
            ),
            ("K]'Q%$\"\RSZ2O\795\"", "\99141pp\40147I\41930+>"),
            ("O\ETB'Vj':\ESC'\SO\NAK\6382\SO\CAN\nQ\1107745\STX\EM\51052;Jx", "}\EOT!\RS\ETBMyk\1074940\146115`"),
            ("si\158818\\Z96?\aF}\b\83444\r=C\37107\44897\vx_", "a\GS\181693"),
            ( "VR:v;ZqL\183938l\USn\992515\1061218\161309\66717M\132632",
              "\GShu\23833l\1108324\131688?1\42858>\DC2D\1038180\1091974"
            ),
            ("}\DC4\1053586I\SOY\1031277", "ld\DEL\SIx&\1008012\42453\986710Mg(\1066044\aa"),
            ("\26634{\49212<xZ\RS\167687V#3^\30073%S8\1101207;", "?GnhDhP0\b"),
            ( "\38789A\1004443\SI(\NAK\r{\177991\1056903",
              "a\1062881\NAK_\188631\&0ya\36886*\ETX`\ENQ\1021386\1061121?\165859s\179648<\1073656\ESC>\v"
            ),
            ("\136652 \1024340fN?`\1111185M8+\DC2Ai@\ACKh\f", "0\DC4v\5573\fU\990977WV\991145c\97698=\SO\EM%\149365"),
            ("\1070976_\CAN\9468\"9\SUB\34276@\DC3|.\ETX02!{8*7\EM\158828q~t\151776", "@^Zb\1027800^K\55182\DELT0T~@x")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\ETB\1028921\CAN",
              richFieldValue = "\1102929\44209\1112970\175634Ih\63283Wi\1012582\DLE\190837Y\ENQ/\a2w\989014"
            },
          RichField
            { richFieldType =
                "\176000\1100835H\SYN2V\1039249&\92476*\t_M%\191397\CAN;\1074124S\SYN4\STX^\USk\1088603,!98",
              richFieldValue =
                "\NUL`r\DLEY\1102587\1034451\&4\166294@\1084921\&1zCYPqi\1006156\SI\58745p\995662\1043262\SOH\1112751~"
            },
          RichField
            { richFieldType = "iJ>\1028036f\175431\SUB\45400\ETB\EMG\993617\1056285\"\ENQ",
              richFieldValue = "H\1017260g8\DC3<cl\12237\175519\a\1039305\156694\STX\1108958Q_\NAKHX[V\32050\&9q\GS."
            },
          RichField {richFieldType = "~\178318pM1@2s\10579k^;\1113906-", richFieldValue = "X\147226e&3t"},
          RichField
            { richFieldType =
                ">\f8p\67122\150163\1007636rj\170640LA\CANq\996681EG\1091126\USgF\1086605<X\1102154\151054\176082\b\1033566\95903",
              richFieldValue =
                "\GS<l\ACK:i>\1073305\95139<\1101082\DC4\14223\31755do:\nZ@*@K\1008021\1047329a/\b\190930"
            },
          RichField
            { richFieldType = "zu\14791X\EM\1026999m\166071\"o\\G\165311aJ\SUB",
              richFieldValue = "gR\ACK[Il*\141972,\DC37sb\ETB\1045231\58013zSo6\t\13600\83082z$\GS\DC4\42606%\ENQ"
            },
          RichField
            { richFieldType = "\EM\SYN",
              richFieldValue = "\1021376Yq<&8\SYNb\120405\USOi\SOH1/8\1017260\10473\SUB\DC1:\NUL3"
            },
          RichField
            { richFieldType =
                "\147712-K4[6^/\181634tLt\SYN\129186\1019826\175957\20283\1041885'\38284\&3\1085802\1035352",
              richFieldValue =
                "\b\140598&\1058279ww\DEL\26686m\16482r\13860]\1038937M\CAN\1028432\SOH\a3Z=\1106760\191074B!\SUB\1093109f\54706"
            },
          RichField
            { richFieldType = "\ACK\144056,R\b\STX(",
              richFieldValue = "&42\40054\aSXk\42616$GT\1046779\&6\1034064\1067204\rY=\1985\GS\174373"
            },
          RichField
            { richFieldType = "E&(#ZwW\SOH\1057331\&80\r&.N]H\1110033/$\"\US\61428\40791\&9\n\FSAd",
              richFieldValue = "2. n\DC4bHjg\83293b"
            },
          RichField
            { richFieldType = "sbSx1\1062630>92|\1070703\v|\DEL)\EOT\DLEM\b2k_",
              richFieldValue = "T\1074925R6\1059631K\STX`\EOT8KCK\172584\1067031"
            },
          RichField
            { richFieldType = "\EMXr_svft\\i\98504\1005552\62119\189306\135519|l@c#q\52137p\\",
              richFieldValue = "W#q\7135\1106012\20928"
            },
          RichField
            { richFieldType = "\ENQsI~jB\1071425z\989923\STX\SO\1039847\ETBq4M4\1114035\a]8\EOTZ\1099283j",
              richFieldValue = "#\49940\\:zs\SOH5K\1044726C\SOH=\"\146107\1045637\SI\1069084\23493"
            },
          RichField
            { richFieldType =
                "=\27551Y\134547\DC42(=;\DC1V\35005 \150797\1019078\181134Zp\r\1013314\1056249\&4F\1068630\DC4",
              richFieldValue = "\194849\6614"
            },
          RichField
            { richFieldType = "?f|\156180psj\1005905]y>\1111801\1037872-\6502",
              richFieldValue = "|Wa_K\1091452\172742"
            },
          RichField
            { richFieldType = "\b\1108066\&7b\66357Z(m7\RS\30522p]E^-m\EOT.\USq);\149286y\137949(\US",
              richFieldValue = "\DLE\SYN+he\FSx\998260\"\GS\DC2P5<\RS\21259\18135+"
            },
          RichField
            { richFieldType = "|\tKli\\\98809\143023\37329d\n!\153054HO\1096707\&5\GSu\DC4+\1089025I\GS|\ENQ\US",
              richFieldValue = "JU\1000244O\ETB\CAN\185069\133322^G]5\996307"
            },
          RichField {richFieldType = "x_H3J\1074422\EMg\1100163", richFieldValue = "\18360Q>(>F,\ACK5"}
        ]
    }

testObject_RichInfoMapAndList_user_12 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_12 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "\1083885;)XGD\EOT/Qr$Yg\SOHA+d_\SI+\SUBI,%{\ETX<'\15371;"),
            ("\r\SOHU\ENQ\EOT\1044851\ETB\US~\SYNC\147655!8\v\25144\989691\FSN@\38109S8{:\64824e", "U;rKHG+9"),
            ( "\EM\a`<*?\GS#\1091963/5\DC3\52627V\NUL\nBi\US\US\54687\v\179155HP\5825\1039168\64310\SOH0",
              "f7]\DC2\NAKV\SO\&HT\43387$P"
            ),
            ("\RS\154364DC\FSD\153881I)6ik", "\4974PA\47701r\ETX\1083541\&2U\t q\170204(|\4549fS\DC3A"),
            ("7V\ETBZ)\996365", "_H\NAKc\41046G2W9\48972+*"),
            ( "9*\1003630}~\EM\1098880,^+Nrw'\EOTe\FS`\77834",
              "\1003373\DEL\183653J\189391\1111867\1005029\139975_\163529pGuO7<\992034I\FS"
            ),
            ( "I\f\US\t\120640k]\1069451Ml*\1083757\988925\a,lt}",
              "^[\1015878\NUL[g~U\by\SUBX\GSW\a\1061261\\_\ETBpK\118973"
            ),
            ("qv'Ip\b \138554}\149834mU\184356", "O\1011066D1\170767h\DLE\SOH\16293\28331\b=!\ENQ\SOH\178629!F\42679"),
            ("vy\ETX\NAK\"\191310%g\180332\1103310\1108066i7", "H*o\15751y3!r'w\13669%Y0\vUO\48125g"),
            ( "\28107/\v/\SO]\183248,\"\165116\DEL\1087134.y\32199g\t\167779I\EOT_9\1082603\v\1044571/",
              "\13049\15893\8595>d6\1077580;\DC3n\ETX"
            ),
            ( "\169732 \998174nCx-t\RS;",
              "T\988657F\DLE\1009453'7r\65241!HF\13064\991049\ESC\tt\136962\166561$\GS\1055415\SYN\1005820\ESC,\1006985\1032653"
            ),
            ("\998574)r\DLEr", "\ETX4M\US>\NUL`y4\DC2\EOT/MJ5\189674T\GS"),
            ( "\1033775\180149x1(~W\DC4\23052\ESC]m\GS\DC3\NAKA\ENQRm\SO\ENQ\SIC\f\174718:]\DLE\SO",
              "\1086701\993831(.Vi]\1078519VQ\1040785fi\SUBh@.\RS\ETX7ij\"U\183007L\983338"
            ),
            ("\1038329", "N\SOHD+\43990Y\1112880QY\62836\&1M7\142119\ETX\147825W\144580<p"),
            ("\1100714\&3\32867Ka\1113966\1090135;&#_G\167563\&9\\r\SI\171899\EOT\SUB1Wi^\160636\US\DC4\33991.(", ""),
            ( "\1101629\1033765\40471\&2\987303\1048909\EOT\131189\1105976\GS^\1064758KZ=/\173131\n#V\ESCj\22586\62808",
              "\ACK1"
            )
          ],
      richInfoAssocList =
        [ RichField {richFieldType = "\1084014\n[E", richFieldValue = "QT\UST\1039349\US\CAN/]6\SIV"},
          RichField
            { richFieldType = "S\1079806\68402\ESC\STX\1111196o\NULtGT\DC2<xh\165855\1095141\&1\NAK\33021\1022953!G\FS",
              richFieldValue =
                "\1014637\n\SO\1012007Lj+c\ACK\40210L\996462\b\52233{\t.\1054087\t\33420\3330\1072764\169295\1036288\1100641E\f\SUBt"
            },
          RichField
            { richFieldType = "^0\1100824Qg\147595)\5959<P\1088770G\ESCL:k\ETX\1076725[aOk6g=\SI\170009\STX3",
              richFieldValue = "E\9320\149555\&4OG\1064080=No4\43108"
            },
          RichField
            { richFieldType = "\CANNiO\STX\1040441A?)",
              richFieldValue = "|e\78465\149277\SOH\b\1012524\1002150ZHN\191437\DC1U\FS/\DC1Yu\f"
            },
          RichField
            { richFieldType = "o\51863n\NAK+;\1085374v\\\US{\78330j\1102115 \FS\CAN\1061518<2?\EMWrz\SYNhE",
              richFieldValue = "\STX\60961\1063623\&1sS|Nl\1041394\GS"
            },
          RichField
            { richFieldType = "\rN\DC2-C\1109957H]s\2665?\EOT[# ",
              richFieldValue = "W\GS\\\GSBPI\ENQ?a\SYNQ\1104667"
            },
          RichField
            { richFieldType = "/\r_q\983181\135835\&8a\92231\FS\"-",
              richFieldValue =
                " [\DLE\DLE\DC3g\1043750j\57914 h\v'\990489\ETX\rm \v\1059799\1112238\1027484\DC3K]\EM@\43953\66433P"
            }
        ]
    }

testObject_RichInfoMapAndList_user_13 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_13 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "}X\1003432W<\1066124+1\DLE\100246\1068057\SUB\rqC\142512>p|\170597"),
            ("\EOTs\52539\1077694 \"u><M\ETB6jq\nk>\EOT\994271\&2a\ACK+\1004972c\ESC", "\1084717\SUB"),
            ( ")j\78068\&9$\SOHXxjZ\162124\&70\991754\EOT*@..\999293",
              "\t\f\1100922j\EOT\GS\1047725re\FS\t o\ESCEx\SUB<xt"
            ),
            (",-[Q.Y(,\n\169892G", "\139561"),
            (">\1051517\ENQ\EOTT\38752@\b_L<\EOT", "B(H\ESCTU\EM\ESC"),
            ("\\O^f#", "m\GS<i\STXk\"ULmhw\71203\v.Q(@s\1077635-auK\CANDa"),
            ( "]=k\17222Ra\b\DC1L",
              "\98500\1040086\DC2\1016992\SUB5B\1012566?\"\1035205\160968>\1055674PBv~Pc\ENQ\SIvg\164765\984585UP\1009054["
            ),
            ( "e\1025340D\f7\SUB\DC1x=W)0R?xAw[K\DC1\DC17j",
              "\NAKum\t\141085pI|7\a[\22735\EOTF's\1089186\&2\1017228\t\1018515Eu\64063\1086975"
            ),
            ("JV|\1107491FR(k\1019650Kr\1043818\52718\1051850#/\45280#6W/\CANpk}\EOT@\1068656n~", "SH5Ou_\r\ACK82j"),
            ("k&", "7U\7602\n8CI2fjtH"),
            ("O\"l\\\31242a0 .15\ETX", "v"),
            ("P\58446\EM)\n/\NUL1O", "m\ENQ\EMP\b^\GSN\1039476us(\v}\1027386\DC1zd\1072241|\DC49\10104va[@\EMO"),
            ("~AE\ESC\74334\DC2U\SOH4\SI\180994\1048429", "\f\NAK\1098683\DEL\99154I\47358\127363\b\987227Ly)[<U"),
            ( "\DEL|k\150588\1003103\SUB\1012354\&7\1084193KS\GS\SYN\188015\DC3\1106492F$9\US\1040245",
              "nQhrg\4327nc\14002\991559,g\1018852B\1089031GX\59316c]V\STX\32717h\179981T\1081331g"
            ),
            ("\4465C\SOH\30040\"v\1100657\b'bj*(DRvR", "W\ETB\ESC\FS"),
            ( "\4921a\179700\989654{\128209Lx\SYN/\RS|\DC1n,\31383V\17280\54643\1089280#\164671",
              "Atv<W}\ESC\ENQW\DC4n5\167673o:\1024287\v\DC4$bK\27914{\1080622\DC33j4"
            ),
            ("\157495&r\59246", "m"),
            ( "\1056519\42864\f\1002487A!\1042320\DLEB\t\b/C\175503g\SI\134224\&7}\121402",
              "c\f\DC4zN\1050393\b>W$\ACK\1014220\STX`\SYN\990507I*"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\144214\990892:\t3R@\110991/5Vw\ESC0\1041520\SUB\DEL7\1068267]",
              richFieldValue = "\15315@o\1096740\DC3\36714\35767\135717g\1028134k\39645\73677\a\ACK"
            },
          RichField
            { richFieldType = "hR\176524\12076\144026\31596\DC2z\169100\bK\30206\42248\99703gM\ACK\1090014|V\bx",
              richFieldValue = "\1039687\1032408\CAN\999226\CAN\1089837G^\ACKx|K\ESC-\f\ff\182989\74813["
            },
          RichField {richFieldType = "\b\DC2", richFieldValue = "D\")`\1100740\FSw^\123148"},
          RichField
            { richFieldType = "\ETX\SYN`\166434;\CANJ\"s>s\nN-&\1043736>ZMK\SYN\5254b\61001\21825",
              richFieldValue =
                "i\RS\1086619\100983_\ESCb\181127\&2C\52608KKqLhT\1094458,\v\6592\ETBW\33260\1014248\1113697"
            },
          RichField
            { richFieldType = "FL\120196\1083118\EM\17816\1084691\rk\EOT\DC2MF\17587\&1\rYZ\t\1026268\SOH",
              richFieldValue = "cw8|\t)"
            },
          RichField
            { richFieldType = "\1023291\US\ESC\b\1015980\DC1",
              richFieldValue = "/o\DC1\169272!B\1036120\1086667\NULt~="
            },
          RichField
            { richFieldType = "-i",
              richFieldValue = "9_7\1023908!\166072\b'\1025226\SYNN(N\a]\190228\&9A\97383s\DELm[0"
            },
          RichField
            { richFieldType = "4g\1017341\163912>\akK\34590\SI\SUB[W??!b",
              richFieldValue = "\134756\US\DC3aD\\\1078083\1098680>.U\v\DC2IV\DLEh%\ETB\1005105?7\1091140\"\n"
            },
          RichField
            { richFieldType = "\68222#\100903\1040659\132882\1091894\&1\1077651\&5p\1010876\1030836\28275",
              richFieldValue = "\RSP\180743\53861v\ETByfj\11804z~6&\SUBs;Pz0"
            },
          RichField {richFieldType = "", richFieldValue = "TBA6)r<"},
          RichField {richFieldType = ",B\153638(s\9287eh\1061894", richFieldValue = "\"\DC1P@n."},
          RichField
            { richFieldType = "l5\38719\FS8\1038694\63311",
              richFieldValue = "\5410\DC2&{\ETB\49907o`\25430\EMK\SI6j5L+\1100295BtM<"
            },
          RichField
            { richFieldType = "/\ENQ\b`y\r0C\SI~f>j\DC48q g=vwx\SO\GS \2837\155289",
              richFieldValue = "\157997VXV`\\'jT\1039191o@h]\ETX\SOH\"\NAK\SOm"
            },
          RichField
            { richFieldType = "\NULf\121161`vc",
              richFieldValue =
                "\1032756\158917\1044293v!\vS.g\ACKV\\*k\8879p\989859|\DLEr@$\GS\CAN>\1070214(\1028886F\611"
            },
          RichField
            { richFieldType = "_<\1071884OO",
              richFieldValue = "\1046328CE\ESC\DLE\NUL7\1035361\ACK\ESCM\NAK:~\47545\154480/"
            },
          RichField
            { richFieldType =
                "\160591\1052097\ENQ%\FS\1105685\988838uZ.\68041\t\EMf\990882uoe!\74827\&1\DELli\159673#8\1028659_",
              richFieldValue = "Hs\1003980cZ]\94294\1066192*\1047989\SYNk\153579\&4\181276\DC1\ETBxL`jh$\62298\FSR~"
            },
          RichField
            { richFieldType = "\1039447*k\999532\96108",
              richFieldValue = "\1073251\&9p\STXp\toB\45207F'\145543#lG6e\147192P"
            },
          RichField
            { richFieldType = "0\NUL\1085225\v",
              richFieldValue = "x\ENQ\ENQ\94808\STXF\1094085/tuVf&1\30683\GS\182054O\163705\1102758"
            },
          RichField
            { richFieldType = ";\1084450i\1020423\&3\28119\10711\1105270\&5MG:G",
              richFieldValue = "\41834\989824\119216\1087060\DC2d\140650\&8A\32082f\1000962[^4-<\137587"
            }
        ]
    }

testObject_RichInfoMapAndList_user_14 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_14 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\NUL\1082393\125007", "& [\r\bC\GS\SUB]F\vp\SUB>d"),
            ("\ENQ<\b^\fO", "X\988837\DLE#\NULs$\1109898\99145\SI"),
            ( "\ACK\USF;a\1082803\CANx\SYNQ\1090014G/EF\t",
              "[\188400v\US\1031852\&5&\10847\1106936}\1052044&;IGz\991468\&8\1102456#\1030654"
            ),
            ("\t\1048784i\SO\RS@R\SI>[\n#\144703\19029qQ&\18741T.", "O\bT\28125}Z3\\Cvd"),
            ("\ft^\8269_\21073\100236d\139398\1077518%", "\1088549\nLGSY7\1049971;N\v\146751\EM"),
            ("\DC2j\132323i\SYN\SUBUAalz2\167120\DC2hK\7131", "\67608+\1045280G\150216\61784IaHb0$\DC3Y0)uJO-l\1104528"),
            ( "\ESC\1101448\&6\171913e\183690\195004I}\22976\RS\FS\983472F\GS",
              "\SYNo\37455\74015xMem2r\62398?t\DC1l\137407^\1091374VjG\EOT\94440[;\47281\EM_\83171"
            ),
            ( "-+\aNS\STXdY\EM\ACK\EOT\1063327er#<<\24188i\1018098r\ENQ\1113752grq\166403\ACK",
              "UE\1093061\52110\DC3\1068965\1095906\&9\1099743T\1060117\GS\1035947\12484\7047z\95939`*\62770\1106332HO$o\1005006!\998704\a"
            ),
            ( "0\100094U5\DC2\FSZMM=\17099",
              "\1022381_\1108029yw\1054070Z\1004585q%\DC1]\DC1\1005926\ETX\DC1\172839{\63240hsb\1016547n\1011894\SI\GS\1035251\SO"
            ),
            ( ":DO\1017993\SOH\STX,u\1020244\993921W\SOHH\SYN\NULag\1100256\1093001",
              "zFb]K\1005183\NULzLQ?\DC3W,&i\178150`\158756U\147609WMLZ\40372|"
            ),
            ( "D7\164081\ETB\63247\GSV\GSg\ESC\1074695_(4zO\136481#h\144679\&8l\1008616\ETB3\1014949P\1073879\DLEr",
              "]\1008036\3366T\GSXq*@\DEL\97187hx\27918*\SYN\152513\\\SYN^\24746^\USTBTv"
            ),
            ( "f\SOH@\ACK\FS9\\WXL*S\\{\f0\b\EOT",
              "G`\1011098\r?\190371MJ\1082645\1031612f\b\ESCM!jqqn\178384\ACK9\162041<\SYN]t\ENQ"
            ),
            ("v", "\SUBc\40121jl\r\EOT\ETXF\1104671~JS(Y\EOT\1061324\991171\&3l\EM4\US\SO\DC1n\63759"),
            ("x_\FS", "!S\CAN6\8862o&\72298\1081201"),
            ("z'~9\72329:2\1032892\10316>", "\181659F\179970\ETX\1020426\1026286we\SI\42102\&2#"),
            ("\3996\1015755Jd\188871\"\38364X\f\a\48655\vg\CAN\\", "\CANG\SOH\STX\v\39075M\72123\US\36582:A(C"),
            ( "\1009408b=eD\1033353\ACK\GS\EMmK\ETX\1070152\r\ACK\1109001\DEL-",
              "\ETBEz\10437IEd\59407.a\1072547bS 6f#\DLE\34513'\SYN\34614"
            ),
            ("\1014107y(\t", "\RS\aw\1094711:")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "K\52903H\FS\DLEtG\DEL{z\SIw1\SYNI\1056437\RS\1031465=&\22919j<\DC307_r",
              richFieldValue = "Sw\986997\1026171\986718("
            },
          RichField
            { richFieldType = ": )L!G\1047454xZ\175423\&9\988080\32956PO1RK\1047208^",
              richFieldValue = "vpm)P\149135\US\1051891-\1056191<\21894\b\SYNn"
            },
          RichField
            { richFieldType = "\FSq\998229r\179676{\177296\162536\1028488K\1024411*g[\38366\CAN",
              richFieldValue = "\DC3B"
            },
          RichField
            { richFieldType = "H\987148\r5\988059\1080917\54459\1017608\&7\147785\1050619",
              richFieldValue = "Q\42909\&0\\l\NUL\ACK\r\165524\54595\ETX"
            },
          RichField
            { richFieldType = ".Z\DC3zn}\DC4",
              richFieldValue = "\996576\1052202wQ]i\ETBS\USkh$\RS\CAN\ETX8\DC3\998922R[\NULE\1106599>+0Zg"
            },
          RichField
            { richFieldType = "\EMq%*Bq\181414q*\1002073e\DC2\DLE\SOkBv\60269\SYN",
              richFieldValue = "\169996\53249\DC3eYmQ8HmG\1086764\174684N\v\187675"
            }
        ]
    }

testObject_RichInfoMapAndList_user_15 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_15 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "\DC4"),
            ("\NUL\1057819WB\EOT\DLEMC\141364e\990542,z\SI\1036133-h\1103943IF\ETX\CANW\v\988478F", "l\34637cS`"),
            ( "\SOH<",
              "\NAK4\1064992k\DC18I\RS\1030304\1086350'D\21077u\1079886RZ\1027148$\ENQ+\SUB<E\DC1L\RS\1015980\&3\1092086"
            ),
            ("\DC2T", "\1003093\131953+.\rQ>L\1006588\&9Z<m0\47976Gp\53933\997068\GS\n"),
            ( ";4\1079955",
              "\SUB\SYN9R\DC2\49669\1048420h\"\ENQ\144095\1020405P\DC1\1027370gR\162995\EMD,x\125101)\18658\t\1003161\CAN\v\135654"
            ),
            ( "Uz\FSv*U\42689$\GS\111238\156784D+.",
              "^\45398\ACKYT\SYNfi\SYN\1111430DA\US\NAK\38015\1081091\1111075[\43589a\a"
            ),
            ( "YQ\EMVx\1080358\183292\1051459)QFD\4815\1075630\1061632o\31391\1031217\147124:J@\SOH",
              "Mh\SUB7\1029466/\141225\ESCuW\166180\173262\EOT\45474\r\ENQf}\1032044\1113809N/\FS\NUL(h\NAK"
            ),
            ( "}~y0\59971~]w!e\158046':\RS8n\1001856\ENQf P\6752\135518J",
              "u@\ETB\ENQd8Y\1046559\EOTK\1023113\NUL#)\"\1019339;\15882p_\20280\ETX\USDQD{@\152560"
            ),
            ( "\27703\ESC4n\143542\&2)\1059590\&1\59982kbf\SO\RS^T \8523\EOT6\20579^!\CANdgm\1031217",
              "Y\f^\993418\SYN\1088444v\1042503"
            ),
            ( "\1107402\b\ETX\aY\1016404\DC3\1019180\US\ACK\992980\158809\145697\US{\FSP\18677",
              "\v\984699\1073483Q\b&\DC4n\83313\175917uu\1104688c\1113157!\21697\16842\31603\a!\151812\US;Xut"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType =
                "\995749\1048848\8234\1062560%\132419Y|\29511%5&\1044202A\r4>\1006378\118986.\1037041\a\RS",
              richFieldValue = "["
            },
          RichField {richFieldType = "\28961\SO$\a3\143275\&5\ETB\EM\ENQ", richFieldValue = "8q\67174L\181850\CAN"},
          RichField
            { richFieldType = "\3458\59549\131936\43944\1104609t%y-V\SUB\CAN\988529}5\69853\1039669\&0U+ \54119",
              richFieldValue = "\ETB\ACK\1112015I\1048758\&5\ns\158753_\ETB\139786i\SYN\40743"
            },
          RichField {richFieldType = "\1105836bY\1050983A\NAK", richFieldValue = "^zj,\r\NUL}\1012251\ESC$c #L"},
          RichField {richFieldType = "bhAL[v/CW\r.^KU\ACK.HnO\98377\ETBnDg\fqZ\n", richFieldValue = "|9!"},
          RichField
            { richFieldType = "\141470\"\FS6y}\983601A0b\190660[",
              richFieldValue = "\t9\134490\93823/o\121429j\7003Z\8214D~F"
            },
          RichField
            { richFieldType = "\183401\1081375@\118913\1107441\38458\b\n\SO\ESC\t,/\1087911\179850C",
              richFieldValue = "4\vBv\38077\1078263\ESCZ\rj"
            },
          RichField
            { richFieldType = "x\985536RA\RSNXCq\1102293\1024657\RS\v\174745}nf$C\2790=R\1094438\FSl\DC2\SYN'\"\NAK",
              richFieldValue = "\8630y\SOH"
            },
          RichField {richFieldType = "\ACK\t\630", richFieldValue = "Wcc_\1054323"},
          RichField
            { richFieldType = "\184881\f\54680U\1087560q\ENQ<\58381\165094",
              richFieldValue = "`VP\995756)\1062055\t;"
            },
          RichField
            { richFieldType = "\163932\RS\52818&:Xel\NUL\1069902B0\1064177iU\GS y\NUL1b)\26274z",
              richFieldValue = "I\1025262\US\1020586\&7\ETB\\\189419\US\1048857Y8E\GSI\68803\GS\tl\1031973\SUB"
            },
          RichField
            { richFieldType = "? *\STX\GS\1112062\DLET\STXNT/Nl.0\f\144462Z\SO\ACKu\1001651?\ETX\1097797z\54846\175717",
              richFieldValue = "\NAKDV>xR\881\NULH2\DEL\1065983\EM&\EM\ETBl-$\DC33aD2-"
            },
          RichField
            { richFieldType = "5\153763\DC3[k\1045029*\SOo\45761",
              richFieldValue = "\34143}\NUL%H\190775\25598\ff\983191\RSaF"
            },
          RichField
            { richFieldType = "\29867`m\1067056i6\1000447a\1012620\35257\1108194_\159481R.R\NAK?",
              richFieldValue = "\US5\1103421\CANc\SUB\bv7\172463\ACK\NAK\1076743"
            },
          RichField
            { richFieldType = "U\48801h\1055013\1060780S\DC4;;\SYNk\1109419\ETB\aS=7\998263\RS>\1110390 ",
              richFieldValue = "\1109696\1009189V\59582\FS\SUB\66800\vl%\990479XU<\166131TB\DC2\100710`\1076175v0\t"
            },
          RichField
            { richFieldType = "0\"\190724\EOT",
              richFieldValue = "r\\\DLE\64678\n\CAN}UJ\ETB\190237|':B1F\24493o!v4\nGU\1075621n<\129044"
            },
          RichField
            { richFieldType =
                "\EM<-\DC4N6,\r\74283\179078\f\99483\95412G\20190{%\1003460\US\DC38\1088944B:e#\31220]w\70434",
              richFieldValue = " \SOH\164748.\1063201n4\19211g\72721\fR\r~!"
            },
          RichField {richFieldType = " ['\1094304\1070786", richFieldValue = "\176261&Q\38869b"},
          RichField
            { richFieldType = "?e\NUL\DELq\1004968\ESC`\DC2{\157694[\f6:\SUBY\1041318,\988159&\NUL\NAK",
              richFieldValue = "`u\SOH\DELf0l_\GS4R\12327!\36169\&6\49032\&3BI\32971"
            },
          RichField {richFieldType = "", richFieldValue = "*8\DC1f7\166415E\SO&"},
          RichField
            { richFieldType =
                "\SI9ML1\165737 !\991263\RS\1028494\f{\1040644\NAK\1101048zw\ETB9I\1046115\1073127\NAK\140330",
              richFieldValue = "p\ETX"
            },
          RichField
            { richFieldType = "`>\163380\95458\1015758\1069997\1007247fB\DC2Q\DC3Y\43538>",
              richFieldValue = "jE\177777'l"
            },
          RichField
            { richFieldType = "\15976\171996I\996698l#\"T\1101174N:\rPK\CAN\ESC\1044372\SUBh#It(e\136510\1024672",
              richFieldValue = "yL\1068780L\DLEfK\1020376\DC1I~yv|"
            },
          RichField {richFieldType = "\v\FSeauk\1093956\1036139\999689w", richFieldValue = "MH "},
          RichField
            { richFieldType = "\SOJ\a'D",
              richFieldValue = "z\ETBPqjO\n\142187ma-w\60549VCy\1012968\DLE\DLE, #\SYN\36826_\111106"
            },
          RichField
            { richFieldType = "s\1090643\1038321\&5\SOH\\\182862",
              richFieldValue = "\rn\181960V7$g\69944\1080659,\1035695\US\RS[\1073423\1025592.\CANY\49214\27776\SUB"
            },
          RichField
            { richFieldType = "q)4j\1103006?Q8a\547if!zA\EM\1071476\153677",
              richFieldValue = "\aP\148706pW\131762"
            },
          RichField {richFieldType = "B-8d\SO", richFieldValue = "NzDk:@T5\DC1\CANjmg2V"},
          RichField {richFieldType = "\EM\b\11025\97556w\1089038", richFieldValue = "'\RS"}
        ]
    }

testObject_RichInfoMapAndList_user_16 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_16 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("", "Edpv\1079851y^{[\171826\13409\EMf\CAN44\1030570\1026304\1031822"),
            ("\ETXdC\186183\v\US\995837", "M\"f\170016q\993083D\RS>\SI\139633"),
            ("\t9$]\133075\&4jn\EM\RS?\RS", "pE\n\151261a+-\DC3\1015608*,yzB\DC3\12073=\ENQ"),
            ("\SOPT", "6^#i=a\SYN\EOT"),
            ( "\DC2\34543\3629\11826\55270_\146199\1078774\t\ETX4EG\141370Lm\160185\1001292\1103843\39405\EOT\65759?",
              "\24092!\SOW\DC1\FS\1108236!E6\NAKj\1027929t\178233Ko5pwx\CANT"
            ),
            ("\NAK(\1112481\1019249L^\1011131-\174853~", ":\r\r\29120\27608T\STX\GS2/s\1105953"),
            (")4n\994673ZN2", "\FS\1045159Wq\fqZ\FS\DELJA\NAK\rB?f<\"\DC30M|\EOT"),
            ("b\1079860F;", "N\1015915&;\1106564z\1031345{\1105744C\ESC#\v\GS\1019897VG"),
            ( "c\74383dVzM\SYN\n%a\135559CT;y\1102231}\148773mT\1016284S\DC2g",
              "(T\NAK\FS/BG\NULAqn\1101035.jJ^I\135547\&8DX\1072386m"
            ),
            ( "Gs\1099524\&41\ESC\1083948\995852\11657\160171fj\rd\176716\ACK\SOH8qj<?\GS\NUL\ETB8\"b?j",
              "]\fV~\128777\1093030!U'\a\SOH\SYND\STXZ"
            ),
            ("hx\15921\US\RS\1049068AO\b\1049046}\175230", "\CAN"),
            ("o", ""),
            ( "t\SOHn",
              "\DELk\"\STXCu\CAN\ETB\1061839\&0c\1026037q\\6\1060695\1057543(_\DC2'\1036323\174109,\1099644S=}"
            ),
            ("T`M\SIa\173096\135723!\DC3Q\137145\&6\RSU[u\182504\SYN\1004957\f\nh", "\"D\ETX\DLE\DC1$#\DC1"),
            ("{\SI_W}\134159\&7I!!<el\1058167\CAN\STX\1088321\&5\RS", "#\rx\4894\1044394\DC2w7f<\992999a1"),
            ("\26143", "??\SI\1018090\2768lc\EMI\DELC"),
            ("\51593\t\14262pM9\993726", "\SI\987493|z\1005606T+\\yP= \f\DC4Ie\6783\1052525\FS\SI\ETBT\131606J9"),
            ( "\98604\1056606{\"\RSd~i\1035586",
              "\GS\EM\183871\1081261n\27794\SOH$\47811\&2\FSG-\ESC\1060293)\26582Tz\1090602I8ao\137362\&3qg^#"
            ),
            ("\165541\DLE\b\\\160024\v\1032412_J\r#\SUB\NAK", "\53802\&77\997220\996615eM\1068929gm"),
            ("\191363\7957\NAK0{\1022509Z\180525Eo\DC1\1033755w", "l@y"),
            ("\1040987!\1007067z", "IS;\179093\a\1072978$"),
            ("\1082253co-\98338V.?[p\ACKw\72192=ob<g\1104325.-", "\vo\SOS\FS\t/\SYN[\"\DC4!$\EM"),
            ( "\1085750\v_$u\NAK05\t]D]x1\SOH\32757q%\47042S\1059175%\993378\DLENn\158375\998285\1039609",
              "\fN%\DLE{Bt\DEL`\SOL\1110262\a=erz\62458\&5\DC3l-\vE7\nYZ"
            ),
            ("\1100936'e\FS5", "\53963cQ\b\n\27700R2\1014810\983775pr\CAN\t[+h\v\1102998")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "w~vmF\1086840\ETB'\1047873.dr\1082295\DLE\EOT\24862-\f`\GS\b%\1640\ACK",
              richFieldValue =
                "`Rl\37684\164599\20141K\EM\SYN^\ETB\1043760\983175Vn\1024439\EOTG\1030425x\32432F\168193?ps\1033933\ESC"
            },
          RichField
            { richFieldType = "8<_\1041436\DLE+\NAK\1089233j\1107743H\1018361nl\v\NULC@\24372k]\GS%9\128744",
              richFieldValue = "g8+"
            },
          RichField
            { richFieldType = "8\993712y\ESC\SUB11l\182001\1030.(\1071348\28433Vv#",
              richFieldValue = "L[2\100432r,\ETB `9\ETB\1001122<\16482\GS\SO<u"
            },
          RichField {richFieldType = "\DC4\158865jQ\NUL\ETX~\1009725\ACK+nd_\NAK", richFieldValue = "iH\1104542\DEL!4"},
          RichField
            { richFieldType =
                "dT\6049\163548/!HV\RS\50825\EMh]\58581\&5Fc\DC1w\148060\189922+D4_\1088320~\30606\f\1099919",
              richFieldValue = "X\1027045EJBz\140143\1095475*+\1057588%k\1006878i@\SIs\1022539\DC4."
            },
          RichField {richFieldType = "\f\1080051\6093\DC3", richFieldValue = "q\995377\1113075`s\DC1DDc"},
          RichField
            { richFieldType = "\1069967\986252\37929",
              richFieldValue =
                "\DLE(\143245\148796V\95110\152376\&9\191021\1079339{\1102721\NAKu\RSs`i\ESC\71883^j\1081069\1095916"
            },
          RichField
            { richFieldType = "1>\63461\"<\DC2\70870|\987273",
              richFieldValue = "\1084288\CANuS\987579w\1086865\ACK/\132561\99700\SO\DEL2q\DC4;h\1009002\ETB;O?\1078358"
            },
          RichField
            { richFieldType = "\DC1q\985613Yz\1021125n\1042087\tn\36234P\182769\&9",
              richFieldValue = "\137890\NAK\30301\26919Vx#\STX\ACK*\1102033\SYN 06"
            },
          RichField
            { richFieldType = "\62103\50514\DC2>\"\1091637ON\USG\1012210GC\DC3@0\v\SYN\rA3:\ENQ*",
              richFieldValue = "\1106757\39230\10824\ACKSUu\1023269z\51098"
            },
          RichField
            { richFieldType = "8e\1092079\1031572\&04\992184r\tK\164968p\FS/\5783",
              richFieldValue = "Lp*J\CANS/\22624\EOT\1083845"
            },
          RichField
            { richFieldType = "\1055848,{3\137156\&1\1055068\1104006\r\"?hGYO\1045951t\167966\&4\60717{\SYN@",
              richFieldValue = "x5\1063811o"
            },
          RichField {richFieldType = "\160962\ESC\25354{\1056421", richFieldValue = "Z\52932\rq)D\DC2z\53197]@t%]\SI"},
          RichField {richFieldType = "\1100879\SUBq\a\SIBs", richFieldValue = "Q"},
          RichField
            { richFieldType = "\184578\&2,Y\1065717$h5\26854-B\EMx)\SOH*I\46496O;\\b",
              richFieldValue = "\165993\&3mGW\22642l\47820\64261\f\145314.="
            },
          RichField
            { richFieldType = "\1112019\2879=\1083112\&4v\t\141212TB",
              richFieldValue = "&\ESC~\DELs\1080928\46596*y\ETXzL@~\a9D\163584"
            },
          RichField
            { richFieldType = "q\1020779r\1069479\&8h\fk\DC3K9\127941\1004987",
              richFieldValue = "\vH\1094278\SUB\ETX$\SI+\74222\46277z\1096064`>\1070494S\SOH_qo"
            },
          RichField
            { richFieldType = "\4043&@\DC4}\1028923\\~5\142816*\66698]\ESCZ\158429",
              richFieldValue = "22g\EOTj*\145560\&9\ENQ7a:"
            },
          RichField
            { richFieldType = "\1038424<W\32245\1034887ch\DC3u\1101963O\n",
              richFieldValue = "l\148447\163877;U\DC2\"\1078627>LT#\189806\&5O\1051638\a7PC"
            }
        ]
    }

testObject_RichInfoMapAndList_user_17 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_17 =
  RichInfoMapAndList
    { richInfoMap = fromList [],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\1090953D\40727j\ENQHMgz\1027766(r",
              richFieldValue = "(7D\1053300\1059143\DLEFP\DEL%Y\176020,}\NAK\NAK"
            },
          RichField
            { richFieldType = "ybx\SOH6\NAK+o\\d%\DC4@@",
              richFieldValue = "|\SO\b&\"\47463\986920j\150535`l\1075178~\STX%zl\RSp\1062377\11320\&4\1037502\n\39880y-"
            },
          RichField
            { richFieldType = "0\DC4s\1009478\1078374\64673T\r\NULS\3720\189327\1031607S,",
              richFieldValue = "<N\63284\ACKf\DC1>\ETX\STXa"
            },
          RichField
            { richFieldType = "6\v\DC1.@p\SYN\12157\&7 m2nm\1093812\&1\1040947\996555\110974\ETB-\1099786E\1057283/",
              richFieldValue = "\1069491\ACK[S\STX\1004943fIBz\1068155\DLE[d-"
            },
          RichField
            { richFieldType = "YbO\t\ETB\72824dY\43796\v\r\1110538\1018639\f(\83178q\95503\174672\ENQ\147011\1021002p",
              richFieldValue = ")R\DEL\997474."
            },
          RichField {richFieldType = "\74615t#T", richFieldValue = "\1029105>G{9"},
          RichField
            { richFieldType = "q \63979\1032341\"\1108625c\EOT>\1094516'B\987613\97049\95210\1073699\EOT4\FS\DC2:ew",
              richFieldValue = "\b!\990134\31454\1017613Mi*'M\7385]\45188\18138\SYN%\100239\US:\RSg(]6,1\25362\95467hX"
            },
          RichField {richFieldType = "\168989lB~53k\32174\165028", richFieldValue = "&l\1058556\&0"},
          RichField
            { richFieldType = "p`\47891\92215O7^\t{D\ACKA",
              richFieldValue = "U\1084470X\FSi\213\fU|7K5\ESCm  \1024526?\1058254Z\1096290\157117D!,\n"
            },
          RichField
            { richFieldType =
                "\\\171898\ENQ\DC3\1096965l\EM2\DEL\SIj\1077069\\\1038930v3v*\US~-dM\1039922e\DLE\GS\1090187",
              richFieldValue = "\DC4N\74064`Pp\f\140943|9K\n`Io\1001516\30610\\j,\996690B4:).uY>\v"
            },
          RichField
            { richFieldType = "z\175084\1089700Y\1005940Y9qM$ b",
              richFieldValue = "\DC3V\DEL}\RS\181695%\1112683Y\ESCH.eJZ;sZ?c\187383\SOH\GS\STX=V"
            },
          RichField
            { richFieldType = "22\153981!<$R\1088477\vE\170101\1098195 _c\1052675",
              richFieldValue = "\1111943\152105/lb\184015(]\1006529\74367G)9\119002`A\1006048s\DC1O\1070544[Z"
            },
          RichField
            { richFieldType = "\1006267v\SUB\1052321",
              richFieldValue =
                "Cs\167806\1095876\SO\1077563\DC1dA\164787\tt\10692iSU=\r\1074323\SO\134296\1016705Z\1108703@\120844\&231"
            },
          RichField
            { richFieldType = "bu\1057564\US\1026897E\57436\1095896\63950x\NUL\SOrRw\a",
              richFieldValue = "\1101143F\1026278<\DLE\NAK5/\SYNIlgX\168558KE."
            },
          RichField
            { richFieldType = "\131896@\63319o\1562^M\1058227!\f],\ACK\"4",
              richFieldValue = ";yG<.D\33414k0X^\1048522\ENQ!\1065059z\DLE\EM>I$W$"
            },
          RichField
            { richFieldType = "#K\57723\1096142\DC2e\NULt(u\ACK}q\1083604c(i\1004230J\9122a$Z<",
              richFieldValue = "P|M6t0 \61626\ETB\EOT\48311p-N\RSEd\EMn\"{\1060945/\195047*u1"
            },
          RichField
            { richFieldType = ",\DC15\991051\182213\59706\RSk*\72259S\1066769\ro$,2\179381*S]\1008705^",
              richFieldValue = ")0\SYN\GSCG\DC2\1059387j\37029%\ETB\1060066"
            },
          RichField
            { richFieldType = "#\1054266\ETXg\SOH\f\ENQ>\1101152,&\1097994\168271\EMh\v.\GS\1028940",
              richFieldValue =
                "\53208@\61446\NULo0\1011692\1023006\1012583}\1004797\1060559\14562\GSw\ESC,\21816%/\ACK\SOH"
            },
          RichField
            { richFieldType = "\ENQu\FS<RZ^N\133748\984168\&7:h\155499\NAKs\169210\GS1,A\RSS\DELv\USgY0",
              richFieldValue = "y\1019886\1008410i\1084153\113824]ZkG\174682o\b\993203\986941\bo\SO:s"
            },
          RichField
            { richFieldType = "\ETXy\138368-X(\1063143\SOYea6\1075935",
              richFieldValue = "\156879q\134953\DEL\1052926#\36571\SOH\1111385j\NUL\160815\1022952Ks\167157D\133237"
            },
          RichField {richFieldType = "H", richFieldValue = "\4988\&0r\RS\SO\1090914\5740\1015231\&0"}
        ]
    }

testObject_RichInfoMapAndList_user_18 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_18 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("6\54727\11716q", "{?\1055470\ACKy~"),
            ("y\164827\25391Y2O[\145726(h9N", "m7\13956na\f\169921\1050209\n\NUL\DC3SyB\1048585"),
            ( "\35911~\ACK\DC1\SYN\1050522\1103153\ESC\NAK.C7\SOOk\1056673\64750/<\1055901@I\1009863",
              "\EMlc#c\1073113\1093398dPc6Lr|.M\"\48243y\FS\\:\1044780/\1111055m"
            ),
            ( "\137634+\8922QVJ\997961fV#!0sI\US%\DLE\1014116\b~\1036073(\SO",
              "\48642i\1105717\&8\SIq\1018723k\n\21437o4Hw\STX\FS"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType =
                "o\"\178223@}\99710C\1106153{!)\1002306\t,\1033045Q,l\1020558\1036716\1108657\\N\25862Cq\r\SOHQ",
              richFieldValue = "\5504:\RSJN\10911C9JoA)i^\ENQ]vZ=\1109172p\1097743K\EM\174447nb\NAK\1075183"
            },
          RichField {richFieldType = "^\1103871\999833`", richFieldValue = "\DC2\FSMl"},
          RichField
            { richFieldType = "{\GSuHh1\61732c",
              richFieldValue = "wwMX@\DLEt\NUL\1079935\DLE\DC4'/Qb\1070573\1036579\986281B\SUBz"
            },
          RichField
            { richFieldType = "\1030795)\1097546A+r\92611\&0fHG\49259iH\EM\1038612\rl%u'\v\1062649\t9\156142Ud\157566",
              richFieldValue = "\1107556\&0\54765\1075079\21584\1012248A\185804dF\DC2g\DC3a\NAK|\b"
            },
          RichField
            { richFieldType = "x\177159s",
              richFieldValue = "\n\bG\144915\SO\EM\ENQ5%\78710\&6Gz!Wj/i\DC1qk\DEL\1086561U\1023058"
            },
          RichField
            { richFieldType =
                "/`\145454/}\30770\118940\1012827Qr\1064113>p\1018870\1022766$\nt6\985766\1103201Y}\DLE\DLE\1100056,",
              richFieldValue = "\9641\f"
            },
          RichField {richFieldType = "G7t\CANc", richFieldValue = "~*\rm"},
          RichField
            { richFieldType = ">-E[?l\1073421\DC3h\ETXA$u{p|-W97u\ETBf\DEL\SYN",
              richFieldValue = "#\SOH\1098268/9\1031294E\1018H\DC2P\DLE"
            },
          RichField
            { richFieldType = "\n\1039925&3\25504~\b[\DC4o8\1086024\72236\22054}\1001673\1037232-\n",
              richFieldValue = "=\ESC;>\190739E\274\b\998682p\1088718\&9\92883,\SYNF\985328\1062747$"
            },
          RichField {richFieldType = "\162652\10294*2\1091056\1014630\SI\v:", richFieldValue = "B\1053466J;"},
          RichField
            { richFieldType = "s\95540(\1015062\DEL\111108\USi\1005579Io\128443q1%l/\DLE1e\ETX",
              richFieldValue = "\DC3\DC2\41300\32840M4ri4K=zA"
            },
          RichField {richFieldType = "j\DC2\NAK8a_", richFieldValue = "g<\1076586\&2FV\EOT=[!{U`"},
          RichField
            { richFieldType = "\1084565p-\DLE\DC3\148118\ACK\1082810|\NULe}yY",
              richFieldValue = "\154920L+j$\1022935}{\48899\t_=\78748\12474\51335[\CAN\n\174421i\68386Y)\994901"
            },
          RichField
            { richFieldType = "Z\1082637;}3\FS",
              richFieldValue = "\NUL\71042\1106151\993695@\1029866\SI@\141690@"
            }
        ]
    }

testObject_RichInfoMapAndList_user_19 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_19 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ("\r\"\40366T|l0$\DC3", "\5498q\DC1\ACK&\29304\12388p\157147\DLE\FSY"),
            ("P2\b\ENQ1&\1031947\a\14670\RS\1014494_q,-u\159609_u\ESC2\74221;k\CAN", "GJ\1094026"),
            ( "\134208K\1079221P.7\1063368s|\ESCUv\DC4w\92892\SOB/\RS\82989/[T\SYNYF\169867",
              "\f\NUL\160767E\US47(\n\DEL"
            )
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "l%\35922\ACK\DC4b\82954\119943\EM$\1011647\&5~f\37664H",
              richFieldValue =
                "d\34056\1074598)A\18354k@\SUB\1097784<\1054362\132080I\1003614\DC1}:\SYN\1085924\994398$\v\EM\DC2&OMW"
            },
          RichField
            { richFieldType = "\DC3\DC2\1026333vS\t\NUL\DC4\1073519<\EM\539\DLE<\NULxR\SUBU\FSx\SYN\\JtiN?&\1092138",
              richFieldValue = "\1000631\1058954x\23412\&3\n\18517\1040637\20472\r0e\DLE\1029504"
            }
        ]
    }

testObject_RichInfoMapAndList_user_20 :: RichInfoMapAndList
testObject_RichInfoMapAndList_user_20 =
  RichInfoMapAndList
    { richInfoMap =
        fromList
          [ ( "y\6786=o\134067\ETB8Cs)\148070@\DLE",
              "\DEL\SO\32445\v\95243D\1091410\DC2\120264B*\ACK2\v\FSM\1090696kR*V"
            ),
            ( "}\1000267\96390&\ESC\CAN\1020984\156934\21982z%\GSq\27039",
              "\1075992\1033547'84\RSn\1093281\991341\67725d\US\141549\163960|\1076063:w\NUL-\EOT\72417\ETX"
            ),
            ("\157707Bf\\\986805=\CANYq\68144`", "\RSo\EOTBPSc")
          ],
      richInfoAssocList =
        [ RichField
            { richFieldType = "\1096287>MS\1016913\tZ\r(#\34261=\1043593+d\1037003\172367",
              richFieldValue = "V\ETX\1092439\SO\51629r\ACKa"
            },
          RichField
            { richFieldType = "^\1038705\&3d\36922uw\ENQ\ENQM\133620E\41063[\1057882\31216\NUL\27221p\167589",
              richFieldValue = "T<\3000\"\fz\US\b"
            },
          RichField
            { richFieldType = "\31180x\153686\SOH",
              richFieldValue = "\155629\a\v\917963\&5]\9869\"\SOHu\53229\182172\vv-"
            },
          RichField
            { richFieldType = "\1017151\"}i]\SOH\NAK7\150559\DC3\STX\ENQX\vGN\DLE\STX",
              richFieldValue = "EhJ@{x\47415\r\156838O\r$HD1\162267\DEL\1031055\DC3\1063259\996904\1005457\11712}\STX"
            },
          RichField
            { richFieldType = "\63701H\992630;",
              richFieldValue = "BtVr\101034\151593\1045068\&5;FN<\DC3\171261\n\1072952'Ag"
            },
          RichField
            { richFieldType =
                "T\1039030a\1023056O'\990348\165374y+7\1109358H\a\162292D?\FSN)\SUBVG\164918\158175\SUB)\1074222\52531",
              richFieldValue = "\DC4-4\1051423y.\EMQlz\DC2s\1001688\1104524^\ETB^\n\ENQ\39083["
            },
          RichField {richFieldType = "", richFieldValue = "}\US_SE\169920^\36244i\SYN0"},
          RichField
            { richFieldType = "LE\1025429Y\138000\50613\43161\171048`",
              richFieldValue = "]\ETX\1070967X\fT\176943\USy\34949\fR"
            },
          RichField
            { richFieldType = "\STX\987935",
              richFieldValue = "\DC3\1038953\1070912\ETXa\182022\SO/\156255\177800=z\ACK,\145929\1028281%2\ACK\DLE*\SO"
            },
          RichField
            { richFieldType = "W#@WNS\1083779\121093\USv4H",
              richFieldValue = "Y%\ESC\1081275\RS\SYNf@\US{\60103\ENQ\ESCiM\1099834ii_"
            },
          RichField
            { richFieldType = "a\48924xp\999083M\FS\40103:\53958\51616l\USG\1060492",
              richFieldValue = ";cnns\98880\1007446Wx<#\1023480@\1096493\SYN\1102198K\1054189M,\1098496G\5736F\16303':s"
            },
          RichField {richFieldType = "a:XV\16574Z\1101931c:J\178991#\1008335|maO)FD:b", richFieldValue = "3Ly?\ESC"},
          RichField
            { richFieldType = "\1013988;\31159\35380Ed\SOGn\1026021!\SOH/4<f\1072163^b\DC4(\1032626m",
              richFieldValue =
                "!~\26193\ETX\a\EOT\1099541\1074811\NUL*\1038734D\1104828gC\DLEh\EOT*  \US\SOH\26506\1046735\189178"
            },
          RichField {richFieldType = ".^A\31181V\t\ETB1qjYV\1022367\b\DC2^_%}\GS", richFieldValue = "*\ENQ\f\1105576"},
          RichField
            { richFieldType = "`D'v;}{\SYN\fl\29491Ar=",
              richFieldValue = "\1014484lJ`?\ACK_,\ENQ\176320\&5\1027147t\fU\133095\SI\1088183b\DC3y-"
            },
          RichField
            { richFieldType = "}IgmU\153194zFN\1028953\DC1\CAN\SYN#",
              richFieldValue = "w$L\160394\f\FS\194733\&6\b\38397&\DELn\1018892nT}\22634ddRi"
            },
          RichField {richFieldType = "\988085`o1\49573H.\SO]HQ\1074731B", richFieldValue = "\1087977v@\1055579Z"}
        ]
    }
