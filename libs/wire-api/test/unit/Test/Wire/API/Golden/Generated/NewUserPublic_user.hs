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
module Test.Wire.API.Golden.Generated.NewUserPublic_user where

import Data.Currency (Alpha (AOA, HRK, PAB))
import Data.ISO3166_CountryCodes (CountryCode (BD, CK, IT, SL, SV, TG, UZ))
import qualified Data.LanguageCodes
  ( ISO639_1
      ( BG,
        EL,
        ET,
        GA,
        HE,
        IG,
        IO,
        KN,
        NV,
        PL,
        PT,
        RM,
        SM,
        SO,
        TG,
        YI
      ),
  )
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import Imports (Maybe (Just, Nothing), fromRight, undefined)
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
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    BindingNewTeamUser (BindingNewTeamUser, bnuCurrency, bnuTeam),
    ColourId (ColourId, fromColourId),
    Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    InvitationCode (InvitationCode, fromInvitationCode),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    ManagedBy (ManagedByWire),
    Name (Name, fromName),
    NewTeamUser (NewTeamCreator, NewTeamMember),
    NewUser
      ( NewUser,
        newUserAccentId,
        newUserAssets,
        newUserDisplayName,
        newUserEmailCode,
        newUserExpiresIn,
        newUserIdentity,
        newUserLabel,
        newUserLocale,
        newUserManagedBy,
        newUserOrigin,
        newUserPassword,
        newUserPhoneCode,
        newUserPict,
        newUserUUID
      ),
    NewUserOrigin (NewUserOriginInvitationCode, NewUserOriginTeamUser),
    NewUserPublic (..),
    Phone (Phone, fromPhone),
    Pict (Pict, fromPict),
    UserIdentity (EmailIdentity, FullIdentity, PhoneIdentity),
  )
import Wire.API.User.Activation (ActivationCode (ActivationCode, fromActivationCode))
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))

testObject_NewUserPublic_user_1 :: NewUserPublic
testObject_NewUserPublic_user_1 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name {fromName = "\\sY4]u\1033976\DLE\1027259\FS\ETX \US\ETB\1066640dw;}\1073386@\184511\r8"},
          newUserUUID = Nothing,
          newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+35453839"})),
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "" (Just AssetComplete)),
              (ImageAsset "(\1032842\ESCp\997564u]'\1069371" (Nothing)),
              (ImageAsset "\1056720f" (Just AssetPreview)),
              (ImageAsset "T" (Nothing)),
              (ImageAsset "5]m\ESCnwr\SI\66869x\ETX\1002148\&2" (Just AssetComplete)),
              (ImageAsset "\8334\1068187<R\69996" (Nothing)),
              (ImageAsset "778@F]\15306\STX" (Just AssetPreview)),
              (ImageAsset "i" (Just AssetComplete)),
              (ImageAsset "\DLE3i\1043960\1101303\1076066\1060299t\986171\NUL}N\DC1kY" (Nothing)),
              (ImageAsset "\ETBQ\NAK" (Just AssetPreview)),
              (ImageAsset "s-5\NAKBq<Fj\SI%\1069155\150477io" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 39125}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("cfTQLlhl6H6sYloQXsghILggxWoGhM2WGbxjzm0=")))
                  }
              ),
          newUserPhoneCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("wCWrnJoscPLT")))}),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  (NewTeamMember (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("ZoMX0xs=")))}))
              ),
          newUserLabel =
            Just
              ( CookieLabel
                  { cookieLabelText = ">>Mp\2407\148999\&9:\1027133\1097490\DLED1j\1043362\1057440;\1065351\998954f#]"
                  }
              ),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SO, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "dX\1020562\36210\1011406ht\99919\1002511\a>\CAN\vO95\NAK\n(\169817\1030028\23965f]_\172078\NUL2FQbNS=6g\1048060P\142526\1063467\985597\1071417M\RS7\SYN~\ETBm\1037229\ACK\SOH\vkgmBp\ETBw\24748\169199f\1023790%Q\EOT\140598kP|G\154373\ETBB-\nJWH(8)4$\989238\DEL<7\186902\FSI\bA\DLE\r?\1019914\b\b5\ACK\1009640d \SYN6\1102454G\CAN\b\t=qG\1060976 D\STXvV\tYpg\1016558\21533q\n \ETBL\1056539-\1111371\DC3\1024221F7Q\1090844]\25539i?\r\DLE\ESC{=\1107323_?e\1079481%\SOR\987580\ESC+\SOf\ETBq:g\\Rk\39309\173918[l\NAK\1087232VK\njwp\EOT3TJ\3983Ej\STXR7d83ON\ETBq\29567\EM\190684N8\n\SI\1030588u:G\42235FZ\FS<\NAK\194749\&7\1086892tH\1047800;hbS{\43951\FSsMs\994770\&9B4\1052158\&35c(~CUc\1016298\\V_XD3<L=\v\SUB\58328\SUBuH&\n(\986678\SOH\DLEtn\SOHnU\SO_\NAKt\121457\DLE|\b\133066UK\CAN[w<\1002559\1106328b+r\r%g'v\111126\25924E\1060948\996180D\147569\1065131\12003T@Fv\b%?U\\O]"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_2 :: NewUserPublic
testObject_NewUserPublic_user_2 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\64631\1077605TL\988534M9\139293+\SO\41220\987236P\DC2\190328\63929o\123139\1028696\&0\GS\NAK\1019444d\RS\186981'&\SIc\118877\NUL5\1066035sRi&_l\3988j#\152076M\DLEj`\1006072Y\1101631<\ACK\988965\&2/\r\FSbE\39893|n\ETB\983849.Q\SYN\RS,\142810S\166394\146660\&3\157852\1029461\52772yg-h,"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "\1052365m" (Just AssetPreview)),
              (ImageAsset "9\ESCu" (Just AssetPreview)),
              (ImageAsset "B^A\72852*" (Nothing)),
              (ImageAsset "a" (Just AssetComplete)),
              (ImageAsset ")6z\1112010" (Just AssetComplete)),
              (ImageAsset "\1018067\DC1l\30734f9\STX]a" (Just AssetComplete)),
              (ImageAsset "\174805\997987\1039887\&5n\ETB:\EOT\177179ey`" (Nothing)),
              (ImageAsset "\NAK\33636|\1039108\140152.\1086826\DEL\1020608\EM\SOa" (Just AssetComplete)),
              (ImageAsset "\182427\22265\DEL\165684+*" (Just AssetPreview)),
              (ImageAsset "\RS\r\EMRL?\71914\44680\141989\DC2\ETB" (Just AssetPreview)),
              (ImageAsset "\985851\ACKois\ENQ.\161958,\DC2\176462wQ8\739" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 83826}),
          newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}),
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("f8P9kJNmXRDbYKMdk-M4-uszmYmBIHZ4hLmc")))
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("diTpWbMfSg==")))})
              ),
          newUserLabel = Just (CookieLabel {cookieLabelText = "\DLEw\166029 4\n\7773\&6\DC4>0.\ESC\ESCR\NAK"}),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "2\r+7k;B\r$=\1011233^j);._bux\32488.\30806ohh\\m\10647#\1074778\ESCQ\SI\NUL(\v\EOTg\n=\184574>.`\ENQu2_\RS\t\f1%a\CAN\DC1\161054(z\140016\ENQ+e!s\65400\1073704\NAK)\SYN?V5<jB\DC2\144315Pc;\7650\1065528jf[8\1026422\1096299\ETB\78116\RS\RS\995451\1036081\FS\51291y\fr\170738&_\USg\tF\GSm\NUL')8\1102965\ETX\tf6&\155466\vT)\RS\50206M!\RS\STX?\NULP<n*\1060548\ESC\"m\ACK3\NULKe\ETB|YZ\a\177573\1101475\1000983\EOT<e\GS\ETB7W\ESCE\40637H%\rLt\ACK\67584\51858\1036986\1040649\1099868\DLE\994808\fc\SOVNE\fQ9\RS^>\1008342\991830e\GS\EOTaZ\51494#\1089765W5ej\EM\NAK'()\8231\&9=%i&K=\SYN\EOT+\CAN\DC3ti\SO\CAN(G\1085496E;\182505\a\DEL\1082111fw\21976\FS\62115\&4vM`B\ENQ!,\21513g=s\RS`<Q;\NAK_Z\ACKn`\29149b\21857\GS\155598<\186530\DELd\n<vW?\ESCuUB:a8\1067379\144948\FSb\128349y#\EMP9R\1041128k\t-n\ACK#1%A`BI`\ACK\1002756`\1044440M\1035273\DEL\132138\142732Z$\RS\1006601Xq\1087335d\1031994zcH0\SOH\143836\997738L\DC2G)\NULI/Z\188472d\aXa=:\1050752\DC27\CANs\185728\714\EOT\1110028O\1074359=4]\NAK8\166621y3V\181454W\1062563;V Z\1097082c\153435PA.\SOn>6\1033316\EOTqJ\162945\"^W\DC3\119170\137734\&7.\\\24770y5\DLE0\t\15543fXk+\1040724\&0JFKHU1_\1100297\33613\&3\NUL[mjLZ\1023825w\43177\998772O\DLE\NAK\989503\990166w87\\ms\tv\f\181222\DLE\1080871]\GSY\DC4.^\CANcXz3\1088500wFP\995741\DC4DED\1003926~s\EOT\US\r\DC1T?%]f\1059607R\996718\EOT\DC4Nt?&wk\"\1076956V\1081458R_lO\1005930\NAK|\NAKO,\DC4\992100\&0\1038541{\1038807\1032791\1009174\1100707\CAN\b\CAN\SOH\134189\SOF\US=\170022\139432zWGwm\DLE1\187165z%\ETB\132120&\DLE\1054097\146917\72335\ETB\vl\ENQ30L<U\aj\54599\DC1E\16014\&1\SI\1087983\&07\DELZ\EOT\1074655 \194797;\a\157483\1062096\1019861UN.[C\985935V\1001762\1055401\174236n\1026960\NAK|>8u.6\1027715\1061594\132515\DC4P\51576\nUr;@\EOT@\1091206\1013197q DB\1108633\&7\1113853\996078)T\ACK+\99560\RS\60904?\23432 x\NAK2\ESCo}\t*0\1095089\ETB[\b>\EMh\SOH!\CAN {H\61631'\SUB\1083923\&1Bj0L\vDf\188129\SYNT\FS\154971+/B\1053646\47730|v\DELE-\128999d`\58542JN5}*n+a@sw\35241Ll\135638*B\ETB!'\1088467R\STXRHv\57995=xl\175165)\9600\1009213=J\ru\DC4ej1\t&\5571;\STXeFIc|:\1080794T\1058505Qr\134756u"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_3 :: NewUserPublic
testObject_NewUserPublic_user_3 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "&W\SUBQ\173415A3N\SYN4Y(Z,\v\DLE\vpv\1085461h\SYN\186050^`P:\NAKr\f_\137215,\61239\1085960&\1041706\1038008\25023\&8\NAKixb\SIhE\1068139\EOT\1019908\5004t=\149472\991777\\\54373iX\v\ETB\149843\n\33152`acU'\37807\a\GS\1061631\GS\1034013\&4fGC-"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "\f\r" (Nothing)),
              (ImageAsset "Mt\166003\60761y\121246\&4&\RS" (Nothing)),
              (ImageAsset "\1011766\993921-\FS\rK59(" (Nothing)),
              (ImageAsset "j\EM\SUB\67741\159020\991596a8\2128\DC44V\121320y\1014829" (Just AssetComplete)),
              (ImageAsset "6\1066411\b\1025376\&4\DLE" (Just AssetPreview)),
              (ImageAsset "`1j'C3'}\DC2" (Just AssetPreview)),
              (ImageAsset "e" (Just AssetPreview)),
              (ImageAsset "*B" (Just AssetComplete)),
              (ImageAsset "\158946C\142196\17756!X\1100276\SOH4\SI+\1028706" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 26132}),
          newUserEmailCode = Nothing,
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("6xSzqRncYoN4AWAUeSKRw5ZKPWz8PBSQkOx-")))
                  }
              ),
          newUserOrigin = Nothing,
          newUserLabel = Nothing,
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\SI\1059134\DC22\EMx\160821[Bo_I+\184482(\ti\1057504l\b\136069\187362\161780y\1009107\SO\SYN2/^\nN\ETBj\42198 \65464+I\988051\1003392\54156E)+\150297\1095191\EOTC\34942\1108575\6552\1048532E%O\155779+e\thZ\1113140\DC1\1098631\SOHp@\50597\148234\a[\STX8F\997845 45\159401\n\SUB^*1h\120981\986477g\96547d\v=\184407UM\ESCi\191319\GS\DLE\1025144\vb0vV\ETB\1041111\&7\1111836\&8W<\144911\29930\1089605d\ETB\1047487\1023196\rh\1078252qr[\157547+\\)a7L:\SYN\NULD\58493@@Y$\USn\1067931\ENQ\SOH5\ESC3\DEL\1023791uC-t>\DC4\STX\18223\1077124\&2]\a\1022964\23554VK^Pd\166864Vp\178734zW\1068364(\1043124\&9`x\180112\984431\DC1\1009922<=LP\f\1018507e\fz=3<\153967e\99629\&5.\DELA\9663oL\DC2]\1101432\1107797\b{cDQ\68848\44367G\1039367qf\73893_f\986346D\"\DC2u\SUB\ENQ\1048953++Es\1074432\RS<\1022492?[j\ETB\\\1012059\61570\SI^}5\178432\139967k\NUL\\X\EM\1054403qG\ACK+\SO\ETX\DC2U\29438!:\t._\1010857\1744\64663S.!\SUBl_\1046671\SUB\1022334\&5M\139012\188507v^\t\b`P-\34133\182065\141343\166249\131350\165365\1065140(Zo\n\1042367\168120b\48242\CAN\SUBS\1088914O=U\1055684Y]\1001392\1021915.N\USg!\DC4L$VbD\1074986\NULp\26096\re\f\17301Sz[\14517>\SYN\DEL\ESC\SOHg\DC2\178013L.\EOTUz\1000735\r\EM\SO\127239X\1078180\&8{|\SIU\SYN$Q!9\n\US\1023153\26711\ACK'_w]\ESCQ|:\v\SUB3H\1034225M:d\NUL\1035421OkD\DC4\1004292\&4\FS\111301\989264\23660p_\94487M"
              ),
          newUserExpiresIn = Just (unsafeRange (13846)),
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_4 :: NewUserPublic
testObject_NewUserPublic_user_4 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\tP\STX\SI\41190\v\\\SYN?`U\CANt<7[i\1029767\DC2\"\133746L\ESC+Z<_\12825M\1001769\DEL\148410\&5Y\38497\1045187`/0\1079960.\ETB]\DEL,N?U_\139355\r&\1070979j\54234\ETX\32668U!3vXgT\77840\131794\DC2@?\40856\1006483\40586\DC1\NUL\135007\13986G\EMjM~\r\ETB]S\61311\n8\USkmi"
              },
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( FullIdentity
                  (Email {emailLocal = "\1041369\27245\&5h4", emailDomain = "I\13211\RS\ENQL\GS^i\FS\DC3{"})
                  (Phone {fromPhone = "+900959711"})
              ),
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "\1030466m\1068811\146692I\23591<\DC2Ww\996626I\ACKU0" (Just AssetComplete)),
              (ImageAsset "\vMBX6[\ENQ`o\179346yW)\t\DLE\ENQ" (Just AssetComplete)),
              (ImageAsset "k\DEL\ENQM.\SOH^\DC1\NUL" (Just AssetComplete)),
              (ImageAsset "z\n\SUBa\50623{j\DC3" (Nothing)),
              (ImageAsset "\986026*s)\SYN`" (Just AssetPreview)),
              (ImageAsset "\1043406\997953\992688\&3\ENQ \USo,\1045930;\1090225SF5\120128" (Just AssetComplete)),
              (ImageAsset "\133429\1099931" (Just AssetPreview)),
              (ImageAsset "/\RS\1049509\35984Q]ppr" (Just AssetPreview)),
              (ImageAsset "'u\DC3:\144012\FS\STX\1044888o.\26889\1109331v\1102603\FS{\r\ENQ" (Just AssetPreview)),
              (ImageAsset ".&Vc\1081777" (Nothing)),
              (ImageAsset "\DC1" (Just AssetPreview)),
              (ImageAsset "*\b1M\1273\&3\DC2\1039970[F\1107051" (Nothing)),
              (ImageAsset "\37581\CANz\1034862M^\1046305Wdc\1077950\&6\v~\174867c\\\58849" (Nothing)),
              (ImageAsset "h\31507&;b\GSu]\EMt\f9\175893.\9429 n%" (Just AssetComplete))
            ],
          newUserAccentId = Nothing,
          newUserEmailCode = Nothing,
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("2HSeS4jKke89LUPF1_upoD2sztr2wqUm1wUagZKM")))
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  ( NewTeamCreator
                      ( BindingNewTeamUser
                          { bnuTeam =
                              BindingNewTeam
                                ( NewTeam
                                    { _newTeamName =
                                        ( unsafeRange
                                            ("T\1042979y+f\990020Td>#N7P\DC2\tU:T#\1091663Ib\r\1113728\ESC7e\1033100\1100469\vTr\n\23502\&7>Kp\100154H#&\DC1-A+\119530MD\73865gbmyh\ESCzHtUH\176861\vO\SOH\63998\DC3b(l\SIA\ENQUW9s\62075\ETX\15386\140042\144673\1021153\&1\995714\18770\1055994\ETB\DC10\4438\US\183571f\1001539Y\133103\ACK\96811z7\EOT\1096379DY\1085983\1103521PT0\NUL\1034876\CAN\164686a\1095116\SOc\ACK%U\1043822Q0{4\1010432!\ETX\SUBhTV\1019850\ti\173971\&4NX\1000013\ACK\\3\1110549q\1034799\128209\1113878\&22^I\GS*\95824WK\a1\DEL\97854i6\DLE!\146730\NAKhHf\11622\a\994517G\SYN\11240\1017292\1074048bXw\1105825\999860\ESC]r{\1058818u\1081618\1019836\1020478G^^&\177902\DC1l\992294\1005697D?*\nz\186284^\149608\1047451\99103[S\1024941\986593\169392t&\GSxd$\ENQ\1065600\b0\42427A\1088867\1009756s\ESC\1004193\v$\191155\&4\1007667\CAN2@\ETB\US8|")
                                        ),
                                      _newTeamIcon =
                                        ( unsafeRange
                                            ("\1000306<y\1025229M\1110510+0t\GS\ETX7)\RS1j\1014264\1046080I\23944\RS\21339\DC22\1047293\NAKk\SUB?U\5808\1014039/aa8\DC3\194795ODP\ACK\163169I\1098141\1084325F\1001582y\68252\1012616)\SOH\\\1070124\&0?R\DELg;'}")
                                        ),
                                      _newTeamIconKey =
                                        Just
                                          ( unsafeRange
                                              ("\RS\48551[\NUL;\ACK6Ti\61990\&7\DC44\138576t\SYN'\145282#{\48079F\rx3me\92625\&2\SOH?\SUB\t\983887HM\1053686aX3M\t\b\5629Y\1049117a\1033385\DC1P\134112I\4930\59318,\SO\DLE\CAN\SOH\aya\1052393u\1021592\STX\46617\DC4T\1038603\DC1j\1073662#Vi\97104\94733[\ETBO\SI<\36292Z\f@\n7I\993792\&4x\1003114\1043245p$[d2Mn&\"\15737[=\1043827\1085738\DC1\n\r\NUL!\FS\181588\&0\SUB\DC3K\b:1\SO~X\1069096\1051103=;\1050757\ACK=\EOT\96073\&6\DLE\GS\1111597H>]\141382\1040189v7\1082869\986419\1027295.\1048716,A\b\DC2C-\vX\184623\&9\DLE\NUL?\rK\1102006\ESCg_m\RS\f\ESC\ETX<>d 2\170990h.,\\\15336b\1082636\SI'.j&\24040'`\ESC\EOT\DELNA\b")
                                          ),
                                      _newTeamMembers = Nothing
                                    }
                                ),
                            bnuCurrency = Just PAB
                          }
                      )
                  )
              ),
          newUserLabel = Just (CookieLabel {cookieLabelText = "\NUL\1088821vK\SYN\1039691\22731"}),
          newUserLocale = Nothing,
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\EOT\DC4\180925\59737\&2\1103547\1079266C3\SOB\SUBm0\989152?[9kX\39882\1012837\EM\134388gDmi4n\1073912VH\1013600\EMl\ACK\NUL\174805\a\190351\2335%\NAK+5A/H\43566!2\US\"V\1096814\28733\1015720\185883Yet\1020988=Q\NAK$o=Pg\70145\1049808\150871\163344#\NUL;\1026623\\\1045401R\19399\1000014-\1041680`;0\1002882\39498T\1045734\1101160\1024416\154240\138362\&0\EMEM\987020I\ETX\52368\&9O\ETX\1081334? s&\1027585\29513#6\64413S\NULj)Lp\110670\1074749Ba$\DC4H5\1080463e>[\1004769\STX\120382__\1103635O\1014083Mi+NQ\r\ACKo@\171130\23723pX\1056743`\FSo\1023422Wbu\ETXW61\190438`\SI$\1087537q\178517\&9"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_5 :: NewUserPublic
testObject_NewUserPublic_user_5 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\1021464\999886\65403\EM^\DEL<IuI\ETBx8\DELs\33172+\rA\DC4\18586LaI\"*\CAN\ETX\1096174\&6#\"\161085^\DC1\1037714\996306z2+\21848\1078762\EOT\EOTFv|\FSU.o\985842\DC1\185300r\1066228\&3~408|X\1020373P\157637\1001952I\169794\162933\SO\43457p7\SYN\SO\STX*02WO06z\129593\&7\1063216\SIp\65784Z\f4"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "\\fDs.\63569\SO;]\98838\f\DLE\128713\22058+\183687" (Just AssetPreview)),
              (ImageAsset "\178876WlMjr$~" (Just AssetPreview)),
              (ImageAsset "%\1110787H#\98403\SOH\1056932d" (Nothing)),
              (ImageAsset "\n0\ACK" (Just AssetPreview)),
              (ImageAsset "`E_\1039735gcy`La%\t" (Just AssetComplete)),
              (ImageAsset "\1093490g\1075286\FS\62373)\29993M\1044664C" (Just AssetPreview)),
              (ImageAsset " \DC2yXO\EOTf9\STX\100301\SOHD" (Just AssetPreview)),
              (ImageAsset "@OV\134418wK%&" (Nothing)),
              (ImageAsset "\13034!\1030893\1090272^\b_\r=~" (Nothing)),
              (ImageAsset "\SO*\16265\US\SOHG`\DLEO+R`Q_" (Just AssetPreview)),
              (ImageAsset "\1082436_O&g*\DEL}" (Just AssetComplete)),
              (ImageAsset "QLDr\SYN^gg" (Just AssetComplete)),
              (ImageAsset "\1053670\26781\1058315\GSZ0RnH#e\133960w\v\CAN\1004697" (Just AssetComplete)),
              (ImageAsset "|EZ:\1110818\SYNr" (Nothing)),
              (ImageAsset "/n\FSSpZ" (Just AssetComplete)),
              (ImageAsset "o\SUB/j\61057!\1028500\49905\991164\195065" (Just AssetPreview)),
              ( ImageAsset
                  "\CAN\151155\a\DC2\168207.\1045407\1020972\189659\CAN\1051029J\1096007\&7\1017051X"
                  (Just AssetPreview)
              ),
              (ImageAsset "xv\172661\DLE\1018528Wh" (Just AssetComplete)),
              (ImageAsset "g9\34734" (Just AssetPreview)),
              (ImageAsset "CAi<\64567GV\1028342}M" (Just AssetPreview))
            ],
          newUserAccentId = Nothing,
          newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("rn0=")))}),
          newUserPhoneCode = Nothing,
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("rSv20mRblPLCYnVt")))})
              ),
          newUserLabel = Just (CookieLabel {cookieLabelText = ""}),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "O#\n\9251sn\SOc\t5\1011004\"\ESC|N@T\SOH5\SI&F\38683\170143~\64416\1078384\DC1O\EOT=\SO\173834l\1035230\1083473\47236\63220\DLE\49903\n\167286\1093555n\35882s?\NAK\RSY1^\ENQ<V\170653}\1005956\1043555\1055703/I*cZkk\157470;\CANU\DC13Nz\GS\1094471e\1008447 \"\38309\&3'\74133N\1036080ZirY\49222K|\SUBi\DC29\CANP\"#\DC18E\STX\1112822qX\1111375\1068936'-k08Hn\SI%uVD3\1067087\1064695\tO\ETB\SO;\RS?m%JA_x*\175241nA\"\DLE\134524\GS>\49178H-9o\CANtwv\994746\f@6\1048888 \150621\v\ETXb\NAK\1032699A* ~D)\1074618IX,^|e\1030643\DC4\DC4\ACK#\FS\SYN\1085172\74515\1046258O\988042\FSQt%\v-\"\DC1\127814g9\DC1Ss6n~Y$m'\RS\v\984384Mm.`\20126\ETB\tYv\29158:2fg'\SOH\1048672?\akm\EOT\NUL\EOT\68356\155860\ACKie93\v\v\999509\\U\1030283&irA/\DC4tdP\144682l\1062417J!\97123\1035663\f\137940D+\1053021i\NULQ8\NUL]1{\ttD\24677{\STX\a\NUL\b\NAKY\b\NULv\1113070\1097667\6161\1080393f~\aGua(Q\FS\47125H\128031\1873\25996\ETX'Q\\39W~\131997$&\1095500Mhp<\ENQ\997394\&8{c+uj\78429LJ\GS0.\NUL\"\SUB-\16687r\1111856R\r\ESC\1085981.]\581\1040648\ACK\th[\RSO\1043947\42896o\ETB\996846iT\DC2\STXF\ETX\CAN\NUL65\33495\EOTY\1034785\STX3\1065876g\DLEkn;;\985915[rGNM1\ETB\10140y'#cw\99291\ETXw8Xp\NAKMv\ni\SUB\1053803\SO\ETB\b\1032626j8\41054\999593\46568=TYHq\991906~'\RS\1061846\NULI9\180815\baoZ\a\1022309]\CAN\1095398J,g*N\NUL|ad\35397\NAKH0IfuH?Y=^h\DC4\138709'\ETB\1054900@\ENQ\1100760\54135[_?\60427p\1098671\1047789\1100227\1024031&S\DC3\1068974)\24355\EM8Xb\f!,DL\1113158\SUBY>\1003368\DC1mg&\992221\1084878$\SI\RS;|\1075749!y|\165485\r\FS\1030520\1034728\990076l\69819f\ETX\37391#P(\\\a\DC4ce\bIeec2\FSc\ETXK~\NUL6\177518\26434V\rqV\FS\1105066\58410rz}l\2072\DC1\1040145*s\140939\1050743\GS[\1012577\SOHf\150811?\1007430\1006162\DC4*BK\69676\1047626w6C\EM=>\36649"
              ),
          newUserExpiresIn = Just (unsafeRange (183097)),
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_6 :: NewUserPublic
testObject_NewUserPublic_user_6 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "b\SUB\128804\&5{})\vqe7\n*M,!i\EM\RS\161522~\CAN\98667\NUL@\1044342t\135813Bb|/b\48258\1038771'W\149482[]\ACK\1041462W!]>l\996933\27891,\SO:\US\95876\1013876y\1020398Q+|`Q\NAK6\987189>\SYNk\1001736a6\186068oCQ\EMy\1041746\f\SUB\NAKA\SYNZ>W\1020183\&9\DC2 P\SO0\\\168443\1102296\987534>\DC2m\SI\DC3SC%C\1042356"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "\n{{\CAN\f}-(k\2413H\1106357" (Just AssetPreview)),
              (ImageAsset "m" (Just AssetComplete)),
              (ImageAsset "7\1017129\FS?R" (Just AssetPreview)),
              (ImageAsset "\vJ\t\NAK" (Just AssetPreview)),
              (ImageAsset "\n\1035551\SUB#7\6078\DC3\r\1033278\SOH\ETX{\169241W" (Nothing)),
              (ImageAsset "?\120385\DC2]SA'T&" (Nothing)),
              (ImageAsset "$P\154802\1014388\&2\STXb\n=\999389\GS/" (Just AssetPreview)),
              (ImageAsset "t\83375\NUL\144550A" (Just AssetPreview)),
              (ImageAsset "\SO\NAK4x:m-Q5\1020833Xw\188042\1111932<" (Just AssetPreview)),
              (ImageAsset "\1113525" (Nothing)),
              (ImageAsset "%\1090720N\1014266\DC1" (Just AssetComplete)),
              (ImageAsset "\DLEhB~\15934" (Just AssetPreview)),
              (ImageAsset "4\144547_\NULO)oc9V\a" (Just AssetPreview)),
              (ImageAsset "\r8\1006897F@v\1003996r\DC38" (Just AssetComplete)),
              (ImageAsset "mp\SUB\96602n\NAKQ\1068582\&7{7W\44131)8" (Just AssetComplete)),
              (ImageAsset "\23462z\b\DC2\65162\1080896" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 24037}),
          newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}),
          newUserPhoneCode = Nothing,
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("XYcMU-BCwqn3aNTQPp6q0KU09PhX")))})
              ),
          newUserLabel =
            Just
              ( CookieLabel
                  { cookieLabelText = "#l\161073r\140812\&9\DC11\992856\183603\DLEy_\SOpw\1083559\&4PowP\DC2- ;m$\ACK\NAK"
                  }
              ),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HE, lCountry = Nothing}),
          newUserPassword = Nothing,
          newUserExpiresIn = Just (unsafeRange (217311)),
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_7 :: NewUserPublic
testObject_NewUserPublic_user_7 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "IO\1022408K\1111282g\1103117J\tiZJ\RS}0'A\SI\US\78301\132874`\133778zNo\1084419nb\999005@\ETB\NAK8\4606yCz\36123?!\\\46652S\f\997058H\137813b\GSv\GS\131099\29437o\25211}\18501'\b*M_}\US\120141\1100595\35029V\63276\f*\38060K2\121310K_#77]-&\94914\140231\45105`\68448\1063903L\STX-\1041315>\99506\154804QC]+wqU"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "[z\ETB9)" (Just AssetPreview)),
              (ImageAsset "Q\98574;'\37627N" (Nothing)),
              (ImageAsset "{" (Just AssetPreview)),
              (ImageAsset "r\74772Y\1104773\DC4\ACK" (Just AssetComplete)),
              (ImageAsset "\ETBv8*C" (Just AssetPreview)),
              (ImageAsset "K" (Nothing)),
              (ImageAsset "\SO\1107481P\172934;t&g\1022589(" (Just AssetPreview)),
              (ImageAsset "0i\f\b-E\ENQR\30571\&2j" (Just AssetComplete)),
              (ImageAsset "\STXBK\66614#\v'\17884\CANz\165016\1040479$jH" (Just AssetComplete)),
              (ImageAsset "@>r\f%" (Just AssetPreview)),
              (ImageAsset "\"9\NAK\139455ci\1034310\&3\ETXARh\RSX" (Just AssetComplete)),
              (ImageAsset "." (Just AssetPreview)),
              (ImageAsset "6#\SOHHo\38610\78602" (Nothing)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "FO5A" (Just AssetComplete)),
              (ImageAsset "Qe\DC1DK\NAK,X\ny\EMP\1105194" (Nothing)),
              (ImageAsset "\1040216\\1\1027746d\30590q\DC4\74631" (Just AssetComplete)),
              (ImageAsset ")RU1?\1039067r ^$Wq" (Just AssetPreview)),
              (ImageAsset "h76F\ESC" (Just AssetComplete)),
              (ImageAsset "'\44454\t\1102768" (Just AssetComplete)),
              (ImageAsset "\FSZ^+\US\STX\70289V\DEL_Z\1075520n" (Nothing))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 30744}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("pwYYlQn1WDrF_I02s6jDhkfLyudwpqrum063eTo=")))
                  }
              ),
          newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("0fz6tQ==")))}),
          newUserOrigin = Nothing,
          newUserLabel = Just (CookieLabel {cookieLabelText = "cz\CAN*[|&\1000648\EME\GSW"}),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IG, lCountry = Nothing}),
          newUserPassword = Nothing,
          newUserExpiresIn = Just (unsafeRange (406641)),
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_8 :: NewUserPublic
testObject_NewUserPublic_user_8 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName = Name {fromName = "1Ox\1036046\DC1+z+\DEL\USqV\DLE\61685"},
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "D\1069425\&3q\GS\133342z" (Just AssetPreview)),
              (ImageAsset "W*=\1103280P\852\1036885\166416" (Nothing)),
              (ImageAsset "\26016D\1014238i\n\1066914sG\b" (Nothing)),
              (ImageAsset "\1000220\23877X\SUB#CcL\ACK\6337\STX\ETB27\1059878" (Nothing)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "j>!\57789i1" (Just AssetPreview)),
              (ImageAsset "o\DC3w" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 121476}),
          newUserEmailCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("ynbMi06R2nC6tta6KB4=")))}),
          newUserPhoneCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("rQKyQz8TBUb9Klrb")))}),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  ( NewTeamCreator
                      ( BindingNewTeamUser
                          { bnuTeam =
                              BindingNewTeam
                                ( NewTeam
                                    { _newTeamName =
                                        ( unsafeRange
                                            ("Id\n40hO\1047760\65343z\SUB9\49445;4\STX[\nA\RS\158323\1048150_.P{\DC1%\148846\1091828Cu\GS[<\49220\DC3s6mZ\ACKB\61027!\DLE\6616\1049432\US\ESC+\1016835\1038198A<jS\US'\EOT\DLE\134443)v{\1099328\1050260\DELh\" \FS\ETX\EOT\148430=5\GS\RS>\188484\66213T,`/\b\22353\179877*'\NUL0w~k6`\v\1075789\RS\1094974\24004n?\ENQ\SUB\ESC\40023p?-\f.Ba\DLEo\NAK5.!\69902\1101828\78786T%\1013532\ENQ\bC\USQ\SI\STX\95946\54926\1102263i$N\1056649\DC1\DC4\CAN\162800\1083937\40552AX\1094876\ACK\95659\74398Vi4E+'t\1037852/\66446\1044484\&6C`\995174\1022677\118954\29531\999389D3.\v&\175611`\161649*Ey$\DC3\6250\STX\GSb=Uf\DC4\147553\r\14562\&1p)\1080248cq9\142751L#\991810HF\184010`\995736\153434\1089142af\ru\ACKoM*\1038001\ETX\1050442 (\DC2:e\58692x\DLE.~9b.")
                                        ),
                                      _newTeamIcon = (unsafeRange ("\CANMS\b9U{Td\vPgKDy\ENQ#(}G\55288Dd/\DC4%;")),
                                      _newTeamIconKey =
                                        Just
                                          ( unsafeRange
                                              ("B\128570\166656\DC4\ENQO%O\16382Q\183819{\ETXm\83358$\DELA\n\ACK\".n\1043017s\175583k")
                                          ),
                                      _newTeamMembers = Nothing
                                    }
                                ),
                            bnuCurrency = Just AOA
                          }
                      )
                  )
              ),
          newUserLabel = Nothing,
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "z\133426\1002369\994519\28223\121367\164831z\154424\ETX\SOHe\183383S\188974\12413\b\FS\110593\177232\EM4(CD{\154966\69858\&8\17624/\SUB\ETX\1076336\1015315=\GS\1074509#\DC3\EOTd0L,\ETBW.*\1112933Y(\DC4~L1J\ETXm{t=VR\5090\&8y\42538\188191]Rv(Q\CAN\1077126\CAN\f5\RS(u6OMe\v\STX\ETB\bf)\6275]u\1108647\9970\NUL\DC47\DEL\165587L\94380%Gd\1042500h\26499\t\GS\\\1079242Y\DEL\EOT>RYX]\93009W/[\DLE\119253\r}\"\1085257W[$\SI=Xr&\4055\&4\1034302\ENQ\EM^\149650%4\97672\SYN\157729;:g\RSo\ACK\npm\NAKMe\1095177\151649;\1016570\160280R~\1102211\RSE\59426\&8;%\1020181j\175327\161932\SOH\1011909kJ\179392\1107234\135431L\1026438~j\ENQ\DC3g\47502tP\96358(qn4\137103I\120149nE\DLE1g\157105\147494S\\\EOT\25660H\132009\30637f/_\1053684C-;r\EM_6\1101029MQ|\135768\\gY\ETBx\176252TqT\133227_b\SYN5\EOTd\1051678](\1100131\1045501Ou\DC4d\tgvU\177293\EMk\SYNs\v2\DEL\fla\1085314\v\SUB{US%\1031525\1019691n\CAN\29123<\")\SOH\48117k\ACKH\191273\&7\rl\SUB\1051417-\178778\SUB\1088413\1036169\&2{T\fO\189603Qd|\139447>\985960\128348\STX)\135818\1075896\ETX\1050281R\v\CAN\160825\150329u\a\aU\FSOj` \b\FS\SOH\"\DLE:Kw\CAN\143773\51082\29840\&2F\31042\ENQg\173420\1098085u\984046\DEL\21798\1006942xk\ACK\rAE+$\SI\993555\98363\1022476ja\FShL\1072555\\T\1002810WD((bZ\1106549V}\EOT\GS_?\1093553\"5A\1021170\ETXn\1113671hv<;rO*\SUBchIS7[\SYN|\ESC)\ACKA_\\?vC!u\DC1\37667\SUB\DC4\52936\1111370K8\DC2\SUB<@\1070433\1066570\1032844vZ6\EM\CAN\1041502*3\\\135699\ETX\v2\1032693\1019648\SUB\GS}\v\92667\DC3P%\4834*\DC4\fQ\EOT\12688\1093473\t\1018390{\SOH\179090\NUL/{\SOoJ3\SOHH5-}\152409\164636\ton\r\154007\170226OV\GS)w\144386t\a@d\SYN\a\39265\1067603l\22205\&2`\n\1001492Q\29175\986799V\NAK\CAN\159234\1006700XB\1001481I\1096498\EM\1087002\176382WwX\1102662\v~=m\58276\&0EYY\SOHx\\'\74428dI\1066936m=\DC1*\147855e \GS\SI\ESC\987964j\SYN\fAI-eDY\999673\158811]Y.\1070824O\65157\ETX;\1051502\EOT\1044396^\44861\ESC+:Z\35333\ESCZp 7=L\171577Ptx\1087446\"\39630\&2\DELKL\vM\994083C7^\9410\194944Or*e\1042773dX,^f[\f\ETX2X\US\aFov`\SUB\rj8P\ETB`\10895\&8\CAN\NUL\3988.\EM\1091035\98402+\1038711L\EOTA\158835`t?\\j!\120650e6\NULf\ESC.`M\SOH\53758N\EM\NUL\ENQ2\995251\19263U\27667\190722)W\1063529\1071276\&2\DLEI\NAKzT\1103689@3#\47054\190193\61874]NL?N\1078922&\99287(?\143739\&1\ESC\1082874\1099455\DC2F>Fu\EMl\65504\&5/\a\99981\DC32Z"
              ),
          newUserExpiresIn = Just (unsafeRange (419615)),
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_9 :: NewUserPublic
testObject_NewUserPublic_user_9 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\SOH\148366~*'\1091611\DC2\ETBA\GS#0\NUL\f8P@\24522\99031\1013785\NULLn\SYN![\ENQ4nU\f\120949\FS\DEL\1106736>%\NUL:\141119\DC3a\1043611\SI\CAN\132537\ESCi\tI\ETXg~1"
              },
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( FullIdentity
                  ( Email
                      { emailLocal = "\1073976\SUB\b(\32406\SO\1068114\&8n\1009973F\165859\992969\94321",
                        emailDomain = ""
                      }
                  )
                  (Phone {fromPhone = "+14993214"})
              ),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "\172838(@Z\1055919\96792(" (Just AssetComplete)),
              (ImageAsset "%\1068070\DC4/" (Just AssetPreview)),
              (ImageAsset "\1071816&5b\62518o\47074\&2$}\151268" (Nothing)),
              (ImageAsset "\SYNvQkm|\150806&t-W\NAKx" (Just AssetComplete)),
              (ImageAsset "|5\1108400\CAN|te[E\37588#S\1063624" (Just AssetPreview)),
              (ImageAsset "{81\1091379.\FS\ESC" (Just AssetPreview)),
              (ImageAsset "@y\DLEk\36106'jS*k!\181796SQD" (Just AssetComplete)),
              (ImageAsset "z\53959z\r&'" (Just AssetPreview)),
              (ImageAsset "" (Nothing)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "\1051708b \1108336gGz\ACK\154017O\\" (Just AssetComplete)),
              (ImageAsset "-9\ETX'\bs\CAN\145159\NAK\US\a\150741>;\STX" (Just AssetComplete)),
              (ImageAsset "\b\22000\49326\4855P\fX" (Just AssetPreview)),
              (ImageAsset "\98531Lw~8" (Just AssetPreview)),
              (ImageAsset "\DC3Pd" (Just AssetComplete)),
              (ImageAsset "Q\151981\469`" (Just AssetPreview)),
              (ImageAsset "a\SIS\55073\1477o\97394" (Just AssetPreview)),
              (ImageAsset "R\190252\133627\v\175378\178064\DELm\ENQ\985251" (Just AssetComplete)),
              (ImageAsset "1\1017047" (Just AssetPreview)),
              (ImageAsset "E%\n\DC2\169591\f\NUL\1087389yP" (Just AssetComplete)),
              (ImageAsset "Xz\1094953?yV\r\DC3-\35365\11506\986350E\153851" (Just AssetComplete)),
              (ImageAsset "\1050374\995845V}o\DLE\vC)k\1056787%\v\ENQv" (Nothing)),
              (ImageAsset "5r\1103827-c\1079457b\17080v9\55084\1031252z" (Just AssetComplete)),
              (ImageAsset "-\1020911E\DC2W\ENQ}\98222\DC1" (Just AssetPreview)),
              ( ImageAsset
                  "\159309\1006536\1062142$U\999283\ESC]\1113812\67809\185704$\187918\1032854\1069596"
                  (Just AssetPreview)
              ),
              (ImageAsset "\1085476OMt\1092661\1070865\1056513x\DC1\SI:i" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 6550}),
          newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("X4k8TiOW")))}),
          newUserPhoneCode = Nothing,
          newUserOrigin = Nothing,
          newUserLabel =
            Just (CookieLabel {cookieLabelText = "J\1062049\1068280d\1071118\1012786X\150320\&9C?aA:\1011062)\1053955"}),
          newUserLocale = Nothing,
          newUserPassword =
            Just
              ( PlainTextPassword
                  "8\SO\1057360\172918c\1087208\a3_i\STX\177181\1001599sf\ACK*\142118~\1058834cZ:\32832\n\1098399\1060247\1036580I!\7724}zw%\RS\74351$\158628\94271i\1082266V\100721'&/\ETB\1108615dU{:X\38866N\DEL\vda<g\DC2\NAK\184089\"\179706I\1051760L!\6584K\38596sf\7486Q\ACK\147429&&\FS\NUL\986358JdO\NAKmlk$\14306\9312\1004897>VjT\1029507M|T\SYN,\97329I[!5%\EM\EOTWDB=?Oe\ENQ\DC10p\EM\DC18D\985673p6]\47768-GOplF\1113534tc\5161\DC1U\1042134\STX=dVF\RS\v\v\95369\DC1A\132688\DC2\1055974\1065964}o5\1078643j]\RSF\157176\&9\ESCl\10918\&2\a\141399p\1027207\139422\183628LtJ7~])\1023137\NAKO2\133558PJ\1054255J\vsb%\131125NB1\ACK\DC4e\DC2'\DLE\RS\DELftG^k\ESC<r\1027987pN\10864\1072999\&0w\ACKL1bW\1033758\ACKdZ\190638>\129513Y\EMr\168888\NAK\1082421B\1058057#6\NAK\t\1045489_\134740\1016847m8\47101\&9HIp\9081\SOHsh\1052654X\US+C\"\DC2\983721F"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_10 :: NewUserPublic
testObject_NewUserPublic_user_10 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName = Name {fromName = "\1086629\51209W\nX\\"},
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( EmailIdentity
                  ( Email
                      { emailLocal =
                          "zl\1112297\1100954\ACK<Hn\DELC\143280\31147^k!\39652Mw\19409\169841\42669<\1100001\r\v\1113684\160942\ESCV",
                        emailDomain = "m\53477C\SIaR\128125DV\127268\ACKF"
                      }
                  )
              ),
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "\DC2\100319\SYN\DC3P" (Just AssetPreview)),
              (ImageAsset "\DC1\999909\994250" (Just AssetPreview)),
              (ImageAsset "z" (Just AssetComplete)),
              (ImageAsset "5\43169 g\fM!7" (Just AssetPreview)),
              (ImageAsset "]\1081165\ETX<vo\987767-%\191190\&7~B\f" (Just AssetPreview)),
              (ImageAsset "\99708\68844\&0\SOs\SO\992007\1104801E\SUBc" (Just AssetPreview)),
              (ImageAsset "J\1024513\&9)" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -4837}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("4PYpdjJAvFqyXmM0Yr885x5iSuZUzQ==")))
                  }
              ),
          newUserPhoneCode = Nothing,
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("lGTLiCWOo127k6MaZVi9VpiQxd8=")))})
              ),
          newUserLabel = Nothing,
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = SL})}),
          newUserPassword = Nothing,
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_11 :: NewUserPublic
testObject_NewUserPublic_user_11 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name {fromName = "hG\DC3\30881e-\1047703f5\f\1010519\CANd\1025676\176955bR\f\997346\DEL\1090429\1108431pJ"},
          newUserUUID = Nothing,
          newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+43575413322561"})),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "~\ENQ\1107099ld=\NULJc\DC1\tw\1108536\SOh\RS\SYN" (Just AssetPreview)),
              (ImageAsset "E2p\ETBh\1098864" (Just AssetComplete)),
              (ImageAsset "\156182,\178781\1016168\1935\NUL|@Q" (Just AssetComplete)),
              (ImageAsset "\SUB-/\1071122\187630\1049744'*\DC31\984000\t\1109175\1059185\SOH8\NUL" (Just AssetPreview)),
              (ImageAsset "" (Nothing)),
              (ImageAsset "pEf" (Just AssetPreview)),
              (ImageAsset "`\1044417\162029PX\CAN\994632\RS{" (Just AssetPreview)),
              (ImageAsset "4A\vj\33048oc" (Just AssetPreview)),
              (ImageAsset "\a\DC2B\1073401J\996173V2?G" (Just AssetComplete)),
              (ImageAsset "\191036W>" (Nothing)),
              (ImageAsset "\4869\1036689+\6941FMEp\1049868H\t\ENQ" (Just AssetPreview)),
              (ImageAsset "\SOH;\DC3\DC3\62614\ACK\EOTb5^s:\FS\ENQ\985680y\DC1\ETB" (Just AssetComplete)),
              (ImageAsset "v\1063838" (Just AssetPreview)),
              (ImageAsset "\1070138" (Nothing)),
              (ImageAsset "^ \100451\34269\RS@" (Just AssetPreview)),
              (ImageAsset "8\64945\171234\DLEE\74509\1028111\\Z\ESC5<\DELc\160082\&2" (Just AssetPreview)),
              (ImageAsset "\DLE\1038907\157993Gc" (Just AssetComplete)),
              (ImageAsset "\151793\&0@\\r|J\1060002\DC3\ETX\ETBd*\ETX\1038354]" (Nothing)),
              (ImageAsset "\1113676Q\USh%1\f\121373\28332:`" (Just AssetPreview)),
              (ImageAsset "6z5\1021987\GSe:}\99663" (Nothing)),
              (ImageAsset "\28727\\Q<\SOHX44\39834d\1053626" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 174052}),
          newUserEmailCode =
            Just
              (ActivationCode {fromActivationCode = (fromRight undefined (validate ("Z-Zl_Qv7tQ2uarU-IzTDMXzogSXj")))}),
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("XnBOrq6YI4Q_OfxeADfxgHnlqFMQaauOlT003dC1JCCI")))
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  ( InvitationCode
                      { fromInvitationCode = (fromRight undefined (validate ("w1bhwQG7MiCYb7O4aguK7iSgND5JBw==")))
                      }
                  )
              ),
          newUserLabel = Nothing,
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = BD})}),
          newUserPassword = Nothing,
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_12 :: NewUserPublic
testObject_NewUserPublic_user_12 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "S\134455[\RSVD, \DLEBH\n,\SUB\t\1065607M\26879\&1\SOH\RS[\1108255,\153608\"v\1043886MP\fObI\RS\DC4\50462\1040103\FS\t7\57518i\133613\b\STXT#\49073O\1013355\SO\GS!C\DEL\SUB\EOT{1\SYN  +\DC3\EOT\1100935\145256(\150928\1012160\DC4\184145lh\SI\138361\DEL"
              },
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( FullIdentity
                  (Email {emailLocal = "g\FSn))s/\161354?\GSQXb2 ", emailDomain = "U\166992=B3Cod*\1004740\7137\13268"})
                  (Phone {fromPhone = "+195504776"})
              ),
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "\1036631\n\1110060N" (Just AssetPreview)),
              (ImageAsset "ENiRZ L\1075377. \166145\NULS\SUB =" (Just AssetPreview)),
              (ImageAsset "h\119878\DC3" (Just AssetPreview)),
              (ImageAsset "\141635x\1041874p\SO\52276,\NAK](\SI" (Just AssetPreview)),
              (ImageAsset "\ENQ.P\64193_\51732\1073872D" (Just AssetPreview)),
              (ImageAsset "\168952\1063660b" (Just AssetPreview)),
              (ImageAsset "\ESCo\DC46" (Just AssetComplete)),
              (ImageAsset "t,\176774\STX\"\b\STXmE\SO" (Just AssetComplete)),
              (ImageAsset "t\83219\\\DC4\CAN;" (Just AssetComplete)),
              (ImageAsset "p)LK" (Just AssetPreview)),
              (ImageAsset "\1082561\735qx\SI8mQ\STX\DC4\FS\190305\358" (Just AssetPreview)),
              (ImageAsset "e" (Just AssetPreview)),
              (ImageAsset "\997995zY|" (Nothing)),
              (ImageAsset "Y\ACK" (Just AssetPreview)),
              (ImageAsset "\1002331\SI\STX\ETXMux\163688\53496\44761j\n}7\RS\DC1" (Just AssetComplete)),
              (ImageAsset ":" (Just AssetComplete)),
              (ImageAsset "j\1074909\DC1n\997815`\DC2n\a\72823\190358\EOT\1102765\b\182431" (Just AssetComplete)),
              (ImageAsset "Mx\CAN/\ESC\"G_\NAK\ESC" (Just AssetComplete)),
              (ImageAsset "[[\1053950<\1034436+I" (Nothing)),
              (ImageAsset ",\992742F@\1015314" (Nothing)),
              (ImageAsset "]\1023794X(q\ETBO\158969" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -48922}),
          newUserEmailCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("UVZ6MaxnslUCv-4=")))}),
          newUserPhoneCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("fsYfmzlCyBE3ZK8opHg=")))}),
          newUserOrigin = Nothing,
          newUserLabel =
            Just (CookieLabel {cookieLabelText = "\DC1\bg\1014044-\b\CAN\45826LP|l\8514\1059525/\14935\ESC\ENQ"}),
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.ET, lCountry = Just (Country {fromCountry = TG})}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "c\146091~b@qM\1071283\1047727\1079832BL$b\n.^{p\nq\r\SUB,\154P~\995481`\SYN?\CAN\1087882\&7I\45464\60369\DC2\1055198\ETXf\SUB\145276\fk%\US\1070329\135511;0&k\SI\NAKHY5_u\137015"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_13 :: NewUserPublic
testObject_NewUserPublic_user_13 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName = Name {fromName = "\EOTui"},
          newUserUUID = Nothing,
          newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+40846434926274"})),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "6\1100678|\1004331\EOT5z\1080906\1030948" (Nothing)),
              (ImageAsset "\NAK\1097014\1095069y\STX\1037487c\b-" (Just AssetComplete)),
              (ImageAsset "\1059934T" (Nothing)),
              (ImageAsset "[\1031581\EMWU3d0/K" (Just AssetPreview)),
              (ImageAsset "A\184589v\US2R\1058582Ff\178786&\1107011&" (Nothing)),
              (ImageAsset "\1093099\1106189#\ACK" (Just AssetComplete)),
              (ImageAsset "K" (Nothing)),
              (ImageAsset "6$(TkM" (Just AssetComplete)),
              (ImageAsset "+\13063\150064\DC2\146580" (Nothing)),
              (ImageAsset "\b\ENQ\1084766La65=\995400\176949R8" (Just AssetPreview)),
              (ImageAsset "\1014296\vj\172641k" (Just AssetPreview)),
              (ImageAsset "\DC4\ESC|\1049166" (Just AssetComplete)),
              (ImageAsset "D\SOH\CAN\7753l:" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -18284}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("jtAT26_-fRZ3qS2Gx5Cfvrk0mgi5gnPx")))
                  }
              ),
          newUserPhoneCode = Nothing,
          newUserOrigin =
            Just
              ( NewUserOriginInvitationCode
                  ( InvitationCode
                      { fromInvitationCode = (fromRight undefined (validate ("oYu2EkO4OdjPAeBx-tOeGAHapJ62b5ZcuA==")))
                      }
                  )
              ),
          newUserLabel =
            Just
              ( CookieLabel
                  { cookieLabelText = "d\1067047\170156\SI\1023169\172056IZ\33984sS\"wjcI\1066600\147543\141048`X"
                  }
              ),
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.BG, lCountry = Just (Country {fromCountry = UZ})}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\986251\&2\SO\27281vXm1\165680>\GSY\131859g\1018593pjer\1088948\190977?\50896'JKn,Jat\FS\aq\1040570\120209\DC1\20612\1036571e#\1004169{.\1039791d\1110055\RS\1097618\1071825!\1001364\1010352\a\23078\a~Nv!\al\1011942z{\SOeF\t*\ACK\ryt\1087249i\97650\DLE%`tZ\ETB HC'\1073J\STX\RS/\71067e\b\990360\983141 C(Ef\ACK\1073923`{/%\CANox3\132841\1039962\62311\1092675\158024\13496\SYN?\STX\DEL=\1101745J\62762\156782o>\DC1:\132415\NAK\1007357!F{\n\ETXQ\DC3\5010\STXYzrH\nG9ZS\182757\US\v\CANO\1052954\1017050< Mu\1088501\146953yi3\SUB3\DC2#M\68874\SO\b,.E\29924\1079063@2\1022636\"Kd\SOHt\1009904m\DLE\SI\61926\SUB0\bs]C\r\SYN\USoXD:\DEL\DC1%\v[\1091642=U]\1035026\&8\STXx7\150861\USJ\1091605n?\54014\140824K \67609]\1051217@\21867`H\rL%\151943Z<J\169666\f\991679-7\179451\ESCX\1110937N\RS\DLEw\bZ~\8416VFrJ6O\16043\121239n\159290\DLElO\v\SUB~B60dS\984801\1028264\1013673|\ACK\1052200\1093485\1111866fAW \SI\37209R\51593w\CAN@\39441\DELlE\SYNn\1042798zy\US7\135160Y\150672j\SIAO6Fv\18736\26510\ACKq\988787\182632iW\1068170\1019533\NULpA3\SOH\1107928\&9H7\1079732\f\1023523\1040487S/\1077719o_g'8Q,?')\b\190852\ETB5y\1099194\40638`Q_w$f\185650\DLE+qz=\992564\"+\\\48776\ETB$qi\SOHk\25599@v\GSI'l\42137|\SOH'0\16761\DELa\160601Sp1\8829^\1005368y\188243\SO5<\NAKP\RSY\a\49002\ETXo\1091201io\94776ww.9Y\b\1054214J\45514Sf\NUL\FS:;)bh\997958\99570\119266$\rn\ESC\SIw\v@a0+\985294\DLE\1072619\132847\1110898\DLEK%Vtn\1022095\986532\DC4\986242\43216\b\1066684\b\ESC\1073799\&5,Qj\1015384\f\163215\b\SIWm|\1051111C\1000663#\GSz\138193\96412\66435\1010773%\f\f\SIXqVza\RS\DEL\1080729pc\2856r\SUB]w\DC2)\45006iW=Sz\995981~j$\1044070\&1\1017325\&2\SO\SI5\n\1071136(\RSQ\74237\83492\DLEc<x\181921^\SUBF\170135\ETB\SYN'\65042\165056~76A\NUL;L\1107883\EOT\NUL.z\1058579\1046133G\1049307v\18517i\SYN>\r\SYN\1086999oqtFEB\172764[\NAK\a\62397\vv$H\62510&J\ETX\128543\1059840\1023515\&8D\38949X\1073161\SUB\1100179\1101702\94007)uV\10059\19106\NUL\DC19o\DC2(\17757\ESC\ESC\SOH*YO>fWl\SOt\3546\47321s*\"\DLE\1025032\1028388\SOHvgKYmv,\170813v\161542\n\GSO\1061804}\1068179Dfj\ETB\SYN\DC4\917908\RS~\172650[\168875\1071520\37632s\69897\1011855@W\22914\163452w.\NAK5<\146922n\1025530E4is\141686o\138314V'\3302\fy\SYNa\STX\ENQ\SOH\SI@\SYN\39705\33822AO>S\v\1040186#`\125194.\ACK\120917F\1100243\1021420#\1016488R\n\147763\DLExL\158099\\\1067176(\1074249G2\162690 $\1090609\1081923\156383g\r'#<Zsc"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_14 :: NewUserPublic
testObject_NewUserPublic_user_14 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName = Name {fromName = "H'[\DLEEp+G\SO\191198M\"\1000594"},
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( EmailIdentity
                  ( Email
                      { emailLocal = "_\138673\&0yK~}|8\50942\&5\r\1103473A\v>\1067920?f*n\97481'F\50324kg ",
                        emailDomain = "\DC2Zi\6474\147535V\23438\SI\a\CAN\1111394\&6E\SI\v7,\1035595f2N"
                      }
                  )
              ),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "\1023367~\\\b]\1021511\&6'$\1030591" (Nothing)),
              (ImageAsset "k>;m\t\SYN\1054065\1087696\180495\EOTU(\v\EOT" (Nothing)),
              (ImageAsset "4Hi\179576b7" (Just AssetComplete)),
              (ImageAsset "\ETB\1096561@\25608\&9\RS%\8554*\EM\183402]\1074635\DC4{" (Just AssetPreview)),
              (ImageAsset "f=X\USF|xOI\DLE\161091\100945U%a" (Just AssetComplete)),
              (ImageAsset "\FS\CAN\100855" (Nothing)),
              (ImageAsset "\25703\&4I\ESC\SYN." (Just AssetComplete)),
              (ImageAsset "\EOT\189229M\132588" (Just AssetPreview)),
              (ImageAsset "K`jC4x]" (Nothing))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 18920}),
          newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("879HiMc=")))}),
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("2IG53xZ-sTN_Om5U9MMGmxMXgD4I8m0OOhHe2VI=")))
                  }
              ),
          newUserOrigin = Nothing,
          newUserLabel = Just (CookieLabel {cookieLabelText = "\993012N"}),
          newUserLocale = Nothing,
          newUserPassword = Nothing,
          newUserExpiresIn = Nothing,
          newUserManagedBy = Nothing
        }
    )

testObject_NewUserPublic_user_15 :: NewUserPublic
testObject_NewUserPublic_user_15 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\\x\ESC\146311/\1017598\1099133E\58537Go\23597\1109725\135889\1103281Q.@\rp-~\1051115q\DC4+\SOX<Q[@t\vt\999934lN\1017530K\92974\51187\1026655\25154?,/fG\994307\&1v\n\DC1$$2\NAKh<i\92162A\ETX\166650\a\CAN\1056373\36351\&4_F\33637Qo!\1024305b\1043209i*Q{]\16366"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "|/\143960\21674" (Just AssetComplete)),
              (ImageAsset "\EOT" (Just AssetComplete)),
              (ImageAsset "\nwa^|?\154938\DC3v" (Just AssetComplete)),
              (ImageAsset "}Wh" (Just AssetPreview)),
              (ImageAsset "^e]\1102431" (Nothing)),
              (ImageAsset "g" (Just AssetPreview)),
              (ImageAsset "q" (Nothing)),
              (ImageAsset "9b\SYN? /\SYN\DLE" (Just AssetPreview)),
              (ImageAsset "\1106155.\172488r\62167" (Just AssetPreview)),
              (ImageAsset "9L\18463Azm\171528;\16364" (Just AssetPreview)),
              (ImageAsset "<Tq2(\1068943S\SYNw\1076289\998590" (Just AssetComplete)),
              (ImageAsset "\DC4\1080548k7\EOT\DC4\3870D\ACK\1050720:\CAN\CAN@" (Just AssetComplete)),
              (ImageAsset "" (Nothing)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "" (Just AssetPreview)),
              (ImageAsset "a}[c\EOTF(2\DC4\SO,`" (Just AssetPreview)),
              (ImageAsset "&\147186$%\1087890{\1051227" (Nothing)),
              (ImageAsset "\ESC" (Just AssetPreview)),
              (ImageAsset "\DEL~W\10023\STX#" (Nothing)),
              (ImageAsset "VvXFZ\35449" (Just AssetPreview)),
              (ImageAsset ",\25560\SO8\181014\140713\&4\ETB" (Just AssetPreview)),
              (ImageAsset "4J\ENQ5/=)DEPo\151914\CAN\DC4\NAK_" (Just AssetComplete)),
              (ImageAsset "O\SYN*\58946" (Just AssetPreview)),
              (ImageAsset "\USLQO\ENQ\163360\1055746" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 55236}),
          newUserEmailCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("BJRDcvUPrlO-pgxRa76Vnl4de3eygJ9240aMhw==")))
                  }
              ),
          newUserPhoneCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("lrdPhnTqFFN2mVM=")))}),
          newUserOrigin =
            Just
              (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("")))})),
          newUserLabel = Just (CookieLabel {cookieLabelText = ")/\ESCb\FS\RSULi&PV#2dC|\97163lU#+y'_\1029068"}),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NV, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "E4LVn8xD>\1097928!\1057890sQnJ\32605h08\189561-\1093099Sf\66808o\STX\73664\&3*\1028384e\US\ENQ\ESC[\62491|`\175742\RS\1075261P\1073401_$\153728\&9RY\SYN\1065974\&3#\SYNf\SUBhN_!\149949\SO(@|>C\FS\13094_U\DEL\\-\93766.\v\136176|\1062790=&\987151\ny\DC2\996054\ENQM!\145028\59066\DC4@qj\1005356x\187979\42382\DC3V\31065\v=\996032\EOTAfq\1101761\ENQ0k\SO\63917\SON\SO~*\a\194699\a\DC4\ETX\33943U#\NUL{kb`\SOu\US\ETX/\1024207\&6j\1051088j\EM\1045525|`\1044895\SO5>\149285\US\9345f>}b\1003419C\57559V\bqx\USC\1044571\DC1\EOTk3;qC\1042077\1073193`\172168\1036914Z_:\1097171\1053576\DC1v\153836\1064246t\DC3\27385w \DLE\998172am0\143115\1033373\\\NUL]\bNwqW\DC1.A\1093225rn\NAKL2\DEL\\@v\1033703\1106347\59010\ENQ\139396JsifO`\1086981ZDD|Il\1093491Nd\1035564\r\1057167\NUL\v\1073137\1024124v\1067531MPW>&D\SOH\1084132\55173h\NAK\1110069\bm\SI^)J>@I7\NUL\985909\NAK\DC2]Jr\1026466Qi\b\988866\DC4-\995688\ESC\1099517Yb%vY\DC4 KP\1020023\GS\1010749\1050640d\EM(0\1093281nz]*$\1003221\1037148^D\ETB9\1075886X\1066939\144075\"]\992225#\44010w\163701Y\EM\137264{0\US\19631\187100QqW\22734\35012\994484\133391\DELO\DEL<`jAam\62514s7xC\185942p\NUL\1101176\DC3\1054508fd\"\EOTq2?\24562\151961\&7DxTn\DLE!V\\\20732A\ENQ;\NUL\59451?\ESC\168679\EOT)A\SUB\ETX\1033346A\DLEO\GSw\34730~r\24784*\1020604\&3Y\RS\f\SIl]p\1013564Nb\987654a:cm\STXY\\`\52591XV;WWFG\DC4\140314i7\US4(\68371\EMZ]A\EMp\1087038o-e\NAKSn\9066\189587\STX\1029453\1011266p\143931\b\SOT\1073550Hi\986720\172296\174109\1079222e\9241{\9020G*c\1104508'\142543\FS*9TgW\r\46796,vV=+{\167479\1040808|\1104133E(\US=U~\SId#9lkWi*x\NAK}E?\26235q\15604\94587\&4em\120634\STXJ\f\ACK\b\vByD\SOH\190039\f\ESC\39189\t*\987285\"\GS\FSA{XvC\60179)\72162A\34507\"\1000726\n\987159j\994849\137034\CAN\FS\1088857\169426\ACK_\1003809c\47830\1076021tccC\15287\1058445`\f9P\64198\20126\1112932i\1057633>\1020881\EMG\USa\DC4J\1085621G|\STX\ETX3\SOEZ.\CAN\fy_A\DC3F+sX{-\ndy\1081908\1091905!8a1a\1056587Rv\1000833\ae\100606\1069223=\995223\STX\t0V\f\176342\&8I\1062172_\CANl6Ra+sX\n4\1035091R\1103434\92668]%\"r\14581s\58967<\1036345\168205\1037284\&0\983494=K\45340ZQ\987198\1026655lx`\ACK|7\f\t\73904\DC1\54430\n\SIO\1011117 \SUB?<\1012954\37421Di\1076760.U~\f\188507,>w.\40458\172210!\61330\&3\1113346\EM[l-tI\"y\22776\991720bS><\1024687\151109n\120909&\1099687X+\nvhY^\ny\SO0\147657\STX\ENQV\DC1~%\SOHK>\172592(;G/;\27750M\1083242\CAN$t\1056444~\1066983\99644JZ\985822e\100269\NAK\1049555-{}s\989781v@\EMTf\EOT+\46000\a\1024177pS\DC2\FSJ?\134705\9522y\50495~\SOH\ETB\92695(,\994936=W\159338J\ACK%*9;q\1024350\170359\USp\a9@; \SOHN\v!\18059(on\\!0P<&X\1019613QmuA\1109599\ESCC%\ENQ\154173\DC3\DC4vRnch\1089444'\1023868\4506\SI\SO\995426wB\983743h?q\DEL\DELt+\ENQ\\<ui\v2Z\28714Jtr_\STX\RS]L/5-\34916=\1004362\27802\v\1020456\tX^\7049\DLEd\136259\DC1}\DC1\1033462w\1098223?\32622/%\92461\f-):93\US\"\136344\2083\136603\SUB\SOH\\<\121512\SUB\GS>7\39222\1041355\ETX\\;\1075855"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_16 :: NewUserPublic
testObject_NewUserPublic_user_16 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "36\20709\EM\135657\1009046\DC2\1109280L\\\162440\31843\\:\65347\145957\998043\160930-\54004N\16141\146346[oe\161450%4\ETB]\189710"
              },
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( FullIdentity
                  ( Email
                      { emailLocal = "x\a\119915\136542\1106264",
                        emailDomain = "C\1053176\FS;\1113985\t?\1064188;\GS`\NUL\95264\a"
                      }
                  )
                  (Phone {fromPhone = "+534443880"})
              ),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets = [],
          newUserAccentId = Just (ColourId {fromColourId = -26284}),
          newUserEmailCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("XYsxg7ZJ_P0cEi1JGv7oJTI=")))}),
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("zCo9DoCXHJwpHDl4N7m-hyRBkTE__sgzyg==")))
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  ( NewTeamCreator
                      ( BindingNewTeamUser
                          { bnuTeam =
                              BindingNewTeam
                                ( NewTeam
                                    { _newTeamName =
                                        ( unsafeRange
                                            ("\DC3!\az\1025296\SOHN\999368^lC\n\988422\1032606_\169160\1112920U8 \n9\60210\&0i3Z\SUB\SYN\DC2\141346\&6\42693\92639\131907~~)\ESC=\1063269t\151890\RSs\v\12396\USs\b\SI\DEL\1077652b\151508\b\1024282\176197+e\1076701: \1021924rU")
                                        ),
                                      _newTeamIcon =
                                        ( unsafeRange
                                            ("\1047532%\US\DC4y0\38568\USx\31628\996560\&4\7462\1109751\1088094*=\8707#V%(\58108\t\ACKx\CAN\58120k\1064552\RSl\nG\DC2d\26943\"W\1087990)~\993395fx\SUB\\\1006509\f\1050318\NAKO\DC1t&\188270\SI\EM9\1082486\DC3\92912Z\50662J\1082330#JjO3\EOT\a\1112891'\v><%5SFN\DC4S +7+\17309\&2\1076871\129425\1086732\11728]\t\NAK\1000514v\ESC&D(\SYNc\EM\42376\SOH\1061826\155478'\b\1007144\SYNcv/Z\\\10796\1019756\1078283O}\DC3\b1X\988341K\CANn\133123I!\1016804\&8\aso\1051261il\983110\1029033\1020213\NUL\1020621\144725'\126704\36252BJ\\.v\1032586\f\1029669gB\1023569\1035549y\STX@0\1023358\DC2L)\136057Q\1017189&m\12305u [\1044069'\DC3n\189707\GS\SYN\160673\1069926\127746Mq4|\tz6\1018304\\hN\14823<S\1098711x\DC4!s1\SI\1105716\SUB\1068933\992445\183209\EM\EOT\SUB\19562\ESCire*R7\40047\1082808]\EOTMlL")
                                        ),
                                      _newTeamIconKey =
                                        Just
                                          ( unsafeRange
                                              ("\16240x\1009676{V\\S\1040501E|\9644Ra'hV\1021623;\NAK\131567\DLE2/\DLEjD]d{\135731\996935\156759O\b\187986\175823\\]\fr\1049031\1073470t-xD0\174449J\\v\1078277\137562y9\1023855b\1101887}Q\DLE;\f\1053980?\ENQ\STX<\EOT\1033304r\f\1026763_\21941\42513")
                                          ),
                                      _newTeamMembers = Nothing
                                    }
                                ),
                            bnuCurrency = Just HRK
                          }
                      )
                  )
              ),
          newUserLabel = Just (CookieLabel {cookieLabelText = "9\61673"}),
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.YI, lCountry = Just (Country {fromCountry = CK})}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\989572B\6697x\1029465v*Sh\ETBW k\DC2D ThP6$\1051919\\gc\26334\181843LS\1100158apGZ?8+\1092994h:\FS)\1055988W\1033101Z'C\vH\1080832yI!u9[ ;*_\69681[\SOHF\1032763\EM.,\993457\1014270\DLEwL\NUL\NAKS\1032308\28523\1026515_K\EOT\990075\SOB\1061933EM>;\1073043\&1\DC2MT\ETB\1028139\nX\SI\DC4\1044749&s\1040325\&3\SUB\DC3!cz\166003\72745Ab\1079697\74897\"\DC13\1007725#%`\NUL\rr\1073798[0\39721g\DEL!/\175664\ACK\SUB\148067X\\\EMy\1071997Ci\SOH)8'\SI2\a\bj4h\21618\1078197!\1066187\1085515\1086653jp\1034708\1041246\167329.\131772[<\25302\SIve(\SYN K\1095698\aE\EM\58748TC\DC4L\1025756j\NAK`j`\1078236g\STX?QQKh\1067173Gdp/\RS\47776\vj\174365&;,qr\17853\DC4Bz\RSW\1046496c\DC2}\ESC\995667LdU\\cwz\995574\CAN|gY\1031518n\1086715l\40967oi\984954\143215\SUB\f//\1104792L=\29000]\1009222\9023\151487\1010621y(\NULw\US_\ENQm\1093427K|\64044d\EOTR,\1112819\1015922\997242Nx\67200\994158\a\SOjS=27,k\SOHw\v\ve \1046105\n\32096{\vC\172339\&6K \1046074\DLEn[zPl\SO\t\ETB%\1084892\v\1081941J\1101417\99504\SON!\1001975\41585\1109213)\DEL\RSN;0\ETXH+\172677?}6\50059\40624\1047371\24297\&5S\DLE\\\v\thJ@\SI\DC2\1017755WD\1071566\155468`\DC1e\ACK\EM\131654\v\1106734\DC1h\DC1'KR\99746cB\SI5\a^\10316?0{y\1104136\63491`\16265\ESCS(\n(6:=\1019759uvwL=\988642\US0t\RS|\1075903<\1068688\DLEU#=\EOT)KR~(\27459\174565\SOH\ETB\26497\993529\r\SUB-\170513<\146792\"iVGqt+]c\49355\SO\FSl\1063689\181983\NUL]m\1017136R\US\44965\DC1B\FS\DC2\DC4\t\GS\38678\23244#FB/W\1030265\1103402\ETB`\ETXB\1008534h]\42134U%\NUL\n\SOY\146357\50671;t\t-%\1014098L9\96504Ju\NAKv\FScw\GS,\1093921\131397\&3 \1056565x0l+_i\b\1009851?\n&\1089671\")H-C-\1065483_\SYN(b|\49030\SIo\66370?e\9115]\154725\&0E\NUL1\DC1\36790\1087582\49965\ESCl .P5XY\a\1080806\1043862&\1066695\1023301tKm\133299\129603'\STXJ\SO9\1054858N\"\b\SIRo\1080609\17190i%\1028500\1088366\1043228\&1\"\DC4j\1061688\189324\27084\ACK\EOTV<5\SOH\NUL]N\38516qCf\162501\60047\DEL;.$~a60\171127W\"\SOa\SOH\1056274\1113282\RS\ENQ\989844q\33519\52220\n\9028\EOT\\}\190174w\DC2(\FS\152606/\1013896kv\ACKT\1083158Ze3\1093298l\18314M}e\NULU]\1044325@DOW\tu2t|@\STX\149331xc?%\1022535?]\v!\ENQ\NAK8@G\111076~l\162761E-\NULw\fFl\RSv\vNb:\34400e\DC2@\169237\ao9G\1014326B\1058375f|\1094308\"\STX-C*>RM&\DC2\12308\DLEuPt\1047719@\SUBk\1042383?\a\1015632\&5Q~dUMlfk\SO\aMC\162315'J(\SYN\DC3\9206\171736/\134580<\1014349+\1008252XSzcB\164451/$\120578n\99031\1091527_v\SO\1059505\&8\33943\aCO\1108402\1003998\1105155\152772:\53008qy\151746\SUB\142336\163300elk\\\EM\139516\n\1023711\&7\52018\183789e\1027968K\61367+\59867e\1015488\n\EM8h\CAN\DC3eLD\DC1\16915Y\EOT$#\DC3\44162\1838T\139827=!\ENQ\1090467<\DC3F\6351(5?\DC12\158325\USr\3796\&3%\1108225\999891\ACK}9\a0rF\CANMH\DEL\DLE8\179113R\1012156\65889\61048\1061741Q*\ESC~%\65793C\EOT\n\SUBr#\DC4\DC4n\DC2tx\134308e\1053161\169975.Z\ry8O\ETB"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_17 :: NewUserPublic
testObject_NewUserPublic_user_17 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName = Name {fromName = "\1101266\SI\rM\169588\8791B\128121\174232\&2\n\STX\DC21n]\21849"},
          newUserUUID = Nothing,
          newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+251656534"})),
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "J(Yl\v\47278" (Just AssetComplete)),
              (ImageAsset "\13472;s " (Just AssetComplete)),
              (ImageAsset "\4504g\1079513`\12652F\1037198\26999" (Just AssetComplete)),
              (ImageAsset "V\1088844\78375}\9958\1091466\SOHX%{2\32071\EOT" (Nothing)),
              (ImageAsset "\EM\1021992\1066366\5326-\1089398" (Just AssetComplete)),
              (ImageAsset "\DC3O[j\1071249\21369\\\1008478Pr\156442\NUL\\" (Just AssetPreview)),
              (ImageAsset "\1015032d^\1088258\&1m\DC4j" (Just AssetPreview)),
              (ImageAsset "\1082641*k`\96688\121417\67845U\ENQ" (Just AssetComplete)),
              (ImageAsset "\1000136\&8\7054\ETXr2\aM#q2\v" (Nothing)),
              (ImageAsset ".|" (Nothing)),
              (ImageAsset ";\\" (Just AssetComplete)),
              (ImageAsset "\1028757\SUB\1014681" (Just AssetComplete)),
              (ImageAsset "\57923S\136911\RSA\1109666*>\as3r" (Nothing)),
              (ImageAsset ".-Vja\NUL!\SOH\SO9\8039\59710" (Just AssetPreview)),
              (ImageAsset "6\161143Eu{s\127879a6\1019972\b" (Nothing))
            ],
          newUserAccentId = Just (ColourId {fromColourId = 13420}),
          newUserEmailCode = Nothing,
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("KHcnKK_qpFuroGfcTO0nxQ5ntFkICAdG")))
                  }
              ),
          newUserOrigin = Nothing,
          newUserLabel =
            Just
              ( CookieLabel
                  { cookieLabelText =
                      " T\121456\&9!\US P>x\1065648.\168741\1052544].\1068571-&\820\1043443\DELx\ESC\NAK\1064909[\RS\"\175359\STX9"
                  }
              ),
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.GA, lCountry = Just (Country {fromCountry = IT})}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\181503YALj\"\1079292\1096402\DC46\1032560\NAK{\v2\138711\1057941\&59yPA,%\r\NUL_\96514\n\1103407n\5829pA\71867P%>\b_0\f\ETXo\ACK>\1055943\175828JG$7uj+\144221\\\169751\USS\SO\1098151\NAK$Z\a\SUBG\137786\54450j \126640sE\SYNN\166128\1055791\ETB\SISG8-\95064\1009074\v\DC4\SOH\1112858a\GS\94068Y,\100845\RSF\SUBrZ\30250\nEo\STX\SUBl\DC2\1069330}vJ\DELM\1042226\&65\FSP5\vK\986716=]P\1107419\71189\188993\63087\177290T\140335\157238\153754\DC2Y\1037484E\1036472\995555\SYN`\1109190\n\1055273\&6\NUL\a7\53422{4\1051904jx\n\358\1017656<z\158291B\172229U\ESC\26110j\1013005\\\5050\EOT\1099767\ESC0G\146674\DC4y\ETXf\169004E}\\\r\135403\1110332\ACK\DC4\1108134{o\NAKyu\1057299\FS\4762\151356\b|\1003136\1084417\140012\38730\&0\31434\ENQ\1034179}^G4\1093506\1023168Ql>.\23318\1022569\&7\DC3\a+f\65908N\DEL\1039392\SYN?G\1029806\147641M\r\SYN\190633_\1111387\27464C\1045385G\DLE\1030187\1015222T$\SOH}\993957\NULL\1051615\324\38224Y*v\14162\1065070\157561\\T\SOH\1027903EL pS\1025944\&0Ny5\b>\48922\25552\&4}8\6803\SOH.\917853\NUL\171900s\ACKj\1001178\159040\54836>\1075858x\ETBM\DC1u\58020\13668\35010I\b\ax0|U.\ESCb|'l\SUB\1027691h\1065428<\25857hxJ\12580o]g\FSdUj\1113735\SUB7\STX\187929\1105381\1073522\SUB\988559 ?ji@\SOH\1077941\1010608Q\83240\ENQ<>\1105173(\1001208h\ETB\DC3m*j+\4958t\r\1062561\1030677sv\DC4\EM\1114061\1028200`.a\917587&\184293c\ETXS\rEq\SUB=Zc\188279\5357\ETXv\49087\ETX\ETB\RS\EOT \1015390jMt\1072601O\37910,\19924\177989_>#BB\121063-t\1004461\162764\"q@\1032626\1112717\41938\188735%,\994163\&9z\97066\nM\134758\1021399p\SOH\1106837p\74635\129184\f\92628r7+\1005227\SIX\DC2\SUBPtOj\SIZO@'\1001038\1047034o\135659\1000832P\1073325\fb(skyZ\32666b;@\1051451.\n4v'gD\1024820\&9\SYN00Ih|\1035066\18497a\STXt\NAK\b\1014532X\6235Qh-m<Vb?Dz\1103803\139366&^&\1059117\STX>\194771@\29373q\RSw=:\1019837\SYN\158989&&n\CAN\RS\DLE\DC1\ETB\27195\1063628\1054662*zQ\EOT`O\1014436|%[\SUB\1006985\STXx\DC3ezypH4\1026851\SYN<\US/wb\"\181999\SOH'\1056474\166114|\60420\ETB\177869\ENQS\CAN[\ETBD\1066925\128256{\34534(\SYNg7u \apvj\1031342Vr\SYN8\996660\SI\n\1009589j\156771,\165504Nn\NULd\44565^]#\aa}k^\148538ZPzT6|JA\n4w4\992910\166788R^z7\14616U\ACK\1113645\19783\5304\194638\156905W\1055611-\SYN\160462L\1002902\GS\SOH\1108138\a\DC1\6622\&5p\US\24278V&I\175402\\ #\DC4J\t\1049942pb.\1010603<j\DC3\ETXL\GS\RS17'\DC47\163685B\126618\&7\1037374 \1045260(;\"\1113918L\1056262\127041np\SI\28159\&6\1082124\993049\GS\1064424e6t\CAN\DC28t{\32150{\\jfj\47379'c`T@\996769i[0U\GS+V\STX|\t#\1046962S\US\97607\SOQA\94275VTRa'=\b0\DC2\1112366Z\1103044>Y=\EMB\996067\1040774\ETB\\\ETXFu3=fgk0tHMV\1009527L&\FS\1079640\167548\96447u\985529\1086845#$\SUBHr\NAKbC=\1037096\165144f}@FOXE\15726b\1099625\t\EOT\994002\SUB'0\SOH\8475\EOT\96228|UBm\1102844U.0\1038890\3046\&0\5457qC5\166740qTEeK?Y\1043642V!Po\186183y\1004864\1006600\1078806)>\998435\SOH\161694\166530@\ESC\n>-q\n8\EOT\1003026E*/\1006129\&3\EMq:JRP\983716\1105473\190584L\46488yl\992613!Iczg\ESC\23903Zdc\1023922\991606IlG\DLEfwx\37330b\33715/{\SUB,\DEL0\70340\ENQ5>\17289\1091599U7J\1035804\NULz'\128593\20935'\EOT\3047_f\DC1B\"B{Q@\NAK\98324\73757b3y\147047\1078365W[j\62876\142006j\1064011[\180857]\SUB\EM\CAN{\SUB\54227^%\1027156T\r\51537\DELqIfX\74961w\100446}\aF\1101058\SIq,\985054teNy\152514vM"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_18 :: NewUserPublic
testObject_NewUserPublic_user_18 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "!1\CAN1\144175=o/+\1087748(\DC3\DC3f\DLE\41173\8283\191178\1020517;=\DC1\SO}\EM\1063427Vla\DC4\169289{xx 1rYe\1050991K/"
              },
          newUserUUID = Nothing,
          newUserIdentity =
            Just
              ( FullIdentity
                  (Email {emailLocal = "", emailDomain = "\154087\1037287\128710\1075384"})
                  (Phone {fromPhone = "+5329622943"})
              ),
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "*e\1089302\10851" (Just AssetPreview)),
              (ImageAsset "H\DEL\154233\180007_^" (Just AssetPreview)),
              (ImageAsset "\1099902\DLES\n\137441Z\1084230\ETBz20\1020520\&0S\19310" (Just AssetPreview)),
              (ImageAsset "Zi\STX\SYN\171579#/\55223}\"\ESC\1034142" (Just AssetPreview)),
              (ImageAsset "Z\138009\ETB]NL1t\DC2\EM\tE\DEL\EOT\1029357" (Just AssetComplete)),
              (ImageAsset "\149286%9\1002620\1015461" (Just AssetPreview)),
              (ImageAsset "%y" (Just AssetComplete)),
              (ImageAsset "\\H\r8~4\996400\17577V" (Just AssetPreview)),
              (ImageAsset "`j\20036" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -14264}),
          newUserEmailCode = Nothing,
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("lQ-HusGetqIpBMbG7sWjrEOE4v2-Ym0=")))
                  }
              ),
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  ( NewTeamMember
                      ( InvitationCode
                          { fromInvitationCode = (fromRight undefined (validate ("mYNFyKFVL1hf66I1Exr3P8kiIA==")))
                          }
                      )
                  )
              ),
          newUserLabel = Nothing,
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "\STX\23956-\SOyRe\ETB\ESC\DLEE\150261/-\n\ACK\ACKz\b\\)\1037849:z\176217\1082403W~\ETB\54707yU@\RS9!\1099610S&\DC1U\STX3f\a\180451D-:8x Z\v\145560y\a\DEL\994805t\100914\&0\1023273\CANLvB\141872\1051474`dq\92965\b@Dh\RS^\DEL\182091!Z\DELrdy&\37095njK\164516\1044360\60347c>k\\\1020435a'{?\1079834\&8\64052\&2\1014683\DLE{0\20518<\142055\74242Jyp\1029535C]{{\50311EJ2q\5027m\993187\&8\FSj?\SUB\SIq\135875%\996809\NAKc\113697\1034249B#D\1071738d[)H%1V\GS\DEL\1034278Ul\1061968\1068236i\ESC\32157\53150}1\SI\1074573e_\SYN\1013349\1088850/\DC1h\1111142\ESCniQ\167524/z\NAKBHL\SI\RS\STX\1101337\987848\1076526\f\51315$\137724\US\ENQ\r\")\27022\1059806\SYN\151409\1004808ZTY^\1107169E\1004349\"\3157\SUB.;;\43277vw\1047269\&7\157540\\\STX]</\SYN\6728\1103885\SI&\988968)L\990669/\157521D\1072889uNR>\1044056Sk\DC1)\tx\DEL'b\995601\1099391\1021026v1L'b\SUB\183708.t%\41436\1049072\DEL\DC41\155461}\EM{{/0\n\25454A3\984954f\b\1013990\1095352\ENQ\173463\135530\RS\1096641\20218bC/\98191\67849_~\16801>\1055272\52708l\r\96379\27914\&6Sl\EM\SI\173222\155481 IR57\EOT\v=H4h>%\1092605Z\158591\CAN_\1036045\SI\1071688Q8e\185935\GSn\f\by\999322\149469RWn\ai\SUB\NAK\984865\RSVG{\1041949,Sw=\998755QB1V\EOTeb\DEL\78455\DEL+\CANA\37142bR\ESC\7098\1046757\NAK\NUL&\1032816w\RS%\t\142973`*\1029796e:\1059486Nbk\n]tYJ\ACK(n\DC1(2%\65266a j`)\DLE\1111251\1096252\187962c'\1056681k\GS\DLEwsz\12960+\1030605\&7\185360\171135\DC4R\ETB\1112743n\DLEB=\SIj{\1074190\&05\16369w\DC4X\1016487\SOH\US\161186\SYN{d2c\DC4cn%qT\68800N\128162\48763\&0\1060310P9N\ETB\NUL\DELe9!\129635\13632\&2\r\a@U\FS\GS\98397Z\v=(w\9517\b\EM.)Lp\1096101RJJ\DC42\1088136\GS\173056A\DLE\nZ\ESCx\1099963f1b\37479cn3>9}e\4720\1005886\v\DC1]K[\DC2h\1053150C\1084182\tQ\"@hD<P_DK\21036=~{\165855e\1103802a-\f5\93816D^\72118\142105\1043057\NAK\EOT\170421\75062\NUL\NAKF\boh'8];3\1033607&\78455u\NULC\1112852\DC2ox'\159631\1029780D\175813\NAKpuu\FS$\58061\SUB{\ACKK\ETXe\SOH\995546\RS7\167722\53013/\1026331\SOH\GS\26184 ~|V\b\NAK\DELDMt\1027866\1105037\137137\SYNO\1096385H\1043040'S\1088163\140179\1107935y#.)/\27169P\"\USH\a=#\39947JV\1107483\1000671\&2FBEHi\23003B\ENQ~L\150508\DEL\ACKW\1099343`%\1087539;\t\1082768&\1088412\STX\1039256q\STXT\ENQ<3\39867z{\SUBe}\ACK\1064946;C\ACK\CAN\96934i\DELx\tL\DLEq\DC1t\1047122L{M+\US\a6\182844\&0.\DC1?4\1029316\1081833P\1048050\EMqRP\r\1062381\GS\57613cI*\45926[\152131Ij\a<cv\DC2o\1003362J\1023976i$(`t{\33480\993718PN\1055784n[\aej\b\EM|6!\1103613\181829N|xr\155192\1047676\1054895^\127171\1074512\DLE\1101256CLX\SI7M\rys2@\DC3\1076347\160510:/\1022501mm\EOT\CANc$/s\160717]\RS|\SUBz\1085374\1081213\a\1041704\47932\ETBD\SI`M%\149235\1022171"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_19 :: NewUserPublic
testObject_NewUserPublic_user_19 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  "\31892!\v\128590EZm<1\t\1058832\168338\SIdL\1110204\1081875=\n\187099\1049095\SOQ-\163868\&98h\985094\160376\&8}T\DLE\1048947z'\"\139579\1079246\ACK\a\1107127O\185727\1022003\ETB\989454\20219\168407\rjr\174023\STX\1108925F)|An+vO=+]\DC1\US`\995994)8\181766 \n\EM5D\1063380\RS\988284\1058314N}\8685ha/\1107898\&1?*k?4\DC1k\ESC_\184239h\1067717\&7'\1071888\DC31\ENQ4\RS\ESCoYL\1005247A\1018862@O"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Nothing,
          newUserAssets =
            [ (ImageAsset "#rp*?\SO\EM/f^" (Just AssetComplete)),
              (ImageAsset ".s\997192" (Nothing)),
              (ImageAsset "r;J" (Just AssetPreview))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -5205}),
          newUserEmailCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("SY1yub-qH1N37ltqE8QIz4pb")))}),
          newUserPhoneCode =
            Just
              ( ActivationCode
                  { fromActivationCode = (fromRight undefined (validate ("ZdxnNFLHOnpzlZ3zpURGlFYKDDX3bA==")))
                  }
              ),
          newUserOrigin = Nothing,
          newUserLabel = Nothing,
          newUserLocale =
            Just (Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Just (Country {fromCountry = SV})}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "J\190174\51052c\1112230\&2x\EOT\SOH\r-'\1063791\15012T=F\162478\1071046\v\DC3\22988\STXg\165532<\18360m\RSp\19221L\1024354\132583J\13979\1094281#p\14355\f\1107349\t\DC4Qd\1097527QuZHVz|x\1010055\993073$!8ueAT\1882F+\STXv\1048683Gl\GS\CAN\97900)W\SYN\170378\1102702\156722{\DC3qO9R{\1080555\1014522\185761!\1014453\NULt1$\148596G\vD^gJ*\EM$\SYNE|]3\35093\n{P3:\12914\40571}\164718\ACK\GSt,\1092264,\171514@{C\1052543\1011287=\1072509H\1085969({Zo?#\ETB\1113983T=\a\t\25790\1062299\1088828<0=]|\1101158\1057242!\1026651!&\1064094\n\161577\22300C\EOTM:-\ETBu\99367t,\1037104C=E\NULN6\v\f\DLEG\24601\DLEB\47165\151550\33091<\f\1025446[r\DC1\r\65507\184871\1035849.}A\US\74504\b)+%0\156451O\1096973\&3X\59075B3i\v!\"\1094860\155704\158444\GSP|\CAN%\EOTZe\144840R50nB\SUBl/=+9\USG3\10490\ESCs\rj]Q+/p\1065259#\1107090\DC2\23382\1023386\&3U~\te\EOT\DC4_v%b*\DELP&7AJ[\990857uGH.\US\1023401\1046826:\GS\1044814 \1111213\a\SI/\DC1\SI\1053875\1071821&3#\SOH\SYNM\994893X\1065761\8469\31617\1049841\1057939-\DC2*;z\1058824\f\40224\65354\NAK;\1067304\RS\ACKVU/b\SIW\172509rS\162914Z\1020791K4#7\1071844\SOH$1J\191028\&8\1111028>Y\1106479\FS\DC1\ETXsY@\EMOAc\EM)k\DC1\br9~q\ACKR~\1074883m=\1014601$\DEL\67701P#s\DLElH\EOTK\NAK*\US}z\17632\25323\RS\185248W`U\DC2@\12527v.e7\\\STXUQ\NAK\1046772\v^C%&[9p\f_\ESCg49)\ETX\13863r\ESC\179979\&8\1042860V\vX\bqh\1073214\DC1|\CAN\1050558{0\27050-\DC2y\DLEtH&P\1015792x\n\"G\a\EOTm\164691\37856\1047080\58628;#G\t^sSr\163410c"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )

testObject_NewUserPublic_user_20 :: NewUserPublic
testObject_NewUserPublic_user_20 =
  NewUserPublic
    ( NewUser
        { newUserDisplayName =
            Name
              { fromName =
                  ")Q>\t\EOT\1051835O&G`)wJ\1057873\1032479\1024350\164902~\STXV.\1106833e+d\ESC\65762\1036361\1022695g[~\133553\&9\132587QxR\ETBHMtNg\NAK\133684H\190788_x\1025677\11540vx3\ACK}\ESCNo9Pd\FSV>"
              },
          newUserUUID = Nothing,
          newUserIdentity = Nothing,
          newUserPict = Just (Pict {fromPict = []}),
          newUserAssets =
            [ (ImageAsset "06m\NUL\171780" (Just AssetPreview)),
              (ImageAsset "\1100778\&1\DEL?9t\998692g\DC1\t\a\SOH\NAK`g" (Nothing)),
              (ImageAsset "92" (Just AssetPreview)),
              (ImageAsset "9\1050850\"B\t1\999607|" (Just AssetComplete)),
              (ImageAsset "B " (Just AssetComplete)),
              (ImageAsset "\SUBg" (Just AssetPreview)),
              (ImageAsset "[\RS\1108970n/" (Just AssetPreview)),
              (ImageAsset "\22015" (Just AssetComplete)),
              (ImageAsset "\1067432\&3" (Just AssetPreview)),
              (ImageAsset "!\7963|R!81" (Just AssetPreview)),
              (ImageAsset "\"U))K\139106\DC4`P" (Just AssetComplete)),
              (ImageAsset "\DC4\NUL\185666\1015713\30286\1101583D@@" (Nothing)),
              (ImageAsset "M\DC2,\ETB\t\31708" (Just AssetComplete)),
              (ImageAsset "&\100259%e5" (Just AssetPreview)),
              (ImageAsset "Fu\1095038X\ACK\990196\&8wq\62709" (Just AssetPreview)),
              (ImageAsset "L" (Just AssetPreview)),
              (ImageAsset "FyZs9\64067\&2\ENQ" (Just AssetComplete)),
              (ImageAsset "B\USZ\984716o\f\RS-\1097587" (Just AssetComplete))
            ],
          newUserAccentId = Just (ColourId {fromColourId = -14182}),
          newUserEmailCode =
            Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("u7kZwe4BWVUT4ofDJg==")))}),
          newUserPhoneCode = Nothing,
          newUserOrigin =
            Just
              ( NewUserOriginTeamUser
                  ( NewTeamMember
                      (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("B7ams5fFLxcw1iSuEZIpbQ==")))})
                  )
              ),
          newUserLabel = Just (CookieLabel {cookieLabelText = "\DC2\DEL\45582I'\NAK\DC2,\t|"}),
          newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Nothing}),
          newUserPassword =
            Just
              ( PlainTextPassword
                  "$LQ\RS@\1029575Q\101013y5\132754T\v\1047497mMd\1073933\&6@0T\45642}S\126514>\985306\1043535\aC\131618\DLE=m:$^g\995451N\DC3Q\ETX\DC3>]hmg(\35623Ae\ETX\98548@-m\"@\v\STX\55029yY\1060794]\fby1\44325O\1085672ZO\12036\1019496:\"&\24266\1065045\SUB\139216a\1042858qE\DC1W\46141gB\688\&5MT\41562I\174039\1106796;\a&2\94738`;Uy\132207\120577\&3\n\2053\983533\SYN\DC2\581\71070k*\SOHv\1032152\EM\ENQ\DC2\1064999\DC3]+\40240'\111203\1080567,"
              ),
          newUserExpiresIn = Nothing,
          newUserManagedBy = Just ManagedByWire
        }
    )
