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
module Test.Wire.API.Golden.Generated.NewUser_user where

import Data.Currency (Alpha (VEF))
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( CC,
        CG,
        CH,
        CO,
        EG,
        FM,
        IL,
        IR,
        LU,
        LY,
        NP,
        PF,
        RS,
        SI,
        TM,
        UM
      ),
  )
import Data.Id (Id (Id, toUUID))
import qualified Data.LanguageCodes
  ( ISO639_1
      ( BO,
        CV,
        FO,
        GV,
        HU,
        KW,
        LI,
        MH,
        MN,
        PI,
        QU,
        RW,
        SN,
        SQ,
        SR,
        UZ,
        XH
      ),
  )
import Data.Misc (PlainTextPassword (PlainTextPassword))
import Data.Range (unsafeRange)
import Data.Text.Ascii (AsciiChars (validate))
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust, fromRight, undefined, (.))
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
    ManagedBy (ManagedByScim, ManagedByWire),
    Name (Name, fromName),
    NewTeamUser (NewTeamCreator, NewTeamMember, NewTeamMemberSSO),
    NewUser (..),
    NewUserOrigin (NewUserOriginInvitationCode, NewUserOriginTeamUser),
    Phone (Phone, fromPhone),
    Pict (Pict, fromPict),
    UserIdentity
      ( EmailIdentity,
        FullIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    UserSSOId (UserSSOId, UserScimExternalId),
  )
import Wire.API.User.Activation (ActivationCode (ActivationCode, fromActivationCode))
import Wire.API.User.Auth (CookieLabel (CookieLabel, cookieLabelText))

testObject_NewUser_user_1 :: NewUser
testObject_NewUser_user_1 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "V~\14040\38047\NULw\1105603\1077601\&1\73084\1020199%\14699]y*\121297jqM\SYN\74260/\1108497-*\US \RSA\SO}\64347c\60361v [\1022394t\1012213R\181051Y\1036488\&6tg\SYN\1044855+\DLE\99976;\ACKOj\DC3\48593&aD:\nf\1002443!*\DEL"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\5112\&5\DC4\1019826\95472`\a\SUBG{?" (Just AssetPreview)),
          (ImageAsset "KE" (Nothing)),
          (ImageAsset "34A\ETX" (Nothing)),
          (ImageAsset "\1016563\SYN\96595\8454" (Just AssetPreview)),
          (ImageAsset "bN\GSj|z*dS7\1101290\RS\f`" (Just AssetComplete)),
          (ImageAsset "d\b,U" (Just AssetComplete)),
          (ImageAsset "\1042200\&4\78817\SOHZ" (Just AssetPreview)),
          (ImageAsset ",\DLE\ETBkd\n\44652\1088214" (Just AssetPreview)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "\GS\36694\985607\&2" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("1YgaHo0=")))}),
      newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("z1OeJQ==")))}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              ( InvitationCode
                  { fromInvitationCode = (fromRight undefined (validate ("DhBvokHtVbWSKbWi0_IATMGH3P8DLEOw5YIcYg==")))
                  }
              )
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "\186640\&15XwT\991660: \DC1Z+\ty\94985\SOH"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.SN, lCountry = Just (Country {fromCountry = LY})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\143961%5T5'<IQtf]\13622\EOT\1019095\FS+\f\98493\FS$\fY\RSG=p\182047V\DC2E_ni\\Q:\118823>\17286\7398\ENQ-\1063783\100891\&7W@\59062)!$%v{\f\n_I6\1088622\52764]r\1105300\61079\STXGi_L\ENQ@tr<\35715\&2Dr\16519\\\v8\49277\DC4\1069631e\b\190386\71324srN\34600\26071Qk+\36197\999209O\\c6\1032813X\1026685\1074390VV\\\999471^\1105556\DC4(P~y\SI(\nrO\1037710U=$\1038971k\1011736\&7.\NAK[dn\1061566\31927_\NUL\997265\vNVd\54706z\1029333pV6\RS\166743#/m\1065646w\NAK\27792u\144303\SIs\DC1\136497^A\95500>\SUB#\EMsC!3#\59953`\159877q\65860\\VrnT\DLE\SYN\1060441\DC4\STX\156538\1003845\DC2d \1028483#\CAN\179878/k\14627X\"I\SIO,`GU+\DC1\DEL\"\n\47090n)\ESC\1059861x\1018430\1097583%\DC2\SIVr\f\1044385H`\128647W\FS\NAK\1050334vii\FS\a\ENQ\1005180&d\GS\146823\991562.\1090052j\1008159$=a_s\DLEQ\1020394\SO\f\ETX\1019724B\ENQ\CANL\STX_ZX\NAK h_sGj)\1047298|\NUL\SI\rlUN)\ACK\DC1`8\f\1018610\999181\b,A\DC1\tt/0lT\1071777\a}\SYNj\SI\az|\ENQ\152944J,26\1022981\ETX9\11179\&0\EMw'\NULO&g\USF0\1001389kg\STX\DC1|Q\1048680\SUBM\131896\1038590vuPgVp\180615)/<W\1077985R\1069905,\39037\v\145751y\DLE\vS\SUB(\DC4!\SI\181381\RS\988082U*(\194576\&4rX\SO\8572A\1035058\SO\DC4z9\1050749\1083893{^\1053567\168917\n\b\1105884rYL\31991h\167262?J\ETBn45X.L\1098255^{ot|\EMq\34593\a(\137115I\v\ESC\168793\132359\ENQ.\17815y0a\32961I\DEL\NAKKk\154911\NAK\34647\1063811\SIM`\178041\47494N\NAK9`\1026852\FS@Y~\SOL\1018994\1098118o3\95471;\1011338\1010407\DC3fz\SUB\1025379(}.)[!'2LJS\rBK\3667\ETX5<u`\SO\"\12511i\1018516H&\DLEP\1106835#\188694t1\ENQ{\ENQ\1083098\fS\NUL=\SYN\1080920\ESC\30501\ESC$\1044616\&3\1687@GKvG/\1000255\157153,\176780i\1028057\&9TW\ESCoe\f\FSf\1110205S\GS\42634\998348g\1053453{\DC2\63337fg\DLE\1095355\DLEbc\1030987\1023379f_}.@\17549{r~{\f[?`\1050469\1070751[!\DC2B7%"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Nothing
    }

testObject_NewUser_user_2 :: NewUser
testObject_NewUser_user_2 =
  NewUser
    { newUserDisplayName = Name {fromName = "\NUL`)\a|>}\EM5z\70179\t>w\SO\1007537"},
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          ( EmailIdentity
              ( Email
                  { emailLocal = "\DLE_\v*]\57697\b\t\1003072 ",
                    emailDomain =
                      "wE\51639Y$\ETB\t\f\STX\DC4]u\985277[<\78227\rd\EM\153024\137838\61361:r\1057444\63453\NUL\SI"
                  }
              )
          ),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\GSg" (Just AssetComplete)),
          (ImageAsset "\CAN" (Just AssetPreview)),
          (ImageAsset "7|{" (Nothing)),
          (ImageAsset "\931[E\ETX\8219\NAK\EMR" (Just AssetComplete)),
          (ImageAsset "\GSQ\RS\184224*[OCr\1092309" (Just AssetComplete)),
          (ImageAsset "_\SO\NUL\128089p\1010367\&7n\176864\SYN" (Just AssetPreview)),
          (ImageAsset "\178226\1064501\6913kC\DC1\SYN\1072813{/\1004430\986308\"\152075" (Nothing)),
          (ImageAsset "*\1036896f" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "\993111\&8'e4\\\DC4\169718W\SI&f" (Just AssetPreview)),
          (ImageAsset "x\143400\GS\FSF" (Just AssetPreview)),
          (ImageAsset "U\170868u6\998839\1024108/\8506\SYN<\154678\184567\96453:\US" (Nothing)),
          (ImageAsset "S" (Just AssetComplete)),
          (ImageAsset "'\SOH\SO\63302" (Just AssetComplete)),
          (ImageAsset "\t\38487\&8RR[#" (Nothing)),
          (ImageAsset "\13881\NULU\4412\&1A\120304\13635T\SOHx$P" (Just AssetComplete))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("XJbRdz2tmf7rgAU=")))}),
      newUserPhoneCode = Nothing,
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("RnoP9ThWPcujkA==")))})
          ),
      newUserLabel = Nothing,
      newUserLocale = Nothing,
      newUserPassword =
        Just
          ( PlainTextPassword
              "\178767\"-\42435\28244Og'}\158476\DC3Sz`}^\EM\189958SS\184861\164033\DC3p\n[\19946\33430.l\1014022p\r\SO\SO\1024282$\SUB[\1009905\1107088\153618fV\DC3[\DC3\1114043Rc\1023417H\US\1019446\164797\186995qO\996740\a\156339\NUL<\1062145\EM\1035248\94367\US\n\DELd\33695\v\DEL[2\1045678s\RSb+E\172930W\60192&\1063736f3\DC3[#,\US=f\68041}\1031008\\@\1059268}n"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_3 :: NewUser
testObject_NewUser_user_3 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\1068382\&9\29192F.G\USn\t\CAN.K?B\52405\1092770\994411&\987778\64009\1108933NijZ\1001115zr>\CANy\1026961\1049424\97973\174172\141601\32980\1006618rz\SOH\1021005\DC4\r\DC4\t\186828%\ENQjK\1043685F\65905\&5\DEL[31\DC2H`\142577l&%\EOT\999979R*\988883"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          ( SSOIdentity
              (UserScimExternalId "_{3\159847")
              (Just (Email {emailLocal = "\66514\145735iC\1096882`", emailDomain = "*"}))
              (Just (Phone {fromPhone = "+08713477"}))
          ),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "s~\a" (Just AssetPreview)),
          (ImageAsset "/=sG\b\74796\GSKx" (Just AssetComplete)),
          (ImageAsset "/K\1092610\1018470K\ENQ\184797(,\990273" (Nothing)),
          (ImageAsset "\155055SM6V4w" (Just AssetPreview)),
          (ImageAsset "e" (Just AssetComplete)),
          (ImageAsset "\1022357\156151[0RX" (Just AssetComplete)),
          (ImageAsset "#\CAN\181775Y\152578\142820." (Nothing)),
          (ImageAsset "85C>%fMrl" (Nothing)),
          (ImageAsset "\1050678\NAK\ESC7\45634\CAN \1098391\1077719" (Just AssetPreview))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("kLnw_0l8qwKitd6ZFiCd_A==")))}),
      newUserPhoneCode =
        Just
          ( ActivationCode
              { fromActivationCode = (fromRight undefined (validate ("SuKUHXzNTjI1FqdQiBjh5b92HrcebYYq7Vif")))
              }
          ),
      newUserOrigin =
        Just
          ( NewUserOriginTeamUser
              (NewTeamMemberSSO (Id (fromJust (UUID.fromString "0000519f-0000-2c2d-0000-26150000010b"))))
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = ")!\1072457!"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.SQ, lCountry = Just (Country {fromCountry = SI})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\1006682&1`\1060911/j\143816\ESC@\1029096\147893ael\141244\1073074\186354(\4451\995732\SYNkSCY\1061755nA\SIv3NT\SO\1026898F\\]9<W\DLE\142350`j\n0lD[N\SIW@:\1072326{\DC2IZC\1510\US~\25902\48840\1010236u=\1076031y7\aA[\1049971(\DC4\CAN\141570\SYN\GS,\EOTh6O\USbK |d;0o\59215E\NAK\EOT;\1085606\DC2\1021605\ENQ\141242\b/Tl,W&t\40792@$\1021389\ETX'19 \1096230\1066976|8;M2`l\1492N\9374\995336k\1057151\US\171059x 9!T\a\ESC0\SI\144401,\1065126&e\71484Y.f.9\SYN3*^\t\45446\152655R*-N\bKk\1075131!\ETX;nMCFt+\DC1\1053439\129105\22586\SOF= t\v=}>\STX\"\ETB\ESC8\NUL:\DC1\1029969s\\O#\SUBs\ETXZN\vN\EM\DC4Iq\1017878w\US\1090622\22754\DLE\ETXh0\SOH\DC4G_,8\SOH\1043656*MX\1012605(H\v2\t\1058824M%J(\1001332\1004711@\NUL\990247\v\187365d\DELAm&~\148667\50012\1024231\68642S\1059526Oo'\a\189198\999417)mGV\135457\147460$(+>(ok\RS\DLE\DC4x\DC49\2891`\SO\1023481]\a\DC3[\f\26263h\1069921Lg\vK?h\ENQ\DEL'\ENQER\CANhG1q7Hw0\1033060\166098R\1076814\1108568Xq\DC2\1061716/\1059864\&2\1107844\168234\191137$\CAN.\986034\STX\aX\996159\ESC]\1027300r\1018675^,A\35957\1065595!\ETB\10158\DC3>\ACKh\r~{N\STX\154171p\1113549T\1083716hs\DC1=s>5\NUL^Bh\182047\1052799~\ETX/\SUBy\tPs\1093016m7h9\132920\989902\132044r\1026138\&4?o\1057718+\1032371\1019109\997209\NAK\t|\DLEG\161998\151372\35112q$\986429\167673\38940\111043\\\EM\ACKrI1A\1040422YpW\DC2ef\191062fg\"|\26780\917574\CAN\1078768)\1103505y\1032757SZ\1017253\&82\SYNa^#e\46908\1029655a-}uY\57411X\1092193<\32405\NAK\27580I\"\v\DLE\NUL\ESC\991177f\185152\\\27490[\1101836=%\28960Ea\984993\1097665\1053017e\1089091\94371I\1080089\1101910\43300\ESC3\4216\EMU|\61154\SYNjtQ`\1031221\DELrW\DC3\ENQZ\NAK/kHL\CAN(\t\SI-s-\1107550'\1020570n\998043\&5\1920/\1004147\1101677\61266q\1082252\1097735p\139262\&6\nZY\tq%\43819\1008683\DC2m\DC3\185092\18249\167055\&1\b:F A\65909\1084546\1013856~vk\1082131\1110019\1062378,Z\1025818\FSk\66199\171592P5_m{\\\t\21751,\1107402A\RS)L0)+\a\US'X17U\STX#\48263>\DC3|P\1069959po\EMRf\1019294\a\SOH\183860x\145286iL\11178h\1108376\nq\153553\STX@\68035c\13063j\987586\&9?R\30388\DC3 NB`\ESC\163993\28145\SO\t'/k\DC3w9eFy\NULt\SO\1100884X/m\US\EOT\5630:&\CAN\CAN4\v[k/f=\1073031bC\n-X\14469O6\RS><a\21553\SO\DLE\DC4\15780\a_|v\70455\1078722\9783Q:\US\STX'\7354\&8\1036116v \1054866Hl\1103971\1001992\&9\ba\USuq0Ra\RS#\EM\r\1026792\EM4Y8\157640\EM\1093020nBz;iim\165302\24825V\SO+\146481\1001989\1030047\186120=\ESC(yxl?]\170156pe\1062135\6199\NUL\1064668{H&\998205S5\141339\SYN7I|\77993%<\r)\v\9024XQ<\1062089G\DC4<\RS\NAK?\989864J\a^\1039016\NAK\98469\n\1055944)0\US\SI\985148\DC3|\STX\1062\DLEe\986628\1040508\190586Jr\US\NAKj9.T'7\16273\162060\35012?\r9\DEL\61204$\54172 )"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_4 :: NewUser
testObject_NewUser_user_4 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\34253\1063892\SUB\1045725\nN\1021360\ETB\986802\ENQ\DC4G\fN?\998052p\b\57570\78656\1076029\&1\ETBr\SOH\100265?\ETX|\1103522{Z\r\EOT"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          ( EmailIdentity
              ( Email
                  { emailLocal = "K}V\1053781t\DC1\61430\8356\EOT}nR\151902\1104809s",
                    emailDomain = "\fI\1452\EM\SOH2P\\\136186x\GS\1097826\&2!\1039116{\1029254\164493\194753\1072433\&4"
                  }
              )
          ),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset ">A\EM\US\b\b,\1102841_m\1079034G'" (Nothing)),
          (ImageAsset "\EOT\RS\1109233U*\1028384\42938\153119z{o\b" (Just AssetPreview)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "\1060069*\aE" (Nothing)),
          (ImageAsset "\1020097N\US7WkL\1061623G1\45476\169283\1099983" (Just AssetComplete)),
          (ImageAsset "=ntM2\n7=:\SYN \1032706" (Just AssetComplete)),
          (ImageAsset "\ENQ\172536J\1092317\"\50184\EOTE" (Just AssetComplete)),
          (ImageAsset "B\GS\ETX\1072082\SUB9\164729nQ\1109982,Ps" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "\DC3rR" (Just AssetComplete)),
          (ImageAsset "K\v{O\n\22328\1104220@G" (Just AssetComplete)),
          (ImageAsset "MT\EM\b\ESCC\\i(2\52129(l" (Just AssetPreview)),
          (ImageAsset "h\1089311\1077334\\\SYN\33487\74110" (Just AssetComplete)),
          (ImageAsset "\147810\v$E+Uu]" (Just AssetComplete)),
          (ImageAsset "[Y" (Just AssetPreview)),
          (ImageAsset "\\P`\1082951" (Just AssetPreview)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "'\NUL\STX)G\GS" (Just AssetPreview)),
          (ImageAsset "\21469A\GS-?\\SM\1005769_{`\ETB\EOT" (Nothing))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("uqc=")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("FYKXjOe_umVeNz8oszsuHQ4S")))}),
      newUserOrigin = Nothing,
      newUserLabel =
        Just (CookieLabel {cookieLabelText = "\ESC\1020022U<\1039752\"w\a<9s\1101139\a(?A\1014049\r\ESC\1003753y,@"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.CV, lCountry = Just (Country {fromCountry = IR})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\DLE\RS\22311\CAN\a\1012327K\b\1049980S/\23572\GSH\1017395D\"\CAN1y:\EM1g\f~\1098762\\\ENQi\1105245S}\50521\1096679\135387\SO\94618A\995325\DEL\SIt\aN\SIQJg6kD\ESC\1023496\CAN\62375\1041013QU$PN%\170215'`\1104907k\DC3Z<\n\EMu]\99040\&6\1068331\127755P\67617(\SUB\SUB79\r\189272\ESC\DC4\1106024\1073208=\29427\v@\26218\EOTF{\182957<]\1019216\&6e8\ACK\EOT\31272[\\m\SOHR>\v\178437\1069835L\41138(,5)[s]W\23773N\20236\27980&}\ESC@*p%'~s$o\1099903P\ESC\ACK\ETX/.&\SYN.\ESC\1695\10271\96928\ESC.\21993t\138860\147995\182406\EM7\fzO-$K\99528\ESC\DLEl=gcx\143695$h\CANB\DC1xT+ ?f+'dH\1111883\1020511\&2Gu\133052\181417\&4ar[+P\ETB\46625vU}\DC1~\SI\SOWMi6\CAN_~\1100418\STXD\3235^\n~Zf:\"o\1045430\&3\1037460>\DEL|G5\DEL\83316\STX'=%\"\"|5\NULy\n:5\ESCG\52775}|\23629C\1044600\1071086e\vi![?\141724\fw*xIV\td F}X;\1076311\135141\f=\CAN%\GS\164985(\985299\16826\23962i=y6t\ENQ\1019088\DC1\991519 -5F6G\DC1\1023224NO.\131187ly\1057069e,c\"f\GS%\SYN\DC2\1016422<g:R\ETXk}\147928~?G\47618\9578\ETB6g\12513&\DC1\rA\1074074k\DC2+R\v\1051732\1096263c7\988728\n\151156s%Bs\DLE\983618\SIn[aIRFg5K\1101824\RS\ETB#^z\985179\998061J\186962\992022\DEL\153403D/\1041119\176079\1056551\72850\1083330\172122\EM\DC4\184854\1003545\&3\168775na{\184105aZ\v\"`\49889hZ\64743\120360:(@FG6\DELN0I\DLE|o\1031469$V\47510+\SUB\1059278\985803\138958\136036sMV\97069\100084 \176136\1021111\19010B\3625\49866\&8PZP\ACK\160902W\182924H'{<\1008307\19344H'\US\46936\EOT\ACK\ETXnQ\38228x\1010015\151041YC\ENQ\21567\t\DC3\987732^u%;\FSS_\GS<Mw\r\173550$\83259\&9/\73796\&51\a\NUL J\1072338\EM'K\5220\176930\1005492}<Zl\28233C>71P\11668\1013544\EM\1018759\1052439\&1\DC2|\ACK"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_5 :: NewUser
testObject_NewUser_user_5 =
  NewUser
    { newUserDisplayName = Name {fromName = ";\1099655\100579\SI\147095\SOH"},
      newUserUUID = Nothing,
      newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+8057919910"})),
      newUserPict = Nothing,
      newUserAssets =
        [ (ImageAsset "\ETX\1033833\&8pq" (Just AssetComplete)),
          (ImageAsset "\37164c9}+}T" (Just AssetPreview)),
          (ImageAsset "7\139387ui%4\1084083\140805" (Just AssetComplete)),
          (ImageAsset "\f\ENQVd\t#Aj7#\1107786" (Nothing)),
          (ImageAsset "nTW\f\137912|\175047\US\988402\&0|}\r\14982" (Nothing)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "Q\1110287\156143\147943\EOTx}$" (Just AssetComplete)),
          (ImageAsset "\987997\1040930\59400\t\NAK\a~\SO" (Nothing)),
          (ImageAsset ">\r\22592)" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "?<\1041571\NAKr\168534\SYNQi" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset ",-&b\150546\&1cw<={" (Just AssetPreview)),
          (ImageAsset "x7\b#" (Just AssetPreview)),
          (ImageAsset "\\\ACK8p\ACK" (Just AssetPreview)),
          (ImageAsset ")\a\40668@\SO'z\1082628 E#\1059790\993661" (Nothing)),
          (ImageAsset "\1033719\nl\5499\r?" (Just AssetPreview)),
          (ImageAsset "\EMu\DEL\1021612" (Just AssetComplete)),
          (ImageAsset "8\138730\n8:\ACKW%M" (Just AssetPreview)),
          (ImageAsset "!\DC4\SOH\SI\142581\a\188168\ETB|" (Just AssetPreview)),
          (ImageAsset "\994814\SUB\45923\1108043\1081324:\177542" (Just AssetComplete)),
          (ImageAsset "\98954\39273Ub" (Just AssetPreview)),
          (ImageAsset "v}\SUB\36339\1055932\n~y\b\USk" (Just AssetPreview)),
          (ImageAsset "\SIX^\1049463f" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "\1015676\135484\1018495f6f)S\132862\1075770\"" (Just AssetComplete)),
          (ImageAsset "Xbj\f\DLE" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = -20961}),
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("RZICMcCOzqyRTB0d17Rbsw==")))}),
      newUserPhoneCode = Nothing,
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              ( InvitationCode
                  { fromInvitationCode = (fromRight undefined (validate ("EXZtnNu96rBu0DQCJ_vGdZkjhH1SSzT2MAHgTQ==")))
                  }
              )
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "\185600"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.HU, lCountry = Just (Country {fromCountry = CG})}),
      newUserPassword = Nothing,
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_6 :: NewUser
testObject_NewUser_user_6 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "T+u\145108fe01`)S%{\1044883}u\24094o8-W\994850\n\"q]B\aU\13432F\136569\ETXn\a37\t\996717J4*_\1000083=f\DC3\29539\&2\1022153Go}\99405Lh\62174A\34546\SYNPM1v\1100506\FS\ENQ\1013929,\1046180\EM\1063966\FS9+9,1YT\SOt\164648\185504\NAK\ETB\STX\1009053\ETB%\GS\163626G\60181\&0{\1107077\127468\EMy\1001388.Q"
          },
      newUserUUID = Nothing,
      newUserIdentity =
        Just
          (SSOIdentity (UserSSOId "" "\1048112") (Just (Email {emailLocal = "", emailDomain = "S{\1020264"})) Nothing),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "l\1006930zuLAm" (Just AssetComplete)),
          (ImageAsset "l*+}\1068198\1089601\16651%Q\DC1\GSns\161000\DC2" (Just AssetPreview)),
          (ImageAsset "\1072536\USd;\SUB\1020396\1089337s\\fo" (Just AssetComplete)),
          (ImageAsset "\1032141ty\1010430v\1079758\&8\189006_Pc\1039009\&5" (Just AssetComplete)),
          (ImageAsset "{" (Nothing)),
          (ImageAsset "\te:q" (Just AssetComplete)),
          (ImageAsset "8]\1022516\&9\1076388\t\94949\NAK{uc" (Just AssetComplete)),
          (ImageAsset "\991477s(4J\ACK,\1048935JQ" (Nothing)),
          (ImageAsset "\181040\64401\v" (Just AssetPreview)),
          (ImageAsset "\161413'\bm\1072323\33136" (Just AssetComplete)),
          (ImageAsset "\CANs\16071\GSX_\DC2u3\DELYT\125222" (Nothing))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 30946}),
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("ZTf52SPQ5jk=")))}),
      newUserPhoneCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("7dJcoVoTKFo=")))}),
      newUserOrigin =
        Just
          ( NewUserOriginTeamUser
              (NewTeamMemberSSO (Id (fromJust (UUID.fromString "00000d6f-0000-2269-0000-6bf4000062f9"))))
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "`r}\DC4m\1090568/h\154808EP(-u9?qU)\1058565\a1"}),
      newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MH, lCountry = Nothing}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "+':l\10116\178360\nk|}@Z`D.\NULZ\167287;\SO\f\r\98388*l{&|\NAKo8\38228)+\STX\1047842\143079\SIF\DC3\997260\DC3R\52249<4N\1014692\1111353z@;\1006672#\1062918$}\DC1sAXuIWoB[N\1022681X\be\ESC\STX\42350$4\151613\1035680S)\CAN#w}3p\SOH\DLE\\?\DC1S7\1060798E"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_7 :: NewUser
testObject_NewUser_user_7 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\SO#KrlW\EOT\1003232\1001629wzLN.*BR\FSJ\1097013/zi[\161068I\59380n\r.)\1000284\1016099s\64768p8\"i\NUL\1109192\SUB\996411\SYNk\194709\145941n\EM\1033329\FSS\1017087L\38543i=\DC1\RS$\ENQ\168725E_\18633\f$u\157405`\SOH\1053974\DC1\1024150\1020127\995689\176993tl\38920P\GS\1951&<\ESC0\US:\SUBbm(![\STX~l.\r\1052270\167599)z\135412O\1067678\STX\1097218`\1051463|\EOT\1113413o3iy\132753\EMtSd\1022899"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          (EmailIdentity (Email {emailLocal = "%'\ETX\SUB\52073\a\1080987Ow\1002656\32778\991135", emailDomain = ""})),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset ",m1.I7" (Nothing)),
          (ImageAsset "\STX*RM" (Nothing)),
          (ImageAsset "sO\\\GS\14670" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "$.A>Z" (Just AssetPreview)),
          (ImageAsset "7r\1062899\1047197" (Just AssetPreview)),
          (ImageAsset "%\188437\1047569iW\USf\1058575\1077937\SI\ETB\":\157403\1034545" (Just AssetComplete)),
          (ImageAsset "\169156\1024923p" (Just AssetComplete)),
          (ImageAsset "\ETB\155117o*" (Just AssetComplete)),
          (ImageAsset "Bbt" (Just AssetPreview)),
          (ImageAsset ">^\"\1051300\120416\ETX[\1102249\179443" (Just AssetPreview)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "\DLEA\49671Ue\1011251\EMK$" (Just AssetComplete)),
          (ImageAsset "Lj\GS?\SUBt\145770\1084863" (Just AssetComplete)),
          (ImageAsset "\fL\96553?Pa\STX\1018010R\RS\ACK\1087347\a=" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 18148}),
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("34dAzMrB0CunAvR9")))}),
      newUserPhoneCode =
        Just
          ( ActivationCode
              { fromActivationCode = (fromRight undefined (validate ("1Yzr8-Lo2FnYwYYaJFeGEh3yaODV8pFYx3E=")))
              }
          ),
      newUserOrigin = Nothing,
      newUserLabel = Just (CookieLabel {cookieLabelText = "n\989384\&2"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.GV, lCountry = Just (Country {fromCountry = CH})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              ":Y\1064280@T\997084\1082084\SOH\169346\DEL~\1047751\n\SO\ETB\f\987766$}\1104943\EM\1094330\169942\191040\191304Al\1001917\96685\1107356\b,])^\RSsz?\1034526\&9S,M)\as=h\DELmpCp\SYNwS\1000690c)z\v=\26802-dU\160295|\DLE@\SYN\SUB\a\ETB,\f\14348\140286(L\158204\SYN\ESC1~B\SUB\1085576+P!{@\65454\1030464\SO4:G_\nI8\v\1016135g|\1051609Re\EOTE\CAN\1027704Dob3\STX\ESCA2\SO\45988&\r?\14985\&0M5\1104409,&ey\SUB\1024749QB\RS{\NAK\5351\&76\985196D\1059513J$\1080025qZYgd0i\GS\fbZ\EOT\92519\1084718D&m\"rM\10292\97295\n\DELhKg=7%Il\1099938g\1007473C}\47943\19042v\1026647 7!\RSrK c\CAN %A\EOT\147321\v\tIb\DC3W8\SOHi\174561\1108472|OfiJ@@j\1028789xO;`\1032453\ETBc#\r{\186360\998808\1001960\167999n \ETXIP\ETXY-\ESC\DLEEv\US\1087523\v\NAKzgx\SIE\STX\1025401'\SUB\GS/\983101DCD\FSj\DC1\\\144976\46785\1062381{1A\"\DC4-\1004921\&1\1048911\1076182wT\GSB-q1a\"$S2x;<X\n{(QN\16957d$P>\998957y\1031681\NULt\1059162\1054991J\19882\f\42778]0F( d\1095736~h\65008zt/E\4752\&3\30173W\t-\1046651Lh<\995338\1007432\GS\DC2B\DLE\160583\190017\1067598a\98184\1083343\&99\21570\US\DC3f\STXJ\FSc\16240F\CAN8\2623v\ACKm\1052138k\1074078U\DC4b\57660Ym\USI\DELV\167100\ENQ,n\bx\fl\\e\1028130;\150928\DEL\998735\1087406\DLE\DC1\994056o\1005388KF$\STX8\r\fZ\DC3Z\190875\FS\1093487_\990546:,\n \1079886 AM\159597\DC2w\39116WB\1015167\fCS#\153305=\1107374\988015pC-.\165512C~q?_O\ESC\1113113)\149323\EOT\\\SOH\ETB-\33022)\"la\EOT\148970HAf\1077042\1093914\49704l\STXd\t)`.>\1043695\60257u\163182\1097422c\999601\STX|LiP\1094711L=\993695a\836\1067062EE\1106112\1013947\ETX\1000293hQ\ETBW\1080539\94691G\"\43628D\DC4_\1056397\STX#\1087677$\194807\SIL(\1077025\STXt\1017609$IQX]p\ESC4&0\1110876\DC4\1060121m\SYN\t\\(^\ESC\118818,D]\1089937\164239\EOT\DC1IAd\33797\&0N\1029630\bg6\DC3\DC37\NAKk\"\1064942)pI\149677\\M\120101\NAK\30729\US\151204\&6~T?\ENQ\EMs\47424-[\137919w\4918Dq\157593\\\1041784%b\ACK9\1000877\&2CC\2853\1042497ojG\DELT g\53971\1007774xSeL\CAN\15010\SUB\985658'0:j4\71865q7\1000131m\ENQs\SO\1029677\ENQ\37674iG\68423\190220\5139wv\188572n\ESC\ENQ\US\EM\1077453\997058\DC2Ce) U\1074321\ACKfo\1051487\1107513c\1058590L4\DC1-\SYN\1060734L\1022647\185748\ETB|oW\ETB\ESC>\DELbD\NUL\98915\59361dm\67292vW\986731(2\CANz\1026512\27723?n\985498\1013509w\135214\NUL\DC4}\1101001|H<\54945U\53878\SYN\1038698\142287\DC1W\182543TM\ACK+LT\NAK;-!E>$\DC4j.\DC2q^\NAK\5537IN\1059744>V\STX\184118\1037245OA\127402\vvCf\169329n\NUL\66315\ENQ?^\SYN6\ETB\t8:at\"w`\1112149rl\19705\119110\174184J\993510~\1094824Q\996852Lm?c\DC2v\GSi~i\DC4\fG%I\US>l!\STXL\GS\EOTUW\ETX\1053673\\Bg\58292\13051\SUB\10530\&0EQM\NAK\1084309>\21299\vY\49643\152180\98008L%^\1000259r\164644%>\v0z\994733m\vEh(\180334u2\DC1v\1039286Q\61925\RSB\72711I+\ETXkaR\1002397=)\SUB_\1032377Tv\SO\58962%\183143M:\148104&\DC4\a0\f\"\EOT\128107{\1003812\ENQ)\SUBF\DLEN`08\187686\SO\SO\149986\1039718k\1069652\159769Q\US\EM1[/\1011611\177380Q\1058916\68755re\131476du*\1018976\FS~\8761a*4\DC2cr\997141b"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_8 :: NewUser
testObject_NewUser_user_8 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\STXO.\23742\97516\"\DC2k\EOT\995859C<|\98890\159067\34115\&2\SOu+`9\SOb#A7n\SOdr\1110736w\ESC\DEL\1104511x8\180094x\131506\993202O\146331\994882\3601\NAK\NUL4<q\94549<z\57992{K\166103Z`\NUL\1096538\EM,\1035594sw\SO\SUB\NUL\120368\&1\1077503\&2mM\DC2O\1008743L8\CANL\ESCY\154016\1095790,Z\fl\n\ESC;g\ETB=8y\t\1070349\59752\EOT\DC4\DEL\ENQQ\vE`\f\GSq\1053962s\SOS7J?T\74100\1014356!*2]\DC1"
          },
      newUserUUID = Nothing,
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\EOT\1027400\1053558/\1103909uIXl\ENQb\164699\1072895" (Just AssetComplete)),
          (ImageAsset "\DLE&" (Just AssetPreview)),
          (ImageAsset "FP\191319\&4@" (Just AssetPreview)),
          (ImageAsset "\131276\158426\188704\&9~\"\SI" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = -26710}),
      newUserEmailCode =
        Just
          (ActivationCode {fromActivationCode = (fromRight undefined (validate ("OXIgclTUyV_J5am0ZDqm7eLN-c8ecvk4")))}),
      newUserPhoneCode = Nothing,
      newUserOrigin =
        Just
          (NewUserOriginInvitationCode (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("1579")))})),
      newUserLabel = Nothing,
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.FO, lCountry = Just (Country {fromCountry = FM})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\1028098\ACK8pS\138548\v\151148G\FS\SYN'Z\149569\170000`\68079\SUB\135646\1094230L8(A9!)\33079n\GSz\SUBt\ETBj[%\ETX\ETB\65292 G@h7~\13116X~\SOanPf\1068087!t\72331\15978\ETBHy\DC3"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Nothing
    }

testObject_NewUser_user_9 :: NewUser
testObject_NewUser_user_9 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\SYN<h\985478z\DLEaf\1041872\1085582u\SI\1065831+\1036221!h\179451J\189801>\96807\1055650b\\#\1060016oC2-=+\97826L5\FS\\*L\RS@F\163064/\ACK9m4)\GS\at`\DLE\1013206\&8)s\ao\1075958n"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just (SSOIdentity (UserSSOId "L\171350\1007858*" "\1007850") Nothing (Just (Phone {fromPhone = "+71827573"}))),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "<%\DC2\51064" (Just AssetPreview)),
          (ImageAsset "\SI" (Just AssetComplete)),
          (ImageAsset "\DC2\1092208\171820K<\EOTF" (Just AssetPreview)),
          (ImageAsset "P" (Nothing)),
          (ImageAsset "c\EMY\v\SUB\SO{\ETX\988641\77928\135016" (Nothing)),
          (ImageAsset "P$i`\DC1(w`" (Nothing))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 24261}),
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("EGk4BobXeO_5YaLyYwCxuZv6-A==")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("ohTCWvY5M-J1AbWoFfqtjg==")))}),
      newUserOrigin =
        Just
          ( NewUserOriginTeamUser
              (NewTeamMemberSSO (Id (fromJust (UUID.fromString "00007c3b-0000-1728-0000-6e4e00005f30"))))
          ),
      newUserLabel =
        Just
          ( CookieLabel
              { cookieLabelText =
                  "d\1025595\1092941\&0\CAN\156310\GSZu552@\161324\r4/\1057226\fo\187210N\CAN\SYN&\1021655"
              }
          ),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.MN, lCountry = Just (Country {fromCountry = NP})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "(\ETXV\1014552\1039715\b\161164\1054965$~&\1038463o\1040090MTfC.\1021984;W\59323O\1096990\&6#{\n\1069751z\1056788K\30134\&6\DC4\1005672\136772(\158756\r?\58421Lc\CANxNE\ENQh/JeS\ACK\CAN\173642+60W3ySv\US@\DC3w\1090870\bB\SYN^%\EM\988823\146784iodDm%\127288\DC1]m\v_\21879\181210$\GS\188686\138708yt\EOTfm\SO'@#\DC4)\182806|\r&$*!%\n\ETBr]{v\FS\CAN\FS\1001993\b>\168328d@X0\GS8\158330>\r8{\99680\DLE\1069883\1026212C\NULk\1026454\ESCM^:\1069428WF\SI\t5Qi;}\n\1105500\14806\1001513\DLE\ACK*\DELPk\SOH\69696*\175908m\59197Np\24497@O\RS&\1025701F\995106tx\94547\60298?\1041368Ps\1070094r\1011316{E\ETX\GS\1104697\190467zcn\1017312e\SOHkr=\1036309z~e\\D\40996c\FS[J^\DC3\78393\r\188974\7150w1\1104314\991563\FS\179957\t\1013475\1020262;\173824 \vM\1108401IF2\ESC\191321cbvP\b0\33031q\57419>\1006526#%k\100443Zw\1001498\1010394y\25441x\DC2|\1050019\SYN@\CAN>\GSR\NAK\SUB3%\n\1072784\STX\1080467\DC1uOs\1107842\1065309\138346u\1073372a!\DC2\132548fWgk5\GS\1082431\r\1018566\42900\145709\f\RSydA\DELs=\1024217z\49659\FS\nxR\SI Ng\ACKC:X\146623\&7\f[\1024592k\v}\ETB5\DC2Wm%\1087573\165920,\147518\DLEGr\1074963ZvO\30720B\1005097hjYZiGd\bP\SOHGHG-\DC4\USy\ETBr9$\1039011Kmh\917606\&8g3\12714t\1015918\985504\1021587\b\"\9658\\sn\EOT>\DC1\190507\&6\134237\SI8k\1013568\&6F\129330&\CAN9\f'Bn\50546\FS\46460\NAK0H=\USe6\68136\US0\1062432L\FS\163483o9!\1045621\1094941\1083191r\EOT:\25240\492q1\f\1061443\5079\ESC5u5\160545>U\984651$\63797i\bP<b\nbw\998722\ESCx\1013064\164010\n\120653\RS\DELZ:\48581U\54963\1075536\DC1b8\5521lE_s\142311bJ\1100335D\SI\SYN#\a RK\NAK\NAK0.\93831*\RS\136680\RS\ETB\1065732\167255\74968NtgtA\DC1\1027351Zjv\1040126$\"\SIP\DLEJ\25862]\SOHu\178875\ENQi8\NAK)XHM5^\145824\181986c>\20545~\67985\n\ESC\1044063\ENQ2\DLE_%\160630\998641\DC2b^\\_\1087702CD[Rg\135191l\129188*\986155\128591\GS\1113693\USX\ENQ`\nM\SO\1068094\&0H*i\ENQ\1025344\ETX#q\42514x\nOaE*U\1008269GRw\983690]\1108560/#)E\1018205z\SYN\1090732\DC4\DC3\ESCS\EMo\178559U\tK\1086792\68062`\SOH6\54457\170975\1103293\127213:D\17530\54419\1017949\&7\171785y\1048490/S\NAKz\EOT}7HRV\1109054\1085537.FQ\1004585\64185\1106633\66592a;\1055002\1033858\ETBA\46470\1078667\n\70291\144315V\156365\1045649S\RS\bS%\1037382S^d\EM\b\1001776.gd\GS1!(\DC2(DE\184742\DC4\995808\1107107@\1048351m\1041999F=\DC4\986846\83184\22080aAp\136773\NAKf\1014163\989717C\23448\131724\1073590(@\CAN-\140976\68640\&4/]\1000419\GS\1085981\&9/E\150573<\133245`\21020#L\DC2\EOT2J\t\SIj@w\ETB9OoyN\DC1\ENQ\26865[d\1081420\1096723\FS\1038391F[\SOH\DC2\146772uE\1009102H2\20167H%5]\b\167790\\\SO\DC38N\62366r\EMe\acE\1101700S\SI\1075974\nt\fI\83105{\1020892.E*\163162z\110969#]z\1316\&6GM\b\1037989lb\b\t|RJ\DC3\1035777\1064843\169562|^\"E\EOTmc\1027583\64586\DC2@\1100891\b\1005566\1068304\998796\ax2w\DEL\30195\1059722\1110921\ACKO\1082461\1029952\NAKfp\ACKS,P&\CANVw\USqI^:\SO46m\68761rJyi\8091=D\1104645&\SYNUij\1058019\CANvi\"f\fX|\ACK\1018942A\48140\1083022\1043067YE\DC3\SOoh\145212\1088596\f\44657u&%\ACKv\101105k#\1108474`\1076522\f{\SO,\t\CAN\ETX= \1093724 ?\1068395\DC1\SO\167150\&7U\32304Y\FS\1072260\156532\176131g\SYNs5B&\SYN\f\NUL\EM\177193\ENQ\US\1040385+L\DC2iE^.\1111352|l\SOHb]i(h"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_10 :: NewUser
testObject_NewUser_user_10 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "nz\ETXZ\CAN4>\1029814\190555ocxZ'ngJn,u\155624\1017945.\1013797\DC3c\EMG\1036614\1050144 \1018072t=\\|>@bc\1056933\3897\\\182097*#\1004444\NAKEuR\"\133815\US~'\EOT\46779(A8m\1092697\131539F._\1086413\SO\SI~3.\RS\132761\1094333\DC1o\DEL\aS@4\DC1VT_k\1071840\137558D\DLE:\12462>U\11281\989076YkzU\1018004\172591uejP\1069374\7378\986798\DELn\ENQ\CAN9~f&"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just (SSOIdentity (UserScimExternalId "\n\STX") Nothing (Just (Phone {fromPhone = "+97450398211"}))),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets = [],
      newUserAccentId = Just (ColourId {fromColourId = -2576}),
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("Ofo=")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("g5W5rIJFTLUrJkd9j9W3BzbyLEnW")))}),
      newUserOrigin =
        Just
          ( NewUserOriginTeamUser
              (NewTeamMemberSSO (Id (fromJust (UUID.fromString "00003fd3-0000-6075-0000-09cc00002b7a"))))
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = ""}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.SR, lCountry = Just (Country {fromCountry = CO})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "g]:\1018228\147738\NUL\1072954\CANL\1081335\GS`L+i\9363,&Y\NUL*\ETB3En5C\RSI(\NAK\143207=\a&B\1008147c\vj\NAKC5\FS*\143391\36838h2\988019(\31555\990993\&6r\DC2\DLEQ\137143m{\1099384\178081Y#\1029034L\ETX\51506\7090\1081898\32626\183117l'7/\31432\NAKR\SOHT#InO:F|j\US+<\t}j`6\92250wl\DC4-a]\1033485 O(\ETBX\984652?!\177845\DC2,Q\1009349\69773\FSO\20790\1005567{+\DC1\NAK\DC2\45601\flk\ESC5u\1048485{B\vg\ETB\44152\NAK+\1091805\1082527\\*mUn$\DC2m\1106217\131352\35492\&03\1059109\996291\139527\&2\DLEBE\SOk\166992b\1024979{\128262jv\1015451_\3961\1049226\60155?/V!YoY\1038377\ENQ7P\1053628(nf\1087594+\FS\STX/\DLE\7114;\1061175\DLEf\175191_Qsvt}DFm\DC3_\1083208\f\a_;\59884=E,6|cAH\a\6402\68130y&\184374\168795\25974\ETBo\b\SUB8/=!\SUB\24280\EMGD9+\999661'y\13222\SO\DLE\SYN\DC3\NULZ5\ETB\27546\DLE\SOHsMf5KAO\ETB{J \a\44984\142292]g|\DC4\1016043,4\DC2\137758\98702\11389\&3\1101728az\13963e}\156069<MDg1^\GS)\DC4r7-\bE\1072301\&70\1035450v&\60112\r\ESC0R+\1094716k#`\US&)w\NAKdn\GS\NAKY\r\52891v\DC2"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_11 :: NewUser
testObject_NewUser_user_11 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\1034520\bb\\\992528\177372\62005C\1098702$'\170711wG7\SYN\RS'\ACK\SO\"0)\46816@\FS\1044451\nay\1021781\173747\1041251\SYN)\183552\ETB\1078875h\167293\1090915\129106:\a\1045946\ACKu\NULt\1084340EK\40162\\\153013\174793e=F-\1069484<E\188978ov"
          },
      newUserUUID = Nothing,
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "Up\DC2\1078295Z0\NUL\1075511E\42392rn\1048039" (Just AssetPreview)),
          (ImageAsset "g08" (Just AssetComplete)),
          (ImageAsset "\5175" (Just AssetComplete)),
          (ImageAsset "T\58560\b\1017198\FSaiS" (Just AssetPreview)),
          (ImageAsset ":\983950\1107314\SI,\119257]" (Just AssetPreview)),
          (ImageAsset "S^6\1086331\28918\SI1\GSRd\128696\18188" (Just AssetComplete)),
          (ImageAsset "\NAK" (Just AssetPreview)),
          (ImageAsset "\ESC\57836\187420m\SYNy$\1091617\20344T" (Nothing)),
          (ImageAsset "g" (Just AssetComplete)),
          (ImageAsset "0\FS\27710\&334l" (Just AssetPreview)),
          (ImageAsset "\44819C" (Just AssetPreview)),
          (ImageAsset "\1038017\35297)>\EM1\65465Or\1105620\1091998\1067761v\NAK" (Just AssetComplete)),
          (ImageAsset "Z\SOH\72857/\SO\28423s\\H\EOTE{\r" (Just AssetPreview)),
          (ImageAsset "eb\25344o\1025305\CAN\b\1018499\SI\995488n," (Just AssetComplete)),
          (ImageAsset "LH\1059755\EM\NUL" (Nothing)),
          (ImageAsset "W\46312B}\1056938\1082969](\99087I\1045191" (Just AssetPreview)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "\FSHb<\162081\1068855Z\\" (Just AssetPreview)),
          (ImageAsset "\GS\f\US\95832\"\1066625+" (Just AssetComplete)),
          (ImageAsset "5\1038310" (Nothing)),
          (ImageAsset "\168685j\SUB\21288\1005667\1043496\1034542\CAN\vN^\147572n" (Just AssetPreview)),
          (ImageAsset "X8&r" (Just AssetComplete)),
          (ImageAsset "H\184627awc\DELA\1069712\&3/" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 20313}),
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("GBV8H4iTzUI=")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("DjbUAo-oR5c59MDxSrR2TA==")))}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("hYCkzvFW6QzlqvDBJwPh")))})
          ),
      newUserLabel =
        Just (CookieLabel {cookieLabelText = "\992217\999846\1085439\SYNu\SO4M\10098\rS\1093686o<9wHa\23265pxst"}),
      newUserLocale = Nothing,
      newUserPassword =
        Just
          ( PlainTextPassword
              "Z\62152Jg\167486\142606\158743uY\DC3~gq\1015435\EM\45450n*\DC3jE\ENQ\STXGUQ\STX\CAN\120562(/\a6%FD\1076127|]\f\FS\1000881<e\SYNMVBu\16624M;\NUL\SUBW WaI]\STXa\39894\\Rw\t\SOHh'\DLE's\ESC_\n1E\1048279a\NULO\169966\37537\SO\r\1063764\NUL_\SYN\tDJb\155909[\179100\1111491\RS#/foa\21405\50509B\40695\DLE\992670-+\1036159N\7787L;\135393\162965L\1079678\135931ktE\vj\78845*\1064806V\NUL\157926wn.(d\DEL`zIER4\155633\12236W\131492\1032505Jl\135873\1029706sy\188055\3592\DLEY\EM\59616\SYN\159820S|d=@/qSjX\DC1.\DC2F<\157733\SOH!8\1074476/v>\989197F3e\148090\r\159326h4\NUL%]\66721,c2S\95053h\142399\174158\ENQ_\1076170_\20559\nQ\DC2fHy_\EMpB\1095370E\DLE%\f\"\1108839*Y-\DC21\175061o=4A<4\1108943&t]\1036846kDM\DC2\61414Q[\985741\RS -yq|\156679zIt_\DC2mRT9\1012059$l\1056258\SO`MQ\77905\1051266_\ENQk~N\167077h_\EOTo\1098570Ub\EM\a_\SI\1012786f6'XKDLq\1043985\&8\1095647\61921\NUL`\994308\DEL\ENQkQ6Gf\FS\1016438\DC2<pi\119166 #\65258Z\37898\STX3y\r]RH\DC2v\1077559\SIC\ENQC?W\1072833\175300e\8917W\984489\48557~]jB\188989V\33690\SYN\DEL?a\57523P^\1099791\ENQ\1029633,\174154aD\173191\GS\1032850/ I}\t \21301\1040267\DC2O\b['?|wH1\1072979OUS+\129367\EOTIU\1106598\f\DC4\NAK[<\161849\172739\EM=s\1069090\vZl\SYN\SUBBhbY\1013298]\ENQ\182282N\NUL-60@\46354\1009375\1012468E\DC4c\30268\151011\143156LPM\SO]X \155771\SYN\1075206\1032833t\1026590\100874\EOT\1035422'&\n\41518\49821b\f!<|_\r\50702\DC3\187622\ACK{5\GS<\NULJ\1046630OH\166503;sy\18607LK\66727h\991191f\FS!\1102200]\RS,N\188357\ETX\43564\r\1015559\167947\1022140\vYVk\ETB"
          ),
      newUserExpiresIn = Just (unsafeRange (378975)),
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_12 :: NewUser
testObject_NewUser_user_12 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\13652\ETBY\DC46B^Ds \167612&\DLE\141739_\ETBi/\ETX\159121\143368Z}9aj\37815V\fZ\"\DC2 \SUB\t\1044582\&1fx\182021\1020489\ESC\143705UQU0\186046\ETBvRS\98713\DC2h\EOTx\138598\n\1031030x#-\1067389\985743_\STX\996559\\\GS\142507\1054277\170327saBlO\DC2J.p\EOTnoU2#\DC2\SUB\1077376`\NUL@\57616\DC1\62359\SUB"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          ( FullIdentity
              (Email {emailLocal = "]\1019696,\ETB7%\EOT\DC45^5\96793\147391\GS\17237", emailDomain = "xW)\ACK"})
              (Phone {fromPhone = "+504111413588"})
          ),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\DC1c\172520\SUB\989837!\190031\b;\DC4u" (Just AssetComplete)),
          (ImageAsset "IDVH\1067029G$kZ" (Just AssetComplete)),
          (ImageAsset "ka\DC3\USD\1071595\49776_" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 23561}),
      newUserEmailCode = Nothing,
      newUserPhoneCode =
        Just
          (ActivationCode {fromActivationCode = (fromRight undefined (validate ("AQmcp-TSIeo05ixCVAzzxMGsAtTH7l0=")))}),
      newUserOrigin = Nothing,
      newUserLabel = Just (CookieLabel {cookieLabelText = "\\\996278\1012166tn}<\42069\24152^\DLErAi\1057390j"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.BO, lCountry = Just (Country {fromCountry = IL})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\ENQk\141146\1077511\156911\154175[\n SW'\a\46886\DC4\a\1048488\&3\1081820(h\1063163\1076261\SOH\NUL\DC4^1$\SUB#XO\1076051\132367\48055fJ\13103Nuo\a\20882\&9\154450xL[l\990097k \52177\GSe/E\r g\DLE8liG\1051318\f\"\1113790\1046748raLD,\1049649!={\ETB[\n\1109617L))\EOT\ETB\tT\139974\1066102\1043565\STXL\SYN\148946=c~\SUB\DC3V\68900\&5\164575\GS\1085482\DLEWZ\STXgLh&h\3096\nb\b\65492\GS{gfR\FSp\r\DLE\t\CAN\1034740[\r\171914\1052978\SO\SUBJ\94041\DC1F[2c\165043$\1007358cwB6P\1079137\174679\&9B}:\180273\&3\36996[7\DC3OM\SYN%F\38953.h\72881\126631(\f\178890N{\GS&*\SUB\t\DC1\999268\1021777\1077901\27569$DO2\47327\1013392\1007630\53105<pUO\CANm\ETB\1013861\ETXD\ETX2\fL$\25304Zi?Z8N\151369E\\D\1030951XS\57749]\DC2,t(\190488\94775^W\RS\1074986A\998331\DC3l_\1067762\1029361HM\SO\DC3S\1022262c#\SO\36954\188684\1040571\1004573|\78215\EOT\171263\1006572\1044476Z])x\16229l\"nFY'PI2M\STXf\62492O\188970\SYN^*>\1064620\NAK$8r\136852u\183590b 2\1029223\SYN\153017}A\SOHY\1034463S\1007690#\ETX\SOH(u\\\163441T\SUB!\1058033h\DC20Xx'\132409!\NAK\DLEA9%\ENQ\DC2p\151481\181687\1008617%S@@\59016!d\13014=l\EM$@% h\92260@6;U\1046710@1b\1066220?g;s{'\168073*v\1003595{+.\9515Q\132528u\9480:Vg4]:\138468\&5\FSz4\32015\1110508\31687\991048\&2!x3J\1048020Sd#X\NULx\DELJ\1070285O5\f\146902f\EM\bA\fU\159778$\STXC\1016855\SOL\GS\US\DC4.12\158083\DC1\153984\195026eA\STX\nX\1044171\USW?\CAN\1064050pSCi0\36595\23764^\SYN(\GS\49118\DC4\1087107\SYN\ETB\1027341\bZ\10490\&13d\1025117S\1080777\&7\97003\996022*LQ\186591nS\SUB\ETXL\ENQv\b(\1070909S\70743\SUB\1016069)BT\170425\DC29\ETB;\178760\134408'\1048016&\21025\1022557\CANd\1082274J\1099355ZdoMR+~<\191335laS\73444l\1031779\r\166749:\1102447\60092f>Fe\1076015i\SYNV/L@\1043545e\1017778\&7\SOHv\DC19\1025751!L\DLEX%\GSjx\61605\CAN\1076090\SYN]w7W\SOH+\1029832E\DC4:\28199\156994\5439\CAN\r\SI\ENQ\16339\RS\SYNXb1\DC2Et\188031\13695Ez\1051017\USo\DLE\1023562\19199\175912e\190441\&8\SYN\CAN\1008838{\NUL\18622jF\ETB,\1111678\190629\178374U\t\1105212Ku\1101900I?\119101p\US\ACK\63584MS*\157356\138620\39752\983333 \1061680\1671\1026251\146337\aU?\190046V^\ACK/:l-.Uzz\SYN\tm#\1065339XcY*\1104092\f\1063451+\US\100984Y\994009\&7\DLE\SOHrL<+K\1058219\1107452\1015133$e\6558(A\FS\n"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Nothing
    }

testObject_NewUser_user_13 :: NewUser
testObject_NewUser_user_13 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\1104031mx0D\41340\1061121\1002725\DC2\a\bW\DC1PY\n\153502A_\154481jiL\NAK\1043244;~_\DC1y|\SIp\EM=\EM\148317\164702K\EM\bVV\ETXl\SOHp\1002077V\DC1\179321\&0`xPp:d\1074886\ETX\1008945\SI\1067394Kb\992920\1029412\f\141426k\1110100\162484+\DC4\ac\188651\188058G\110929\rC0;gKQ\30166:Sl\NUL\SUB\SOH\FS\DC2T\60299Bm$\GSO\NAKBRw\21414\CAN\1019564\15840"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets = [],
      newUserAccentId = Just (ColourId {fromColourId = -32104}),
      newUserEmailCode =
        Just
          ( ActivationCode
              { fromActivationCode = (fromRight undefined (validate ("8XW3_eppEXD038Pz14guN-pLnL6rR1538NZxVA==")))
              }
          ),
      newUserPhoneCode =
        Just
          ( ActivationCode
              { fromActivationCode = (fromRight undefined (validate ("byyj1Wn5lFbsGpe3VZ7pERkfpOujdm5UzTIy2ok=")))
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
                                        ("[3Y\RSF$r9,/1\DC4\51722*\24329\DC1\1091933\34031I]\f#\CAN\EMITc\160631\1010777\1054846\ESC#\t\1042500ic\bU\1058195\SYN'\1113521\35874\&0xC\1045798\ESC\156725\&1U\48911</i2\f\DC3\CAN)\EOT\vB\1071726Gv\174968L\128008@5\ETB\23326M\r&o\1612\DELHBK\5449\149144\EOT+\EOT}\1013480\v\ACK\ENQ\1113960\STX\SO\\\1079196\SUBmN\128909\RS~\1109470\987495\&8J\DLE5I\185510O\1034052\ETX\CAN2FZ\1021485B\DC1\SYN\32880\USh\83273\28495\12445+\\V\1033050D67\1108459C\166244R\DC2\SUB\1025189pJTN]\18736\ENQj\150747jAy\DC3r\EMzLH\49212en[F\97166a\4696\&9I*\EMx\1003075n\DC1ZZ\\\GS_ wwM7\rM\6036\&2M\51668\1018388y\f!\21794\ESCV\v;\1044545\vq}\53047J\SOH.S\1027791\&8\35311")
                                    ),
                                  _newTeamIcon =
                                    ( unsafeRange
                                        ("KH\DLE\32367\"\988721\\k$l\189156\SI5[rG?\1113410\52412g^\1036176kW\1012919\&7+\1103295\DC23W,b\7985#W)\\\1006957\1032966\1040891K]s\v\160741\\92\DC3\STXL\22385\DC3O\1029181\NAK\35901[\1107699E\1067436\23185")
                                    ),
                                  _newTeamIconKey =
                                    Just
                                      ( unsafeRange
                                          ("\1027101\165486@\DLE\176973>'\DEL\165612/\vV<^zu\EM\ETX\ESC>\174231\95943A")
                                      ),
                                  _newTeamMembers = Nothing
                                }
                            ),
                        bnuCurrency = Just VEF
                      }
                  )
              )
          ),
      newUserLabel =
        Just (CookieLabel {cookieLabelText = "^\DLE\1091089\1018879\t\147176\&6Th\99794\1105207I<UgH\177526\174518C"}),
      newUserLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UZ, lCountry = Nothing}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "K\bU\983530W\1078491i%\SYNi\b\1068543\SUB\ESCP\1001191N\b\1039326\SO\naq\DELJ\145270eH\USH\51329\1098337\1097742x\1055191\1013665\"}: \181348\"\153216\1012651\&7lZ.\1063389H\t\181919l:rt4x4\171151,\987295`"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_14 :: NewUser
testObject_NewUser_user_14 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "U\50466\1035087\n{\ENQAPw\160474\DC2\993178\1028016\95490\182038\n\DC2\EOT\b{P\DC4\111096\&7I\1038433^I2M\46982K\991481\23908.k\CAN\DLE#:b\ACK|i.M=\ENQo\171035\DC2l\SO"
          },
      newUserUUID = Nothing,
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "6\139182\SUB\985645:\ESC\v\1168\SUB\1042249" (Just AssetComplete)),
          (ImageAsset "R+\19018\16918\r,i\DC3Y\US" (Just AssetComplete)),
          (ImageAsset "" (Just AssetPreview)),
          (ImageAsset "0?\ETX?\15519\&4" (Nothing)),
          (ImageAsset "*\150015\ETB" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "9~|e({\ACK" (Just AssetComplete)),
          (ImageAsset "\DC2\120040L\DC3W\1064231\59866\1104069\&2k\1103442~*/u" (Just AssetComplete)),
          (ImageAsset "O\RSO`" (Just AssetPreview)),
          (ImageAsset "]}" (Nothing)),
          (ImageAsset "[\ESCj\1044731F)FH\1006688Z\1104159\1060268\&1\ETB\"" (Just AssetPreview)),
          (ImageAsset "P\1035693B\"|\26783\180935\133259.dy&\1066916\&7," (Just AssetPreview)),
          (ImageAsset ".\182408\1023811a\41696nd\n" (Nothing)),
          (ImageAsset "\DC2Uw\ETB\58037\EOTZ\1017311\DLEy" (Just AssetPreview)),
          (ImageAsset ":\138059\1002457\EM\SYNS" (Just AssetPreview)),
          (ImageAsset "aXP\132149" (Just AssetComplete)),
          (ImageAsset "\ETB^ \985149W<0\45404=" (Just AssetPreview))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode = Nothing,
      newUserPhoneCode = Nothing,
      newUserOrigin =
        Just
          ( NewUserOriginTeamUser
              ( NewTeamMember
                  ( InvitationCode
                      { fromInvitationCode = (fromRight undefined (validate ("hhaIkzK5fTdyr4CLcPzJg9oj1G2WpaOQ5Q==")))
                      }
                  )
              )
          ),
      newUserLabel =
        Just
          ( CookieLabel
              { cookieLabelText = " U\1007692>\ENQ\DELRd\1111509\137004\"$p\178290G[\SUB<U\DC25r@v\ENQn\185777_}"
              }
          ),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.LI, lCountry = Just (Country {fromCountry = UM})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "#\f\NUL*0\1046860\1077540h,[\50073EvR]j(\1016193$ F\CAN1\1107563;u(H=\34026\&35E\1004502\CANPy$^;[\DC1{\NULdf\ETX1\42195$D\1065845\ETB\1065954!@\48944})vF\t0q\48991c\99877\FS\v$0\SI\1091534\1078803h\SUB{\987764u\142559\10845\DLE\SUB\49526\1070606}\127137P\DC4z\"0\78877\1032796\1079044,\a,1\147551s\138413 \21590\NUL\1081701\157126\&8\163801\1112127\&8)\vEzph\1075772Z<\179904\147939eM\DC45\DC35,>W\b\DC4>\1051877u\f9\DLEU\120276W8/;[f1\CAN3.\70062Fw1\b\SO=\1048027Q\EMlgZPCjAFm\186637\&3X\ETX\DEL&\1026815\ENQu\1054670\92320\1082565\1001577CT!+st#%\994498'W1~\RSD\1027704h\146538\16295\\\FS\1101888C\98469R_(\b\132607&>\RS;H@v_\20372\1045195Hg+m\1005562w`\1107567\"\1077448B106Q\988463X\180875\1034341\165415\1012684P\DC41\42060X\1058472\1018704?VD\160350\18122\SOHii\1072326\EOTnD\16918S1\66439\v}\1099142\54994>?\169978T=\n\CANBAi\30089\169825\f,&=\1029383g\1027278f9\"\1052460\NAKpe\985368\ETB\1112751\156981\1019142\183563,\NULj\149084\&1\18217I\1012358[<*S]<7o\7684\1039271d\46743\1061911\&6y7\47520\SIEeE.S:_$LBG\155423*ci5\20438PFl\174281\DC4E0b\STX\12809/;\28506p\DC1\69242\23111;m\b\DC2P@YBve\b\EOT\1052122v_6/\DEL\8574\\op\39368H$0\1029769\EM$Y\aP\1092884\1030008*\EOTfj{\DC2'3*\64962v\146042UW\SOHm\1054837\16901i\ETB\b\26410!S`w\ACKhJ\b\1005244/Deu\181312\1075594\CANz["
          ),
      newUserExpiresIn = Just (unsafeRange (143078)),
      newUserManagedBy = Nothing
    }

testObject_NewUser_user_15 :: NewUser
testObject_NewUser_user_15 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              ")\SYNV%9X,%\153866\RSX:\SI\tEt\1039185C\58466{\SYN\SO\186803X)!ns\NUL8Pf>P\DELW1Il@Koo\1074032\EM;\984619H7\t|],C!lf~_\1002300U!\1096857\1013210!\FS\DC2s;\DLE \170551\1017226\168711\US\RS&[\NAK\a\DC2\993754S'W\DC3.~\SO\1062156\1013546Q\68444FFxnmc<q\74150\1063946/"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+430123790"})),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\1763\1098644\991092\846F\EM\1020927A8\1004904t~K\DC1\57995" (Nothing)),
          (ImageAsset "}ei\160591" (Just AssetComplete)),
          (ImageAsset "{rF_\ACK\1075151\SOu\EM-6" (Just AssetComplete)),
          (ImageAsset "`q" (Just AssetComplete)),
          (ImageAsset "%\27843\t\1089891\SOPb" (Just AssetComplete)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "\1059394\NUL\ENQ\118836cdCD'" (Just AssetComplete)),
          (ImageAsset "\DC4\SIw" (Just AssetComplete)),
          (ImageAsset "lC\DLE\ACK\127481\1109286\998176\&02\1055049T\97550\NUL\1064796" (Just AssetPreview)),
          (ImageAsset "i\t" (Just AssetPreview)),
          (ImageAsset "\133136F\62362\356\1050825\&4p~\r" (Just AssetComplete)),
          (ImageAsset "$\STX;\tX" (Nothing)),
          (ImageAsset "d\SUBe\FS\ETB\983808\&4\US\RS\ENQ" (Just AssetComplete)),
          (ImageAsset "O\35636G\100534/~TYE\ENQ\1032530K" (Just AssetComplete)),
          (ImageAsset "6K.^\984437" (Just AssetComplete)),
          (ImageAsset "\1014791" (Nothing)),
          (ImageAsset "\72416\\\v]0m\147086bO\159683@\ETX" (Nothing)),
          (ImageAsset "O\169836\&5\174500H\1019633\CAN\155271" (Just AssetComplete)),
          (ImageAsset "\1004018\DEL\992136\1019394iMl\1013042g\f\EOT\34124\1102696" (Just AssetComplete)),
          (ImageAsset "h~\24798le\984981\9548\&8\vaV\DEL~" (Just AssetPreview)),
          (ImageAsset "2!\1088493\t\SI\ACK\172546\5694o" (Just AssetPreview)),
          (ImageAsset "a\1096873[\58626mz@?P^" (Just AssetComplete)),
          (ImageAsset "Y\DC4c\1026742" (Just AssetComplete)),
          (ImageAsset "\140514\166184\\\r\\\987061z\DLEv\ENQR\NUL\EOTm\66357" (Just AssetPreview)),
          (ImageAsset "\aL\1007691LhVA\12992\182143<b" (Just AssetComplete)),
          (ImageAsset "\1077350" (Just AssetComplete))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("pFdCBFBAtFfC")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("8VeH6NTGEGaO1xqt2kUhVxoO")))}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              ( InvitationCode
                  { fromInvitationCode = (fromRight undefined (validate ("fzNEyAxbDTJfagCaVbatNQwszvNSxwsW-A==")))
                  }
              )
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "\1004423E`\RS\998815GjC]YD6\DLEHZ"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.UZ, lCountry = Just (Country {fromCountry = RS})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "Tf;\1035811t}\r\SI\r\50481n\rl\170365T\37904,1\1032826\r(~\SYNK>y\EOTLxr\7126`:\1014165] x\7030\DC3\36172% =\NAK\f6S\GS\1027285p!\a\135857(;!$\SI]\US`5\DC2\1023998\1090553\1073462}9=O\137704\1005594(I\CAN\EM\CAN\188480\1040031]'\22874\DC2\1105670\1083882@R\SUB\NAK\99642\&3\59901\&26\ACK\1105828sy\157383\63920Kp\ACKB~M\1035845\1054255ty0pJK\STXzv eWY\1100908v\ESCG\CAN=\1008262\DLE[TZ\1068423\178016\991067`\95931\ESC\1046608d\EM\b\RSd~\EM\1016209\185831\29201L\t,A\133192\SI))Z\ETB{\137969A?\NAK%hS+\150348\5388_\148122:dR,j\141055(_'j{O\133201\\X\1010345S\1049262wTD\1033443\160414R\23522\DC1\DC1\1099627:L\1085899p>\DC3\23704\1028087\NUL\1020105\1078456A\1089417\"g+PD\SOHtk?N\1013241\52065\170977_AdL2\1016996./b\ESC\t)\FS\FS7:\ETB\1025682P\aGEwk\1093215-)\DC3p0\nUG\ENQPV=\40816\EOTM\1066630_js\ETX$2^YaP\1039158\20719H@j\18424r\188903Y\DC2*u\166575\1019772-\163220\FSf~ZI[5\t\ACK\178865\986816\144912EH\137316J\8650Tun\1088202\ACKuO:`(0\161605\987465]_\999266\1005299\t\STX\r\40097v\169184o\1073608\1040194h\1005053\1112894\146105! \n\SYN'J.\1029280\995990\v4\r{\NAK\NAKbPQ~\1092014\&3\US\1078011<\988690o`q J\48946]c\"\1006277\v\1024510Q\1048094\149009\989881\36552N>;}\GS=TO\US 6\GS\SUBWy8CZN\46163I\SO\1108285\28058P\45187I7l?\b\DEL3\STXv\13853\15753\RS\1094576\SO\\\1104977\ne\46874\160230\&0mS\CAN\1020423\157180ng\1094315xTt/\50705Id}b\36346\141394v\ENQ|;C\2360Z\67294\v&C\SOE9O5\ESC\1092158w*}\EMQ\178541\ACK\1076279\149460\1045852w~\83091\&7TL_\nH\1058057$\110864\1080351\ENQUPQJ\15524\RSS\1067353\r$\CANU\1036973wT~:\49728/\128381`&\137102(\176521\vn\DLE1\142173\SO\1050330)M\993353\1071867<'=d\DC1G\166805\b;\ETX8g,;\189792Z\1025221\DC1\991266{trA\178621\&5Xe.\142518,'R\SUB<i\65160\US\989519\ENQ2^ s\123146\DELuL\92223\&6s\41593\&1t\SO\CAN+U{D,U\1104873\45560Wka$\1039424d4r\n>\32889N\1050084\149070\136431\1061037\1000339\ETBmW\1094041\ETX\t\1011082dQ\97197d]#\EME;\132675Y1\NUL\1029979\SUB\RS\1048292\10876\a3\146447\n\45281\1010335a\141069s\ENQLZl8Dq\ETB3Vp5\"\t\EOT\1011270n~\1012295\1111143HD-\1039481\983889\180948\ACKuQj\1078022\FSx\31208\ESC\t$O\v\989459BI#4\997454\\\1075613\1051318/D\ESC\1085849pX\1001950f]%S2\120271z"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_16 :: NewUser
testObject_NewUser_user_16 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\STXnH\917551\DC3\1069710\DEL\DC4\n\149984\1024736\149590\EM`I~Q{\\:\994054\DC3H\GSem\1058165!\1004899\r\SI\DC1B\1090802i\GS\NAK\ESCY\161298\1050465\36573G\169606UOB*Q\GS]M\SYN"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity =
        Just
          ( EmailIdentity
              ( Email
                  { emailLocal = "R\138103\51554\1093578\nTAS5z|q\64031z}\12959;\ETB8\DLE",
                    emailDomain = "\66578\131096ZBqrtK\ft\1051009h\b8iU.&"
                  }
              )
          ),
      newUserPict = Nothing,
      newUserAssets =
        [ (ImageAsset "\DC3\1047857\ACK!FC" (Just AssetPreview)),
          (ImageAsset "f{\ENQ" (Just AssetPreview)),
          (ImageAsset "Hl\SO\n8\ENQ~\EM\1096241B" (Just AssetComplete)),
          (ImageAsset "\1109662m\6286\165081x\DC14c\9896o%:" (Just AssetComplete)),
          (ImageAsset "\\57FFJ\1066532\&8F" (Just AssetComplete)),
          (ImageAsset "\984281\DC3" (Nothing)),
          (ImageAsset "(E\21704hxM\"_" (Just AssetPreview)),
          (ImageAsset "\SYNQ\SI7z.zs\68119Z" (Just AssetPreview)),
          (ImageAsset "\DEL~$" (Nothing)),
          (ImageAsset "h\ENQR\1003331\133393w]sc\1029711\51538\DC1`!" (Just AssetComplete)),
          (ImageAsset "p\SI\173440" (Just AssetComplete)),
          (ImageAsset "s,\1061760#\US" (Nothing)),
          (ImageAsset "I(" (Just AssetPreview)),
          (ImageAsset "\STX7\f" (Nothing)),
          (ImageAsset "\ETB\1087559\12770\147491\995554#2{6Gwz" (Just AssetPreview)),
          (ImageAsset "\\\1047650\&8n[m\119169\92305\1027086L\168740\SYNY\7424'" (Just AssetComplete)),
          (ImageAsset "\NUL/dn\DLE" (Just AssetPreview)),
          (ImageAsset "\1046223\159089\&8i\SUB" (Just AssetComplete)),
          (ImageAsset "9=3i=5\CAN\DC1" (Nothing)),
          (ImageAsset "$\NULe\998875\DLEP\SInN" (Nothing)),
          (ImageAsset "#\SI${q4]s]\1032167\1089192" (Nothing)),
          (ImageAsset "Z\129445\62976\8937\127944\ENQ0F\178204(" (Nothing)),
          (ImageAsset "\1048007v5\997299'" (Just AssetPreview)),
          (ImageAsset "3\b\US\988616\156638\1003003\CAN\SOH?\176124\SOH" (Just AssetPreview)),
          (ImageAsset "rZ\133817l\186667k\153956\v\1088523\32361Jt\GS" (Just AssetComplete)),
          (ImageAsset "2\137752-" (Just AssetComplete))
        ],
      newUserAccentId = Nothing,
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("sQZw_A==")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("5dy6ya1yHl7TyDF03xClFXFT")))}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("krEHX0N2tg==")))})
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "\163041\2569\ESC`c&\159184\52923\&9b\t"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.XH, lCountry = Just (Country {fromCountry = TM})}),
      newUserPassword = Nothing,
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_17 :: NewUser
testObject_NewUser_user_17 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\61160A6\DC3\169031\167858O\SODu5C\1012451\SYN\1039893K3R\92574\1014127\GSl\1033424\tl\aRm-\1011199\SOH\92715&z\23438\&8\1077050Ho\155446,3\50868s"
          },
      newUserUUID = Nothing,
      newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+479088519197"})),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\1054929\1104670\1026527)D\1091434N\1099702?" (Just AssetComplete)),
          (ImageAsset "\1077899\&6;o\DC4\t\\\DC1\\\1078560\15754`R" (Nothing)),
          (ImageAsset "''@\ETBK\EMi*YcomF.\EOT" (Just AssetPreview)),
          (ImageAsset "$VX\US\ETBM:)f/" (Just AssetPreview)),
          (ImageAsset "\SUB\997723lkaU\FSy\1011749s" (Just AssetPreview)),
          (ImageAsset "$}\DC4W)\EM\127827\&3s" (Just AssetPreview)),
          (ImageAsset "w`\53952\SI\1022516\SYN]P>J" (Just AssetPreview)),
          (ImageAsset "\v=d\1040737\172335<eL6" (Just AssetComplete)),
          (ImageAsset "r\\KA\164287\1062708{T\ETX=N2gw\GS" (Nothing)),
          (ImageAsset "\1006064g" (Just AssetComplete)),
          (ImageAsset "\b\30106\78080}\DEL\78186\RSKL t\83207VJ" (Just AssetComplete)),
          (ImageAsset "p[g8t\DC3Z_" (Just AssetPreview)),
          (ImageAsset "\38770&f\1088894)gl\1085570]\1053416\"" (Nothing)),
          (ImageAsset "\1097146\SI" (Just AssetPreview)),
          (ImageAsset "\SYNUU)\31500\DC4.y\1086056\1012496" (Just AssetComplete)),
          (ImageAsset "\f;1AsB\"\1099836\1080661\r\n\41825\ACK" (Just AssetPreview)),
          (ImageAsset "uq\vT" (Just AssetPreview)),
          (ImageAsset "\1113590\1010982o`L\DC2" (Just AssetPreview)),
          (ImageAsset "e" (Just AssetComplete)),
          (ImageAsset "Uk-:U\1079716\146032\ESC*.;" (Just AssetComplete)),
          (ImageAsset "N\1019373q5Bf \bq1" (Just AssetPreview)),
          (ImageAsset "\t_J~B\f_\8912i\1038533\166526" (Nothing)),
          (ImageAsset "\155178S" (Just AssetComplete)),
          (ImageAsset "a\ESC\r\131607sG\SOH@" (Just AssetPreview)),
          (ImageAsset "\51215d" (Nothing))
        ],
      newUserAccentId = Just (ColourId {fromColourId = -657}),
      newUserEmailCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("iyxSVsuGh0j1N_E=")))}),
      newUserPhoneCode =
        Just
          (ActivationCode {fromActivationCode = (fromRight undefined (validate ("G1SWE8oSdxfc7Es1vzzLBkAkCFGDjxo5")))}),
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              (InvitationCode {fromInvitationCode = (fromRight undefined (validate ("fq-V3UW3vm5d")))})
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "2+\NUL\997130\1039508\\V+:RR0\RSu$:\29189\&1-<f"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Just (Country {fromCountry = EG})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "%\r\GS\19921F\EOT(\1071158\75016\1104463\155596\nqc\vWiYNwG\96730\ACK/\SYNd\SI`9 _\176918\986233\1084416\&47\1077788O)\f\1067025w\1102236+lf\168854rE\r\SYN\DC1\1001483E\1038357\151786Vo\RS}H\DC3\1111484\\[\SOH\ETXa\SOHe\1009692\t'\62576hg+r7h\FSY[<q\f\1058977\1062732Vd{\32961\152131OG0Kw\1113048\26785\GS\1030719Y\1022554\DC1\b\140020\150599#\155692\DC2ZZ\1050532\177099\53126\CAN\1064889\&6\RSY>\GS2\1062889uw\1105836\151923f~\1037461\GS\a\999352\1004166'1\61759Yr\1048156\&7\176345\16625\39794y]\141545~/\53208$\1002235u5JF\1015936\a\SI<\SYNn\US/\\\t\132648k\NAK7]d>?i\ETX\1079984,=Hok$q\179503\\=L:\74103\1071169*\94473*\1095220F\DEL\1021524Hk\SOHjPPsA\SOH/c\991892]\NUL}|V7N\1041408\&7C_\DC3\1075503{B;E2t\NAK\b\SIoK\1006676\1091220\b\5871\13831HQ#\147700\1084819$u7\b\41070JMT>\1009303\DC2*R\996626?p\DLE1E\152416\1011076k\1066801B;\ETBSg;O:\SOyW~\fb \1076060W\SUB \186267%PU2o\t{<Z{`\985565G5\ENQ\83190fH\1106225\"q\37841\127353\&82\tG\23318rfkrHN%L)=\US\SOHa'ygS\SOH\179955\1096655Z\1099796\6764g+sF\61538t-H\EMJA\DC1<\SYN\1052217\&3\984721\NUL\42324\tp_\n\50240'gm\RSc\159146INN\a2[mg\DC3\1111132\1030750\1036169\&4!h\74117\1093976KiL \96360\50673\DC1\98825et\DC3devxX\GSdS\1100986iX}\37219\988001\172839\1101259K*pd\"5k\499Z\1036155K\5638Q\US\1089773\a\SYN\138692;N\FSRQ7{{d\1051529\1051271#G\1089715Q\GS}\31481\1081786*yJ\STXflpBg\100614L\149214\999622\ACK#\DLEx@W/\160289\45430/HV+\158161\DC1\23910`?\\\FS\b/.]\992019\59272\167192\984895\RSet\52044h<$\\\181678 \1064751moG\36663\f\147493\1029546\1055148qHv\1040719al4\a\132354fx\1054025\EOTO\DLEP\995535\65952\1102369yO\1092701P\41459*\1008057\ACK\177046\t\151583\GS\1032556\1071151 \GS=0Lf\ESCJ\f>Q\RSc&r\94999%V\995230\49332\988585d\USb}`M\175893dw@\1042638\43126\&9eI\3675\1015082|BRW\NUL\1076495ug\1002412#\GS1\1038683O|\7031\RS\EMD\DLE\1051128\&9\1007082*\172813O\159804\1068311\SUBUi;\ETX\24515>\SO7{C\29717I\181664vg5\64555_5m~\DC4e\23254\DC2\1047386\151177\&0Cz5k\SYN\134623\CAN\48946\GS>\SUB:?#U\1091370\&0\73766\STX>=z\US\993438\99576w\2219\ETXC\1079716\STX7\DC4\177545\\ e\47716e(oT"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }

testObject_NewUser_user_18 :: NewUser
testObject_NewUser_user_18 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\\`O\DEL7\74892\1030387g\1059572\&2*a'\RS;x v\1005139j\NUL\19731\RS\NAKE\1057705\1021874G!bB\1081174\140812\1112673\"t\1092069\SI.\1083842|\ESC\95004]R\67340O\985926\ETBF\99416\27860d\1015517yLN\19551\DLE&\54976I\EOT\NULT'>>\ag-'|\43335\ACK\34086\v"
          },
      newUserUUID = (Just . toUUID) (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
      newUserIdentity = Nothing,
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets = [(ImageAsset "," (Just AssetComplete)), (ImageAsset "&vJ`!R\tVt-=i" (Just AssetPreview))],
      newUserAccentId = Nothing,
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("_Q==")))}),
      newUserPhoneCode = Nothing,
      newUserOrigin = Nothing,
      newUserLabel = Just (CookieLabel {cookieLabelText = "V]#X\1032904\DC3n\1063400\1003348u&HV\f\STX:Ah. 9\n\NUL"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.QU, lCountry = Just (Country {fromCountry = PF})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              ",S\162755\EOT\8179\FS*R\vlulO\14862\1025289\987140]\ENQy:\14338M?\1026893X\67856l%@\EOT\"4\1061926(\SOH\n-\1044517\SUB]\139031\v\\\DC1gJ\131484\49582\126218G(&\DC4\DC11\1053916@\EOT\DC4ze\ETB\ETB\NAK:\a*!x5\NULJ\SI>F\FSS>P]\142082|kLmi\DC2*[\9166e\b\NAK\1033976n\1007275p&<2j\v\SOHFZQY\ETB>@uin\DLEcu3\EM%\992833\t-\b\EM/mpW\US@Hb7:\DC3\FS\1107373\f`4zI\1006025y|\US0\EM5y\29897\NAK\180671\RS\160277\t5O\SO\186958\183326{>\1102241\65137\49102~\177218\33079gt\1031163P\SUBqY\r[OO!\1075718#\1006437O\1110156_'v5O\140774\v\72116#]\"I-\131103a\177542:q\17695\57410\41154KK\1061305au/b \t.\aK\1012871N\CAN\1046730\1011543\v\42946l\STXu\NULPiW.u\92280\GS\DC1*\41138YT\183835\68013+\5361\DC2~dZY\"p\EMI\t\38314\RS\30714{\ACK@D\DEL\132232\1058646V\t37\17626\145793,c\1095316\5436\142927\b\SYN?\41011\1037726\SYN\1002654-\1098559\1026435?\ACK\1105385o\171317+\147332\&4US\1053703\1049642\f7\59941/\1030735\&2\95551\ACK|6&\1074134\DLEu\1025878\&0+1M"
          ),
      newUserExpiresIn = Just (unsafeRange (228513)),
      newUserManagedBy = Just ManagedByWire
    }

testObject_NewUser_user_19 :: NewUser
testObject_NewUser_user_19 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "=5^QR$\DC2i\42477\150812\DC1\1004643\DEL\ESC\190504#\1071209m\EOT\1007557\125001+\FSa\SYN\1054267\28292T-"
          },
      newUserUUID = Nothing,
      newUserIdentity =
        Just
          ( EmailIdentity
              ( Email
                  { emailLocal = "\1002663\1023766\128722U+\SUB\1081252]x\1001382Rz\EOTp\1076639U\1045413",
                    emailDomain = "5\153285\n>\DC4l\a\6936\1090901\NAKU\1037714\133475\&3tj\SOHDnWz\1054252\NAK\1104563"
                  }
              )
          ),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset "\f\ESC1," (Just AssetPreview)),
          (ImageAsset "\171534H\SOHp\SI>6" (Nothing)),
          (ImageAsset "W\33286fj\120501xa\SI" (Nothing)),
          (ImageAsset "" (Just AssetComplete)),
          (ImageAsset "7\32647#.E\\\64198i/9." (Just AssetComplete)),
          (ImageAsset "\\!A" (Just AssetComplete)),
          (ImageAsset "\1037883+U\US\67867Pp^" (Just AssetPreview)),
          (ImageAsset "\DC1\1085335\RS\984435Q\\2\62427\SI`{\157793W\1103222" (Just AssetComplete)),
          (ImageAsset "\ETBv8\CAN\170565X8q\STX\917839V" (Nothing)),
          (ImageAsset "\139296}M\133376\ESC\1107091l\STXKu,A" (Just AssetComplete)),
          (ImageAsset "1\n\144038-J\ETB" (Just AssetComplete)),
          (ImageAsset "d1\143274" (Nothing)),
          (ImageAsset "\ETB%1\1032500@`f\ACK %" (Nothing)),
          (ImageAsset "\ENQl;w" (Just AssetComplete)),
          (ImageAsset "0\1046603\162989m\123209Z\USr-" (Just AssetPreview)),
          (ImageAsset ">%?]V\1064825q\50565\STX\DC3\158734" (Just AssetComplete)),
          (ImageAsset "EE,t5\29843\fJ-t\989684Ft" (Nothing)),
          (ImageAsset "\40430u5\23361\&3" (Just AssetPreview)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "x\DC4\98335" (Just AssetComplete)),
          (ImageAsset "l\RSC\1046285\ENQ\USO\\" (Just AssetPreview)),
          (ImageAsset "\SO\26416>G\STX\148093\t" (Just AssetPreview)),
          (ImageAsset ">F{^s\1097049\ETB" (Just AssetComplete)),
          (ImageAsset "\1075178\&0\5126V3:l'ZuA\1009745s" (Just AssetComplete)),
          (ImageAsset ";\US3A\aZ\t\1003327\US\ACKnv\30386;4" (Nothing)),
          (ImageAsset "?b)`\68768-c\"\1081739\1090806T" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 28218}),
      newUserEmailCode = Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("xW2K")))}),
      newUserPhoneCode =
        Just (ActivationCode {fromActivationCode = (fromRight undefined (validate ("zRaFKSA7mqei2mjV9w9ON4OLeA==")))}),
      newUserOrigin = Nothing,
      newUserLabel = Just (CookieLabel {cookieLabelText = "\ACK\b>\1104851\1032167\&73]\26491\&7\1038541&H\\"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.PI, lCountry = Just (Country {fromCountry = CC})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\f\1011458\ESC\DC1U\1019586\a[Zn |\168315>\ETX\34802\174434\1062058Qq0U7\ESC*e\DC4@[)#\1051903&N-N\1073340J\178288\ACK<\v5>^\EM3\154126\128031M\"Q@_7Y\100429oO\a#x\ESC\61952sU8\ETBf\1067818\100695{ \r&\1083878@\32221'<,X\170604<\SOH\18146>\1010935DP\v!\45479[\1043752\RS\1065218\1001951+&\US{\181954\996815t=Dm\1103746\DELB\1011315t\173322\DLE\fs\ETX`O\b\ACK\10449\DEL:\1016437\n\61531\1061309HXJ~\GS\RS\184012d\183399\1020466\1047778F\1040664&F\a\EOT\1096533H]\NUL*\1080800\a]\1048204V!\FS\9466EyK1\181677\STXv\181963)f\SUB\1103910)#F\1071338:\151005\US Nw\1014336\189368\EOT\1041803\ACKV\RS@<jJ3\\Z\DC1n\78266'Q\63228\&8Q\1080394G0\v\170770b\a~\DC1H\1056499\1086892eR\1016103~>k\ETB.\98432F\9782B\ETX\t\99778 SF\180278oy\1079977)\23126\f\fM\40361\54321zw\US_I8j\38042\"\1110702\EOT}\27728m~\12526oea32?\35546d$\EOT\n5\frGK?\35502c\151118\ETBSi\24371]\t\1068362\&1c{:_e'S\DC4\thf\NULG\3375!\bsjmf\EOT\DC3\999255f^\DC1Z]A\1024298\"\48934\1089423p\187852\174541\144201Ax\1064425O\DC4W;\RS\32955H\155553Y\"E\1097133\150775F/H8cV3\SI1A\CAN\DC4\EMi&EZ3Ka7s\1048354yB4\187660\178995MXSQ9\144686\185815q2\t\1051221H\146701g\1012819\1000535(\SOH'\US\147010\151299\EMeg\ENQ,\984983\DC4dF-DnZ;\DLEjz\123200.m~o\n\SUB\1038174~y\1088606\ENQs\GS\vX|}J\STX\NAK$K\v[W6\50762C\1077712C\1019116KA\DLEmL3*\ENQ;C\188594\SUBz\DC1\ACK|\11966%e^\1105640\178046\vr\1003085\DLE`\27889~\1050896R!*\1014406g%P2\184356\180675ugo\54685\SUB\96736\1013445\1105941\SUB\DC3"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Nothing
    }

testObject_NewUser_user_20 :: NewUser
testObject_NewUser_user_20 =
  NewUser
    { newUserDisplayName =
        Name
          { fromName =
              "\b\1026020z\toeq\1078553}i\STX*RI/B(\SYNjS/L\160185n2\DLEMA\64373\&0\r\173546jS\a\RSt\SYN\143284\&5:*6\173385\GS]\ESC\1052230)\DEL][F\1079027)\ETBu{ \177248mC\GSI\997169vp\GS*\8113F]\DEL.\SOH)\SI\96149\DELp\SO\38583!B!N3'e\38472\1013749\&2\v.3t*IW%\SOc"
          },
      newUserUUID = Nothing,
      newUserIdentity = Just (PhoneIdentity (Phone {fromPhone = "+60997378240829"})),
      newUserPict = Just (Pict {fromPict = []}),
      newUserAssets =
        [ (ImageAsset " })$" (Just AssetComplete)),
          (ImageAsset "=(@S\SUBf4#" (Just AssetPreview)),
          (ImageAsset "[P\DEL\1022912\NAKX\1043109\22691\&86!Lg" (Just AssetComplete)),
          (ImageAsset "\999657\&1\1046647u`\171370\"&ea\1047833\156776" (Nothing)),
          (ImageAsset "\1091622D<\1046398>cc\EOT'" (Nothing)),
          (ImageAsset "" (Nothing)),
          (ImageAsset "r\RS\1072457\SOR\69997b\985779$#\141996R" (Just AssetPreview)),
          (ImageAsset "=\992981N'7I\1046634\SI%" (Just AssetComplete)),
          (ImageAsset "X<#$\NUL0\154109\1071362*\1019195" (Just AssetPreview)),
          (ImageAsset "D\4854\FSi{" (Nothing)),
          (ImageAsset "5p\7627" (Just AssetComplete)),
          (ImageAsset "j\SUBG0\1036603\SId\189736:\588" (Just AssetComplete)),
          (ImageAsset "5H\RS\1076088;" (Just AssetComplete))
        ],
      newUserAccentId = Just (ColourId {fromColourId = 13678}),
      newUserEmailCode =
        Just
          ( ActivationCode
              { fromActivationCode = (fromRight undefined (validate ("v2-GQHZFYfDD7eV7gj3dtTZ2RDAqMLpBntdMHg==")))
              }
          ),
      newUserPhoneCode = Nothing,
      newUserOrigin =
        Just
          ( NewUserOriginInvitationCode
              ( InvitationCode
                  { fromInvitationCode = (fromRight undefined (validate ("529Eo7BC6CKbsX0i9lKIvOFyuxhHig==")))
                  }
              )
          ),
      newUserLabel = Just (CookieLabel {cookieLabelText = "#+EA\58647\&7"}),
      newUserLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KW, lCountry = Just (Country {fromCountry = LU})}),
      newUserPassword =
        Just
          ( PlainTextPassword
              "\1005402A\37244\&9\65240 \SYN\1004548]\1074459=\1064969i0\1014470.\v.)_$QYz\1007772s\168133\163502\1002041,f\97688M7\2848\1071977m\63578c\DELO\SOH\f\STX\1003747DQ\fE\62031q={\DLEd\993185\133714\RSz\\\190730\&5.F\EMi<\33839\SIP$\SOi|d,J.\1874\t\SUB\1086095]UA`*\fh\v\STX@K.z\r=\169532\"TG\DC2=X\NULW\1096670x\NULV\77882;\156947}{hRc\b\DC20\1088407:\158813\ACK$6n\78702\1066848\16675\1067648\&6^z{]\t\1111148\36618#W;\FSsp\1063273t\1084107\1039785\n\SO\21412\1034914\100883\993989\168903TW\159567k\997815,J\\!\1089960\989008bs\FS\CANe\989036\186142=\NUL\t\STX\NULs)A\ACK+6z\145155=\SI\a]5\CAN1[O=<Ls\170476e\1045780t\1002486\DLEv\DLE'<\aS\1074294\GS9].6\DC2`\aC\144351?\1033117\CAN\SO>.\DEL ,\DC3RF\NUL\57649\1029240\125001>\SOHkc5Tvp4'u\177927\SI\128449Y\43223\DC3$\DC4q\20124I8\38402GJ\146945\&6\SUB\DC2\185263<\6278i\175986\1082957XR[G\EOT:\DC1\"\41646\1001574\b$\110872\10362\1013074\SUB\1060318Ga\983903\994043\a6\NUL\166918\"M\CANUz\FS\1112779\SO \DC1\1010755=\EM\52307\GS X?p\SOH]3V<PZ\1009114Y\EOT{\185234\166073Mm\3350M\DC1;di2\78875P\DC4\DC4;\1047802\&5\1030199\SO\a*\t\GS,\STX\134697\156291\1102722\&3W\SYN{\121061Zx#}[1\159683 \1070043S\\Se?&~|4\1049003\ETBM{\DC2\DC2\1016870\1113937\NAK\SI^\ESCA\175706J\1076996f=eU\SUB@\50587\18919\ETBg\SUB)@\GS\1041718\USF\1063565XA\DC3tec\ay-\1029047g%\33631$et 1\1033509\147452/\151935?\f\v\NAKU\EOT\"q\1069216\68137\133529\DC3\SOHERd|d\ENQUF^\1051136y\1060087{\147556\&1\1032286K\NAK}W\SI\SUB\CANS89\"\110679\137634bV!W\39835E@t55\DC3\r\188443\SYNt3\22493\42699koK\\dLg\94177\ENQ\162490.\SUB\36038dq\FS\998600O\46255\ETBVQ\1098084l\ACK\27996\ETBbLm|~Q\SO6\1089738\181887\4699c\161340|m\GS[C\SUB\158911f\SI2p\137271y,\1026955=c\DC1N\53362\&3O>\1085215l[ts;P\48909ye\1088437\SI\20936]\1072813\DC4|\1091546>\987019p\987993\166701\136651N<{t\185021\1094830}\28304\t0&\FSuz\CANm\33912\1069311\996754q~S\ETX}KsYVN\t\r8pZ$.p#\25882Qad5\194726\178938n<\1027094\1107931o!\1069932\170881\DC2;?Y\NAK:\DC29{}'\DC38\83222\ETXc\150396\EM\a3aQ~\tl&\\8`\SUB\RS,\31740\EOT\1035767\1001486\46257s\1035518\1001306e\1096342\995856J{`;]\1056856\181520\&2u\12276\1026030\RSsS\1092881x\40261\1098657%z\83035\DLE\1112046N\DC1tSL8]\ACKYi\42440\FSz\n\US\CAN\ETB\ETB'\40035OH\994540M}<\34537\1019547\1056779a\v&$k\172876WP\1048522\CAN\99008\&4oRjU\"\ENQ\1087854\1025289#C\135510'~\1059995\1074987\&3\ETXC\1056389(.T\CANi\DLEw\6262\164158Rp"
          ),
      newUserExpiresIn = Nothing,
      newUserManagedBy = Just ManagedByScim
    }
