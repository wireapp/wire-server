{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserIdentity_user where
import Imports ( Maybe(Just, Nothing) )
import Wire.API.User
    ( Email(Email, emailLocal, emailDomain),
      Phone(Phone, fromPhone),
      UserIdentity(..),
      UserSSOId(UserSSOId, UserScimExternalId) )

testObject_UserIdentity_user_1 :: UserIdentity
testObject_UserIdentity_user_1 = FullIdentity (Email {emailLocal = "N}", emailDomain = "?\9746\1092822\55136\27610z\152202\n"}) (Phone {fromPhone = "+35480757427"})
testObject_UserIdentity_user_2 :: UserIdentity
testObject_UserIdentity_user_2 = EmailIdentity (Email {emailLocal = "GB", emailDomain = ",\SYNeOo\17040"})
testObject_UserIdentity_user_3 :: UserIdentity
testObject_UserIdentity_user_3 = PhoneIdentity (Phone {fromPhone = "+9098380266022"})
testObject_UserIdentity_user_4 :: UserIdentity
testObject_UserIdentity_user_4 = FullIdentity (Email {emailLocal = "\1095782\RS", emailDomain = "`xe7^_\DC174(\983399"}) (Phone {fromPhone = "+151662219621"})
testObject_UserIdentity_user_5 :: UserIdentity
testObject_UserIdentity_user_5 = EmailIdentity (Email {emailLocal = "1Osy./p\SO)\v*\DC3\169583V\ETXD E\42641", emailDomain = "\ETBH\1079136"})
testObject_UserIdentity_user_6 :: UserIdentity
testObject_UserIdentity_user_6 = PhoneIdentity (Phone {fromPhone = "+3275045643324"})
testObject_UserIdentity_user_7 :: UserIdentity
testObject_UserIdentity_user_7 = EmailIdentity (Email {emailLocal = "[\158852_\1104981\n\b\DC4J\1054054m\92517/", emailDomain = ".\SOHS|\1112257s\6079q\1086319U\CANgN7\SOH\150718=\SYNb|\1087259oi\t("})
testObject_UserIdentity_user_8 :: UserIdentity
testObject_UserIdentity_user_8 = SSOIdentity (UserScimExternalId "M\13065\ETXt\53528(\1096711\4707\DC4") (Just (Email {emailLocal = "\DC3\SOHPc \EMO`", emailDomain = "\54518\&4"})) (Just (Phone {fromPhone = "+545954684649520"}))
testObject_UserIdentity_user_9 :: UserIdentity
testObject_UserIdentity_user_9 = SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "E\983489\\2\\\28305n", emailDomain = "M4\EOT\1023716\EOT"})) (Just (Phone {fromPhone = "+736148787252"}))
testObject_UserIdentity_user_10 :: UserIdentity
testObject_UserIdentity_user_10 = SSOIdentity (UserScimExternalId "\1004339!V$?;BW>") Nothing (Just (Phone {fromPhone = "+78914798107455"}))
testObject_UserIdentity_user_11 :: UserIdentity
testObject_UserIdentity_user_11 = FullIdentity (Email {emailLocal = "$\ENQ:L\ACKG\65158Ymu L\SYNz", emailDomain = "\1087269\138029\DC38"}) (Phone {fromPhone = "+85731766517353"})
testObject_UserIdentity_user_12 :: UserIdentity
testObject_UserIdentity_user_12 = PhoneIdentity (Phone {fromPhone = "+20217386747"})
testObject_UserIdentity_user_13 :: UserIdentity
testObject_UserIdentity_user_13 = EmailIdentity (Email {emailLocal = "\15446SsG\r;\ETXI.GA/\EOTzB\1097791\128978$l\135982\1017158\138408\52853^j\ETB&\165327)", emailDomain = "X\83160\23705cq\ENQ?\FSm\132863[\20763Q`\1024923!\b"})
testObject_UserIdentity_user_14 :: UserIdentity
testObject_UserIdentity_user_14 = PhoneIdentity (Phone {fromPhone = "+542797734737"})
testObject_UserIdentity_user_15 :: UserIdentity
testObject_UserIdentity_user_15 = EmailIdentity (Email {emailLocal = "#c|\1089937\twIIl\989135]o(\EMB\ESCh", emailDomain = "\134855?t\DEL\1023871\&1/\52316>\135794Gt\ACK\SI\1057420\aKv\165049\SYN\30900E/\US~"})
testObject_UserIdentity_user_16 :: UserIdentity
testObject_UserIdentity_user_16 = EmailIdentity (Email {emailLocal = "\USA|\1080449;", emailDomain = "N\EOTN\34393-E\42692\NAK-2\38502"})
testObject_UserIdentity_user_17 :: UserIdentity
testObject_UserIdentity_user_17 = FullIdentity (Email {emailLocal = ">bE,$y\"V\1082826\nflN", emailDomain = "L\CAN\1086067\SOH\1056998i7\1004662"}) (Phone {fromPhone = "+551083343072806"})
testObject_UserIdentity_user_18 :: UserIdentity
testObject_UserIdentity_user_18 = PhoneIdentity (Phone {fromPhone = "+9521290358137"})
testObject_UserIdentity_user_19 :: UserIdentity
testObject_UserIdentity_user_19 = PhoneIdentity (Phone {fromPhone = "+0041335190578"})
testObject_UserIdentity_user_20 :: UserIdentity
testObject_UserIdentity_user_20 = SSOIdentity (UserSSOId "3\f" "") Nothing (Just (Phone {fromPhone = "+003950588440774"}))
