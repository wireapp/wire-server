{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Activate_user where
import Data.Text.Ascii ( AsciiChars(validate) )
import Imports ( Bool(False, True), undefined, fromRight )
import Wire.API.User
    ( Email(Email, emailLocal, emailDomain), Phone(Phone, fromPhone) )
import Wire.API.User.Activation
    ( ActivationCode(ActivationCode, fromActivationCode),
      Activate(..),
      ActivationKey(ActivationKey, fromActivationKey),
      ActivationTarget(ActivateKey, ActivateEmail, ActivatePhone) )

testObject_Activate_user_1 :: Activate
testObject_Activate_user_1 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("a7veGtZ9")))}, activateDryrun = True}
testObject_Activate_user_2 :: Activate
testObject_Activate_user_2 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+81629827"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_3 :: Activate
testObject_Activate_user_3 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("RGs2WQ==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("krZOiTo=")))}, activateDryrun = True}
testObject_Activate_user_4 :: Activate
testObject_Activate_user_4 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\NULK", emailDomain = "\DLEq\f\CAN\f\189719\&0\19704Z"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("D-duBQ==")))}, activateDryrun = True}
testObject_Activate_user_5 :: Activate
testObject_Activate_user_5 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+531710159567628"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("ciHu")))}, activateDryrun = True}
testObject_Activate_user_6 :: Activate
testObject_Activate_user_6 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("BxTlnw==")))}, activateDryrun = False}
testObject_Activate_user_7 :: Activate
testObject_Activate_user_7 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+899793638263"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("_7UMf5pu8Q==")))}, activateDryrun = True}
testObject_Activate_user_8 :: Activate
testObject_Activate_user_8 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("gvISpw==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("gAIxoCBFNC4=")))}, activateDryrun = False}
testObject_Activate_user_9 :: Activate
testObject_Activate_user_9 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("j-Kgegbi")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("5-S6hMWfa4p1")))}, activateDryrun = False}
testObject_Activate_user_10 :: Activate
testObject_Activate_user_10 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1030995R$\51227\1096567\US,,X", emailDomain = "\176645g3t\10925n"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("A_w=")))}, activateDryrun = True}
testObject_Activate_user_11 :: Activate
testObject_Activate_user_11 = Activate {activateTarget = ActivateEmail (Email {emailLocal = ">(\SUB\40668t>2\RS\132584", emailDomain = "\1077367m"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("hHX0")))}, activateDryrun = False}
testObject_Activate_user_12 :: Activate
testObject_Activate_user_12 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("YN3nkXVOzg==")))}, activateDryrun = True}
testObject_Activate_user_13 :: Activate
testObject_Activate_user_13 = Activate {activateTarget = ActivateEmail (Email {emailLocal = "\1019779`", emailDomain = "\14243L2\180740i`K\1092485\f"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("D_5gdYRFXPU3")))}, activateDryrun = False}
testObject_Activate_user_14 :: Activate
testObject_Activate_user_14 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("J9plzOtJKw==")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("m4A=")))}, activateDryrun = True}
testObject_Activate_user_15 :: Activate
testObject_Activate_user_15 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+65080806593981"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("XFPJqeE=")))}, activateDryrun = True}
testObject_Activate_user_16 :: Activate
testObject_Activate_user_16 = Activate {activateTarget = ActivateEmail (Email {emailLocal = ":\174198J\NULX>{\50408I", emailDomain = "\STX"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("a47Y")))}, activateDryrun = True}
testObject_Activate_user_17 :: Activate
testObject_Activate_user_17 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+874950872206398"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("CYzUXg==")))}, activateDryrun = True}
testObject_Activate_user_18 :: Activate
testObject_Activate_user_18 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+15851014452067"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("")))}, activateDryrun = True}
testObject_Activate_user_19 :: Activate
testObject_Activate_user_19 = Activate {activateTarget = ActivatePhone (Phone {fromPhone = "+1897061960"}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("_YnYYL2Z")))}, activateDryrun = True}
testObject_Activate_user_20 :: Activate
testObject_Activate_user_20 = Activate {activateTarget = ActivateKey (ActivationKey {fromActivationKey = (fromRight undefined (validate ("TQPUzbw=")))}), activateCode = ActivationCode {fromActivationCode = (fromRight undefined (validate ("pmrSygau7Q==")))}, activateDryrun = False}
