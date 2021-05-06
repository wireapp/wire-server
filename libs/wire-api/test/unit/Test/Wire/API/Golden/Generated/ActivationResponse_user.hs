{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ActivationResponse_user where
import Imports ( Bool(True, False), Maybe(Just) )
import Wire.API.User
    ( Email(Email, emailLocal, emailDomain),
      Phone(Phone, fromPhone),
      UserIdentity(FullIdentity, SSOIdentity, PhoneIdentity,
                   EmailIdentity),
      UserSSOId(UserScimExternalId) )
import Wire.API.User.Activation ( ActivationResponse(..) )

testObject_ActivationResponse_user_1 :: ActivationResponse
testObject_ActivationResponse_user_1 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "Bm/", emailDomain = ""}) (Phone {fromPhone = "+05992582589"}), activatedFirst = False}
testObject_ActivationResponse_user_2 :: ActivationResponse
testObject_ActivationResponse_user_2 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "", emailDomain = "\1010788h!\60011\&4z5'l*\917587"}), activatedFirst = False}
testObject_ActivationResponse_user_3 :: ActivationResponse
testObject_ActivationResponse_user_3 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "A", emailDomain = ")N6My"}), activatedFirst = False}
testObject_ActivationResponse_user_4 :: ActivationResponse
testObject_ActivationResponse_user_4 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\NAK]X\1098123h6\DC42`\SOH,", emailDomain = "\DEL\ENQ\a\r\180389\&0!_&OS\146519 \25600"}), activatedFirst = False}
testObject_ActivationResponse_user_5 :: ActivationResponse
testObject_ActivationResponse_user_5 = ActivationResponse {activatedIdentity = SSOIdentity (UserScimExternalId "\67847\1095277") (Just (Email {emailLocal = "p", emailDomain = "\n"})) (Just (Phone {fromPhone = "+92407145579045"})), activatedFirst = True}
testObject_ActivationResponse_user_6 :: ActivationResponse
testObject_ActivationResponse_user_6 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+19753298"}), activatedFirst = False}
testObject_ActivationResponse_user_7 :: ActivationResponse
testObject_ActivationResponse_user_7 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "g\SOH2", emailDomain = ""}) (Phone {fromPhone = "+22562131"}), activatedFirst = False}
testObject_ActivationResponse_user_8 :: ActivationResponse
testObject_ActivationResponse_user_8 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+928452299938"}), activatedFirst = False}
testObject_ActivationResponse_user_9 :: ActivationResponse
testObject_ActivationResponse_user_9 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\1000528Zh", emailDomain = "h\49861Iy"}) (Phone {fromPhone = "+01850256"}), activatedFirst = True}
testObject_ActivationResponse_user_10 :: ActivationResponse
testObject_ActivationResponse_user_10 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "_{r\1079315\SYNNb", emailDomain = "8\SYNN"}) (Phone {fromPhone = "+617722653515"}), activatedFirst = True}
testObject_ActivationResponse_user_11 :: ActivationResponse
testObject_ActivationResponse_user_11 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "", emailDomain = ",)\1008419N\143120m"}) (Phone {fromPhone = "+67418217"}), activatedFirst = False}
testObject_ActivationResponse_user_12 :: ActivationResponse
testObject_ActivationResponse_user_12 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+16651353"}), activatedFirst = True}
testObject_ActivationResponse_user_13 :: ActivationResponse
testObject_ActivationResponse_user_13 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "\ENQ", emailDomain = "\1113388"}) (Phone {fromPhone = "+5180898516"}), activatedFirst = True}
testObject_ActivationResponse_user_14 :: ActivationResponse
testObject_ActivationResponse_user_14 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\SO\18112(,\a/T\12702Q", emailDomain = "dWJ\148833"}), activatedFirst = False}
testObject_ActivationResponse_user_15 :: ActivationResponse
testObject_ActivationResponse_user_15 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+117364540"}), activatedFirst = True}
testObject_ActivationResponse_user_16 :: ActivationResponse
testObject_ActivationResponse_user_16 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "wgol", emailDomain = "\"Po-\22966T\v"}) (Phone {fromPhone = "+924683790184"}), activatedFirst = False}
testObject_ActivationResponse_user_17 :: ActivationResponse
testObject_ActivationResponse_user_17 = ActivationResponse {activatedIdentity = PhoneIdentity (Phone {fromPhone = "+5944371870"}), activatedFirst = True}
testObject_ActivationResponse_user_18 :: ActivationResponse
testObject_ActivationResponse_user_18 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "i", emailDomain = "?\1028182>:j\1919\SO\1111353\1012612R\1041919d\1062930\120175"}), activatedFirst = False}
testObject_ActivationResponse_user_19 :: ActivationResponse
testObject_ActivationResponse_user_19 = ActivationResponse {activatedIdentity = EmailIdentity (Email {emailLocal = "\1101301y\ESC", emailDomain = "Iu"}), activatedFirst = True}
testObject_ActivationResponse_user_20 :: ActivationResponse
testObject_ActivationResponse_user_20 = ActivationResponse {activatedIdentity = FullIdentity (Email {emailLocal = "", emailDomain = ""}) (Phone {fromPhone = "+88733558"}), activatedFirst = True}
