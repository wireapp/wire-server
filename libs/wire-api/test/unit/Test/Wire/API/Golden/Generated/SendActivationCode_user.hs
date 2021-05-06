{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SendActivationCode_user where
import Data.ISO3166_CountryCodes
    ( CountryCode(PA, PS, DE, FJ, MG, MX, BB) )
import Imports
    ( Bool(True, False), Maybe(Nothing, Just), Either(Left, Right) )
import qualified Data.LanguageCodes
    ( ISO639_1(HZ, HT, HR, SO, TN, KK, BN, LA, YO, NO, CU, BH, TL) )
import Wire.API.User
    ( Locale(Locale, lLanguage, lCountry),
      Email(Email, emailLocal, emailDomain),
      Phone(Phone, fromPhone),
      Country(Country, fromCountry),
      Language(Language) )
import Wire.API.User.Activation ( SendActivationCode(..) )

testObject_SendActivationCode_user_1 :: SendActivationCode
testObject_SendActivationCode_user_1 = SendActivationCode {saUserKey = Left (Email {emailLocal = "%h.n#\66665z", emailDomain = "\1034827f"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LA, lCountry = Just (Country {fromCountry = PS})}), saCall = False}
testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+6713882728"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HT, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\1255\RSe\123196", emailDomain = "o-3or"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+2336436478"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+92809221"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HR, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_6 :: SendActivationCode
testObject_SendActivationCode_user_6 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\1096890 e\141993\SOH\119148", emailDomain = "\DC3\ESC\1073483"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SO, lCountry = Just (Country {fromCountry = DE})}), saCall = False}
testObject_SendActivationCode_user_7 :: SendActivationCode
testObject_SendActivationCode_user_7 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+63116156947964"}), saLocale = Nothing, saCall = True}
testObject_SendActivationCode_user_8 :: SendActivationCode
testObject_SendActivationCode_user_8 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\FSHO\68841\&1Q~\SOH", emailDomain = "\1082252<\DLE:"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_9 :: SendActivationCode
testObject_SendActivationCode_user_9 = SendActivationCode {saUserKey = Left (Email {emailLocal = "Fg", emailDomain = ""}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_10 :: SendActivationCode
testObject_SendActivationCode_user_10 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+483828525551"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_11 :: SendActivationCode
testObject_SendActivationCode_user_11 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+5184379826"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Just (Country {fromCountry = FJ})}), saCall = False}
testObject_SendActivationCode_user_12 :: SendActivationCode
testObject_SendActivationCode_user_12 = SendActivationCode {saUserKey = Left (Email {emailLocal = "2\ETX{-EF\NAKL", emailDomain = "\113666"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Just (Country {fromCountry = MG})}), saCall = True}
testObject_SendActivationCode_user_13 :: SendActivationCode
testObject_SendActivationCode_user_13 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\DC4':", emailDomain = "\168801\1004633uy\1029160A#!\138749"}), saLocale = Nothing, saCall = False}
testObject_SendActivationCode_user_14 :: SendActivationCode
testObject_SendActivationCode_user_14 = SendActivationCode {saUserKey = Left (Email {emailLocal = ".Rz\985647_\184175", emailDomain = "\RS$|\179349/-"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LA, lCountry = Just (Country {fromCountry = MX})}), saCall = True}
testObject_SendActivationCode_user_15 :: SendActivationCode
testObject_SendActivationCode_user_15 = SendActivationCode {saUserKey = Left (Email {emailLocal = "&\DC1\ETBi\43904\"", emailDomain = "\t\985646"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.YO, lCountry = Just (Country {fromCountry = BB})}), saCall = False}
testObject_SendActivationCode_user_16 :: SendActivationCode
testObject_SendActivationCode_user_16 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+08242075645"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NO, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_17 :: SendActivationCode
testObject_SendActivationCode_user_17 = SendActivationCode {saUserKey = Right (Phone {fromPhone = "+93805619"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Nothing}), saCall = False}
testObject_SendActivationCode_user_18 :: SendActivationCode
testObject_SendActivationCode_user_18 = SendActivationCode {saUserKey = Left (Email {emailLocal = " ", emailDomain = "\SI\1067970IC"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = PA})}), saCall = False}
testObject_SendActivationCode_user_19 :: SendActivationCode
testObject_SendActivationCode_user_19 = SendActivationCode {saUserKey = Left (Email {emailLocal = "\f", emailDomain = "\DLE\\\152003\SO\1027250yHPA"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TL, lCountry = Nothing}), saCall = True}
testObject_SendActivationCode_user_20 :: SendActivationCode
testObject_SendActivationCode_user_20 = SendActivationCode {saUserKey = Left (Email {emailLocal = "p\1067648z\161867hdQ}FR", emailDomain = "\1049235r~6\19596\171054Z\DEL"}), saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HZ, lCountry = Nothing}), saCall = True}
