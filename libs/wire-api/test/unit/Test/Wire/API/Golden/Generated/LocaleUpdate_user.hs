{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LocaleUpdate_user where
import Data.ISO3166_CountryCodes
    ( CountryCode(TK, VI, ER, KZ, TJ, SV, DE, AG, EC, CN, BY, LR) )
import Imports ( Maybe(Just, Nothing) )
import qualified Data.LanguageCodes
    ( ISO639_1(TS, SM, FO, SS, LO, KG, CY, EL, KU, BE, BG, GV, HI, BA,
               LU, MT, ND, KN) )
import Wire.API.User
    ( Locale(Locale, lLanguage, lCountry),
      LocaleUpdate(..),
      Country(Country, fromCountry),
      Language(Language) )

testObject_LocaleUpdate_user_1 :: LocaleUpdate
testObject_LocaleUpdate_user_1 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Just (Country {fromCountry = VI})}}
testObject_LocaleUpdate_user_2 :: LocaleUpdate
testObject_LocaleUpdate_user_2 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.FO, lCountry = Just (Country {fromCountry = ER})}}
testObject_LocaleUpdate_user_3 :: LocaleUpdate
testObject_LocaleUpdate_user_3 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Nothing}}
testObject_LocaleUpdate_user_4 :: LocaleUpdate
testObject_LocaleUpdate_user_4 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = KZ})}}
testObject_LocaleUpdate_user_5 :: LocaleUpdate
testObject_LocaleUpdate_user_5 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.LO, lCountry = Just (Country {fromCountry = TJ})}}
testObject_LocaleUpdate_user_6 :: LocaleUpdate
testObject_LocaleUpdate_user_6 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = SV})}}
testObject_LocaleUpdate_user_7 :: LocaleUpdate
testObject_LocaleUpdate_user_7 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KG, lCountry = Just (Country {fromCountry = DE})}}
testObject_LocaleUpdate_user_8 :: LocaleUpdate
testObject_LocaleUpdate_user_8 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.CY, lCountry = Nothing}}
testObject_LocaleUpdate_user_9 :: LocaleUpdate
testObject_LocaleUpdate_user_9 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.EL, lCountry = Just (Country {fromCountry = AG})}}
testObject_LocaleUpdate_user_10 :: LocaleUpdate
testObject_LocaleUpdate_user_10 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KU, lCountry = Nothing}}
testObject_LocaleUpdate_user_11 :: LocaleUpdate
testObject_LocaleUpdate_user_11 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BE, lCountry = Just (Country {fromCountry = EC})}}
testObject_LocaleUpdate_user_12 :: LocaleUpdate
testObject_LocaleUpdate_user_12 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BG, lCountry = Just (Country {fromCountry = CN})}}
testObject_LocaleUpdate_user_13 :: LocaleUpdate
testObject_LocaleUpdate_user_13 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.GV, lCountry = Just (Country {fromCountry = BY})}}
testObject_LocaleUpdate_user_14 :: LocaleUpdate
testObject_LocaleUpdate_user_14 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.HI, lCountry = Nothing}}
testObject_LocaleUpdate_user_15 :: LocaleUpdate
testObject_LocaleUpdate_user_15 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BA, lCountry = Nothing}}
testObject_LocaleUpdate_user_16 :: LocaleUpdate
testObject_LocaleUpdate_user_16 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Nothing}}
testObject_LocaleUpdate_user_17 :: LocaleUpdate
testObject_LocaleUpdate_user_17 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.MT, lCountry = Just (Country {fromCountry = LR})}}
testObject_LocaleUpdate_user_18 :: LocaleUpdate
testObject_LocaleUpdate_user_18 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.ND, lCountry = Nothing}}
testObject_LocaleUpdate_user_19 :: LocaleUpdate
testObject_LocaleUpdate_user_19 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Nothing}}
testObject_LocaleUpdate_user_20 :: LocaleUpdate
testObject_LocaleUpdate_user_20 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.TS, lCountry = Just (Country {fromCountry = TK})}}
