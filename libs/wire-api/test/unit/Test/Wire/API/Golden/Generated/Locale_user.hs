{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Locale_user where
import Data.ISO3166_CountryCodes
    ( CountryCode(LB, LI, SH, DE, TF, PT, GT, CZ, KR, BG, TO, SX, LT,
                  RW, AW, ES) )
import Imports ( Maybe(Just, Nothing) )
import qualified Data.LanguageCodes
    ( ISO639_1(EN, NN, GN, PS, KS, FI, TG, KN, BH, NO, LU, MK, LN, PI,
               TI, SC, GL, IS, SA, MI) )
import Wire.API.User
    ( Locale(..), Country(Country, fromCountry), Language(Language) )

testObject_Locale_user_1 :: Locale
testObject_Locale_user_1 = Locale {lLanguage = Language Data.LanguageCodes.NN, lCountry = Just (Country {fromCountry = LI})}
testObject_Locale_user_2 :: Locale
testObject_Locale_user_2 = Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Just (Country {fromCountry = SH})}
testObject_Locale_user_3 :: Locale
testObject_Locale_user_3 = Locale {lLanguage = Language Data.LanguageCodes.PS, lCountry = Just (Country {fromCountry = DE})}
testObject_Locale_user_4 :: Locale
testObject_Locale_user_4 = Locale {lLanguage = Language Data.LanguageCodes.KS, lCountry = Just (Country {fromCountry = TF})}
testObject_Locale_user_5 :: Locale
testObject_Locale_user_5 = Locale {lLanguage = Language Data.LanguageCodes.FI, lCountry = Just (Country {fromCountry = PT})}
testObject_Locale_user_6 :: Locale
testObject_Locale_user_6 = Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = GT})}
testObject_Locale_user_7 :: Locale
testObject_Locale_user_7 = Locale {lLanguage = Language Data.LanguageCodes.KN, lCountry = Nothing}
testObject_Locale_user_8 :: Locale
testObject_Locale_user_8 = Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = CZ})}
testObject_Locale_user_9 :: Locale
testObject_Locale_user_9 = Locale {lLanguage = Language Data.LanguageCodes.NO, lCountry = Just (Country {fromCountry = KR})}
testObject_Locale_user_10 :: Locale
testObject_Locale_user_10 = Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = BG})}
testObject_Locale_user_11 :: Locale
testObject_Locale_user_11 = Locale {lLanguage = Language Data.LanguageCodes.MK, lCountry = Just (Country {fromCountry = TO})}
testObject_Locale_user_12 :: Locale
testObject_Locale_user_12 = Locale {lLanguage = Language Data.LanguageCodes.LN, lCountry = Just (Country {fromCountry = SX})}
testObject_Locale_user_13 :: Locale
testObject_Locale_user_13 = Locale {lLanguage = Language Data.LanguageCodes.PI, lCountry = Nothing}
testObject_Locale_user_14 :: Locale
testObject_Locale_user_14 = Locale {lLanguage = Language Data.LanguageCodes.TI, lCountry = Just (Country {fromCountry = LT})}
testObject_Locale_user_15 :: Locale
testObject_Locale_user_15 = Locale {lLanguage = Language Data.LanguageCodes.SC, lCountry = Just (Country {fromCountry = RW})}
testObject_Locale_user_16 :: Locale
testObject_Locale_user_16 = Locale {lLanguage = Language Data.LanguageCodes.GL, lCountry = Just (Country {fromCountry = AW})}
testObject_Locale_user_17 :: Locale
testObject_Locale_user_17 = Locale {lLanguage = Language Data.LanguageCodes.IS, lCountry = Nothing}
testObject_Locale_user_18 :: Locale
testObject_Locale_user_18 = Locale {lLanguage = Language Data.LanguageCodes.SA, lCountry = Just (Country {fromCountry = ES})}
testObject_Locale_user_19 :: Locale
testObject_Locale_user_19 = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Nothing}
testObject_Locale_user_20 :: Locale
testObject_Locale_user_20 = Locale {lLanguage = Language Data.LanguageCodes.EN, lCountry = Just (Country {fromCountry = LB})}
