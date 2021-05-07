{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.Locale_user where

import Data.ISO3166_CountryCodes
  ( CountryCode (CR, DO, EG, HT, IN, LK, LS, MA, PS, TL, VI),
  )
import qualified Data.LanguageCodes
  ( ISO639_1
      ( AF,
        BG,
        BN,
        CV,
        DA,
        ES,
        IO,
        MN,
        MS,
        MT,
        OC,
        OS,
        PT,
        RM,
        SQ,
        YO
      ),
  )
import Imports (Maybe (Just, Nothing))
import Wire.API.User
  ( Country (Country, fromCountry),
    Language (Language),
    Locale (..),
  )

testObject_Locale_user_1 :: Locale
testObject_Locale_user_1 = Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Nothing}

testObject_Locale_user_2 :: Locale
testObject_Locale_user_2 = Locale {lLanguage = Language Data.LanguageCodes.MN, lCountry = Just (Country {fromCountry = MA})}

testObject_Locale_user_3 :: Locale
testObject_Locale_user_3 = Locale {lLanguage = Language Data.LanguageCodes.SQ, lCountry = Nothing}

testObject_Locale_user_4 :: Locale
testObject_Locale_user_4 = Locale {lLanguage = Language Data.LanguageCodes.BG, lCountry = Just (Country {fromCountry = VI})}

testObject_Locale_user_5 :: Locale
testObject_Locale_user_5 = Locale {lLanguage = Language Data.LanguageCodes.MT, lCountry = Nothing}

testObject_Locale_user_6 :: Locale
testObject_Locale_user_6 = Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Just (Country {fromCountry = TL})}

testObject_Locale_user_7 :: Locale
testObject_Locale_user_7 = Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Just (Country {fromCountry = IN})}

testObject_Locale_user_8 :: Locale
testObject_Locale_user_8 = Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Just (Country {fromCountry = EG})}

testObject_Locale_user_9 :: Locale
testObject_Locale_user_9 = Locale {lLanguage = Language Data.LanguageCodes.OS, lCountry = Just (Country {fromCountry = CR})}

testObject_Locale_user_10 :: Locale
testObject_Locale_user_10 = Locale {lLanguage = Language Data.LanguageCodes.BN, lCountry = Just (Country {fromCountry = HT})}

testObject_Locale_user_11 :: Locale
testObject_Locale_user_11 = Locale {lLanguage = Language Data.LanguageCodes.ES, lCountry = Just (Country {fromCountry = IN})}

testObject_Locale_user_12 :: Locale
testObject_Locale_user_12 = Locale {lLanguage = Language Data.LanguageCodes.ES, lCountry = Just (Country {fromCountry = LK})}

testObject_Locale_user_13 :: Locale
testObject_Locale_user_13 = Locale {lLanguage = Language Data.LanguageCodes.CV, lCountry = Nothing}

testObject_Locale_user_14 :: Locale
testObject_Locale_user_14 = Locale {lLanguage = Language Data.LanguageCodes.YO, lCountry = Just (Country {fromCountry = PS})}

testObject_Locale_user_15 :: Locale
testObject_Locale_user_15 = Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Nothing}

testObject_Locale_user_16 :: Locale
testObject_Locale_user_16 = Locale {lLanguage = Language Data.LanguageCodes.AF, lCountry = Just (Country {fromCountry = DO})}

testObject_Locale_user_17 :: Locale
testObject_Locale_user_17 = Locale {lLanguage = Language Data.LanguageCodes.MS, lCountry = Nothing}

testObject_Locale_user_18 :: Locale
testObject_Locale_user_18 = Locale {lLanguage = Language Data.LanguageCodes.OC, lCountry = Nothing}

testObject_Locale_user_19 :: Locale
testObject_Locale_user_19 = Locale {lLanguage = Language Data.LanguageCodes.OC, lCountry = Nothing}

testObject_Locale_user_20 :: Locale
testObject_Locale_user_20 = Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Just (Country {fromCountry = LS})}
