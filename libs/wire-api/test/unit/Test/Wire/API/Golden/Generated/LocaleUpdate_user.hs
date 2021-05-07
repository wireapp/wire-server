{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.LocaleUpdate_user where

import Data.ISO3166_CountryCodes
  ( CountryCode
      ( AE,
        AG,
        BZ,
        CH,
        CI,
        DK,
        MC,
        MD,
        ML,
        MT,
        NU,
        PG,
        SC,
        SY,
        SZ
      ),
  )
import qualified Data.LanguageCodes
  ( ISO639_1
      ( BO,
        EE,
        FA,
        FY,
        GU,
        HI,
        II,
        IK,
        KU,
        KV,
        MI,
        OS,
        RW,
        SA,
        TN,
        TO,
        TS,
        VE
      ),
  )
import Imports (Maybe (Just, Nothing))
import Wire.API.User
  ( Country (Country, fromCountry),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    LocaleUpdate (..),
  )

testObject_LocaleUpdate_user_1 :: LocaleUpdate
testObject_LocaleUpdate_user_1 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.BO, lCountry = Just (Country {fromCountry = DK})}}

testObject_LocaleUpdate_user_2 :: LocaleUpdate
testObject_LocaleUpdate_user_2 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.II, lCountry = Nothing}}

testObject_LocaleUpdate_user_3 :: LocaleUpdate
testObject_LocaleUpdate_user_3 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.IK, lCountry = Just (Country {fromCountry = BZ})}}

testObject_LocaleUpdate_user_4 :: LocaleUpdate
testObject_LocaleUpdate_user_4 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = AE})}}

testObject_LocaleUpdate_user_5 :: LocaleUpdate
testObject_LocaleUpdate_user_5 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.TO, lCountry = Just (Country {fromCountry = SY})}}

testObject_LocaleUpdate_user_6 :: LocaleUpdate
testObject_LocaleUpdate_user_6 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Just (Country {fromCountry = AG})}}

testObject_LocaleUpdate_user_7 :: LocaleUpdate
testObject_LocaleUpdate_user_7 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.GU, lCountry = Nothing}}

testObject_LocaleUpdate_user_8 :: LocaleUpdate
testObject_LocaleUpdate_user_8 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Nothing}}

testObject_LocaleUpdate_user_9 :: LocaleUpdate
testObject_LocaleUpdate_user_9 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.FY, lCountry = Nothing}}

testObject_LocaleUpdate_user_10 :: LocaleUpdate
testObject_LocaleUpdate_user_10 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = SC})}}

testObject_LocaleUpdate_user_11 :: LocaleUpdate
testObject_LocaleUpdate_user_11 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.TS, lCountry = Just (Country {fromCountry = NU})}}

testObject_LocaleUpdate_user_12 :: LocaleUpdate
testObject_LocaleUpdate_user_12 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KU, lCountry = Nothing}}

testObject_LocaleUpdate_user_13 :: LocaleUpdate
testObject_LocaleUpdate_user_13 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = CH})}}

testObject_LocaleUpdate_user_14 :: LocaleUpdate
testObject_LocaleUpdate_user_14 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Just (Country {fromCountry = MT})}}

testObject_LocaleUpdate_user_15 :: LocaleUpdate
testObject_LocaleUpdate_user_15 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.SA, lCountry = Just (Country {fromCountry = CI})}}

testObject_LocaleUpdate_user_16 :: LocaleUpdate
testObject_LocaleUpdate_user_16 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.FA, lCountry = Just (Country {fromCountry = MC})}}

testObject_LocaleUpdate_user_17 :: LocaleUpdate
testObject_LocaleUpdate_user_17 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.OS, lCountry = Just (Country {fromCountry = MD})}}

testObject_LocaleUpdate_user_18 :: LocaleUpdate
testObject_LocaleUpdate_user_18 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.HI, lCountry = Just (Country {fromCountry = PG})}}

testObject_LocaleUpdate_user_19 :: LocaleUpdate
testObject_LocaleUpdate_user_19 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = SZ})}}

testObject_LocaleUpdate_user_20 :: LocaleUpdate
testObject_LocaleUpdate_user_20 = LocaleUpdate {luLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = ML})}}
