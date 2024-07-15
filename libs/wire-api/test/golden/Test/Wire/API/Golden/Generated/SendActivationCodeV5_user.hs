-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.SendActivationCodeV5_user where

import Data.ISO3166_CountryCodes (CountryCode (AO, BB, FI, FR, IN, MU, PM, VI, VU))
import Data.LanguageCodes qualified (ISO639_1 (CU, DE, DV, FI, GD, GN, HO, HY, IU, KK, KW, PA, TG, VE))
import Imports (Bool (False, True), Either (Left, Right), Maybe (Just, Nothing))
import Wire.API.User
  ( Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    Phone (Phone, fromPhone),
  )
import Wire.API.User.Activation (SendActivationCodeV5 (..))

testObject_SendActivationCodeV5_user_1 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_1 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+77566129334842"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Just (Country {fromCountry = VI})}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_2 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_2 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "\1021635", emailDomain = "nK"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DE, lCountry = Nothing}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_3 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_3 =
  SendActivationCodeV5
    { saUserKey =
        Left
          ( Email
              { emailLocal = "#\ACK\1103236l\1069771F\147486",
                emailDomain = "-\DC32\1101045\&1\DC2\1014718\167922\SO\68149"
              }
          ),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.GN, lCountry = Just (Country {fromCountry = VU})}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_4 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_4 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "b", emailDomain = "4M\1076452P\149723$[\DC2j"}),
      saLocale = Nothing,
      saCall = False
    }

testObject_SendActivationCodeV5_user_5 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_5 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "test", emailDomain = "example.com"}),
      saLocale = Nothing,
      saCall = False
    }

testObject_SendActivationCodeV5_user_6 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_6 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+38093636958"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = IN})}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_7 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_7 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "B+l\1054055\1082148", emailDomain = "\a%"}),
      saLocale = Nothing,
      saCall = True
    }

testObject_SendActivationCodeV5_user_8 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_8 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "\NUL3", emailDomain = "\59252g\155998\11926Ea?\DC2\\\DC4"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HO, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_9 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_9 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "Rn\STXv", emailDomain = "(\NULN"}),
      saLocale = Nothing,
      saCall = False
    }

testObject_SendActivationCodeV5_user_10 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_10 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "\t\1040376\NUL2\160662t\152821", emailDomain = "^s"}),
      saLocale = Nothing,
      saCall = True
    }

testObject_SendActivationCodeV5_user_11 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_11 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "rT", emailDomain = "a\tL\DC4"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.HY, lCountry = Just (Country {fromCountry = BB})}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_12 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_12 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+6599921229041"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = MU})}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_13 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_13 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+260369295110"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Nothing}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_14 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_14 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "B;b\164357\DC1\SIHm\DC3{", emailDomain = "?\64159Jd\f"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KW, lCountry = Just (Country {fromCountry = PM})}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_15 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_15 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "\1024828\DC1", emailDomain = "t=\69734\42178\1032441,AG2"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.IU, lCountry = Just (Country {fromCountry = FR})}),
      saCall = False
    }

testObject_SendActivationCodeV5_user_16 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_16 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "O_\37211\1022996^t", emailDomain = ""}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FI, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_17 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_17 =
  SendActivationCodeV5
    { saUserKey = Left (Email {emailLocal = "T\vI9H}C\STX\SO\1017900", emailDomain = "\151457\35555=N"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.PA, lCountry = Just (Country {fromCountry = AO})}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_18 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_18 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+715068856505655"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_19 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_19 =
  SendActivationCodeV5
    { saUserKey = Right (Phone {fromPhone = "+22888251856"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.GD, lCountry = Just (Country {fromCountry = FI})}),
      saCall = True
    }

testObject_SendActivationCodeV5_user_20 :: SendActivationCodeV5
testObject_SendActivationCodeV5_user_20 =
  SendActivationCodeV5 {saUserKey = Right (Phone {fromPhone = "+8943652812"}), saLocale = Nothing, saCall = True}
