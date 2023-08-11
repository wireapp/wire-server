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

module Test.Wire.API.Golden.Generated.SendActivationCode_user where

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
import Wire.API.User.Activation (SendActivationCode (..))

testObject_SendActivationCode_user_1 :: SendActivationCode
testObject_SendActivationCode_user_1 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+77566129334842"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Just (Country {fromCountry = VI})}),
      saCall = False
    }

testObject_SendActivationCode_user_2 :: SendActivationCode
testObject_SendActivationCode_user_2 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "\1021635", emailDomain = "nK"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.DE, lCountry = Nothing}),
      saCall = False
    }

testObject_SendActivationCode_user_3 :: SendActivationCode
testObject_SendActivationCode_user_3 =
  SendActivationCode
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

testObject_SendActivationCode_user_4 :: SendActivationCode
testObject_SendActivationCode_user_4 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "b", emailDomain = "4M\1076452P\149723$[\DC2j"}),
      saLocale = Nothing,
      saCall = False
    }

testObject_SendActivationCode_user_5 :: SendActivationCode
testObject_SendActivationCode_user_5 =
  SendActivationCode {saUserKey = Right (Phone {fromPhone = "+883124214493"}), saLocale = Nothing, saCall = False}

testObject_SendActivationCode_user_6 :: SendActivationCode
testObject_SendActivationCode_user_6 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+38093636958"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.DV, lCountry = Just (Country {fromCountry = IN})}),
      saCall = False
    }

testObject_SendActivationCode_user_7 :: SendActivationCode
testObject_SendActivationCode_user_7 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "B+l\1054055\1082148", emailDomain = "\a%"}),
      saLocale = Nothing,
      saCall = True
    }

testObject_SendActivationCode_user_8 :: SendActivationCode
testObject_SendActivationCode_user_8 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "\NUL3", emailDomain = "\59252g\155998\11926Ea?\DC2\\\DC4"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HO, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCode_user_9 :: SendActivationCode
testObject_SendActivationCode_user_9 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "Rn\STXv", emailDomain = "(\NULN"}),
      saLocale = Nothing,
      saCall = False
    }

testObject_SendActivationCode_user_10 :: SendActivationCode
testObject_SendActivationCode_user_10 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "\t\1040376\NUL2\160662t\152821", emailDomain = "^s"}),
      saLocale = Nothing,
      saCall = True
    }

testObject_SendActivationCode_user_11 :: SendActivationCode
testObject_SendActivationCode_user_11 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "rT", emailDomain = "a\tL\DC4"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.HY, lCountry = Just (Country {fromCountry = BB})}),
      saCall = False
    }

testObject_SendActivationCode_user_12 :: SendActivationCode
testObject_SendActivationCode_user_12 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+6599921229041"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = MU})}),
      saCall = True
    }

testObject_SendActivationCode_user_13 :: SendActivationCode
testObject_SendActivationCode_user_13 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+260369295110"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Nothing}),
      saCall = False
    }

testObject_SendActivationCode_user_14 :: SendActivationCode
testObject_SendActivationCode_user_14 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "B;b\164357\DC1\SIHm\DC3{", emailDomain = "?\64159Jd\f"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.KW, lCountry = Just (Country {fromCountry = PM})}),
      saCall = False
    }

testObject_SendActivationCode_user_15 :: SendActivationCode
testObject_SendActivationCode_user_15 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "\1024828\DC1", emailDomain = "t=\69734\42178\1032441,AG2"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.IU, lCountry = Just (Country {fromCountry = FR})}),
      saCall = False
    }

testObject_SendActivationCode_user_16 :: SendActivationCode
testObject_SendActivationCode_user_16 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "O_\37211\1022996^t", emailDomain = ""}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.FI, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCode_user_17 :: SendActivationCode
testObject_SendActivationCode_user_17 =
  SendActivationCode
    { saUserKey = Left (Email {emailLocal = "T\vI9H}C\STX\SO\1017900", emailDomain = "\151457\35555=N"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.PA, lCountry = Just (Country {fromCountry = AO})}),
      saCall = True
    }

testObject_SendActivationCode_user_18 :: SendActivationCode
testObject_SendActivationCode_user_18 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+715068856505655"}),
      saLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Nothing}),
      saCall = True
    }

testObject_SendActivationCode_user_19 :: SendActivationCode
testObject_SendActivationCode_user_19 =
  SendActivationCode
    { saUserKey = Right (Phone {fromPhone = "+22888251856"}),
      saLocale =
        Just (Locale {lLanguage = Language Data.LanguageCodes.GD, lCountry = Just (Country {fromCountry = FI})}),
      saCall = True
    }

testObject_SendActivationCode_user_20 :: SendActivationCode
testObject_SendActivationCode_user_20 =
  SendActivationCode {saUserKey = Right (Phone {fromPhone = "+8943652812"}), saLocale = Nothing, saCall = True}
