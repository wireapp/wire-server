{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Wire.API.Golden.Generated.AddBot_user where

import Data.ISO3166_CountryCodes
  ( CountryCode
      ( BN,
        BV,
        DJ,
        DK,
        EC,
        GR,
        GU,
        IQ,
        LI,
        LR,
        MK,
        MR,
        MU,
        PW,
        TD,
        TR
      ),
  )
import Data.Id (Id (Id))
import qualified Data.LanguageCodes
  ( ISO639_1
      ( AA,
        AZ,
        BA,
        BS,
        EU,
        KI,
        KV,
        MN,
        NB,
        PT,
        SD,
        SM,
        SO,
        TK,
        UG,
        UZ
      ),
  )
import qualified Data.UUID as UUID (fromString)
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Conversation.Bot (AddBot (..))
import Wire.API.User
  ( Country (Country, fromCountry),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
  )

testObject_AddBot_user_1 :: AddBot
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000015-0000-0010-0000-00110000000d"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-000c-0000-001800000010"))), addBotLocale = Nothing}

testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-0014-0000-00060000001e"))), addBotService = (Id (fromJust (UUID.fromString "00000005-0000-0018-0000-000600000005"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UZ, lCountry = Just (Country {fromCountry = GR})})}

testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001d-0000-0009-0000-001f0000000f"))), addBotService = (Id (fromJust (UUID.fromString "00000000-0000-000b-0000-000000000017"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AZ, lCountry = Just (Country {fromCountry = LI})})}

testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000012-0000-001a-0000-000d0000001c"))), addBotService = (Id (fromJust (UUID.fromString "0000001d-0000-0005-0000-001500000019"))), addBotLocale = Nothing}

testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0013-0000-001500000020"))), addBotService = (Id (fromJust (UUID.fromString "0000000b-0000-0005-0000-00060000001a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Nothing})}

testObject_AddBot_user_6 :: AddBot
testObject_AddBot_user_6 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000d-0000-001e-0000-000b00000003"))), addBotService = (Id (fromJust (UUID.fromString "0000001d-0000-0002-0000-000e00000001"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BA, lCountry = Just (Country {fromCountry = PW})})}

testObject_AddBot_user_7 :: AddBot
testObject_AddBot_user_7 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001c-0000-000e-0000-001700000018"))), addBotService = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000a00000011"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TK, lCountry = Just (Country {fromCountry = GU})})}

testObject_AddBot_user_8 :: AddBot
testObject_AddBot_user_8 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000020-0000-0018-0000-00180000000c"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-0009-0000-001200000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Just (Country {fromCountry = MK})})}

testObject_AddBot_user_9 :: AddBot
testObject_AddBot_user_9 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-0019-0000-000f00000012"))), addBotService = (Id (fromJust (UUID.fromString "0000001b-0000-000b-0000-000500000007"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.UG, lCountry = Just (Country {fromCountry = LR})})}

testObject_AddBot_user_10 :: AddBot
testObject_AddBot_user_10 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000f-0000-0016-0000-002000000010"))), addBotService = (Id (fromJust (UUID.fromString "00000000-0000-000e-0000-00000000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.PT, lCountry = Just (Country {fromCountry = MR})})}

testObject_AddBot_user_11 :: AddBot
testObject_AddBot_user_11 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000a-0000-0015-0000-00030000000c"))), addBotService = (Id (fromJust (UUID.fromString "0000001e-0000-0004-0000-00080000000e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SO, lCountry = Just (Country {fromCountry = DK})})}

testObject_AddBot_user_12 :: AddBot
testObject_AddBot_user_12 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0011-0000-001500000015"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-000a-0000-000d0000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SD, lCountry = Just (Country {fromCountry = BV})})}

testObject_AddBot_user_13 :: AddBot
testObject_AddBot_user_13 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000019-0000-001a-0000-001800000009"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-001e-0000-001d00000019"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EU, lCountry = Nothing})}

testObject_AddBot_user_14 :: AddBot
testObject_AddBot_user_14 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0008-0000-001b00000014"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-000d-0000-000800000003"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AA, lCountry = Just (Country {fromCountry = IQ})})}

testObject_AddBot_user_15 :: AddBot
testObject_AddBot_user_15 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000006-0000-000c-0000-00180000000a"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-001e-0000-00020000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NB, lCountry = Just (Country {fromCountry = TR})})}

testObject_AddBot_user_16 :: AddBot
testObject_AddBot_user_16 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001f-0000-001b-0000-000c00000008"))), addBotService = (Id (fromJust (UUID.fromString "0000000d-0000-001e-0000-001000000007"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BS, lCountry = Just (Country {fromCountry = MU})})}

testObject_AddBot_user_17 :: AddBot
testObject_AddBot_user_17 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000020-0000-001a-0000-001600000004"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-0013-0000-001600000014"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MN, lCountry = Just (Country {fromCountry = EC})})}

testObject_AddBot_user_18 :: AddBot
testObject_AddBot_user_18 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0005-0000-00010000001e"))), addBotService = (Id (fromJust (UUID.fromString "0000001a-0000-0003-0000-00020000001e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KI, lCountry = Just (Country {fromCountry = TD})})}

testObject_AddBot_user_19 :: AddBot
testObject_AddBot_user_19 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000019-0000-0014-0000-000c0000000f"))), addBotService = (Id (fromJust (UUID.fromString "0000000f-0000-0005-0000-001d00000007"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AZ, lCountry = Just (Country {fromCountry = DJ})})}

testObject_AddBot_user_20 :: AddBot
testObject_AddBot_user_20 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000f-0000-0003-0000-001400000006"))), addBotService = (Id (fromJust (UUID.fromString "00000010-0000-0014-0000-000600000016"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KV, lCountry = Just (Country {fromCountry = BN})})}
