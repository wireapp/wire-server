{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AddBot_user where
import Data.Id ( Id(Id) )
import Data.ISO3166_CountryCodes
    ( CountryCode(BT, JE, GH, CL, AQ, KZ, PW, SV, AE, SK, IR, SI) )
import Imports ( Maybe(Just, Nothing), fromJust )
import qualified Data.LanguageCodes
    ( ISO639_1(LU, KK, SA, SU, TL, LB, BH, RW, SE, SC, AA, SV, NR,
               NL) )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Conversation.Bot ( AddBot(..) )
import Wire.API.User
    ( Locale(Locale, lLanguage, lCountry),
      Country(Country, fromCountry),
      Language(Language) )

testObject_AddBot_user_1 :: AddBot
testObject_AddBot_user_1 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-001c-0000-00080000001f"))), addBotService = (Id (fromJust (UUID.fromString "00000018-0000-0018-0000-00070000000f"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.KK, lCountry = Just (Country {fromCountry = JE})})}
testObject_AddBot_user_2 :: AddBot
testObject_AddBot_user_2 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001d-0000-001b-0000-00060000000b"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-0013-0000-00020000001d"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NR, lCountry = Just (Country {fromCountry = GH})})}
testObject_AddBot_user_3 :: AddBot
testObject_AddBot_user_3 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-0020-0000-00140000001a"))), addBotService = (Id (fromJust (UUID.fromString "0000000b-0000-0016-0000-00180000001f"))), addBotLocale = Nothing}
testObject_AddBot_user_4 :: AddBot
testObject_AddBot_user_4 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000005-0000-0012-0000-001400000011"))), addBotService = (Id (fromJust (UUID.fromString "0000000a-0000-0018-0000-000b0000001e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SA, lCountry = Just (Country {fromCountry = CL})})}
testObject_AddBot_user_5 :: AddBot
testObject_AddBot_user_5 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000002-0000-001b-0000-001a0000000d"))), addBotService = (Id (fromJust (UUID.fromString "00000000-0000-000a-0000-001a0000001e"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SU, lCountry = Nothing})}
testObject_AddBot_user_6 :: AddBot
testObject_AddBot_user_6 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0004-0000-002000000011"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-0005-0000-000300000001"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.TL, lCountry = Just (Country {fromCountry = AQ})})}
testObject_AddBot_user_7 :: AddBot
testObject_AddBot_user_7 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001f-0000-001d-0000-00170000001d"))), addBotService = (Id (fromJust (UUID.fromString "00000012-0000-001b-0000-001800000000"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LB, lCountry = Just (Country {fromCountry = KZ})})}
testObject_AddBot_user_8 :: AddBot
testObject_AddBot_user_8 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000b00000013"))), addBotService = (Id (fromJust (UUID.fromString "00000013-0000-0016-0000-00010000000a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = PW})})}
testObject_AddBot_user_9 :: AddBot
testObject_AddBot_user_9 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000013-0000-000c-0000-001e00000006"))), addBotService = (Id (fromJust (UUID.fromString "00000016-0000-0012-0000-001900000001"))), addBotLocale = Nothing}
testObject_AddBot_user_10 :: AddBot
testObject_AddBot_user_10 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000009-0000-0019-0000-000a00000002"))), addBotService = (Id (fromJust (UUID.fromString "0000000e-0000-0014-0000-000a00000009"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RW, lCountry = Just (Country {fromCountry = SV})})}
testObject_AddBot_user_11 :: AddBot
testObject_AddBot_user_11 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000001e-0000-0004-0000-000500000019"))), addBotService = (Id (fromJust (UUID.fromString "00000008-0000-0015-0000-001a00000000"))), addBotLocale = Nothing}
testObject_AddBot_user_12 :: AddBot
testObject_AddBot_user_12 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000011-0000-0013-0000-001b00000006"))), addBotService = (Id (fromJust (UUID.fromString "00000007-0000-000d-0000-001100000017"))), addBotLocale = Nothing}
testObject_AddBot_user_13 :: AddBot
testObject_AddBot_user_13 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000c-0000-0011-0000-001600000002"))), addBotService = (Id (fromJust (UUID.fromString "00000004-0000-001d-0000-00010000000c"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SE, lCountry = Just (Country {fromCountry = AE})})}
testObject_AddBot_user_14 :: AddBot
testObject_AddBot_user_14 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000013-0000-0015-0000-000800000009"))), addBotService = (Id (fromJust (UUID.fromString "0000001d-0000-000d-0000-001800000018"))), addBotLocale = Nothing}
testObject_AddBot_user_15 :: AddBot
testObject_AddBot_user_15 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-001c00000015"))), addBotService = (Id (fromJust (UUID.fromString "00000019-0000-000f-0000-00050000001c"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SC, lCountry = Just (Country {fromCountry = SK})})}
testObject_AddBot_user_16 :: AddBot
testObject_AddBot_user_16 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000017-0000-0020-0000-001d0000000f"))), addBotService = (Id (fromJust (UUID.fromString "00000019-0000-0005-0000-001100000008"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.AA, lCountry = Nothing})}
testObject_AddBot_user_17 :: AddBot
testObject_AddBot_user_17 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000000-0000-001e-0000-001800000020"))), addBotService = (Id (fromJust (UUID.fromString "0000000e-0000-0009-0000-00020000000c"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SV, lCountry = Just (Country {fromCountry = IR})})}
testObject_AddBot_user_18 :: AddBot
testObject_AddBot_user_18 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "0000000c-0000-0016-0000-000900000005"))), addBotService = (Id (fromJust (UUID.fromString "0000001b-0000-0009-0000-00100000000a"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NR, lCountry = Nothing})}
testObject_AddBot_user_19 :: AddBot
testObject_AddBot_user_19 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000010-0000-000f-0000-001300000015"))), addBotService = (Id (fromJust (UUID.fromString "00000017-0000-0002-0000-000b00000016"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.NL, lCountry = Just (Country {fromCountry = SI})})}
testObject_AddBot_user_20 :: AddBot
testObject_AddBot_user_20 = AddBot {addBotProvider = (Id (fromJust (UUID.fromString "00000014-0000-0017-0000-001300000013"))), addBotService = (Id (fromJust (UUID.fromString "00000011-0000-0004-0000-001a00000003"))), addBotLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LU, lCountry = Just (Country {fromCountry = BT})})}
