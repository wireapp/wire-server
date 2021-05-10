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

module Test.Wire.API.Golden.Generated.User_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Handle (Handle (Handle, fromHandle))
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( AQ,
        BD,
        BJ,
        BS,
        CN,
        CY,
        DM,
        ES,
        HK,
        MO,
        MQ,
        NF,
        PL,
        SB,
        TG,
        TN,
        UA
      ),
  )
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.LanguageCodes
  ( ISO639_1
      ( BA,
        BI,
        CU,
        CY,
        DA,
        DE,
        EE,
        ET,
        KA,
        LA,
        LI,
        MI,
        MR,
        NE,
        PL,
        TG,
        TL,
        TN,
        TW,
        VE
      ),
  )
import Data.Qualified
  ( Qualified (Qualified, qDomain, qUnqualified),
  )
import qualified Data.UUID as UUID (fromString)
import Imports
  ( Bool (False, True),
    Maybe (Just, Nothing),
    fromJust,
  )
import Wire.API.Provider.Service
  ( ServiceRef (ServiceRef, _serviceRefId, _serviceRefProvider),
  )
import Wire.API.User
  ( Asset (ImageAsset),
    AssetSize (AssetComplete, AssetPreview),
    ColourId (ColourId, fromColourId),
    Country (Country, fromCountry),
    Email (Email, emailDomain, emailLocal),
    Language (Language),
    Locale (Locale, lCountry, lLanguage),
    ManagedBy (ManagedByScim, ManagedByWire),
    Name (Name, fromName),
    Phone (Phone, fromPhone),
    Pict (Pict, fromPict),
    User (..),
    UserIdentity
      ( EmailIdentity,
        FullIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    UserSSOId (UserSSOId, UserScimExternalId),
  )

testObject_User_user_1 :: User
testObject_User_user_1 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), qDomain = Domain {_domainText = "s-f4.s"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+340639706578"})), userDisplayName = Name {fromName = "\NULuv\996028su\28209lRi"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TN, lCountry = Just (Country {fromCountry = SB})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T06:39:23.932Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), userManagedBy = ManagedByWire}

testObject_User_user_2 :: User
testObject_User_user_2 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), qDomain = Domain {_domainText = "k.vbg.p"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+837934954"})), userDisplayName = Name {fromName = "4\1067195\&7\ACK\DC2\DC2\ETBbp\SOH\40601\&0Yr\\\984611vKRg\1048403)\1040186S\983500\1057766:3B\ACK\DC3\ETXT"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.DA, lCountry = Just (Country {fromCountry = TN})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T17:06:58.936Z")), userTeam = Nothing, userManagedBy = ManagedByWire}

testObject_User_user_3 :: User
testObject_User_user_3 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "dt.n"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") Nothing (Just (Phone {fromPhone = "+025643547231991"}))), userDisplayName = Name {fromName = ",r\EMXEg0$\98187\RS\SI'uS\ETX/\1009222`\228V.J{\fgE(\rK!\SOp8s9gXO\21810Xj\STX\RS\DC2"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TG, lCountry = Just (Country {fromCountry = UA})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "1c"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T20:12:05.821Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), userManagedBy = ManagedByWire}

testObject_User_user_4 :: User
testObject_User_user_4 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), qDomain = Domain {_domainText = "28b.cqb"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "^\1025896F\1083260=&o>f<7\SOq|6\DC1\EM\997351\1054148\ESCf\1014774\170183\DC3bnVAj`^L\f\1047425\USLI\ENQ!\1061384\ETB`\1041537\ETXe\26313\SUBK|"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = MQ})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "mzebw5l9p858om29lqwj5d08otrwzzickuh_s8dpookvkl_ryzbsvw-ogxrwyiw2-.udd2l7us58siy2rp024r9-ezsotchneqgalz1y1ltna7yg3dfg.wzn4vx3hjhch8.-pi3azd9u3l-5t6uyjqk93twvx_3gdh32e82fsrdpf8qfsi2ls-a2pce8p1xjh7387nztzu.q"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T14:25:26.089Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), userManagedBy = ManagedByScim}

testObject_User_user_5 :: User
testObject_User_user_5 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "k.ma656.845z--u9.34.4ot8v.p6-2o"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "f", emailDomain = "\83115"})), userDisplayName = Name {fromName = "[|u\aFH\1083955\DC3\164710\179183k#\1067479fN4\SUB#G\1003889\SOkK\GS\1047735yP\1065258|H\129482\vi\rAcUp\SO\US"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.ET, lCountry = Just (Country {fromCountry = NF})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "hb"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T20:43:46.798Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), userManagedBy = ManagedByScim}

testObject_User_user_6 :: User
testObject_User_user_6 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "u8-9--eppc-k-02.l5-ci5zk"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") Nothing (Just (Phone {fromPhone = "+90270460"}))), userDisplayName = Name {fromName = "\992528RiM2\DC3\1104965#5T-~=#\SI\1059840rf\994293}\SOH\172366\\K\148731\DC2I\n\SOz'\35982\SUB\SYN>p\1107992\FS\\(\167236\1032144\1042866\DELj\1050995M|\167476\DC4c\181439`/\tpU]#\SI\1056223(9\NAKV\148251\&23i\141711\EMgs[\US:\\\1072651"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "\1099856" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TW, lCountry = Just (Country {fromCountry = BD})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "hp95jkglpreb88pm0w.35i6za1241dt2el.8s1msvq2u4aov_muws4n4xdvv-ocd95oqqbb7.eqdi1hmudsh_9h0nt0o0gtkpnm7xu494-nl6ljfoxsxlm.66l8ny3yejd2fqb5y.zpi2rgo-f8yhkwl0k7.a91kdxflxx4.am_ka62kebtexj97f07bko4t2.6tr1rx1cbabnk0w_dz714nmenx8bscvdw8_ay1o"}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByWire}

testObject_User_user_7 :: User
testObject_User_user_7 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), qDomain = Domain {_domainText = "7sb.43o7z--k8.k-7"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "o"})), userDisplayName = Name {fromName = "$]\EOTe<&\aKfM"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset ";" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.VE, lCountry = Just (Country {fromCountry = BS})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "t.w_8."}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T16:08:44.186Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), userManagedBy = ManagedByScim}

testObject_User_user_8 :: User
testObject_User_user_8 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "2v.k55u"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = ""})), userDisplayName = Name {fromName = "?\142624\101002w\nLi\b\DC1{[8\nd}\29988.Wh^z\74534\3120V}\vAPy\DC2sgvk\1020150 5\1049847(5\v\US_y\44245dcfc\51598\8475)fK\DC2A\996460\1061546zu\1067558GEk\vQ\1060756\144328\NUL\997313\&5\38619\v"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KA, lCountry = Just (Country {fromCountry = AQ})}, userService = Nothing, userHandle = Nothing, userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByWire}

testObject_User_user_9 :: User
testObject_User_user_9 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "9.fs7-3.x-0"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "\ESC"}) (Phone {fromPhone = "+783368053"})), userDisplayName = Name {fromName = "P\1059549o1qr1(k\987545-\USW\SYN\92334TX9F@\GS\EM\DEL\CAN`\20651\ENQ4\EOT@"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TL, lCountry = Just (Country {fromCountry = MO})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "qptpyy3"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T13:40:26.091Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), userManagedBy = ManagedByScim}

testObject_User_user_10 :: User
testObject_User_user_10 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), qDomain = Domain {_domainText = "j6-m-9-nc.b4k"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "u[\68811\"W\172389\1051681\EMS\1048905x\DC1J\DC3\NAKN\1067266I\1034426\FS\"\1047349&\GS\SO\165324\DC21r\ESC1S\1016718\RS+V\v\SIt%\1085478\ACK\1072392\ENQ\t\6277Q\1028565v`\1079541q\GS\95671\RSW\67856I\1029796\1040562\aK/\DLE\1036794}\1050591\49895A5*\1050100"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.PL, lCountry = Just (Country {fromCountry = CN})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "xyz9jol-j7bb9xtifngv1wejx-ekud7-c-koevsi-e.gcubdvvibrsjmz_1uzq8acxu62oqzn8v9nkz"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T20:52:50.974Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), userManagedBy = ManagedByScim}

testObject_User_user_11 :: User
testObject_User_user_11 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "94.eg8.s1.u"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+9360086324"})), userDisplayName = Name {fromName = "\ENQ*U>o:\171361;\1002213+\31270'm\1067198\1088531\&3X!ELsR\GS%\29917\ETX\25722\DC2d#Q\DEL\DC1(H\1057328Al+E[\SOH\1043898\DC4)\149666\174107\1040634\NUL\1035995\ESC\GS\GS\DC1q\a_\1044144.Cq;O\1061742\t\RS\SUB\32927\20189\157326"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = PL})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "qgfs79695bljg0-2gg7d7eigfry969_boy88f_pt2ecyxmaos66dluhp1r2zf1lzdbu8bksy6mq8zoxyh6lh5-nwligu0o-_4kt5zny65hsfa.ydan6c1kftchxh8jlzype-hbfz6821v.ow-ugddhvbz5cii8b23hhnrkz7xbphzz9st4nfft7"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T17:03:19.878Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), userManagedBy = ManagedByScim}

testObject_User_user_12 :: User
testObject_User_user_12 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "36c48v3.j22"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "aM\16681\DEL\t_%\"z>3\144231\DC3!yrxp\17091\a]0RQp5}v\182602bn6\988436\995116g\FSDC\1039960\rNl\SYN\\\1095728\100544\1050922_\146297$,_kk\t.C brXI\1080901\"+p\1094835\DELH4'$\DC3\USv?rf0d5w\1072160\&0"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.DE, lCountry = Just (Country {fromCountry = CY})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "jor0tv5x7jcptbelj4lb8asjp6-1knhjb0y44uxc5m7apnyzwqg-v.cnnbpxmbr_7tcmygsn5wvnjtb8uzvprai6ayk4kp9gcwtkpsadfs7bqz9qk6.nyeone71vfmmfnvw0f6._4apxbqrpju3v-z-l0osvpfdaajsyyr2bvdq_sffgw12.9gr3zl_d43rrc5.zz0xhxqqvv12l85t2u31_c-gdbr"}), userExpire = Nothing, userTeam = Nothing, userManagedBy = ManagedByScim}

testObject_User_user_13 :: User
testObject_User_user_13 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), qDomain = Domain {_domainText = "x-90ql.5.8-he1.9t.04f-0v83.4p.nic71"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+525773726575872"})), userDisplayName = Name {fromName = "/\50476e9s\US\ACK\1033357*\1073446!;$$rF4?\12694\b~{]\ENQ~\1061469Ic&&\119840S\39434\1063726<cL\11680{j\1034905\1100618=-\ENQ\59318\1008088Jm\\\1025607\53537l)\162905\&21d1\v\ACKP$\ESCW\DC25\1034492\5488"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "H" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BA, lCountry = Just (Country {fromCountry = DM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "jq-b_m8hdt36tw9a4owiboeuv8rzxih0g"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T06:01:03.394Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), userManagedBy = ManagedByWire}

testObject_User_user_14 :: User
testObject_User_user_14 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "dj26-05k20.6o.u82981ti"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = ""})), userDisplayName = Name {fromName = "\59251ER\176067:\1031692\a\1037041H\1013878,t[Q\SIGU]?R7I\NAK6"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = ES})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "mhgl9x16g1xz79.0nc_d8bwb2_pei918-60"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T04:00:27.919Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), userManagedBy = ManagedByWire}

testObject_User_user_15 :: User
testObject_User_user_15 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), qDomain = Domain {_domainText = "u94.8-o.t278r"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "1Oz%kWY\65463\\z3\1060623\NAKs\1025837-,\t\17224\3342_[\992987*"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NE, lCountry = Just (Country {fromCountry = BJ})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "wmsb2zu9clwp64c_.4ia1kskbmm4cydu1hasbtczyssh9xy5fw6ub0p41xjr9onur5.raa."}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T23:08:57.869Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), userManagedBy = ManagedByScim}

testObject_User_user_16 :: User
testObject_User_user_16 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "7.o5m.o8x5t79t-vpy9"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+204681013138537"})), userDisplayName = Name {fromName = "@!\129031U<iA\FS\1033168\DC2\CAN1\n\DC4\987665[x\139318o}\1024394P\DC3G\DC2m\a8\1024408x?\159235{\a\141125|i\"~B\167109x]E$k"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CY, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "heowgl9k2xx9s.141ch3ll39je4l4srhqmzi.vvev29yq7-fpgn2d6kim8"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T19:30:23.627Z")), userTeam = Nothing, userManagedBy = ManagedByWire}

testObject_User_user_17 :: User
testObject_User_user_17 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), qDomain = Domain {_domainText = "iws.f6"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "_)l@^\STX'=\1075610\1044135\&6f^\191228<{G\1007951\tM\1012134\DELW{H05\179301\ESC&\ETX\DC1X\26655<D\1006283\64769w\10759\&7\990366\167530\a\1080569\1042215AtAQIM\1060629\DC3Vp\1096667\&6?g\991426+\FS\STX|~a\142278\STX\FS~\a4)\RS\t\1046905\SUBF\DEL\US:JFVN__\ESC<"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.LA, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "markynabp"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T18:35:27.453Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), userManagedBy = ManagedByScim}

testObject_User_user_18 :: User
testObject_User_user_18 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "5rdu.v49c00u"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+0535364775"}))), userDisplayName = Name {fromName = "\EM[e#Du\ETBm\39768\1086882\996449\&6\ty\1098401"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.LI, lCountry = Just (Country {fromCountry = TG})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "ip"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T22:44:19.375Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), userManagedBy = ManagedByWire}

testObject_User_user_19 :: User
testObject_User_user_19 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), qDomain = Domain {_domainText = "m.gs-h8m3"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = "\28493"}) (Phone {fromPhone = "+8377346869260"})), userDisplayName = Name {fromName = "[\SOsj\52515qP\132382\&1ZOl^\NAK=o\29931z\ETB\v\SUB}\GS\a=\27218\177392\66178*c9u\\)1l\STX\17639\17250<ug\26471\nu\fK1\18245nT\t\149194D\1064521\27330\v\NAK\1095784\165917Uz>{\96735oW\151219\62244\134274?\1041546&&,\r\a>9V5&e+O\EOT?m\1048021M\176823V\DEL\1070807\1040396\v\1033296\145519#jm@\1033513x\1099446\b\7714\&1gZ"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MR, lCountry = Just (Country {fromCountry = HK})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "m4o"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T17:37:12.497Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), userManagedBy = ManagedByWire}

testObject_User_user_20 :: User
testObject_User_user_20 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "6mlxl.2v5.gd7"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+43245195312227"})), userDisplayName = Name {fromName = "bW\\&\166022\141394\\k\153943\25617\1075373\&0\NAKi\63940GRh\NAKh\CANO\ETBJ\1011164s_\989976jV\SI\DC1\STX\1106211~N(\ETXrj1r^\1081784Zaw\150200\1086815j\131932\1028541\185724\134546N\SOH\30641ex8*\39157A\DC2nkzr\31591{j\SOH-g\1012550=d<\\Pc+K(vZ\FS"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "apdh51n9mpxew4sose_n_mu"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T13:00:07.275Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), userManagedBy = ManagedByWire}
