{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.SelfProfile_user where

import Data.Domain (Domain (Domain, _domainText))
import Data.Handle (Handle (Handle, fromHandle))
import Data.ISO3166_CountryCodes
  ( CountryCode
      ( AX,
        BG,
        CZ,
        FI,
        FM,
        GG,
        MV,
        NF,
        OM,
        PA,
        SB,
        SN,
        SY,
        TH,
        VE
      ),
  )
import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import qualified Data.LanguageCodes
  ( ISO639_1
      ( AF,
        AZ,
        BS,
        CA,
        CO,
        GL,
        HR,
        ID,
        MI,
        OJ,
        RM,
        SS,
        TR,
        TS,
        UK,
        UZ,
        ZH
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
    SelfProfile (..),
    User
      ( User,
        userAccentId,
        userAssets,
        userDeleted,
        userDisplayName,
        userExpire,
        userHandle,
        userId,
        userIdentity,
        userLocale,
        userManagedBy,
        userPict,
        userQualifiedId,
        userService,
        userTeam
      ),
    UserIdentity
      ( EmailIdentity,
        FullIdentity,
        PhoneIdentity,
        SSOIdentity
      ),
    UserSSOId (UserSSOId, UserScimExternalId),
  )

testObject_SelfProfile_user_1 :: SelfProfile
testObject_SelfProfile_user_1 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "n0-994.m-226.f91.vg9p-mj-j2"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\a", emailDomain = ""}) (Phone {fromPhone = "+6171884202"})), userDisplayName = Name {fromName = "@\1457\2598\66242\US\1104967l+\137302\&6\996495^\162211Mu\t"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.GL, lCountry = Just (Country {fromCountry = PA})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "do9-5"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T21:09:29.342Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_2 :: SelfProfile
testObject_SelfProfile_user_2 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "h2rphp.47t1.pw0"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+28532238745460"})), userDisplayName = Name {fromName = "\1103516\2538SYM\64914\nem\DC3\SO\STX\177763THme\37118\44852_Bo>%G<u\153557i\150385a:%*\DC1\n:Y"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Nothing)), (ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AZ, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T07:51:14.715Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_3 :: SelfProfile
testObject_SelfProfile_user_3 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "60.3pc.a7t75j.9-3sx0.au-rt.y"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\NUL", emailDomain = ""}) (Phone {fromPhone = "+3387841470"})), userDisplayName = Name {fromName = "Sbv\EMB\RS/\tzoCrN\42239\NAK>udt\1053110\11460\189470M\1015177\174215W+uU[5*\NUL\1002875\SOH\1096054\"\v=a\62902V3P\STX\1097050\ACK\177430@\tjUS\"\1094171Sng:3m^\\\173582\&5\17288\25248\&6l\1064236\CAN\12119\1103520\&4[\SYNbL!"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.UK, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "6o7n9"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T19:59:51.146Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_4 :: SelfProfile
testObject_SelfProfile_user_4 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "fa3gz465.g-2"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "", emailDomain = ""}) (Phone {fromPhone = "+30745803086"})), userDisplayName = Name {fromName = "/RCd\163236\184924:\ESCa%[\10448\185549\110838F\f@#{U\rn\176533"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AF, lCountry = Just (Country {fromCountry = MV})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "sncnzzz_ffzdy-8.70xb9gni-jtexm4lbr4h9an"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T10:07:20.481Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_5 :: SelfProfile
testObject_SelfProfile_user_5 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "90i1.84arbm9252qg.b"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+11141922"})), userDisplayName = Name {fromName = "\EOT\187939dE0K\173164:\1099392^v\143120\1111400G0\189824_7\136563a\ACK\1003768&p\SUB`\r6\ro\\\1054757[\SYN>9lx\21499V\35547p\FS\1013678<>j{"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "\66706" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TR, lCountry = Just (Country {fromCountry = CZ})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "29"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T10:30:21.640Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_6 :: SelfProfile
testObject_SelfProfile_user_6 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), qDomain = Domain {_domainText = "u54.h8--m0--752"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+73308549330"})), userDisplayName = Name {fromName = "\1063629\"3@n\1057001\184249z6w3lCo+\128041]6\ENQ\1068518\&2\1059602EVQ\NUL\1056407\&1\STX\FS^\21231}C"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = VE})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_7 :: SelfProfile
testObject_SelfProfile_user_7 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "8.071.c"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = ":", emailDomain = "\a"})), userDisplayName = Name {fromName = "\FSz\1000675\1090431(Y\22123Z5FGu'<pv\r\"\1040755?\1042849&$]e\1059300}\1072428\DC4\ACK\RSkSQ`f\32511`\DC1!\68869\99211\SOJ\129169\164295\RS\60768\&2lZ\180742 \125242:M\t[\156171p{$\SI\SYN]DP.\131345\ETX\145346\SO\1000964@s4w \vI\13935\&0\1029620\1069952 "}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.ZH, lCountry = Just (Country {fromCountry = AX})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "_5dpks7l"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T09:08:35.327Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_8 :: SelfProfile
testObject_SelfProfile_user_8 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "f-s.w"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "F\40642\5804f\19469z\1093839\166717\&8]\EOT+\1070933\984240"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Just (Country {fromCountry = SY})}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T19:17:59.357Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_9 :: SelfProfile
testObject_SelfProfile_user_9 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "8amhh5l.m59x.pkki81"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\n", emailDomain = "0"})), userDisplayName = Name {fromName = "H\48506\DC3\ETX\1056010b\993451\&3q\a$&5\GS\DC4~;\37058H\1029072\&34\34751\GSlYfn,a\FS\1032657\bVT\1025851wd\20208d\SI2\f\1062953!~P;"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "=" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.UZ, lCountry = Nothing}, userService = Nothing, userHandle = Just (Handle {fromHandle = "l62ooa"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T16:59:33.440Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_10 :: SelfProfile
testObject_SelfProfile_user_10 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), qDomain = Domain {_domainText = "0.orr"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\SIdW\998557T\GS\rpx\ESCc\"\1103856\194904@\142706\a\1086986x;\NUL\1098070*\27831\25315(\95175\171475B$6G(\\st\SUBiQ\36422$"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TR, lCountry = Just (Country {fromCountry = GG})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "tq-xm3ywkgplimqfqyd76v696af.zcb2e-svk45z3uw2ba8gxok1gyjy1st01f3ocq6"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T13:37:46.490Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_11 :: SelfProfile
testObject_SelfProfile_user_11 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "j.8-1.of"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "tWWWXt\RS\64525i\ESC\DC4\ETB\SIS\620D\DC43\EMQ\917896}G4r\1021984-\DC3eyf2\31469\1056191f,\EOT~\EM\187305q\71485\164618\74173\&8W\1064892B\1038703Mhj\n\1101626\ENQ)o\"O\STX\SYN;\DEL\1095902\DC4\1113106\1077431c9s\FS,\SUB-w\49589Y!<\ENQ|\ETX\35353"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Just (Country {fromCountry = TH})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "qk"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T20:12:10.459Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000001"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_12 :: SelfProfile
testObject_SelfProfile_user_12 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "i-w8.nea5"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\47153", emailDomain = "U"})), userDisplayName = Name {fromName = "\EM\26705f+\v=]\1089374u9\SYNv\1067143\NULaT\RSKK\70294o87g2T\41714aOU\r;_jUZ`/\GSR\EOTs\USo@:\a\1019436we4g\1087481\1060054 \NULj"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CA, lCountry = Just (Country {fromCountry = OM})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "iz.9z-r0txd10beaq3awhdi00as2ttk4w_-sb73-qa0e-bj9q-do_d.1wivilwgag_da6hy_nx._d_ranqfh94mv4-tisuwevxjcw94-h60ae6-x1tptoboa-x2a1s08q4wywul3rusm37hl.vjn8en__837eodq8134tecr8qzpm2c2hle7ao_wa85hk"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_13 :: SelfProfile
testObject_SelfProfile_user_13 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "7.kr1-96m-s--x1g3"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "\1083875yc\35160y\1113711&\EM\STX\1043122{N>\FS\17032O=]\54557i<`\b1mBS\72203\165141t\36134)x\171180[\1082300\1088848m\53260\&5\163423\1056927\DC2\DELp\FS\1053595\EM ,T*\ETXs.\GST:\DLE\1016590z/#8-e*W\174370W\r\1028201\SI\ENQ*}\190459%\1091895\174313{\21423\DC2'N\US/\50988Z*E\146470\ETB"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.ID, lCountry = Nothing}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T20:25:30.218Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_14 :: SelfProfile
testObject_SelfProfile_user_14 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "o33xllsl.br.w1a-cl"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\1100477", emailDomain = ""}) (Phone {fromPhone = "+3606473750"})), userDisplayName = Name {fromName = "\1015853dp\RSW@\DC3$i$)\153322R\1017265\DC4d\DC3\DC4B.\ETX\14719\SOH\DC1x\1043832g\132826\1066923\EM)\DC4K)e=\SI#s\47617d\167409Q\1066458\12557\154949\1041394:mZ\1101237V\1002766\1052091\1053704^y\US\178686\1101090N\120367;%\EOT\1002404(eW&C@yh\DC4\DEL%\6473i\ETB"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = SN})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "9_hc2_-en1a8jck-tkni14wqqw6mx16tzlmo87gw3xu811i9424ku8fbmpl_hf06nus61lza7_kslu"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_15 :: SelfProfile
testObject_SelfProfile_user_15 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), qDomain = Domain {_domainText = "w-csl2vx.rpb.fq2"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\1008204", emailDomain = "("})), userDisplayName = Name {fromName = "2\b\1033582\"\f\SI{?b\"\n>q\SIe8D\993505\1091482\1069161{{eX|-q\b3*59v\1035474s\51424\ESC\1063527\917628wG!uAO\rBZ\GSF[4\8087\v|\"\NAK4b\CANtE\DC2YB\ACK\NAKl5D>%P`\163216\bZ"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)), (ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CO, lCountry = Just (Country {fromCountry = FM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "ncyk2udev3vg1bl_ujr0ff4fwymv_j_5lcse8b.c99i--lwnquz4mpbqzmrc_2ok_ytgqeov4bkkn_l"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_16 :: SelfProfile
testObject_SelfProfile_user_16 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "76y01l79xajp.u5p8-qo--om"}}, userIdentity = Just (SSOIdentity (UserSSOId "" "") Nothing (Just (Phone {fromPhone = "+673892193308"}))), userDisplayName = Name {fromName = "\SUB\1052182\CANp\GS\1056488\146522k\1021341\1009355\32387\1072693\148602\1035440\1017171mzSJ"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.ZH, lCountry = Just (Country {fromCountry = NF})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "v36sek51j__9i-w67.0foj6fpsrb_8-54_4c7yqld4cxu4emk0s67-f0oqyippzwxh9hmbrc-i0vpl0m-ww53-pku0kjb_6uprh4n6wg.xn7n9xp0t_5t.r_itjjmxjgkxud0ih083c6vscdlb-wex8no_4vlo.2llhidhq0awu3xr0craik"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T06:04:44.922Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_17 :: SelfProfile
testObject_SelfProfile_user_17 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), qDomain = Domain {_domainText = "2.dh4"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+023372401614100"}))), userDisplayName = Name {fromName = "T\1028174\SOK\NAKIwh%\92445C\SI!\1073767(*Iq\1032573\DEL'W\150542c=\STXMAK@\47619\US\t)^x\CAN\CAN\\^'s9\57735\DC1Q\65408.3\a5\1070124\ESC\EM\54276\SUB\1102011\1032606\EOTg+W(;W[\DC2!\41026>\69665:5\1008122\EMY-,\DC3\SYNi\25185\DLE\t\139316!a\SO=\1087548\1030610\187180\CAN=0\STX6,\FSzr("}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TS, lCountry = Just (Country {fromCountry = FI})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "6huo"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T08:41:37.172Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), userManagedBy = ManagedByScim}}

testObject_SelfProfile_user_18 :: SelfProfile
testObject_SelfProfile_user_18 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), qDomain = Domain {_domainText = "1.t3yc3"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "}Mr$\38120Fi~O\EMb\\\19207k\1085532\1039196\r^n\1112567\&8\187061\1010217\CAN\DC2xj\RS\1030094\ah\EM\DEL\188337)WJ>Y\1070138[\CAN\989394ed\1113772,\31471=\RSHmMV%x-^;_\SIun"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BS, lCountry = Just (Country {fromCountry = SB})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T16:10:28.222Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_19 :: SelfProfile
testObject_SelfProfile_user_19 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), qDomain = Domain {_domainText = "8y.o9"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "?", emailDomain = "S"})), userDisplayName = Name {fromName = "\996756\&2\1108160\92546\DC3B\NAK5\1066367"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)), (ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.HR, lCountry = Just (Country {fromCountry = BG})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "-bc4hz9hn8ep81cxp.9_jy4wl-w2h8o34wb1we4.77yp9oai6le1fm_lshwh4_j5dhzzpkidmg23t75bzjvms7x-7v.ru1l7cqkkci9uynit6kbwinsy4fug55j5p6pek_9d5g90sx7jgixu3teh_dvo.a-l79pgpxs4iov569j4bnpv-4lck0qj5vjv.5sb9p47w_.5lfyuqcwrpeq.fqfl9miil.epxsert-dh1"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), userManagedBy = ManagedByWire}}

testObject_SelfProfile_user_20 :: SelfProfile
testObject_SelfProfile_user_20 = SelfProfile {selfUser = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), qDomain = Domain {_domainText = "l2.y"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+23457350508"})), userDisplayName = Name {fromName = "\1093767\172141o\1005690\129309.b\134607\f5UDRv8T(\SOT\997389N*GQ\ENQ\ESCjtl\SIDK_;\"v\1099332\SUBr\ACKI\133837Y\50543z%5$[\DC3\rQji.\NUL\1048415j\n\ESC\f\165699\"I\ETB"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)), (ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.OJ, lCountry = Nothing}, userService = Nothing, userHandle = Just (Handle {fromHandle = "v89lg6khwz.ruz1ngo032582p5z2qvmk92m58_5x688fqurqg..j2p9wa.pipqhunk.q-cdtnvntvv16whxmbay63licg.v9nm_bnn1xdlovj7_wa..hwx-horp6oj8yzqne_49qpsdh.shj8q9rjh7384.mhk1244pay9tiale9433tmz7q9upc0lh5wurqo5wpnyidivhrtgk-jm.6wc-02ptct33e"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), userManagedBy = ManagedByScim}}
