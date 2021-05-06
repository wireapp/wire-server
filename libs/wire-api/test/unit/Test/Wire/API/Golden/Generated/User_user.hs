{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.User_user where
import Data.Domain ( Domain(Domain, _domainText) )
import Data.Handle ( Handle(Handle, fromHandle) )
import Data.Id ( Id(Id) )
import Data.ISO3166_CountryCodes
    ( CountryCode(NF, MM, MY, BG, MN, ID, HM, ES, JM, MR, IM, CG, IQ,
                  AL, LB, AX) )
import Data.Json.Util ( readUTCTimeMillis )
import Data.Qualified
    ( Qualified(Qualified, qUnqualified, qDomain) )
import Imports
    ( Bool(True, False), Maybe(Just, Nothing), fromJust )
import qualified Data.LanguageCodes
    ( ISO639_1(PS, MI, TS, IO, BE, CE, CU, NE, MR, XH, EE, AB, KW, ID,
               BM, SC, BH, BI, AR) )
import qualified Data.UUID as UUID ( fromString )
import Wire.API.Provider.Service
    ( ServiceRef(ServiceRef, _serviceRefId, _serviceRefProvider) )
import Wire.API.User
    ( Locale(Locale, lLanguage, lCountry),
      Name(Name, fromName),
      ColourId(ColourId, fromColourId),
      Asset(ImageAsset),
      Email(Email, emailLocal, emailDomain),
      Phone(Phone, fromPhone),
      User(..),
      UserIdentity(EmailIdentity, SSOIdentity, FullIdentity,
                   PhoneIdentity),
      UserSSOId(UserScimExternalId),
      AssetSize(AssetPreview, AssetComplete),
      Country(Country, fromCountry),
      Language(Language),
      ManagedBy(ManagedByScim, ManagedByWire),
      Pict(Pict, fromPict) )

testObject_User_user_1 :: User
testObject_User_user_1 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "341.2g576v292.04q8.e2qfwa.873.l8zl3a.j73i6"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") Nothing (Just (Phone {fromPhone = "+440605618682841"}))), userDisplayName = Name {fromName = "#\DC1e\t\78045\n\FS\DC2J-1\1084089\SI\CAN8NB\a\DEL)Y[G6{}\DC4\SUBCXj1zx\SOHs{2S:U\1027128\ETB\CAN\97019Z\bq[\1107302\1113476iY'Q\1100224 Pj{\EOTud"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "5" (Nothing))], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MI, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = ".u"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), userManagedBy = ManagedByWire}
testObject_User_user_2 :: User
testObject_User_user_2 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "9h.x4.ey.xo--wf-w2"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "f", emailDomain = ""})), userDisplayName = Name {fromName = "\59916\1098559[\1058573\&5=e\137117{d\1032333\1098191\46042\&2a<4[1V\SO\1078958\143020*M\1096142\STX\DC2l\141755>^\ACKs\29994,\50591N=\25088N\1085053\164793+9\1044208\&1\DC2\GSI0@4pZ\NAKG\1020028\SYNR&cc#Gk\131489H\1004852\&2\SI"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.TS, lCountry = Just (Country {fromCountry = MM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "4_cf"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), userManagedBy = ManagedByWire}
testObject_User_user_3 :: User
testObject_User_user_3 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "xw-5.sq0"}}, userIdentity = Nothing, userDisplayName = Name {fromName = ";}\STX"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = MY})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "ud5rcawsz67e29d79tknxz53fzsktv3ok_jgrx2yd_t5rllq4u9riiblzos9.4md1rv-wt6oack-k2k7esy6a6cfir6fdni.dtk4y5xhyqu50fdlbqkti840j6kmv"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T21:00:44.513Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), userManagedBy = ManagedByWire}
testObject_User_user_4 :: User
testObject_User_user_4 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), qDomain = Domain {_domainText = "j--a.cy81g78.s3k"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\1058239\1004646c\141132]V4\1113680[s"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.IO, lCountry = Just (Country {fromCountry = BG})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "zmtoigykdnky8xlawa6nx1yey50r10zgev_6z87n4aog-cw_h8n2m5r4jrjgjxn1x4d1vdcviv"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T12:20:56.088Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), userManagedBy = ManagedByWire}
testObject_User_user_5 :: User
testObject_User_user_5 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), qDomain = Domain {_domainText = "68-p.y-c"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\ESC\v#\1096508$@)\FS\NAKX\RSu1'\1001594{*0Sx\133986\1066899C=\986428\1015400\42094\&1\GS|\FSci\1038544\STXz\143193\1042722\&9Y\EM\38888\&6Ha"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BE, lCountry = Just (Country {fromCountry = MN})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "w8-qd77mwrlg9h_px-iah8esg8gva076gtmoui5x-9-bjj3rfsf8gsah3gukcgklrb-h1ze-rwf_.wptb5kgk7o3ci-7sczogr-.uxqd3euvy8moi_2vteu0qxbw4-v2j3r5i67071auj4r"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T10:34:37.452Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), userManagedBy = ManagedByScim}
testObject_User_user_6 :: User
testObject_User_user_6 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "6l37.g90.y89"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "\ENQ", emailDomain = ""}) (Phone {fromPhone = "+62462012"})), userDisplayName = Name {fromName = "\1039744\SO%\162288^u\n\164201\1000826\1038878\78321\ni\1005452\74621\1060926L8%\FSpvF*\DC2@L\ETBO\ESC\SI\t\177828\1036438\ACK\CAN\25846\1081198\FS\1060643\SO\95657\1093480\1025525R\DLE]L(S\DC2.?u\45762{\1091261gH\ACKl\1097327\SO\NAK8\991898#Y\144940\42238\170903w]\STX\SI\97468X\DC2&&\1060294\1105086BT\60735\1079839\a\39397[\1070541\152769\1001821\169345l\EMh\DLE\1056105,\aA6\129410\fXU\EOT\SI\1112445lx}6\1012965\995121Y"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CE, lCountry = Just (Country {fromCountry = ID})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "wdjay4il8z9c5n.hbu00dm7f6_owjni-ky8uokp.e_m_gvauf0picfmg5sykry"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T10:41:22.693Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), userManagedBy = ManagedByWire}
testObject_User_user_7 :: User
testObject_User_user_7 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "bjawerr.p7"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\2654", emailDomain = "E"})), userDisplayName = Name {fromName = "\RS\1105967(\94403x1'"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.CU, lCountry = Just (Country {fromCountry = HM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T11:35:22.017Z")), userTeam = Nothing, userManagedBy = ManagedByWire}
testObject_User_user_8 :: User
testObject_User_user_8 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), qDomain = Domain {_domainText = "r7qvb2jx.88.lie.na2"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+1716025871"})), userDisplayName = Name {fromName = "99_\NUL\1039200WeSsQ3/T\170265p\SOHLx.0\SICyw\7619{\1041033Aj%yPW\23070\1069367\ENQU\DC1\SOHsE*kfm2P\1040113rVk\83384u?h=a\1049011\ACK\atF\1062703x"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.NE, lCountry = Nothing}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T15:45:25.910Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), userManagedBy = ManagedByWire}
testObject_User_user_9 :: User
testObject_User_user_9 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), qDomain = Domain {_domainText = "8q60j.x6-9"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "\r_\SIJm\159822\&1\aO\ETX\DC1;\CANH|"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.MR, lCountry = Just (Country {fromCountry = ES})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}), userHandle = Just (Handle {fromHandle = "wbiei"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T01:59:34.373Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), userManagedBy = ManagedByScim}
testObject_User_user_10 :: User
testObject_User_user_10 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "16.30u.8v1.x"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) Nothing), userDisplayName = Name {fromName = "O\49532|@s[\FSLN\NAK\DC4\t)\f\SOH*=$W\70808\GS\"\998832\tN\"'=\25317\&8\1038068CT|*WZL\148877\995054\1043969\1051079\ETX\996282\DC2\143187\986017gc\98808)\1101093{\RS}\n!\1053066\987702\DC4L1\1107283#e\SYNcI\v(X\a\aOSIWb\ttj\165711/\NUL`?\23058\1031007e\1109416\CANigs\DC1\EOT\158893\177576\9594Y"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.XH, lCountry = Just (Country {fromCountry = JM})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "o0zgsh2m-sjk7v559l94fafsnodnw.2p.2q37mn6zb2pipdtnunqyejinm5dzkwcd-ccobn4q83_cgz"}), userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), userManagedBy = ManagedByScim}
testObject_User_user_11 :: User
testObject_User_user_11 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "6258.6r5b7.412a1.x7.i.b-yxb-0"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") (Just (Email {emailLocal = "", emailDomain = ""})) (Just (Phone {fromPhone = "+559267270669176"}))), userDisplayName = Name {fromName = "\SOi\4201\1112771\ETXS$;6O]dp\a<3Q\1112531C\1111276\DLE^4~Jvu\61770\168637E\1102308\SUB\53773%p\SO\164994i\FSP\fB\156267\1023066'QY \EM\52153?o\71841\48105\129120J*\a+\DLE\1024357AC\ESCY3l\EOTS \1054956\SUB\1050802+W(e\49930\25193B\1042887\16285h&\135180y@dg2\29815\&0*M\USNG\10198\53836"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -1}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.EE, lCountry = Just (Country {fromCountry = MR})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "hvkh."}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-08T19:19:48.761Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), userManagedBy = ManagedByScim}
testObject_User_user_12 :: User
testObject_User_user_12 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), qDomain = Domain {_domainText = "311z.z82f7-fqa"}}, userIdentity = Just (SSOIdentity (UserScimExternalId "") Nothing (Just (Phone {fromPhone = "+871868908"}))), userDisplayName = Name {fromName = "\DC1\1050551\120218x<\1045513%!j/L>\"\1038590M\t\f`s$\1055110V\SUB\1103434[P08A\148087\DELE\f\DC3l\49906\1113102\181442\&10\1029570i\a\ESC\1021660\26210\166004)L\tx"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], userAccentId = ColourId {fromColourId = -1}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AB, lCountry = Just (Country {fromCountry = IM})}, userService = Nothing, userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), userManagedBy = ManagedByScim}
testObject_User_user_13 :: User
testObject_User_user_13 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), qDomain = Domain {_domainText = "umev.v92-8-rw85-u9"}}, userIdentity = Just (FullIdentity (Email {emailLocal = "s", emailDomain = "9"}) (Phone {fromPhone = "+211717437715843"})), userDisplayName = Name {fromName = "\429\35920\1093500\&2\189497&j\DC3\NUL\FS\ETX\EMD4v\EOT\73010\n2r\16658k\CANV\ESC\a\DC4Y\1061199\SUBegI\28024q \ACK\ESCL\GSR'\1003509w\\\132150nao\160240\1058101\DEL\EOTA\GS\SI\98606i+\RS+i\DC1r=B[kF\54090\59687<&_>u2K6?W\n\SI\51052\13535?\165592\NUL\CAN\100778\1052228\1048143z\58129[\DEL\1069617\&6\181916{DzN\1042917a\ACKX"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.KW, lCountry = Just (Country {fromCountry = CG})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}), userHandle = Just (Handle {fromHandle = "v8yt-nb_t98wjq_07aw71_fdbi7iy32om64inco84i5i76s_kkbwv_e2pwil-h4se66la_gbr95g5mot8s50"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T13:31:10.512Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), userManagedBy = ManagedByScim}
testObject_User_user_14 :: User
testObject_User_user_14 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), qDomain = Domain {_domainText = "2.2a.l.69o.uv4r"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "Q,\1015086\STXC2@\a\67639\&1q\"\ACK=\ro\188743\ETX+\1051229\1049960FY\1020640\1079336}\35367\&1z\CAN\43369QQwkh+9}\ACKK\STX\1065048D\DLEZQaX\156668_M\"\1022577\&0\32034{(O\EOT\1111677\&7\20758\2132\b\1046922]}\1075330l3\1060258\120485f/\EMa\167282\USz\97785}Pqh\SYN\b\SO{|\US\51978>\817\20539\&6\1095023\6071\&6\1004397 V\fA\131821\NAK;\986305s\1006725A\1035376E\SO\132879;(1b{_\1013577\1029160\&9"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "F" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.ID, lCountry = Just (Country {fromCountry = IQ})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}), userHandle = Nothing, userExpire = Nothing, userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), userManagedBy = ManagedByScim}
testObject_User_user_15 :: User
testObject_User_user_15 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "bvz-2e.c.9.5qnxg4.671.o30sq3g1"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "X\1010107mT)<\14500\DC2\EM\b"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BM, lCountry = Just (Country {fromCountry = AL})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "xk9_um91tr"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T00:50:56.436Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), userManagedBy = ManagedByScim}
testObject_User_user_16 :: User
testObject_User_user_16 = User {userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), qDomain = Domain {_domainText = "tm--0k.aucw"}}, userIdentity = Nothing, userDisplayName = Name {fromName = "EG;mF\1002531$\SYN\163214(\1092341$U\aF\182284g_\147881?2\1038131A~<\180360\"L3\78519> .;\163874{\1059332.Wa}\US\SUB\128034C\1052157f]\159921\\@e7\ENQ\v\ETX\EM4\b\DC1=]\t>$\USK<QA]'8\r#U:on\1005341a(\SI\31674Z\43811\SUB4\1102683"}, userPict = Pict {fromPict = []}, userAssets = [], userAccentId = ColourId {fromColourId = 0}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.SC, lCountry = Nothing}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "frrac6n"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-09T05:08:12.383Z")), userTeam = Nothing, userManagedBy = ManagedByWire}
testObject_User_user_17 :: User
testObject_User_user_17 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), qDomain = Domain {_domainText = "0s-b.l68zo5395b9"}}, userIdentity = Just (PhoneIdentity (Phone {fromPhone = "+1132326304"})), userDisplayName = Name {fromName = "1;\66020$x\RS+oq~\1087275\SOH~\38861'\EOT\1005478\ACK"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BH, lCountry = Just (Country {fromCountry = LB})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))}), userHandle = Just (Handle {fromHandle = "-mswcwxfmvrbz71da.9s.j-v7ydu1326tuwzp_-knhue58p09xsltw5-4ppn.uz6ruo1-pl_yh0kbnzrldy.lpjq6qge33_35t4ozma_f3goc98l-7v-snlm.4ksv58rgcfot2x_t.oqumocd5xfsvl58zf8kb4c8rxl65gxubyapvsjy3-.idm1x.xbodg4f-crqv8o8q3cft_t153v8ubi53-xa_.c"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T02:41:22.575Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), userManagedBy = ManagedByWire}
testObject_User_user_18 :: User
testObject_User_user_18 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), qDomain = Domain {_domainText = "e--s.7.mhy-e"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "", emailDomain = "C"})), userDisplayName = Name {fromName = "9\1106461|1lD\153326Q^\120158\15182XAqw\1003483k\134682\r-;st\DC3\ENQ\51110\DC1|[\ACKu=\GSA\EOT"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 2}, userDeleted = False, userLocale = Locale {lLanguage = Language Data.LanguageCodes.BI, lCountry = Just (Country {fromCountry = AX})}, userService = Nothing, userHandle = Just (Handle {fromHandle = "hnuc54_y6589snr1l-rewu.-ih9go.zbn98nein2h.dj67d2g2oai8tdedaiyuo4--tqc93ufzoqbul6_g479urfy-mk5051c-3ssq7-us0w6efgiuf29czsf0z76nvmjriylog_fpnrr_nlgrekhf1q6py"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-11T09:58:40.934Z")), userTeam = Nothing, userManagedBy = ManagedByWire}
testObject_User_user_19 :: User
testObject_User_user_19 = User {userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), qDomain = Domain {_domainText = "433y9.k4f2pk49fa52r"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "]", emailDomain = ""})), userDisplayName = Name {fromName = ")\DC12{\172649U\b\n\141315\&4{L\1067791\159164zd\a\ETX\SUB\1109597$\1037070\GS\1049053V\US/\1065401\&1~!\n+\ACK\187798+\167519p.\EM>D\1062669\13146AQkY\96399\1096707\SYN\1109466\GS\140328\53034~\120233\1088246\182550\&6wcG\985178\30046<GV\177532!`\SO4%\ACKo\47745FGm\169998\&1\ETX\1087157U\RS\1005337\1103928\179981\174346\14149s,\SOHk\DC1\151240-rr\"i9\f"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], userAccentId = ColourId {fromColourId = -2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.AR, lCountry = Just (Country {fromCountry = NF})}, userService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), userHandle = Just (Handle {fromHandle = "rc"}), userExpire = Just (fromJust (readUTCTimeMillis "1864-05-10T00:18:46.020Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), userManagedBy = ManagedByWire}
testObject_User_user_20 :: User
testObject_User_user_20 = User {userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), userQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), qDomain = Domain {_domainText = "dg-4368b.h-n-36.5v.l34"}}, userIdentity = Just (EmailIdentity (Email {emailLocal = "\f", emailDomain = ""})), userDisplayName = Name {fromName = ".\45385\DC4\999005.\165149\DELS{I/A\DC4L\ETX1\"\119346BCx\ENQkPFG`LA\ENQ\ESC4k\1037948:\SYN\1022005\DC3\SUByP\100081\t\SOvQ\DC15\\\NAK}\aH\ACK\1093391\&1v\990846\ETX\ESC\RS"}, userPict = Pict {fromPict = []}, userAssets = [(ImageAsset "" (Nothing))], userAccentId = ColourId {fromColourId = 2}, userDeleted = True, userLocale = Locale {lLanguage = Language Data.LanguageCodes.PS, lCountry = Just (Country {fromCountry = NF})}, userService = Nothing, userHandle = Nothing, userExpire = Just (fromJust (readUTCTimeMillis "1864-05-07T09:47:25.077Z")), userTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), userManagedBy = ManagedByScim}
