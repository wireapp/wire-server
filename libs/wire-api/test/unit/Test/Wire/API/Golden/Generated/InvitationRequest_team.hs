{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.InvitationRequest_team where
import Data.ISO3166_CountryCodes
    ( CountryCode(HN, NE, BR, YE, MX, HT, IE, SX, AG, SZ) )
import Imports ( Maybe(Nothing, Just) )
import qualified Data.LanguageCodes
    ( ISO639_1(HR, EO, SM, RM, MK, SL, IA, EN, LA, SS, SQ, LT) )
import Wire.API.Team.Invitation ( InvitationRequest(..) )
import Wire.API.Team.Role
    ( Role(RoleOwner, RoleMember, RoleAdmin, RoleExternalPartner) )
import Wire.API.User.Identity
    ( Email(Email, emailLocal, emailDomain), Phone(Phone, fromPhone) )
import Wire.API.User.Profile
    ( Locale(Locale, lLanguage, lCountry),
      Name(Name, fromName),
      Country(Country, fromCountry),
      Language(Language) )

testObject_InvitationRequest_team_1 :: InvitationRequest
testObject_InvitationRequest_team_1 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EO, lCountry = Just (Country {fromCountry = NE})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "6\1046404?Xu\163349>\1022949\NAK\\\45852s\179979D:\ACK\EOTBkH\DELo\1105270*i\18009$\NUL\DC2gwv\1064992\&0\SO\13895kt\1030521\DEL\NUL\142298\"}\183757/-/\172578B\\{M\30440?;\1019178\n\SYNV\1076800\179985\DLE\FSM\SYN\160582Su\22060\64685\SO\NAK\1039654Ps\ENQ\1084480\1103134\&7\1053172\DLE;c+\1049749b\DEL{\1070641\CAN\DLE"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\25334q0"}, irInviteePhone = Just (Phone {fromPhone = "+021968750"})}
testObject_InvitationRequest_team_2 :: InvitationRequest
testObject_InvitationRequest_team_2 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "b;\r\ETXma", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+06405863905483"})}
testObject_InvitationRequest_team_3 :: InvitationRequest
testObject_InvitationRequest_team_3 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SM, lCountry = Just (Country {fromCountry = BR})}), irRole = Just RoleMember, irInviteeName = Just (Name {fromName = "yV\1035621I#\r\917802(H\SUBJ\1076970D^n*L\1077897\145631C)\1106204T\59129\STX)\fh\9496\1041404\&9\ENQ\1053729\42616\15427\b\19670`\t\1097633\"^\SOH\EOTZ?\1024964\21761R6\GS\ENQh^}n\1113447k\2859\1086123\t2Mf\SUB#\"\DC22\n</\992034\1043058\ESCE\996019\NAK\37969\1039066.6\ESC[\ACK9R\1072918M(g\vu"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "\\\1062653"}, irInviteePhone = Just (Phone {fromPhone = "+113687447"})}
testObject_InvitationRequest_team_4 :: InvitationRequest
testObject_InvitationRequest_team_4 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.RM, lCountry = Nothing}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "Q#\SYNd\tU|\RS1\EMT{\54231.YL1P\STX.{o\ETXV&~\STX\b<!J\99525q-){\1056229?`\CAN\4524\ETB\165695\v\70440R\1108516\b\DC1M2d\49481"}), irInviteeEmail = Email {emailLocal = "l", emailDomain = "L"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_5 :: InvitationRequest
testObject_InvitationRequest_team_5 = InvitationRequest {irLocale = Nothing, irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\136204h[S", emailDomain = "\EOT"}, irInviteePhone = Just (Phone {fromPhone = "+26464888282"})}
testObject_InvitationRequest_team_6 :: InvitationRequest
testObject_InvitationRequest_team_6 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.MK, lCountry = Just (Country {fromCountry = YE})}), irRole = Nothing, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\"\1050616!O\DC2", emailDomain = "'9\DC2kHc"}, irInviteePhone = Just (Phone {fromPhone = "+219042712314455"})}
testObject_InvitationRequest_team_7 :: InvitationRequest
testObject_InvitationRequest_team_7 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SL, lCountry = Nothing}), irRole = Nothing, irInviteeName = Just (Name {fromName = "\52176#!\34929\1100541\1076839v\t7S/'9\4252&3\35903\"9Q\1071213d\\\DELl\181969*e\GS@\\I\999257V;n:\137776\1012990B\DEL?@H$X\35225T\160453\16331\&2HV8F6\SI{\SO6[\993070\1074612\142324I\2092\999907\\l1v\987846\NULPS1$c\ETBp6x\96124C:US\158020\60169L "}), irInviteeEmail = Email {emailLocal = "|Z\1027663\1004071b\13769", emailDomain = "\SI\158816\1056657Si\NUL"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_8 :: InvitationRequest
testObject_InvitationRequest_team_8 = InvitationRequest {irLocale = Nothing, irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\158809\GSPZ\999279C\nJ(\USKEp@s\1096094|\NULi.|\FS^$\b\155194\\Vd\151090\n\1010797\42194\"\1088359-\n\ACK/1\ETX\1036031zE{f\DC3\1047064\1031200(\1084049\DLE{?\140026\&0\t%.\143878\n\72099f\DC3m{35l\"\988249\&0\ESC<\SIdrU\1001687"}), irInviteeEmail = Email {emailLocal = "$5ODQ\1096300", emailDomain = "n\DC3"}, irInviteePhone = Just (Phone {fromPhone = "+2421729268848"})}
testObject_InvitationRequest_team_9 :: InvitationRequest
testObject_InvitationRequest_team_9 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.IA, lCountry = Just (Country {fromCountry = MX})}), irRole = Just RoleAdmin, irInviteeName = Just (Name {fromName = "~+"}), irInviteeEmail = Email {emailLocal = "\t\178698D\181275G", emailDomain = "3\163093_\NULW\v"}, irInviteePhone = Just (Phone {fromPhone = "+013003227583"})}
testObject_InvitationRequest_team_10 :: InvitationRequest
testObject_InvitationRequest_team_10 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EN, lCountry = Just (Country {fromCountry = HT})}), irRole = Nothing, irInviteeName = Just (Name {fromName = "\1046521$#\NUL\1008412\1100209M\EM\164683\54132&~:f\1084370\156022\DLEyt/q$\t\ESC\1108432\1096539\f\ACK\50819\STX\ENQ!\NUL\168473\25434\&7\187732)0\70018-R\52146`\"t3FU7YaoD_3?\180146zh"}), irInviteeEmail = Email {emailLocal = "\163122u\174356$", emailDomain = "U$$\1094370\142198"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_11 :: InvitationRequest
testObject_InvitationRequest_team_11 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.EN, lCountry = Just (Country {fromCountry = IE})}), irRole = Just RoleAdmin, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "K\bk71", emailDomain = "OKq\EM\EM#"}, irInviteePhone = Just (Phone {fromPhone = "+42189764819"})}
testObject_InvitationRequest_team_12 :: InvitationRequest
testObject_InvitationRequest_team_12 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LA, lCountry = Just (Country {fromCountry = SX})}), irRole = Just RoleExternalPartner, irInviteeName = Just (Name {fromName = "$\11726\&8\FS[\1013387\22816\&1ad\73742m<\147965\DC1Po\1108407d\US:\1113347\15095{\1057114@\51202>'\1054880~*&'lqIsQT\ENQ"}), irInviteeEmail = Email {emailLocal = "\170167", emailDomain = "\995030\nR\RS"}, irInviteePhone = Just (Phone {fromPhone = "+480565638"})}
testObject_InvitationRequest_team_13 :: InvitationRequest
testObject_InvitationRequest_team_13 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SS, lCountry = Just (Country {fromCountry = AG})}), irRole = Nothing, irInviteeName = Just (Name {fromName = "\NAK\a6\1070724F=\1008534\1066459\DELi\f9v\CAN<u\n\164392\1065947}\DEL9x\1087605\&0\1089077\"Xx\\jD\1109548|Wg\n\65888\DC1\r\DC1Hh\28919\30342\STXH#\f\1066534L\155159\27765\42897\33932\r\1038172|\150548s\28292\1081546U\47235.r)F"}), irInviteeEmail = Email {emailLocal = "}\1047911\RS", emailDomain = ""}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_14 :: InvitationRequest
testObject_InvitationRequest_team_14 = InvitationRequest {irLocale = Nothing, irRole = Just RoleOwner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\1046925}\SYN\1040642\a5", emailDomain = ""}, irInviteePhone = Just (Phone {fromPhone = "+00200197"})}
testObject_InvitationRequest_team_15 :: InvitationRequest
testObject_InvitationRequest_team_15 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.SQ, lCountry = Just (Country {fromCountry = SZ})}), irRole = Just RoleAdmin, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = ":\141352=n\DC2", emailDomain = "w#%"}, irInviteePhone = Just (Phone {fromPhone = "+90119298580172"})}
testObject_InvitationRequest_team_16 :: InvitationRequest
testObject_InvitationRequest_team_16 = InvitationRequest {irLocale = Nothing, irRole = Just RoleExternalPartner, irInviteeName = Nothing, irInviteeEmail = Email {emailLocal = "\GS\FS\164163", emailDomain = "_2<8\GSu"}, irInviteePhone = Nothing}
testObject_InvitationRequest_team_17 :: InvitationRequest
testObject_InvitationRequest_team_17 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.LT, lCountry = Nothing}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "\1069521\996037\1027805\15948\998400"}), irInviteeEmail = Email {emailLocal = "\181870L*", emailDomain = "6\"\RS\ACK"}, irInviteePhone = Just (Phone {fromPhone = "+89479829625937"})}
testObject_InvitationRequest_team_18 :: InvitationRequest
testObject_InvitationRequest_team_18 = InvitationRequest {irLocale = Nothing, irRole = Nothing, irInviteeName = Just (Name {fromName = "w\50517\ENQc|\6028\1095515kS\ETXp\GS\ESC\47111\1085502e\1053081\183200\f\1080313XQ\ENQ\r\CAN\1001690k\1080723\rC"}), irInviteeEmail = Email {emailLocal = "", emailDomain = "v\NAK"}, irInviteePhone = Just (Phone {fromPhone = "+36613212"})}
testObject_InvitationRequest_team_19 :: InvitationRequest
testObject_InvitationRequest_team_19 = InvitationRequest {irLocale = Nothing, irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "C:Z\SOr\SI=?`d\1045263\3731\v\1055117\EM\t\154360H2\1084117\t\SYN9\1031209F?`0P\b^Iyq\94995\18567-jpmHf>1p \1004109x\GS\b\STX\1046973\989405"}), irInviteeEmail = Email {emailLocal = "\DEL", emailDomain = "R$l\1000415"}, irInviteePhone = Just (Phone {fromPhone = "+15753647239"})}
testObject_InvitationRequest_team_20 :: InvitationRequest
testObject_InvitationRequest_team_20 = InvitationRequest {irLocale = Just (Locale {lLanguage = Language Data.LanguageCodes.HR, lCountry = Just (Country {fromCountry = HN})}), irRole = Just RoleOwner, irInviteeName = Just (Name {fromName = "$\164767r\v\1100438r\1050177\DC3P\1035889\39235>Er#\146770\161205\162406\163787\EM"}), irInviteeEmail = Email {emailLocal = "2\14206\&6x", emailDomain = "\1067480!A\1004653"}, irInviteePhone = Nothing}
