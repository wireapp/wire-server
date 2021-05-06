{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ProviderProfile_provider where

import Codec.MIME.Type (Type(..))
import qualified Codec.MIME.Type as MIME
import Control.Lens ((.~))
import Data.Code
import Data.Coerce
import Data.Currency
import Data.Domain
import Data.Handle
import Data.Id
import Data.ISO3166_CountryCodes
import Data.Json.Util
import Data.List1
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import Data.Misc
import Data.PEM
import Data.Qualified
import Data.Range (unsafeRange)
import qualified Data.Set as Set
import Data.Text.Ascii
import Data.Time (secondsToNominalDiffTime)
import Imports hiding (LT, GT)
import qualified Data.LanguageCodes
import qualified Data.UUID as UUID
import Test.Tasty (testGroup, TestTree)
import URI.ByteString
import qualified Wire.API.Call.Config as CallConfig
import qualified Wire.API.User.Profile as User.Profile
import qualified Wire.API.Team.Conversation as Team.Conversation
import qualified Wire.API.Provider as Provider
import qualified Wire.API.Provider.Bot as Provider
import qualified Wire.API.Provider.External as Provider
import qualified Wire.API.Provider.Service as Provider
import qualified Wire.API.Provider.Service.Tag as Provider
import Data.Aeson
import GHC.Exts
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
import Wire.API.Provider.Service.Tag
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_ProviderProfile_provider_1 :: ProviderProfile
testObject_ProviderProfile_provider_1 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000300000007"))), providerName = Name {fromName = "\SO.v,vs\EM\b\ETXW[\v\1057216\100083h\184144C)#\SI\170632U\ENQ\1029277\DC2n\fXm\151316\ETB\v\ETB\1083547\DLE+\1094437\GSG^H\191244\&0H\152205\&2\SI\1040267&Rq\DC3\156702\SI+\1016333iY\NAKX{D^9\1111879\SI}\154012\1043073>d\1100198\989472\DC2F\166195\99068\DC2%2rQ\n&\ESCW\988207)\ACKYhXt-\SUB4\92941z@jg\1018171\1068117\1068935Vq\ESC\133502BA\DLE"}, providerEmail = Email {emailLocal = "\EOT?\67246\49600\EM\146475", emailDomain = "l\15911z"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\ENQ9"})
testObject_ProviderProfile_provider_2 :: ProviderProfile
testObject_ProviderProfile_provider_2 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000004"))), providerName = Name {fromName = "\DC4vYS\1056351\t3\CAN=;&\SUB^Pi\SYN\31532\NAKK4N.\SUB\DLE@\997157'Z#1Wl\18024\176952\1051896\996386\101089ei\119850[\b\1005208\1056938fO\RS8=Q\5503K#)s\f0"}, providerEmail = Email {emailLocal = "hSFC\SOH", emailDomain = "GU\1110138"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "c"})
testObject_ProviderProfile_provider_3 :: ProviderProfile
testObject_ProviderProfile_provider_3 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000004"))), providerName = Name {fromName = "&\156684\FS?na6\vG\1062629\47441\1087810"}, providerEmail = Email {emailLocal = "M#=", emailDomain = "\DC15"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "'El"})
testObject_ProviderProfile_provider_4 :: ProviderProfile
testObject_ProviderProfile_provider_4 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000700000005"))), providerName = Name {fromName = "g\DC2\SO\150393ag\SYN\ts\1004832\f\CAN|\FSo\1020036\RSh\1081941r\1089510_\SOP*\fvB;P\DC3\16753\NUL?zU\CAN\NAKj\194943\EM\1087102,\92987W\1007128l\1014408\8814c\1063816\RS2\DC2v;(\13283-By\1008089J\1059802\46407e\30102\GSwl\158990T\EOT\99549\1106197De.D\ESC\74961\t*\ESCe?\ETBz\35543g\EOTW/\b%\993666r\43809H\EM\STX\152314\DC1\46226T@\EOT3JMl!B&\FS\1084590eB\ENQ"}, providerEmail = Email {emailLocal = "\ENQ[\SYNF", emailDomain = "\126243\1058787WY"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\38681\131865\\n"})
testObject_ProviderProfile_provider_5 :: ProviderProfile
testObject_ProviderProfile_provider_5 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000700000003"))), providerName = Name {fromName = "H\DC2\133960&\27146\DEL(i3\1112792\SYN~\DEL\ESC%Ue\153602\t\1059913i\38259/D\ENQ/YH-\36082\&1o\8713U\STXd\137351hhY\DC1\170747[\ETB\n\49403\1011324B\ACK\190278\&4\1028111\32700[^D\146196"}, providerEmail = Email {emailLocal = "", emailDomain = "\1046831"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "[\1051654?r\US"})
testObject_ProviderProfile_provider_6 :: ProviderProfile
testObject_ProviderProfile_provider_6 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000002"))), providerName = Name {fromName = "\a\t\3490,J~xHWx5hY3Jx\985928\ACK@2\DC1)\31166>a\36595\162623\rW\155360I\EMn\97118#\DC3L(\SO<\162915\71169Z\15961\43039^53\1040044\SI[\1058138S[\GS\190434\&7\148214|\DC1"}, providerEmail = Email {emailLocal = "\tV", emailDomain = "\SI\987775\&0"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "^"})
testObject_ProviderProfile_provider_7 :: ProviderProfile
testObject_ProviderProfile_provider_7 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000800000000"))), providerName = Name {fromName = "T"}, providerEmail = Email {emailLocal = "\NUL", emailDomain = "zE&r"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\NULbs<"})
testObject_ProviderProfile_provider_8 :: ProviderProfile
testObject_ProviderProfile_provider_8 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000000"))), providerName = Name {fromName = " \169878\1053227][QF\DC4D5\1007025p8\1045886\ENQ%]6\1001078"}, providerEmail = Email {emailLocal = "y", emailDomain = "\142563\37207\a\26077"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\ACK\50434\8488"})
testObject_ProviderProfile_provider_9 :: ProviderProfile
testObject_ProviderProfile_provider_9 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000700000007"))), providerName = Name {fromName = "\NAKyf\1065528\4259\83190\CAN+;"}, providerEmail = Email {emailLocal = "UF\SOH\38830\bW", emailDomain = "n"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1103470\US\43490"})
testObject_ProviderProfile_provider_10 :: ProviderProfile
testObject_ProviderProfile_provider_10 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000800000002"))), providerName = Name {fromName = "NSTm\STX\ao\94840\1020683\&3K\10888\1045735\174099\140361\&9ht1\66669G?=\USB\78776\&1kh9\USUj\1064352"}, providerEmail = Email {emailLocal = "\1086553", emailDomain = "\DC2\54651"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_11 :: ProviderProfile
testObject_ProviderProfile_provider_11 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000000000001"))), providerName = Name {fromName = "\DLEa\ETBjdn\1021527\173952\991367t\157162lZt\22485\SYN'+"}, providerEmail = Email {emailLocal = "\v", emailDomain = "7e)\r"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\5721@\30084"})
testObject_ProviderProfile_provider_12 :: ProviderProfile
testObject_ProviderProfile_provider_12 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000800000007"))), providerName = Name {fromName = "uF\NUL\163744(X\33081\174930\NAK\DC1\DC4w\NAK\NAK.|+\12243\&14y\ENQ?\52054\CAN:"}, providerEmail = Email {emailLocal = "", emailDomain = "o\185147\40559"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "o"})
testObject_ProviderProfile_provider_13 :: ProviderProfile
testObject_ProviderProfile_provider_13 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000000000008"))), providerName = Name {fromName = "-\25253\991947X\1057317uF^e\4667\1097403\n\ETXW\1062933\72262\173408b\1109595t\US0\21077U\DC1\149440\ESC\1113150\165969\SYN\14297G\1068861W%E\142087\38488\FS-Av-f\ENQ\33670\"\EOTxNQ\DC4\EOT.W>U\ESC?RAi\STXDl\1049130\41437e!E\ac\10956~"}, providerEmail = Email {emailLocal = "P\43471\NAK", emailDomain = "\1008338;\GS6"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "JE\127962\5610"})
testObject_ProviderProfile_provider_14 :: ProviderProfile
testObject_ProviderProfile_provider_14 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000000000006"))), providerName = Name {fromName = "\1072688.\CANDaW_/F\41309\184578\&6A -k\DELZ?{\1094668&Il42.\DC2;@J\1085996\NULH"}, providerEmail = Email {emailLocal = "\41428^", emailDomain = "'\990137\35303\ENQ\1020475"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_15 :: ProviderProfile
testObject_ProviderProfile_provider_15 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000005"))), providerName = Name {fromName = "2\172578\&26C\58526^\r2\167380y\65164\ETB\46718]^\64095\177209\178013$/\DELh\r\1104885\f\SO/\FS^\13752\983275g\rd\SI$[\SOH\1042250\10766Ka\1073157P%\94812p\DLE\34228m\ESCz\1060900\11397p#K<\DEL\v<\DC49{\1027745(\v\162597u"}, providerEmail = Email {emailLocal = "\r\1031369\ACK}", emailDomain = "yyX\64962"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_16 :: ProviderProfile
testObject_ProviderProfile_provider_16 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000300000000"))), providerName = Name {fromName = "\41667\1085883@\41385\ESC\1051126\SYN2\SYN\34472\RS\ENQ5<\ESC.w\1088952CX\DEL\NULhoE\10704h\21097\991032\1071741\&2\1065220a\132701l"}, providerEmail = Email {emailLocal = "Y\67079\19860\158093t6", emailDomain = "x\n$"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ";a[#\NUL"})
testObject_ProviderProfile_provider_17 :: ProviderProfile
testObject_ProviderProfile_provider_17 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000200000007"))), providerName = Name {fromName = "uSDy\EOTl3\r9EEY"}, providerEmail = Email {emailLocal = "9\33093^H\987794", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "d"})
testObject_ProviderProfile_provider_18 :: ProviderProfile
testObject_ProviderProfile_provider_18 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000400000007"))), providerName = Name {fromName = "\159484\1085965\DC2F\1035339\&6\179293\1094257O\999644\1059928C0yC\FS zW\1071028)S~SF\995190 ~\144954\1047121"}, providerEmail = Email {emailLocal = "\1091249\GS\fW", emailDomain = "m-\45000&\1032212\ENQ"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_19 :: ProviderProfile
testObject_ProviderProfile_provider_19 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000700000002"))), providerName = Name {fromName = "@\DC1#Q\48293q\142806bbh2\1096258\&8w?u$Z\38404x,"}, providerEmail = Email {emailLocal = "$\SYNo\ETB\8105y", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\172832"})
testObject_ProviderProfile_provider_20 :: ProviderProfile
testObject_ProviderProfile_provider_20 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000400000000"))), providerName = Name {fromName = "?9rM8![4\1049578\aK9W\138166+i\1065845>)F\SO #\1102024yd\156842U\120416R\SI]u\1068612\28155\DC4\GS\1073425&KJ\STX\1095416X1p\35941\&1^\FS\143851\DC1\FS/y^\1078294.aD\31462vH8vu"}, providerEmail = Email {emailLocal = "ed\69706&^\ESC", emailDomain = "\CAN\27400^"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
