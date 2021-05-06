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
testObject_ProviderProfile_provider_1 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0008-0000-000800000008"))), providerName = Name {fromName = "%\47734DBEs\ETBN\133301dm\US]\1012469\STXB\DC2\1048063\63797\1068651\10391a\CAN\170854M\DC3\DC3Aq\DC3y\aP's8b\1014812 \135540\SUBlY\167121\1051167l\NAK4"}, providerEmail = Email {emailLocal = "\SIs\\\v{\a", emailDomain = "\\\992012\1025748\SO"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_2 :: ProviderProfile
testObject_ProviderProfile_provider_2 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000500000004"))), providerName = Name {fromName = "@\1051541\ESC\53000\148703\1066819\1074559\f%\CAN,x z%p\GSx\72988_\990246c]v,\DC2]]ar\DC2\NAK\1042061\ENQ4\1105815Co\US\28031\SOH\1107092b^\r\159824.TB\1039352b\992130Y\USf\v\r\SOH:\186893\\Ge\1048885\1107576\DC2\1074851x\12821V\1023847J\994483\&2$\SUB9rs}\DLE\65302"}, providerEmail = Email {emailLocal = "D<", emailDomain = "s"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\13037"})
testObject_ProviderProfile_provider_3 :: ProviderProfile
testObject_ProviderProfile_provider_3 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000500000001"))), providerName = Name {fromName = "-\1046225jKe\1057327\ENQN\10194\CAN=\188732:."}, providerEmail = Email {emailLocal = "9\176534\1091021\171105", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ")\v9"})
testObject_ProviderProfile_provider_4 :: ProviderProfile
testObject_ProviderProfile_provider_4 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), providerName = Name {fromName = "\148412{\t!|c\97126Gu\39883\143477\DC1\1069559\22883%\GS4JF>u\US\ACK)n\b"}, providerEmail = Email {emailLocal = "\1023041\1091914Xa", emailDomain = "\13647\1101293\1099105!"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\128236"})
testObject_ProviderProfile_provider_5 :: ProviderProfile
testObject_ProviderProfile_provider_5 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000200000006"))), providerName = Name {fromName = "\"\a\120870\37110LCcrC\131931\146977t\USn\1086262w"}, providerEmail = Email {emailLocal = "f", emailDomain = "_"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\b\\\DC4;o\SO"})
testObject_ProviderProfile_provider_6 :: ProviderProfile
testObject_ProviderProfile_provider_6 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000300000007"))), providerName = Name {fromName = "!\GSh\SYN\v\25828\CAN\SOj\tkD\v-%4\989548;DP\135834V\ETB\1044542\27481D^L~\SUB\1027424\&89W\SUB\1091728/\169803,\992234\18329\t\STXjBv -\999630\GS\5203O\44852\t\8253\4110\f\170845RZ\1011807\133201\EM\177534\1000086s\40435V\t\5544"}, providerEmail = Email {emailLocal = "\1066906k", emailDomain = "g\39530\128562\a\65169&"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\178768Bz&"})
testObject_ProviderProfile_provider_7 :: ProviderProfile
testObject_ProviderProfile_provider_7 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000007"))), providerName = Name {fromName = "j@` ,\78561\156460\SO\132930\1053029+(k|Z\CAN-NS\EOTo\98818+V\65817\\:l\135023Z<u\SIk\12688S')\1093629c\1044486\180774\1085865\4605\ENQJ9K\1028102e%\DC1\ACK\65518\DC2w\GS`s2\1078807h@L\NAK\n\175070\67597\993892f\fFO)\RS*Lc\1061664y\SYN?\SUB%\DLE\rT\EM\1008932Bh\1079850\&7T~ij`@+\1113873\&2\ENQ!\10560fLv@usho:*k`!%8?{6\29593"}, providerEmail = Email {emailLocal = "Cx\22297I", emailDomain = "P"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "U-\SYN\b?t"})
testObject_ProviderProfile_provider_8 :: ProviderProfile
testObject_ProviderProfile_provider_8 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000200000001"))), providerName = Name {fromName = "\1061492\1091685\&8\b\FS\998705t\ENQ\t\DC4\CAN2 5=/+pPQZE\DLEcmIQiN \73952=4\ACK:\60294\120632)T\US8a\CAN*p;\7773t\RS\DC4\DC3<\172570\ACKF{\DC21\43842IY\a\CANc\SO\59973&|W,cZz/~\DLE?\b\ESC]8p\135167a\1043106p{\DC4P\985609|,hMB2ip'\EOT\SYN$\USS\1013100\GSg\1079796b'"}, providerEmail = Email {emailLocal = "\NULW", emailDomain = "]\50637\170421p\\"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\SYNX&\ENQ\SOH"})
testObject_ProviderProfile_provider_9 :: ProviderProfile
testObject_ProviderProfile_provider_9 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000600000005"))), providerName = Name {fromName = "r\SYN~6\1109588y\164167C\186725z<\45228\21240Z\133862\1046191\1070695^\989596\NAK'f\1051909\1067760:1t\ETX"}, providerEmail = Email {emailLocal = "\987990\1043863u\5483", emailDomain = "\28782!1\r\190796\142546"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1032744\1096899\ESC=\1038421\&1"})
testObject_ProviderProfile_provider_10 :: ProviderProfile
testObject_ProviderProfile_provider_10 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000500000004"))), providerName = Name {fromName = "P\1018482Z\1029636\n\183422e\r)\38479\159535\&0~\RS\1048538l\78028\SYN\1050660\ACK%\US\b\ETB\a\138050_x\1103460t\ENQRc\1082059\DLEZO\US\1024840-\t\ETB\153248R~*\994288Ku\EOT\33676\SIEx{\DLE\r\DC4\b{\ESC\\cB\"Z]S\DELJ)En\1094265Y\NAKQW%\ETX^g\23479U\FS6\1060201\58559"}, providerEmail = Email {emailLocal = "!jN", emailDomain = "b\17521\141248?"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_11 :: ProviderProfile
testObject_ProviderProfile_provider_11 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000600000002"))), providerName = Name {fromName = " C\1024528|T\\\28970'\1045612\62379\1042421\n]t['\a\11129\&0\NUL\nx\DC1Y\GSY 6=\t?Q?@4 \"\144678p\1064135\GSh\"\1022650\18089\45569}\NAKT\GS\151704\ETX\176393>\FS\DC3]\DC4IBO\17046\1032737I#x\36830ff\66290\1037855<\ESCU"}, providerEmail = Email {emailLocal = "X\1054677K", emailDomain = "\ENQ"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "1n\SOH>"})
testObject_ProviderProfile_provider_12 :: ProviderProfile
testObject_ProviderProfile_provider_12 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000000000001"))), providerName = Name {fromName = "y\96695]\1086952Jja\1021947z\1054614$\ETB\a@SdS\\_\SYN\1004657\987221\46485\ENQr2\1043603u=\"\156724-\1053764\f\19730I\1033899M=n;\1092939_\1066871\FS\SUB\EOT\1060357H\ACKk.Ql(H\b5\1074672\tG\1030211\9636\&3\SYN\165125\SIb*&?"}, providerEmail = Email {emailLocal = "\152685\18244\DC3\64641\GS\b", emailDomain = "D\163696\SO!\r["}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""})
testObject_ProviderProfile_provider_13 :: ProviderProfile
testObject_ProviderProfile_provider_13 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), providerName = Name {fromName = "[\986545;\50732\DC3.F%\8328}\157918aiJ\\#exC\181024\&96_\215\1041191\1103945YK\1039773\GS~rs\US\1076211\156174*\NAK\1075956\169652u{r\153061\DC1.ypy#g%\DC2B\986781\29706\NUL\150483\175781K2hr"}, providerEmail = Email {emailLocal = "\US#", emailDomain = "\1072182\rx\DLE\1092865:"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "/\ETBm\DC4\SOH"})
testObject_ProviderProfile_provider_14 :: ProviderProfile
testObject_ProviderProfile_provider_14 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000200000000"))), providerName = Name {fromName = "\1096730W\DC2|\1069318Q^i^\DEL\153704K\1006743\DC1}6z\32619\ACK\1031346t\EOT\ETB\1006129i6\128738^\SYNz\US'\SOH<"}, providerEmail = Email {emailLocal = "\1019207", emailDomain = "\EMz4u"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\59703r\20659"})
testObject_ProviderProfile_provider_15 :: ProviderProfile
testObject_ProviderProfile_provider_15 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000700000002"))), providerName = Name {fromName = "+p\179666m)Na7JnE\ENQ6\FS\NUL\1027206#\EM\DC1~\bl&\1051516\2674T\1018832\147742A\FSfUE\STX\134741\989841\SOH"}, providerEmail = Email {emailLocal = "", emailDomain = "E8"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "F"})
testObject_ProviderProfile_provider_16 :: ProviderProfile
testObject_ProviderProfile_provider_16 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000004"))), providerName = Name {fromName = "\189979\185083\GS@)\140208V\EM4X\1088428\ESC\24718`\SUBw'\1102637\&3oQWB\121299f\990639Hm\f]\EOTr-8HZb\1009186QEz\1018766\DC3P\35961\&3\984201\&8g\1010187\1079697\DEL\167847\1075408\STX-UH|"}, providerEmail = Email {emailLocal = "", emailDomain = "\6729U)\154735\151851"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "Shg0"})
testObject_ProviderProfile_provider_17 :: ProviderProfile
testObject_ProviderProfile_provider_17 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000800000003"))), providerName = Name {fromName = "@bm\1053726\DC4\65484|kE\f\\\78607US#\DC4\1003410z"}, providerEmail = Email {emailLocal = "\1015547", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1025929\&4M\SI"})
testObject_ProviderProfile_provider_18 :: ProviderProfile
testObject_ProviderProfile_provider_18 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000300000006"))), providerName = Name {fromName = "#\n_\1060787\145658\29010\&2\35465W.\74860\&2\1097679\48371i\DC3F\DC2\1006878\STX)\1030424I\f\GS\13351\995129\ETBs\1016780\f\1056998d\NAK\aAZM!\1106453\6701)-tK"}, providerEmail = Email {emailLocal = "\b\1027909V", emailDomain = "bV\5614L`\SUB"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\42027\&5\bS\46781"})
testObject_ProviderProfile_provider_19 :: ProviderProfile
testObject_ProviderProfile_provider_19 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000800000002"))), providerName = Name {fromName = "\"\EOTS2wYT.fO^\1025335]\ESC%C\\+S\1056411O=\1041853r\1110800\f\1100655\38113\989603\1015429\995873\1038287V\1048614[\ACK\139169D\fU\23859\NUL\SOf\187568_\162272\&3\1075433\157441yXX{o\179912u\UScp\FS\1077936!\FS\25892(qL\1048884a\1080801Za$\SOH\1045200\1019861\EMu\1087175f\r\1102972\172060\131975Z\166261\1090269d\1044881m\11827D8N\ETX"}, providerEmail = Email {emailLocal = "&*\1055301", emailDomain = "JD\45064\993294\EOT"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\ENQ[\991370\&6As"})
testObject_ProviderProfile_provider_20 :: ProviderProfile
testObject_ProviderProfile_provider_20 = ProviderProfile (Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000600000004"))), providerName = Name {fromName = "^qAs`\ACK!\STX\DC1\38280\USC\t\US9h\1027320\t&\FS\1075257y-CK\189122qF\EOTnH.D5\SOH\1050543\1003457>\22014Z\994971Jx\r(F"}, providerEmail = Email {emailLocal = "o\62254\136302\1050384", emailDomain = "12"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1060647"})
