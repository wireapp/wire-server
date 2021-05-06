{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Provider_provider where

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
testObject_Provider_provider_1 :: Provider
testObject_Provider_provider_1 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000600000001"))), providerName = Name {fromName = "\145134\61089F-\24911\182325\\\996968\1102968a\tb\991405pxp=\SI\ACK\SYND\38710\ETXl@\1080533\CAN{\ENQ\189725-`R\1059609f,Fg\b\DC1\96865\1036680|=/\1042143.\1078162\1051826\&2("}, providerEmail = Email {emailLocal = "", emailDomain = "\163832\1062875Q"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "HS"}
testObject_Provider_provider_2 :: Provider
testObject_Provider_provider_2 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000200000003"))), providerName = Name {fromName = "E\r\40860j\STX\bes\1046850^\1088080\181545X\DELxP\1083531\&7F)X+=@)9?\1062816.Rff\NUL\145714J\\}4H\1112088C#H\100324}\1088734\b\CAN\EOT"}, providerEmail = Email {emailLocal = "w\t\ETX\SYN\US", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "v\167749\US"}
testObject_Provider_provider_3 :: Provider
testObject_Provider_provider_3 = Provider {providerId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000400000001"))), providerName = Name {fromName = "}\DC4\25596^\RS::='y\1033266H7#|\92169{m\ESC\\1 kI\1049742\GS\1051677R\\4\127744\NAK(c\1060459\CAN7e\\_Iy\1038132\48305?\RS\1059545\GSP&\SOH\USB'\EM9mQ\DC3\DC2v\b\ACK\26826G\1062562\DEL"}, providerEmail = Email {emailLocal = "O\14904", emailDomain = "\1009694\&6"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_4 :: Provider
testObject_Provider_provider_4 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000300000000"))), providerName = Name {fromName = "S\99121\1023313C\1093888\1111043bP+i\1013011K\153085eQL\DC2\SUB\DC3/TG0@%#\NUL\181017\30317Y\999195GRo\990692\&6\34269\&4\1069770v\1039619/x4\SOH\1076471\US~\94683\GS\1048493\999558\173626D<\fodI\34291!\v\DC4\SOH:i\1043263\EM\626\1105814\DC4\1088272\143371h\989901Tq\SYN\148758O\RS\138119\DC1\1033974\SUBL\1100506=5Bsa\1054999"}, providerEmail = Email {emailLocal = ",\96991", emailDomain = "\NAKU5\RS\145486"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\SUB"}
testObject_Provider_provider_5 :: Provider
testObject_Provider_provider_5 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0002-0000-000600000008"))), providerName = Name {fromName = "\ETX\FS\15324\174960>2\US\NUL\1051981\DC2lk\CAN\aP\r"}, providerEmail = Email {emailLocal = "_X\DC3", emailDomain = "="}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "p\NAK\179708\&1"}
testObject_Provider_provider_6 :: Provider
testObject_Provider_provider_6 = Provider {providerId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000003"))), providerName = Name {fromName = "UY$)\27567O{Xl\n\ETB\r*O\1054961>%'\DLE}c\174300\"\GS\1065659\1042782;\SIt\DC18[mC\ENQzU\FS\1011058l2\f\ENQ\ETBd\SO\999725\1074401\CANbT #< c@vl\8496\1058088\&6r9\155436\133946[oJ\ETX\176814\154411\1063027[?!l]\DC2b|\ESCK\DC4Q\ACK\tPe\DC1\GS\67660\1074913\STX\19398v~+\158857\1105805\&0R\t\997753h"}, providerEmail = Email {emailLocal = "L}\GS", emailDomain = "Ro\34788'\1107522"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\ACKq"}
testObject_Provider_provider_7 :: Provider
testObject_Provider_provider_7 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000500000004"))), providerName = Name {fromName = "\1053650"}, providerEmail = Email {emailLocal = "b", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "Os6"}
testObject_Provider_provider_8 :: Provider
testObject_Provider_provider_8 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000"))), providerName = Name {fromName = "\142916%\129684]\EOT\DEL)\1050890\DC3\183794<\7644\DC4L\ESC\DEL\ETB\DC4v\STXg\997382\&6\182523R\EOTk}\144560\1112988\59003HOK\USe\163933;\EOTP\1029590Y&\188074\133348MV\SUB"}, providerEmail = Email {emailLocal = "\119043", emailDomain = "\f"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "U"}
testObject_Provider_provider_9 :: Provider
testObject_Provider_provider_9 = Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000000000005"))), providerName = Name {fromName = "\177198\ESCgu\60620\r\1059134\&0\140236\1063601_o\SUB6t@2\44142~\173067\77954U\SOU8[ZrD~:\1041600Y\1098513y^\DC1\SYN\ENQ\DLEH\1086812\1112809\1089123\27019\SYNa#\995709Y$\1041983kP \EM\989440\&6|F1S\GSj"}, providerEmail = Email {emailLocal = "D\37276/<", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "M"}
testObject_Provider_provider_10 :: Provider
testObject_Provider_provider_10 = Provider {providerId = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000200000003"))), providerName = Name {fromName = "\3367`L\172592\49020\1109715\158485\n4]<5mx\STX\NULR\190239{\aT\1010778WX-\1026168F\53163r$tU1\NUL_7\DEL\141976\1098397z1\ENQ\SUB\165890\"\161355\1025777\GSs]\a[F\132608#<\97747H\66333\FS\1068277%\1052991vS\DC4\ENQ\ETB}EmY!A\184974UM\SOH;,\">Q\FS\STXI\23915d\63900\1039728xoA\DC4\SO4D6l\ESCVQ\v\1005266\GS\2128ymd\96674\DC1\164871\SUBMMEP^WoGkT\DC3\ng]\v"}, providerEmail = Email {emailLocal = "'M", emailDomain = "\999178\184616\1044620JUN"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ")L"}
testObject_Provider_provider_11 :: Provider
testObject_Provider_provider_11 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000007"))), providerName = Name {fromName = "\ESCxt!\1023191eaFo\29626\GS\SI\NAK7\1108595$;,-\189986\1018249\NUL\FSH\44978lP\1082670\177840wU(\1072394\34755\139424\94350JU_7\ETX1\\\1051487|ko\ETX\SI|M\1110697\&7\140184z\8702\"\f\40281J){\15918\36000\1030390\SOHB<Q\vIP\1113688?vA\EM%\EMf:Wcq\44493B*%Hq^\DC3\60428@\ENQjH\ETBXd\SO\FS<1WBT"}, providerEmail = Email {emailLocal = "", emailDomain = "\1034665\1097135"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1004849c"}
testObject_Provider_provider_12 :: Provider
testObject_Provider_provider_12 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000800000003"))), providerName = Name {fromName = "\1046408b\21829\CAN\188472DCyQZ.:W\97768&\1013808CWZa\SOH\171216\61489\SYN\1010823Hy`~\EM'-K2#\133486\DC18\DC2E\170586`@L\1090378(\EM\99501.\"\FS}@;\DC2A%\ACK!'\52081p\44380:l\31229x\135934;\1017531 \EOT\DC1x$\EOTOwM\174956;\USk;}gJg\1103857E%\\\995948vg\DC4l\1081124\EOTw\fwEw4\1032421\ETB"}, providerEmail = Email {emailLocal = "m\149528", emailDomain = "\95064="}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\DC4\1083585c\FS\184104"}
testObject_Provider_provider_13 :: Provider
testObject_Provider_provider_13 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0001-0000-000700000001"))), providerName = Name {fromName = "\DC2Ad\1023383r\40825\EOT\1035090\b\146073M2\53594\ENQ\1066377\129396\SOx\RS"}, providerEmail = Email {emailLocal = "Bw", emailDomain = "V\1047025\ETBP"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\EM\38931z\996996^"}
testObject_Provider_provider_14 :: Provider
testObject_Provider_provider_14 = Provider {providerId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000600000008"))), providerName = Name {fromName = "\1093260\152856\EM\CANV\nP\US\1112260~\1088751+\\&\f9 I[\9995\183219@\\\985133\EOT\1036468\1027438$\1018587\989722w0^\1083056m;UIMZ\1017826XvN\DC1e\96905;#\n%L1\996660}\SO[A\RS|\983845\DC2\995578:\992283h\ENQ\f\US$_\138048\149522\1024476S[\SYNl\n<\46091],\rA\ETX\1050095\&4/\9763]\GS\1014429`\987213g\ENQ\ENQ\f9\121031\1081325a\DC2\DC1\1047746\&5\1003977\EM\SUB k\1012206v6\1057559\v>+Rb\991988\ENQ\DC3\a\a_"}, providerEmail = Email {emailLocal = "nN\1020536c\1106619", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\1042096`^\r+u"}
testObject_Provider_provider_15 :: Provider
testObject_Provider_provider_15 = Provider {providerId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000100000006"))), providerName = Name {fromName = "Q`rE8\1012932\150105\1006723\EM9\DELF_\52625\ETBQ"}, providerEmail = Email {emailLocal = "t1\SI", emailDomain = "[\\1\101025"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "m\EOT,"}
testObject_Provider_provider_16 :: Provider
testObject_Provider_provider_16 = Provider {providerId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000100000005"))), providerName = Name {fromName = "P\t9\1015934$<J,\191283\"\989731(._\b|\"\149037\10866=mX]\US\US\139407\&3\b\998212LE\10185\ETXH\1025603\24659d-\1032898\SO\"\ESC%\26651wc"}, providerEmail = Email {emailLocal = "\1070462", emailDomain = "8"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\f\1045402\1055548\CAN\GS\166048"}
testObject_Provider_provider_17 :: Provider
testObject_Provider_provider_17 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000300000001"))), providerName = Name {fromName = "\140718Bs\"3lR\t\25297jO\151523\ETX\ETBF9\SUB\USu%M}\1074068RpZ\SIj\62977\13184\17206\144930nG\NUL\ry\1005102\b8'J\1007250\1047568\DLE\ETXQ\1037152v\US8\17614j\NUL\CANx{F<h3K<7V=\987322\n5\1052334\ESC\1067263W\STX\179587]V\185253+8B\EOT35*s"}, providerEmail = Email {emailLocal = "E", emailDomain = "\30169"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = ""}
testObject_Provider_provider_18 :: Provider
testObject_Provider_provider_18 = Provider {providerId = (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000500000002"))), providerName = Name {fromName = "\10966\1023287n\FS\97661"}, providerEmail = Email {emailLocal = "l\1081450", emailDomain = ""}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "\180384t:^"}
testObject_Provider_provider_19 :: Provider
testObject_Provider_provider_19 = Provider {providerId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000006"))), providerName = Name {fromName = "!\b>\DLE\vq\v7\155566y\n\NULc,\n>-\ETB\156342\989151\SYN\100294\59479\SOHJ\DC2~\140153+k,Qhi&U\1000565`\1057052\NAK\ESC\SYN\EOT\194938\US{\36349vTAw\ETBP~Po0\NULd\190959\174118`\1101022\163889\SUB\ENQ\SUBt\v\DC4n*\n\49634\1094176c8kV%"}, providerEmail = Email {emailLocal = "\31890\DC1\989873u", emailDomain = "2\181634K"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "2\1109347Eg5c"}
testObject_Provider_provider_20 :: Provider
testObject_Provider_provider_20 = Provider {providerId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000600000001"))), providerName = Name {fromName = "\bCfi\CANej\SO3z\1091363\1060865j\164442`\ETX\SOHY\1062318x\78891i\151969E\40713+2\GS6/\NUL/\179968\1095835hf,\6374g\63493\NUL\DC39\1083395\1076323N|\CAN\1046075W#\1103317\ETX\52221l\49611g\1071994pb?\RS\ENQ2\t\31531R\178935\147673e\33653\NAKq\ENQ\RS\1015242?\2454\&6\1009209X\vgWh\1100922\&4Ik'|[\189701\&8\GS\1098360%#3\f\23884\168664W~]\1018081(\DC3\58044\119259\ETB^(\1078448\1011801F:\99684\&2\v\1105521\CAN\96825*7 "}, providerEmail = Email {emailLocal = "TL}\46779", emailDomain = ";I*"}, providerUrl = coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}, providerDescr = "VS\988734"}
