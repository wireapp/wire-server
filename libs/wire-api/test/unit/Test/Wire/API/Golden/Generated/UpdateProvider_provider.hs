{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UpdateProvider_provider where

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
testObject_UpdateProvider_provider_1 :: UpdateProvider
testObject_UpdateProvider_provider_1 = UpdateProvider {updateProviderName = Just (Name {fromName = "\SO\RSY \189669\SUBsURR\ENQ\f\1008991"}), updateProviderUrl = Nothing, updateProviderDescr = Just "~\DLE%P\nY"}
testObject_UpdateProvider_provider_2 :: UpdateProvider
testObject_UpdateProvider_provider_2 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1097979\1033829\DLE\138375c;\38288\13365\1083768#"}
testObject_UpdateProvider_provider_3 :: UpdateProvider
testObject_UpdateProvider_provider_3 = UpdateProvider {updateProviderName = Just (Name {fromName = "]V\50145\1091973\"xE\998930\37883ia\DC4\3416'\1009251l\22365\185123e*\991073LGJtP\DC1 \1099149\DC1\SO\47588\1083407{7\1086354{s\1060054\NAK\1013618\&5IlB\CAN1Hx\1059798\&7)\1027711\165212h\1037850(U\FS\162497\EM\173097Z\71268#`\rq\1010252-\DELI[\SOH`Q\159758\37303\&0<>\ENQ\RS"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "Kcq\SOH\1018817k*\1001045"}
testObject_UpdateProvider_provider_4 :: UpdateProvider
testObject_UpdateProvider_provider_4 = UpdateProvider {updateProviderName = Just (Name {fromName = "$#*\985538:=)X\1056253o\1015072o.\GSU7X\ACKVer{\SYNS=\1109958\41849]0~'\142362W\f\vs\DEL\1057267+'\154282$\b\83452g9\1059635\13386\1027011\&7\STX:\19950\1012906\bL&e*A\1093197}\40537q\183454\1091809E'9\USH\1058668\FS\SOH5]\"&2_O\SUBu\1008714L\142118"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "0"}
testObject_UpdateProvider_provider_5 :: UpdateProvider
testObject_UpdateProvider_provider_5 = UpdateProvider {updateProviderName = Just (Name {fromName = "\1006230y\32161w\1010035\178868\&8\ETB\NUL\CAN\1112489+\29564?zZMJ\152928qk)\DC3lsNY\NUL^\99880vH_\1046713\&77\DC1\USu\1091566\1112114d\SOH\US\a['fg\1099634\174986'\FS=Z\168009\&4(0\ETB\1003187mo\DEL\US2\1078252m\DC2\1026124\CAN\188660p\224X\t\140743\ETX\EOT\34887?\SUB\157112\SYN+G7<~W\US\DC3\1066035\&7\EOT_\1050914>\n\NAK\CAN\38761H5p\64001\ENQG>b[\NAKg\1018263\49741\1104806R\62102U\147637\ACKK\STX"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_6 :: UpdateProvider
testObject_UpdateProvider_provider_6 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_7 :: UpdateProvider
testObject_UpdateProvider_provider_7 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1104758\15229$\141372Mj\NULXf"}
testObject_UpdateProvider_provider_8 :: UpdateProvider
testObject_UpdateProvider_provider_8 = UpdateProvider {updateProviderName = Just (Name {fromName = "\151031\&8UE\22373LY#]\1018051\1026859N\1063819P1@XZ4\NAK\148202g\ENQ3o\NULr\STX\1098146\176537Lin*K%\1021923R\ETXv41\ENQ\ap-d\158570\CAN\SOHM\ETXmF&o\EOT\179497\1046223j\DEL\GS:j\DC2Q]*\51188X(h{\1086853,\a\994318\1025914\189132\1034519`7w{X\990329\SUBNI,U_r\ETBXDk\"\998822d\148623%|\1076711P44\a\146209\1092892^B\1024610<Q1\DLE\v\DELD"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "V\57437\25963\165601\EM\CAN\USzY="}
testObject_UpdateProvider_provider_9 :: UpdateProvider
testObject_UpdateProvider_provider_9 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_10 :: UpdateProvider
testObject_UpdateProvider_provider_10 = UpdateProvider {updateProviderName = Just (Name {fromName = "9\71443\37477\38844:\1025796\180753$\SUB[\ETX\1080801GF\DEL"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ")\1004328Y\1013826w%c\ENQ,"}
testObject_UpdateProvider_provider_11 :: UpdateProvider
testObject_UpdateProvider_provider_11 = UpdateProvider {updateProviderName = Just (Name {fromName = "(Kt:\28889\1017067\1042989\8125B\r,\164837\1082606fS[\nh-Q\996923s\135469\n\989838\&3\n\DC1\128353\EM\b\GSt\174735\32186ZcME~\1010004\1107859c(\FSB\ETX\1057732W\1826es\1088613mw*"}), updateProviderUrl = Nothing, updateProviderDescr = Just "3K\33586G\SI7'\EOT"}
testObject_UpdateProvider_provider_12 :: UpdateProvider
testObject_UpdateProvider_provider_12 = UpdateProvider {updateProviderName = Just (Name {fromName = "\1092307sM\1068713\998030\34643\GS>z\14239u\1086829=\166089\995383T\GS\1049518-X\"\1097184\GS\GS6\993438A.x6gq~m\141530dW\1044578\DELX|_\rI\100999eKt9]D>\ESC\41080\STX\SI1\95417\1084210\US\60757\SYN\167426b,\127173!b[ji\111250(?eE\1053146\&3f\1043608x;oVElMu>S\axUhi\\\NAK6\189604:\ENQ/]7k\1034772\162545\1015001c\SUB"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "6\149840\\\1055806^>\71688N\1074815\163196"}
testObject_UpdateProvider_provider_13 :: UpdateProvider
testObject_UpdateProvider_provider_13 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ",n\1077353\180811"}
testObject_UpdateProvider_provider_14 :: UpdateProvider
testObject_UpdateProvider_provider_14 = UpdateProvider {updateProviderName = Just (Name {fromName = "@\RS*tp\1020133\rn\1011269XSO&\fC(!\SOHyNb\ESCm\1097255\EOTQ\NAK\DC1uF\FS\SO\GS\28959g\44327y\SI\1065892q\1019139\53131\DEL\1091459\994897\&5u\990671Mq\52822x\1047958;\FS\r\67255X\ETBF\f\1050086Q\74771\1058195\&5\NULa'uQ\168869\SO8\GS.o\SYN\1085218\1022940\ESC\1082653\13865^oM/uf\ENQ\1049059]wm\ACKq3t\1021116YF\ETB\1059979\DC3E\t\157027\r`\1049779tYP$\1004410z\48445R?XX"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\49342J\29280\&3\46933\ETB=u}"}
testObject_UpdateProvider_provider_15 :: UpdateProvider
testObject_UpdateProvider_provider_15 = UpdateProvider {updateProviderName = Just (Name {fromName = "\126514\188710f\NULT1\178946\161911?H\1053619s\110858IJ+^\157918\1021328]-\1109147d I\DC4\996958\SOH"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_16 :: UpdateProvider
testObject_UpdateProvider_provider_16 = UpdateProvider {updateProviderName = Just (Name {fromName = "v\164614J}tp\996779\986840\n\EOT(qcIp\rU^\159592\v\SOAY4 \1025782cm\SOH \n8"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "W\4914\1080901\&0?"}
testObject_UpdateProvider_provider_17 :: UpdateProvider
testObject_UpdateProvider_provider_17 = UpdateProvider {updateProviderName = Just (Name {fromName = "-\DEL\tQ&xE\994819\1038007\70064\68083\SOH\n\ab\b\991420\FSa\58816\1021896~\EM\STX\1041277\EOT1\1042242\DLED7#g\ENQ\998638B\158062c7\CAN\ACK\1058296\127469Td\SYN\8271\1105152\NAK\1048670P"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just ""}
testObject_UpdateProvider_provider_18 :: UpdateProvider
testObject_UpdateProvider_provider_18 = UpdateProvider {updateProviderName = Nothing, updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Just "\1019672Gn09\171149g\1054172"}
testObject_UpdateProvider_provider_19 :: UpdateProvider
testObject_UpdateProvider_provider_19 = UpdateProvider {updateProviderName = Just (Name {fromName = "1\NAK\RSIJ\1079187D>Xk\DC4\DLE\29658\&6`=,\1028606pdhyq\1016075\ETBv]P,X\EMv@\1049538\43857I\v`wvjC&\166642M=K\SYN)E!\1032260\57956]E\1075628o\988707\SOpd\ACK#\DC11N{\1072077\991407A-_\STXd_\22822\1095938\ESC2\US\t\1010760<%\1033186\36400\&6\1049433\993564*`"}), updateProviderUrl = Just (coerce URI {uriScheme = Scheme {schemeBS = "https"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = "example.com"}, authorityPort = Nothing}), uriPath = "", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}), updateProviderDescr = Nothing}
testObject_UpdateProvider_provider_20 :: UpdateProvider
testObject_UpdateProvider_provider_20 = UpdateProvider {updateProviderName = Just (Name {fromName = "n*\78359?\RS\61473e\1112032\59670\&4\1111748\189150y#\SO \ETB\EOT\1002728\ESC\1028796G7sP@U@$SPu\83419MrB@\1030366\1109160[`\CAN\1007153A2\141526}\1044963\r\36469n\985034G\b7\1010787\1027071\30327hj\58807*4\NAKj\158239U+-w\1052825\1018558\ETX\35090S\990108\1113404\&9ZH754I1 \23448\69376"}), updateProviderUrl = Nothing, updateProviderDescr = Just "\137677"}
