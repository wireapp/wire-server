{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.ServiceProfile_provider where

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
testObject_ServiceProfile_provider_1 :: ServiceProfile
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "r-\176608x\ACK;\DLE'\988165w$U\FS<%\27675\&5\1059128n\170885\1034611\&8/EdU\148029\ETX\ETX&{\SOHa\NULE\EOT\\G\1100811\&9\1046342\&1\179112_[{\ACK]\SUB\DC1\179594*JHN\ETX\n\vrL_\\qQd\1095930>QN\1001467\GS\\Y~\1069058_:9\49340\DLE~\147039\159488\184459\1099375\994396e5I\148740\&6>\149816\21683\166247\3560\&0q\61685\173955\50785)j\EM-\1083828i~\993153w\ESC-\153951Z\ENQ\SUB\r\184519\100367\1069872:Bn\GS\RS"}, serviceProfileSummary = "V", serviceProfileDescr = "\1036674", serviceProfileAssets = [(ImageAsset "\987372" (Nothing))], serviceProfileTags = fromList [PollTag,ProductivityTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), serviceProfileName = Name {fromName = "\1002785\1084243Z\b\65759C\STX\36907\USh\SYN~\td\43960\SYN7#,Lh\181520\SOHH\1097372D\1096107)\141392\160069CG_o\47525>\1099606\DLEiM\1001862v\STX\136466\151012:L=\137165\vL\100704j\161109w\nX*5Sa\ETX \CAN\r\SUB\1034441\&1k]D\159371\&2&Ak\995977\1008673@\178410E2*\1065898=\ETXz11\ACK\NUL_\DC4J\n\b\30668\&0<qWB\DELL\a\v\ACK\STX\\\1037213h\1034226\NAK\NUL:8\ETBd \STX"}, serviceProfileSummary = "iq\1088124", serviceProfileDescr = "\990976", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "\r k\64374t\169415\&7\NULq2\EM'^\ACK\SI;\f\1028409\171998\US\184909n\3420S\1069855*U_f\1012167\NAKH3\DLE\US\rK\1051983\57966$\vt"}, serviceProfileSummary = "", serviceProfileDescr = "\SI\1025767", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = "=\152889"}, serviceProfileSummary = "4N", serviceProfileDescr = "m(\49210", serviceProfileAssets = [], serviceProfileTags = fromList [DesignTag,SocialTag,SportsTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "-I\1093220*\191183\1082617\DLE7|\1059311\&6\1073265)_Bs\46540VG\28124m\1029982\1104446z\ff31z\DC1Xo={"}, serviceProfileSummary = "~\1003884\STX", serviceProfileDescr = "\STX^\1109138", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_6 :: ServiceProfile
testObject_ServiceProfile_provider_6 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), serviceProfileName = Name {fromName = "O6eI\GSf\1035856\4315\1062264O#O\EOTh\1085381\SI\ACK^D|l%p\f\NAKE\vqo5e\ETB$$G-\ESCQl\177768\136534\DEL/\DC4~\EM([C"}, serviceProfileSummary = "\ENQe\v", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [FinanceTag,MusicTag,PhotographyTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_7 :: ServiceProfile
testObject_ServiceProfile_provider_7 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), serviceProfileName = Name {fromName = "\166590KXz\EOT\1052713\t\1081870k\ETX~US&,\1031240n %\CANv\169008\1082336A\1110917|\1058544h1@'\a5\3239\EM\2670\171649\\Ty\FSc6\NUL\f=F\152763\RS'\DEL#\t9\nSO\1086734-<=\1092159ncI\995974\49148\1035859\1019866\USy3\rP9]\5416\a\133407"}, serviceProfileSummary = "", serviceProfileDescr = "B1", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [EntertainmentTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_8 :: ServiceProfile
testObject_ServiceProfile_provider_8 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), serviceProfileName = Name {fromName = "\1025485ae\1079177\ESCI\68489/g\22138Z\vO'A(l\1104310N\27177^BM\1069877\SOH\1050766\NUL%"}, serviceProfileSummary = "_aj", serviceProfileDescr = "k", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [AudioTag,HealthTag,PhotographyTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_9 :: ServiceProfile
testObject_ServiceProfile_provider_9 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = " S\1022762q&\83319m\NULz-+\1047749\RS\95817\"{;*\ESC@\1023462P\186134\ACK\RS%1\DC1#}*\\7Fxa\41730ok6\1069930\v<|\25072\1090578\1105773(U/8X\ACKrPh4\94635E;k\1108567\1063965v::\7931\1087443\44390\1233Ce\63779\DLE\147543\167059+\1080175\&6Ih\1009275\1105385Wsur#\20876c8"}, serviceProfileSummary = "\SOH", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_10 :: ServiceProfile
testObject_ServiceProfile_provider_10 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = "\13012\78298\DC2\f\49317u\22800qx\3121\171094G\US\ACK\1041839TaJvSR\SI\59038\1052679\1058609y\DLEf_\190864\165566k\rC\1073620&\139572\NUL\DC1CWI:UIR\vd>;\1015627\NAKyA\71880\RS3Aj\1050682\n\1003190\983114\SOH\DC1\1009761"}, serviceProfileSummary = "\SYN", serviceProfileDescr = "p*", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_11 :: ServiceProfile
testObject_ServiceProfile_provider_11 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), serviceProfileName = Name {fromName = "\997466\f\bU]VIL\13866uh\EM?tc#m[E\178120\DC39\b<7\188031$\984662\1101333\23751\1894\SO7-\8079]9~\FS.\120303w\177327\&7T\1064570pZ\US\1092144&:A.\v\49377\DC3_l\153726K\ETX[i8-\156152\bv\ENQ\1002847\US\98701\&0-\1095936\&2GCB\42286)\ESCr\ACK\57769\ESCg\1050995=\SOH\RSAaf\1109206\188029Pii~\DLE\185094[\1003024E\1055416=g\36743\992796\21610zc\ETBe\180948\&5SMS0F\RS\SUB\143056\&6"}, serviceProfileSummary = "\35463\1069382\157792", serviceProfileDescr = "\1007196C", serviceProfileAssets = [(ImageAsset "T" (Nothing))], serviceProfileTags = fromList [BooksTag,SocialTag,SportsTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_12 :: ServiceProfile
testObject_ServiceProfile_provider_12 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = " ,\SI\SO|\SOHb$\v\1064986q\58233:X\1026533\&5LP\133411Z\ESCPZ\1107780-\180172\FSW\ENQ@\SI\NAK\592\1011717\a\EM\1068743\DLE\3366\GSws\1094795Y(\1097937\1022175b\1012158uP=ZtSg\990523gb\SUB\1111320\&5|\NUL\1079295\DEL\SYNHv\DELz0\58654d\EOT\1067209\ETBBb\FS\179535PvwRO\DC3s\23059\1099991*"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [], serviceProfileTags = fromList [DesignTag,MedicalTag,WeatherTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_13 :: ServiceProfile
testObject_ServiceProfile_provider_13 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = "$\153942\144045\164166\51382G0\985410\DEL."}, serviceProfileSummary = "=", serviceProfileDescr = "@\160071", serviceProfileAssets = [], serviceProfileTags = fromList [QuizTag,TravelTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_14 :: ServiceProfile
testObject_ServiceProfile_provider_14 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), serviceProfileName = Name {fromName = "\1110859G\991216\51349\1081747:q2N7Jq\DC3\72977\ACK\1100546Wx\1063978j\NUL\EOT\42495\3500U&\133457\CAN\16681\989220A\GS\44875\1050995j\ag:\DEL\41173\EOT\27318\ESC\46318\1025790dM\144363\&4\749o\DC3\v\997760\NAK\991288l\1041433Q\a*S\1082987\152677}p(\1075840\fZe\\\US@IsN\"1{\1074147k\r\136294\1020706H3\189242\1013711WK.L5\RS-7\NUL,@(fWG\FS9m\RSLZ\"Sv\163020\DC3\US=}V>{\DC3\SUBuV"}, serviceProfileSummary = "\986911\v", serviceProfileDescr = "@", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_15 :: ServiceProfile
testObject_ServiceProfile_provider_15 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), serviceProfileName = Name {fromName = "\1040486>%,]+>\185845b\1098359(\1007989M7Kt,<)\867\170843\994860<\59902\\\GS\1088715hu\tO\65932\ACK\FSd\1080154r,e\SI1\a:t\aJ\1061451\SYN\NAK\1047512S\178445\59148:\1048914\SOHs\151499s"}, serviceProfileSummary = "\13776w\188139", serviceProfileDescr = "\129321\SO", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_16 :: ServiceProfile
testObject_ServiceProfile_provider_16 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "\a\ACK\20824\&9\1021353\1011862t+r\1076493\16535\DC1['5\GSg\EMfG}cp\27307/jO\GS:N\v \157877\1082862$\1090934Kf\ETB\v]\US\SI\SI\1078603\NAK\120226g\1040780oVN\11355\DC21{G]\f{m\1015219_-/\176012\SO\&H2#Z4[B\181620T>A\CANe7\999143g{%D\9369\NAK/+(k\DLE\16527\174157\66846"}, serviceProfileSummary = "9\52793+", serviceProfileDescr = "0", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_17 :: ServiceProfile
testObject_ServiceProfile_provider_17 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "\120267\&2f\t5\ETX\149118\40222\43645R\"\1107287\1018282\DC3\994183\187731\143172')\32642}!/\1068448k\166639\GSR\DLE?|\n\132694U?0D\1042261\&0\DC4g\NAK\1033410\1040242]#8J'\1054501;"}, serviceProfileSummary = "\DC3\DLE", serviceProfileDescr = "%G", serviceProfileAssets = [], serviceProfileTags = fromList [MediaTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_18 :: ServiceProfile
testObject_ServiceProfile_provider_18 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "\1112601b>g5\DC4\f\1033876\2588{3\94569j\52499\f\DC1;_\162581\1035005\&2\"\ACKaw+m\bmB2\STX#QsFT&\95689e9`\160852NVx6\GS S\96857\SYN\ETB\58484Xy\185133J-\SOHLDX6d\ETX\SYNk\a\DLE\DEL\119024L\ESC\1086030:;+-\15295\ESC\b"}, serviceProfileSummary = "\151715\a%", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [HealthTag,PollTag,ShoppingTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_19 :: ServiceProfile
testObject_ServiceProfile_provider_19 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), serviceProfileName = Name {fromName = "\NAK(\RSbz\1088708\186123\CAN|'=M\1090368\\d\185575\94664\139617\1068380U\983508B0\SYN\1109186Z\1010475\20666y\SO\RS"}, serviceProfileSummary = "sbW", serviceProfileDescr = "\t", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_20 :: ServiceProfile
testObject_ServiceProfile_provider_20 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), serviceProfileName = Name {fromName = "\FSQ\5952N\SI\SIh>\1100753\DC1qr\ACK^[\996865\n\40277\1021608\ACKOu\132092\DC1R\\\GSHi<f9\1030510\DC2V\54681[4\DC3\t\"\20001\49138\ACK\n\187656\ESC@'w{\DC1i\1113181n\ESC\1101465\26079~\DLE8\1061791*\ETX\NAK\994336\EOT"}, serviceProfileSummary = "\985097\154505", serviceProfileDescr = "?]\1049887", serviceProfileAssets = [(ImageAsset "\25385" (Just AssetComplete))], serviceProfileTags = fromList [FitnessTag,NewsTag], serviceProfileEnabled = False}
