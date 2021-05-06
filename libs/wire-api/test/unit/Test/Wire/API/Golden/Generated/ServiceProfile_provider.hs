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
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = ";y\DC3Hq\1034006\&8\DC18\96761p\1103737\&34E\62707}B7E\18800\52583LG$Vyd\995750\ESC\CANG\RSr\f BB\1067609\&2h\31698\1098019\DELB\173299H\RS*>N\EM+\DC2/\1033392\1090540/\1038024&\STX\f\4591m\1074366\DEL\189888Q:aq\FSJW\1036596y\136910\36675\DLE\22888\CAN\1092454\&4.["}, serviceProfileSummary = "6\"", serviceProfileDescr = "\"=r", serviceProfileAssets = [], serviceProfileTags = fromList [EducationTag,FoodDrinkTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), serviceProfileName = Name {fromName = "5`\DC2\1110578s)~(\66647\GS>\990645&\1080221\&4|\154654(\t\SYNhG\992593\5068\rro,\1084704\SOH\1001304P@E\158562#UG\97673 &\1036748'\991408pq\158665P\v\991215\a\RS\\m\987646\1025701\v\1038177Z6S\RS\ENQp/\184345XYo\1034954\DC2Ujv8\179224\DC2\1069880"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [SocialTag,TravelTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "k'L\NUL..\47311\178775\1081435|L#}\123179\EOTcw\r\140074FY\1010231\f\1107461\147848:\1098457`\179666\1088387eFvvmRL%\ESC\ETXU\136659f\ETX\EOT8"}, serviceProfileSummary = "\1091838\168147\62337", serviceProfileDescr = "\ETX", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "f\ENQ\ETX\1058486;D\1092137h\132604M\43552VD\1108308V2\NUL\DELbHx\999688\&1\1079649\1068805s\39937W\DC1\CAN\64535"}, serviceProfileSummary = "4\t\DLE", serviceProfileDescr = "\74851J\191310", serviceProfileAssets = [], serviceProfileTags = fromList [DesignTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "b\135646e\NUL^S\190761.\DC1+W'\168498\&3ZC\28725\83475\DEL\1048130&$1/_\141644\STXx(\990216"}, serviceProfileSummary = "", serviceProfileDescr = "~\22891<", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [PhotographyTag,SocialTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_6 :: ServiceProfile
testObject_ServiceProfile_provider_6 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "\1055329-\t[j\12608H)_\99668\1095284\&4*\1074583\994415]iUIE\USl|/\DLEvM\GS\ENQN\DC3\GS\184057R\DC3\1050363y\13479?f\NAKb3\1095298]^\3542\&7G2mY}\aGl\145917\1071618u/\83072\149259a\GS\ENQ\ENQY;X\SIGw\985229~zh>\SOH\1096466\&44\SI\27532cS\170355\&9\16170)HD\1071464\DC2\SO_[97c\35377\DEL\"\155068oc\64296\&3\141956\SOHl4"}, serviceProfileSummary = "\1045593[n", serviceProfileDescr = "\146514", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [MedicalTag,MusicTag,QuizTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_7 :: ServiceProfile
testObject_ServiceProfile_provider_7 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), serviceProfileName = Name {fromName = "\1087552;nG\43091\nJX*z~w\73709fZ\10248:\59447bP"}, serviceProfileSummary = "r\1033675'", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "?" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_8 :: ServiceProfile
testObject_ServiceProfile_provider_8 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), serviceProfileName = Name {fromName = "%\a"}, serviceProfileSummary = "", serviceProfileDescr = "c#i", serviceProfileAssets = [(ImageAsset "U" (Just AssetPreview))], serviceProfileTags = fromList [MusicTag,ProductivityTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_9 :: ServiceProfile
testObject_ServiceProfile_provider_9 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "M\30621W\49445zT\41992\1083695_\52501\&0TpiS~9tY\47300\GS\29128V9WXdT\14554\GS\1079352\RS\ETB\97455\ru\DC1;f`~\SO\DC2l\NUL\1110094\&0\1068922\r\DLE\FS/\CAN\ENQQ(\39475\1016538-\1036077\b\50303\&2\1068402\1012215\142667\&6X\"Q\1064934w\r(G\EM\993406\r\DEL1\ACKZ\USjV\1071530&?z\70278M\1101969Z\1092966l\1055233\1011833\NAK&"}, serviceProfileSummary = "AP", serviceProfileDescr = "n", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [SportsTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_10 :: ServiceProfile
testObject_ServiceProfile_provider_10 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = " \GS\138907\EM3Sa^"}, serviceProfileSummary = "\1022902#F", serviceProfileDescr = "{\DEL", serviceProfileAssets = [], serviceProfileTags = fromList [GamesTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_11 :: ServiceProfile
testObject_ServiceProfile_provider_11 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), serviceProfileName = Name {fromName = "\16583\SYN_\24538|\1058036\59988\1096931\vfU\SYN\t\990517Sb\US\95766\ESC\DC4\FS\143056\100902\DC1>A\n>\a*\36940Ur\ETB\STXvvy\1082044\151034\144894sIk?I\52515\v]\GS6a\EOTM\13313Q<i\\bM\GS\990593\1078041\SUBrxK\1053491P\18018\rc\166467.\1038736(\EOTC<\137591\f\NUL\1006922u\aL,\20351\57660\166557\DC1U:\\@\RS\150589k\EMJW\ACK\ETB\tcU\132735\ENQ\180308"}, serviceProfileSummary = "\47746w", serviceProfileDescr = "\999404#", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [AudioTag,IntegrationTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_12 :: ServiceProfile
testObject_ServiceProfile_provider_12 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000002"))), serviceProfileName = Name {fromName = "\CAN<AGY\96654\186201\62978v\SOH\ENQ\STX\ETB4\134732'-\ETB+\153019\35708-f\EOT\1098460\92518\1038603h%c!C3lU\ENQ\997028G]\28984\6631x\1019741\27125\DEL\SOHhW=ED\1019192~@Fw\134707ayBHW\161864\1079310\46855\&4k\151253\fpo\994434\ACK\ETB)S\2493\rj\1083283c[%o^\EM<v\182822(o\19235-\EMc\987456+dy\STX\"2D\31865d\EMO\ETB\1013038eOI\EOTiz\b\1005353\44799\RS\DC1\173396c"}, serviceProfileSummary = "\ACK", serviceProfileDescr = "\SI", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [BusinessTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_13 :: ServiceProfile
testObject_ServiceProfile_provider_13 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = "K+\CAN<\"![\DLE98Kj\166162\1016113\FS\1076471\1028200\STX_s\152256oh\STXQ\38486\ENQ\173180\1109878\1017999u\1109920\179723"}, serviceProfileSummary = "#\1097608", serviceProfileDescr = "t", serviceProfileAssets = [], serviceProfileTags = fromList [IntegrationTag,NewsTag,ShoppingTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_14 :: ServiceProfile
testObject_ServiceProfile_provider_14 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), serviceProfileName = Name {fromName = "3\1097112L\ESCt,\142917_\1056208\&8\SUB]x\194584\SIA\1036938>gN)Iq,DO\1031921(\61591z\19818\1014953?t\994847\65100\187883\DELB\51114\62952c]\1046747B+\a\989284A,\ETX{_\SYNB\64484Y\142747\146341O5\31859R\"\15092B\SIx\191370|'4R\1066464\v+vQ\1086610\a"}, serviceProfileSummary = "", serviceProfileDescr = "\SOH\1086461", serviceProfileAssets = [], serviceProfileTags = fromList [LifestyleTag,TravelTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_15 :: ServiceProfile
testObject_ServiceProfile_provider_15 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), serviceProfileName = Name {fromName = "\rwZ#(,(s:yP\ESC\39073\3403b6\10151\26928sy\"\119903\996620\63909Z\993438B\1041047x[\1003622\tK*{E\US\161945!d\US\SYN\SYN>\\1,5"}, serviceProfileSummary = "", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [BusinessTag,DesignTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_16 :: ServiceProfile
testObject_ServiceProfile_provider_16 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), serviceProfileName = Name {fromName = "\135826\f'z?\988396#\r\ENQE\1102432\DC4\92482;,\ACK\161337|31<A\ETX\EM-\a)@\173641\&6\f\1111270\&5O\132790\&4\DLE\NUL\ETB*\64488\128784@\1003387\989052\44598|+"}, serviceProfileSummary = "4\\", serviceProfileDescr = "\CAN\NAK", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_17 :: ServiceProfile
testObject_ServiceProfile_provider_17 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), serviceProfileName = Name {fromName = "\\\ETB\134244&_=S\EM\161935\1079554\&5\1089940~5i\994597\36094+3Ut\SUB#?\DELLU\1113302n\1060177\f\DC3D\177185?i\f\EOT\DLEX<o\a1+\120824BHXy\23337\SYNV\1056142"}, serviceProfileSummary = ")\1008519\GS", serviceProfileDescr = "\1106423\a\DLE", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_18 :: ServiceProfile
testObject_ServiceProfile_provider_18 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileName = Name {fromName = ",\tRG7h~\1030977\&0\a)Z\99977p\1068004D\SUB\1062243\&66\54196\917802\&0P\17528\\\1031775r\DLE\";>)\EM\187164\46414\DLE;\CAN7\141028ES\RS\1035125|\RS&\SYN\STX\164327\138689"}, serviceProfileSummary = "", serviceProfileDescr = "4O\t", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_19 :: ServiceProfile
testObject_ServiceProfile_provider_19 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "Y\190557\47382"}, serviceProfileSummary = "g?", serviceProfileDescr = "\v", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], serviceProfileTags = fromList [HealthTag,QuizTag,VideoTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_20 :: ServiceProfile
testObject_ServiceProfile_provider_20 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), serviceProfileName = Name {fromName = "\171868W\1055763\1105460\34991[\DEL\SYNQT\161865\vL\184141;\DC1%\EOT#L8S\b\999014s{\SOH\146409\STX\143827~W\RS7\178328\41402*Cf\ETX\1090411P\190021"}, serviceProfileSummary = ".u(", serviceProfileDescr = "\DC29\985255", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [BooksTag,BusinessTag], serviceProfileEnabled = False}
