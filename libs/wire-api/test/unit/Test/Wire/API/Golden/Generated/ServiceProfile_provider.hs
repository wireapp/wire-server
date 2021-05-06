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
testObject_ServiceProfile_provider_1 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), serviceProfileName = Name {fromName = "*BVqS\EOT&\1096384\NULY3n>cv!\48865r\1018497y\1011810\DC3Q\69994\tG\\\RSVV\ENQ\NUL/\1005120E\SYN\1057675\\C\1004854\1079170Tn \1008955\EMy\1095318sNj"}, serviceProfileSummary = "\US\GS", serviceProfileDescr = "O", serviceProfileAssets = [(ImageAsset "" (Nothing))], serviceProfileTags = fromList [TravelTag,WeatherTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_2 :: ServiceProfile
testObject_ServiceProfile_provider_2 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), serviceProfileName = Name {fromName = "K;IM\v\137043\SYN]"}, serviceProfileSummary = "\1003492o\ESC", serviceProfileDescr = "T0z", serviceProfileAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_3 :: ServiceProfile
testObject_ServiceProfile_provider_3 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), serviceProfileName = Name {fromName = "\tO\37687\RS\SIJ\CAN\46699\ETX\NUL\42385 \163757e\9444\ACKh\t\134541U\187191\51697\9591\1059552\134878e:\vBa}p\SUBC\44329j4r\fB$snGE3\f;\1011369\1069342Xc5t\5756\&7]nf3u="}, serviceProfileSummary = "|D", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [FitnessTag,IntegrationTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_4 :: ServiceProfile
testObject_ServiceProfile_provider_4 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), serviceProfileName = Name {fromName = "(\SOH\DC1\EM\1108593z\DC2J\987811%G\47023U\1059220"}, serviceProfileSummary = "\STX>", serviceProfileDescr = "E\1066022", serviceProfileAssets = [], serviceProfileTags = fromList [MusicTag,RatingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_5 :: ServiceProfile
testObject_ServiceProfile_provider_5 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), serviceProfileName = Name {fromName = "\ESCz9\162730\1012313\DC2I\1096846\v\EM\136033\US\NUL*s\38397\NAK9*8\92699\95851-M\1079848\EM\6749\17614\134899+l/\1011010\55012<j:6}7]W"}, serviceProfileSummary = "\1063033im", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "|" (Just AssetComplete))], serviceProfileTags = fromList [RatingTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_6 :: ServiceProfile
testObject_ServiceProfile_provider_6 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), serviceProfileName = Name {fromName = "8\US\158744\143125\FSfn/vc R\1014865E"}, serviceProfileSummary = "\1089998s", serviceProfileDescr = "\n\1590~", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [BooksTag,MedicalTag,PollTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_7 :: ServiceProfile
testObject_ServiceProfile_provider_7 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), serviceProfileName = Name {fromName = "j\DELN3c J\CAN\47647d\1108117~\1095557l)\1107913nr\ESCQ\n\175882\b-KsV2+VP\985023$BtJ\NAK@\152410\SOH\SOHk\139437\ESC4"}, serviceProfileSummary = "7", serviceProfileDescr = "\bw", serviceProfileAssets = [(ImageAsset "\1067470" (Nothing))], serviceProfileTags = fromList [FinanceTag,MediaTag,QuizTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_8 :: ServiceProfile
testObject_ServiceProfile_provider_8 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), serviceProfileName = Name {fromName = "\171255\172661\165335\48777\EMZD\988007\ETXYC{YMHp\EOT<\166053~x\1049416P\DC4B6{a\1058793\1064825\1008303E\1041232tqA\\\186525@|\SOH8\42367\35692 \1055690v\"n\8737\20232\&7M{\65390\ETBV\167600\DC3/\44132NrR\46491\DLEa\25860sX\US\1059395\178450*f9\NAK7`\ESCWlE\1055514\n%+\aHkL\58336|\v7\US+\40505)\1027265\31774>`(`"}, serviceProfileSummary = "o\164047\1087445", serviceProfileDescr = "qQ", serviceProfileAssets = [(ImageAsset "h" (Just AssetComplete))], serviceProfileTags = fromList [DesignTag,NewsTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_9 :: ServiceProfile
testObject_ServiceProfile_provider_9 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileName = Name {fromName = "\1032569>\147916\DLE\DC1k#Ky\1074686\"jxs&-`>jK(\1014601\182255=\172635L"}, serviceProfileSummary = "\DLE\tE", serviceProfileDescr = "\1094486", serviceProfileAssets = [], serviceProfileTags = fromList [DesignTag,IntegrationTag,MusicTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_10 :: ServiceProfile
testObject_ServiceProfile_provider_10 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), serviceProfileName = Name {fromName = "0f\1039166\DC4\65847Io1cH\ACK\NAK\159282\DC2Xr5\SO\55221\148029\1057368+e\187319;\156206\1015296\"\147085\1071684-E-l\DC30\EOT\DELvg\174077\142078-\16134\SYN\46386"}, serviceProfileSummary = "\FSUQ", serviceProfileDescr = "\1045573\NUL\DC2", serviceProfileAssets = [(ImageAsset "1" (Just AssetPreview))], serviceProfileTags = fromList [PollTag,WeatherTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_11 :: ServiceProfile
testObject_ServiceProfile_provider_11 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "54\ACK\73047\DC2\1072986a\137160Sc0\EME\113788\SOHVt\190104E&*.qA\37443i\164431Sf\12101\ETB^[>\111008\83523\1062805\125221CS\CANjb\44393\FSL\1018379\1068497\DC4RaG2\1041359\DC2.!3/t4S\1096667\46080iql\SUB\1045379\DEL\54701\15915\1008577A\NAK\ax\FSA\1087547\NAK.&4+q"}, serviceProfileSummary = "\97374", serviceProfileDescr = "}\US", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [EntertainmentTag,ProductivityTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_12 :: ServiceProfile
testObject_ServiceProfile_provider_12 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), serviceProfileName = Name {fromName = "KE.p;\EM^j\DEL\RS\rc\DC2\8761}A0\58771\DC1'`\1112901\164847\r\992963\1035154\DC44mr\aO\NUL\1113348\&7#\1040703D}\rC\\\28196\1074661\26792KM\60028\159345]]\1079677\&2\187148\ACK\1026943h\25915Oj\83041\ENQ\1103230\1786>$\23154{\ENQ+t*/VZ\993778q\1023971ge\1100764"}, serviceProfileSummary = " ", serviceProfileDescr = "~|g", serviceProfileAssets = [], serviceProfileTags = fromList [VideoTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_13 :: ServiceProfile
testObject_ServiceProfile_provider_13 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), serviceProfileName = Name {fromName = "![\SOH\1050532u\177145\984270\&1\987038dv,*\128143\a8N\142369\1066866\SO\184205&qv\DLEO~j\176376H\141260\EM2\1061913\41139\b\146847$=\SOHmi\\\US@2!2XCuaq\11950\169000\1014507\ETB\134041\ACK\1064950\DC4 5\DC2lX\DLE\USU\135581\ETBc\r3%\NUL\DLEL,r\1049896\990137\143633`/\175449\97863\168608\159329 \15936d+\1072314?E\987477I=-\990159!Ak#\1091326P@\vTc\985701#\1054312\41822\RS-,6|I"}, serviceProfileSummary = "\993154", serviceProfileDescr = "\a2", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_14 :: ServiceProfile
testObject_ServiceProfile_provider_14 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "Z\ETB+Lm\164781.[b-\1081813\GS\t\994959_Ry_ \NAKc\135881a\997700H\1014527k\1029801\&7\19051\1059484J\DC4#dn#\17443`\SYN@\a\1068685n\\\1092113d\1011293\126489:\rh\145387]\1008828,|\11443X\SOh\ENQ\STX!qR,\154663\&8\1077851&"}, serviceProfileSummary = "\141623", serviceProfileDescr = "\n", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [HealthTag,QuizTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_15 :: ServiceProfile
testObject_ServiceProfile_provider_15 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), serviceProfileName = Name {fromName = "zzEE\71110\SYN\b;lM\b\1017671wr%o\1107366\57527\ENQ\NAK\DELJG\170619\&0\153309|B:u\ETB\1110357<_\1022638\36580\992958\21129_\ETX\NAK\135344U|/\183381`%M\DC1)xY5.?\SUBZ\28410{\a\1078980:O,\bAQ5\GS~\1063912Azz"}, serviceProfileSummary = "bB", serviceProfileDescr = "\4580", serviceProfileAssets = [], serviceProfileTags = fromList [MusicTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_16 :: ServiceProfile
testObject_ServiceProfile_provider_16 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), serviceProfileName = Name {fromName = "B\35749\ETB0d\44907m\US\94808\EOT@\152697\v\156396\SOH0\DC2\DC4u@[aCd\38036\180094C\150178km"}, serviceProfileSummary = "@\"`", serviceProfileDescr = "", serviceProfileAssets = [(ImageAsset "\CAN" (Just AssetPreview))], serviceProfileTags = fromList [MediaTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_17 :: ServiceProfile
testObject_ServiceProfile_provider_17 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "\CANF<\32203\1046956cM+6|\12338\STXU\DC2B\60498>\58336@urx\DLEF\1105839b\SOH\RS\150738e.\fG\SI.\1110055\NAK+%A{\"#jy\998103sz0\RS\2543}\DC2\ACK\STX\EOT*\128552Ak\1101277\&1?Ewe>xS\t4/\1110177o\175709\ESCuC\1022652\&5!(o \a\1025534\113702\26998\1048854'nJ\140889\&4\SYN\159141\&6NW\49722Mm\DEL,\94425k\SOH^\189589\6469ye"}, serviceProfileSummary = "$", serviceProfileDescr = "\1058120\1110898#", serviceProfileAssets = [], serviceProfileTags = fromList [], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_18 :: ServiceProfile
testObject_ServiceProfile_provider_18 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), serviceProfileName = Name {fromName = "<f\1010947\1080473+o)![\CAN;GT\1063732f\DEL\68653Q\ENQ\\Y\1092319\\d\184522"}, serviceProfileSummary = "w", serviceProfileDescr = ">", serviceProfileAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], serviceProfileTags = fromList [AudioTag,QuizTag], serviceProfileEnabled = False}
testObject_ServiceProfile_provider_19 :: ServiceProfile
testObject_ServiceProfile_provider_19 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000001"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), serviceProfileName = Name {fromName = "aR^<\azj\1050162{od[\37256I\CAN\47621n\34011\&2wt6\1062754\158668\t\b\SI\146835-9\1010592\157830hT\62585,bi\52986\58857T!\17608\DC1#\171872\DC2\NUL\153517J|k\1063930 Gp\985435R\1083633 \1075245\GSu\DC4\53299l?5\28200aS\b\\k\SI\19718\USx[Z"}, serviceProfileSummary = "\151920\&5\165732", serviceProfileDescr = "Y\25299", serviceProfileAssets = [], serviceProfileTags = fromList [GraphicsTag,SocialTag], serviceProfileEnabled = True}
testObject_ServiceProfile_provider_20 :: ServiceProfile
testObject_ServiceProfile_provider_20 = ServiceProfile {serviceProfileId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), serviceProfileProvider = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), serviceProfileName = Name {fromName = "\35162\1094913Z\DEL/\72724'BZ\42005"}, serviceProfileSummary = "|u", serviceProfileDescr = "\FS\1055777", serviceProfileAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview))], serviceProfileTags = fromList [], serviceProfileEnabled = False}
