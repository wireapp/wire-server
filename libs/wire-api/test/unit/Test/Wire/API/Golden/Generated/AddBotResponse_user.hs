{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.AddBotResponse_user where

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
import Wire.API.Asset
import Wire.API.Asset.V3.Resumable
import Wire.API.Call.Config
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation.Bot
import Wire.API.Conversation.Code
import Wire.API.Conversation.Member
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.CustomBackend
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Notification (QueuedNotification, queuedNotification, QueuedNotificationList, queuedNotificationList)
import Wire.API.Properties
-- import Wire.API.Provider
import Wire.API.Provider.Bot
import Wire.API.Provider.External
import Wire.API.Provider.Service
-- import Wire.API.Provider.Service.Tag
import Wire.API.Push.Token hiding (Transport)
import qualified Wire.API.Push.Token as Push.Token
import Wire.API.Team
import Wire.API.Team.Role
-- import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Activation
import Wire.API.User.Auth
import Wire.API.User.Client
import Wire.API.User.Client.Prekey
import Wire.API.User.Handle
import Wire.API.User.Identity
import Wire.API.User.Password
import Wire.API.User.Profile
import Wire.API.User.RichInfo
import Wire.API.User.Search
import Wire.API.Wrapped
testObject_AddBotResponse_1 :: AddBotResponse
testObject_AddBotResponse_1 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000001"))), rsAddBotClient = ClientId {client = "1"}, rsAddBotName = Name {fromName = "L\GSn\25500\174958\b~\129611\164031Yi`C;V`\ETX\150216?\\Yc2\1033226\92495g7xx\165188&\1047425\179519\ETX\DC4\1055879"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [(ImageAsset "*" (Just AssetComplete))], rsAddBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000002")))) (read "1864-05-13 15:21:18.455091709758 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cnvType = RegularConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvAccess = [], cnvAccessRole = PrivateAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}), memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = False, memHiddenRef = Nothing, memConvRoleName = (fromJust (parseRoleName "_xp935yhbozmnwr_7x41apydfnkis22acetr023m7kbu3j2oui6chsjk_ug_tywz4p_l8qx6t8_347p6a1btzta8tbm1a9rup_vfd1tcgtiwm84"))}, cmOthers = []}, cnvTeam = Nothing, cnvMessageTimer = Just (Ms {ms = 1}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = -1})}))))}
testObject_AddBotResponse_2 :: AddBotResponse
testObject_AddBotResponse_2 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000002"))), rsAddBotClient = ClientId {client = "8"}, rsAddBotName = Name {fromName = "\1039319\983613\&1HJ^\160614>\SUB~68\1009523\FS\151894\EOT\US{u\DEL(\SOn\CAN\1068440\35194\&8M\DC4\10840H\1048672\\\SI\42620\12716Nwln4^\186624\31872d_\DC2`\NUL-'=Q\";\ENQQ\DC2~\SIw\\@f\1081872\1054861U\1072461\&0\t\DC2+\STXQ\985358M\NAK}*\1021762\ACKz*&\RSd\FS\178113,\983085=\1099615\1048770\SO\189149\&0\1073634\&2`\983303ju\t)\DELt%\ACK/\EM^~\1054671"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000003")))) (read "1864-05-05 19:05:57.262552069456 UTC") (Nothing))}
testObject_AddBotResponse_3 :: AddBotResponse
testObject_AddBotResponse_3 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), rsAddBotClient = ClientId {client = "1"}, rsAddBotName = Name {fromName = "~n\ENQ\13676T\63599\DEL\165363o\1013639\1019818^\CAN\SUB'\NAKg?\DLE\163560z;\v)\n\GSw\62092\EMi\r\SYN/\1058383\985278z~`\1081953\n\1074865\1040652\1082115\f\153916\DLE\7428~p\DC4\SOH{J)\74570\1015096\NUL\STXz:$U\182021!\SUB/\EOT=s\ETX?\ESCCa\1079295\">\ESCK=ITa\DC1lq-\138578dk\ENQC\DEL\ESC?o%YQ6\1095922{\1003662\&0~\17714\1006783OK\1106028\186959R\1057223\1074598}\166117\173495"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000400000002")))) (read "1864-05-07 13:06:16.979133165979 UTC") (Just (EdConvRename (ConversationRename {cupName = "{"}))))}
testObject_AddBotResponse_4 :: AddBotResponse
testObject_AddBotResponse_4 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "S\1040815efx\1093584\CAN\ACK,\ENQ\1015739\1074698\1668\1102892\1055565\DC2<eS;\1024573!!\1072371_\"Hqf=W4\50748a\1056398\&5\STX!8\"X]66a1\SUBm\FS\r\ESC\68319\&2\NUL\fi\rY_\1101712\&3c\1110051\&5X\"`\SI\EOTXjWpIA\\\1069901oI)\65267\26199t\v\DELh4u!\987096;\rO+[b*<\ve\1013397F\1039986\152638e#y\SOHhF\1107495<l\996403i\1057902\US\53481F"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000000")))) (read "1864-05-12 15:39:26.587649407345 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "0"}, otrRecipient = ClientId {client = "1"}, otrCiphertext = "", otrData = Just ""}))))}
testObject_AddBotResponse_5 :: AddBotResponse
testObject_AddBotResponse_5 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000000"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "6\a\STX\28701\a;|"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [(ImageAsset "Z" (Nothing)),(ImageAsset "" (Just AssetPreview))], rsAddBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000400000003")))) (read "1864-05-05 13:04:16.186154068796 UTC") (Nothing))}
