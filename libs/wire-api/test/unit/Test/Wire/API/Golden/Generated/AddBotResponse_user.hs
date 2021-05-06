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
testObject_AddBotResponse_user_1 :: AddBotResponse
testObject_AddBotResponse_user_1 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000003"))), rsAddBotClient = ClientId {client = "c"}, rsAddBotName = Name {fromName = "\1021545Dj2\SYN:oH9jr?\37758p\162823\nLkW[Mf!\143189x\DC34p"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000004")))) (read "1864-05-06 14:45:19.565 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [InviteAccess,InviteAccess], cupAccessRole = ActivatedAccessRole}))))}
testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000001"))), rsAddBotClient = ClientId {client = "0"}, rsAddBotName = Name {fromName = "\FSO\69408hp=^RGIr\1101062h\1021973\133676\1048433.\GS\994887\ACKoi\DC2\NAK\n$UtZ)\"V\GS\NULb\1021426\&9\1081184.\1033175\&3\DELGFx,$\119042AKkep\DEL|P\SUB\t\v~R#\25349 \t\tF\ETXo\22240Ya\SYNdb\DC3S\1020062J\189958\DC2"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "\2108" (Just AssetPreview))], rsAddBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000003")))) (read "1864-05-09 21:20:11.249 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), misOtrMuted = Just False, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 0}), misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Nothing, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "9pnijgv5ltsp1xz2y3dbgdfji78c7o4zpf5u5ltm8q0n_b1xi0uchx"))}))))}
testObject_AddBotResponse_user_3 :: AddBotResponse
testObject_AddBotResponse_user_3 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000400000003"))), rsAddBotClient = ClientId {client = "b"}, rsAddBotName = Name {fromName = "Gk_\1105684\EMo\DC3\26388o)y\1103210\STXz\r\1044773yK\1050478F\STXCU\145118\FS\172954*\US2\a\fu{\178772\1083787}\995222Ff\6181\1036614Zw\SOH- \152261z/)\95755P$z_h\STXb\SOH\ENQ\155494\&0\1031263\176319\f\DC3>@\CANL\44990\1071041\&2l\138805j\EOTz\1048817\152952\161892*r"}, rsAddBotColour = ColourId {fromColourId = -1}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000000")))) (read "1864-05-08 21:48:00.843 UTC") (Just (EdConvRename (ConversationRename {cupName = ")$\1017272"}))))}
testObject_AddBotResponse_user_4 :: AddBotResponse
testObject_AddBotResponse_user_4 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000004"))), rsAddBotClient = ClientId {client = "c"}, rsAddBotName = Name {fromName = "u\1060821\1065518\USP\SO\DLE>[7\1078707\1003099\f\162665Hi:\57866\&5_,\a\ETB\991729\9176\UST\ENQd8\vPc \27776\&0\f\DELXN\988288>\RS5\DC1i\53453\127543 \aE\18746\b\CANU\100603\121361\DEL\SUBz\a\985610\ENQ\1092657\NAK\NUL\121234=\ENQ-*\ESC`|"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000002")))) (read "1864-05-04 16:17:51.714 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), cMessage = Just "", cName = Nothing, cEmail = Nothing}))))}
testObject_AddBotResponse_user_5 :: AddBotResponse
testObject_AddBotResponse_user_5 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000004"))), rsAddBotClient = ClientId {client = "0"}, rsAddBotName = Name {fromName = "D\50068=u\nS4*@\ETBfT\1004027/-\165281@\133953\40019Tr4k\DC3\EOT\49007A\189243"}, rsAddBotColour = ColourId {fromColourId = 3}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001")))) (read "1864-05-07 23:32:00.157 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))}
testObject_AddBotResponse_user_6 :: AddBotResponse
testObject_AddBotResponse_user_6 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000400000001"))), rsAddBotClient = ClientId {client = "10"}, rsAddBotName = Name {fromName = "&\63255$y\21773\1027194\vP_\15297`\ETX\1082056zVg+\FSF$ei)\a\993217\r\ETX5F\SOH\96794\&5uJ\1077496\&9\143071c\1037803S\DC1\132645\EOT\STXe\GS3\US\a"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "_" (Nothing))], rsAddBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000400000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")))) (read "1864-05-06 03:35:43.143 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), cMessage = Just "\DC2", cName = Just "\1060664", cEmail = Just ""}))))}
testObject_AddBotResponse_user_7 :: AddBotResponse
testObject_AddBotResponse_user_7 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0004-0000-000100000003"))), rsAddBotClient = ClientId {client = "0"}, rsAddBotName = Name {fromName = "\ESC\ENQX"}, rsAddBotColour = ColourId {fromColourId = 4}, rsAddBotAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000003")))) (read "1864-05-14 21:11:36.501 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [], cupAccessRole = PrivateAccessRole}))))}
testObject_AddBotResponse_user_8 :: AddBotResponse
testObject_AddBotResponse_user_8 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000001"))), rsAddBotClient = ClientId {client = "5"}, rsAddBotName = Name {fromName = "p\FS6\999661\SYN+M\1093287\131211ql+\ACK\140626!\1002639n\988236\41596Tk\20315i$\n\NAK\DC16\NAK\SOHMfzi\NUL.X,i4\135002\1068453\DEL&r/T\41531r~m\1003162\ENQ\139668h/Xe\989178We\1008059\64627\61792\92711q)\1075995'@Q\STX3e\50041(F(\SIQ>f\1020302\\6\1071660&\ETXp\30054`\1092761\EM\39497\NAKN\1060645\137639\1094855j6g\53260t\SYNv\a\1091181\NULK\158541\146645!\1087744\v+"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "\65423" (Just AssetComplete)),(ImageAsset "V" (Just AssetPreview))], rsAddBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000400000002")))) (read "1864-05-04 17:36:00.35 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = []}))))}
testObject_AddBotResponse_user_9 :: AddBotResponse
testObject_AddBotResponse_user_9 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000400000002"))), rsAddBotClient = ClientId {client = "9"}, rsAddBotName = Name {fromName = "7\1027263$*\CANN2\139728<P\995219\1023682\184496qX\66912@=\1013579:m\1086913\&2rb\"\v\13315w\4434N\24083\49423\1058567w'\32989\USJ\SOQK}o\GS\1108621{\61042\1108069K\1090678\997660w\1101772nP\1015329. [g ;EU\RS\ETXt\161841\US\49704\\3u\t\ETB83\SI\1072545\ESC>\1028771\1074620xS\18381C\1111459L\1105231\182455{o'dj\US\EM\8472\GSli"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [(ImageAsset "O" (Just AssetPreview))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000003")))) (read "1864-05-09 13:40:33.309 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -3}}))))}
testObject_AddBotResponse_user_10 :: AddBotResponse
testObject_AddBotResponse_user_10 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003"))), rsAddBotClient = ClientId {client = "d"}, rsAddBotName = Name {fromName = "<\147381S\20869\147519\NAK\25477\1004664\n0\100892.Y'o\DC2Ajs\bW\175475\DC2\997629:\11291?i&2\64824\ETB"}, rsAddBotColour = ColourId {fromColourId = 4}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000001")))) (read "1864-05-10 02:11:08.31 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [PrivateAccess], cupAccessRole = NonActivatedAccessRole}))))}
testObject_AddBotResponse_user_11 :: AddBotResponse
testObject_AddBotResponse_user_11 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000001"))), rsAddBotClient = ClientId {client = "7"}, rsAddBotName = Name {fromName = "\ETBoj(\1099231Xj\40468r!/\\\1062015O\1026096cf\21472\&8\173128\1085863t=\1059092S\f>k"}, rsAddBotColour = ColourId {fromColourId = 3}, rsAddBotAssets = [(ImageAsset "\1077997Y" (Just AssetComplete))], rsAddBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0004-0000-000400000002")))) (read "1864-05-10 11:55:14.245 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_AddBotResponse_user_12 :: AddBotResponse
testObject_AddBotResponse_user_12 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000002"))), rsAddBotClient = ClientId {client = "5"}, rsAddBotName = Name {fromName = "\120572j\GSx\1106438V\ETX\USR^\EOT\159971T\1070424>W\996911t\CAN2\ENQ5"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000400000002")))) (read "1864-05-07 21:59:48.472 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 0}}))))}
testObject_AddBotResponse_user_13 :: AddBotResponse
testObject_AddBotResponse_user_13 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000000"))), rsAddBotClient = ClientId {client = "a"}, rsAddBotName = Name {fromName = "O\v\162614\1015372\1031218\160943\36695 #R\1003774x\GS\t7\EM \1081321igf\EM\165744_\1004026L\ESC.c\EMRk\49836\32958u\1089340]\143042(\SI\996316e\ETXH=N\SI\26078\STX\DC4:\DELw#\99661\131528\983636\NULrveM6\SOH\b\ETB\DC1\160293\185625tj\1048900\185691\&4_uv:\139705\191377\&1\170140|\DC3\140721\ACK=G.Q D\29015\fL\21240\&8\f]\EM\RSf\26577\t\n\ACK"}, rsAddBotColour = ColourId {fromColourId = 3}, rsAddBotAssets = [(ImageAsset "\154354" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000002")))) (read "1864-05-08 11:54:16.254 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cnvAccess = [], cnvAccessRole = ActivatedAccessRole, cnvName = Nothing, cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), memService = Just (ServiceRef {_serviceRefId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _serviceRefProvider = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))}), memOtrMuted = False, memOtrMutedStatus = Nothing, memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "axqnn57rnpfoxfxry4202rua75r9b2k5a_obxeac9zee3l7t4x780819v5b3rcyk"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), cnvMessageTimer = Nothing, cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}))))}
testObject_AddBotResponse_user_14 :: AddBotResponse
testObject_AddBotResponse_user_14 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000002"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "\34923\DC1\74188\RS\f\990882\CAN\178027Qu6\DC1m\24579Q\f\rSy<\STX5\vC\996628H\1055901$\DC4\29653\183655\DC2\152369\SYN\ny\1021097\1048614P\1093596\138438c$\\HV\1077678[IOF\145489\\{mph\1095673P\EOT#.c\1027538)\1773\1073780\&5w\136820:\a\1062797_\1047493\ESC@\DC3N\SYN[\184363\1113172\ACKg\ETX\1112351\td\t\1045165B\1106754nQ>A\1075268n:\fx2Bc.X\24778A[QPJ\73108\140949%p\1052492Q\190117"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "\ESC\153094" (Just AssetComplete))], rsAddBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001")))) (read "1864-05-13 20:29:59.616 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Nothing}))))}
testObject_AddBotResponse_user_15 :: AddBotResponse
testObject_AddBotResponse_user_15 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000"))), rsAddBotClient = ClientId {client = "7"}, rsAddBotName = Name {fromName = "\164484\v\ENQ}-\140585A\DC4\ETB:.g\f\SOH>\EM\DC3}HSp\161999\&9\60387\1054831vG\155539si\n\1101622=\1020635H3\5331\DC4J\b\SIB\ETB9\1015440\n\11606l"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000003")))) (read "1864-05-13 10:59:13.457 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "", otrData = Nothing}))))}
testObject_AddBotResponse_user_16 :: AddBotResponse
testObject_AddBotResponse_user_16 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0002-0000-000100000002"))), rsAddBotClient = ClientId {client = "3"}, rsAddBotName = Name {fromName = "\ENQa\CAN\31793\&9\177383\f)?\139773\161713\DC2H=\USXkC\SOH'\1037601s\43623\99670Te\1108755\999620\EM\ACK/\1024235\44678V\17719\FS\19687\996287)F\t~\aV*?I\1051903J-5\DC29m\1022634\1113457oQ\179152fyHB\RSjFU\ETBN\53141K\1078884e"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [(ImageAsset ")" (Just AssetComplete))], rsAddBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000004")))) (read "1864-05-10 06:20:48.828 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_AddBotResponse_user_17 :: AddBotResponse
testObject_AddBotResponse_user_17 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000300000001"))), rsAddBotClient = ClientId {client = "0"}, rsAddBotName = Name {fromName = "\1037214\38111\145662z\ncxd))+\ACKe\\#5\153021Z\51779uZ.\143984e\\\DC1<J"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001")))) (read "1864-05-12 23:17:50.478 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "1"}, otrCiphertext = "", otrData = Just "U"}))))}
testObject_AddBotResponse_user_18 :: AddBotResponse
testObject_AddBotResponse_user_18 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), rsAddBotClient = ClientId {client = "2"}, rsAddBotName = Name {fromName = "Kc(\DC4"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000003")))) (read "1864-05-11 22:35:58.557 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4}}))))}
testObject_AddBotResponse_user_19 :: AddBotResponse
testObject_AddBotResponse_user_19 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001"))), rsAddBotClient = ClientId {client = "a"}, rsAddBotName = Name {fromName = "UCQ\r\194629\"\DEL\94888+\22887.YGd\STXu\ENQ.4y\1075609\SON\DEL)\b]|I+\EMlm\DEL3\1082681\EM+v*\DC3\STX\SOH\1092026\994856k\19725\DC2\1352Yn\1039973`&\raTT(1\\)\64751d\140223\GSgYSsNI\51254"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [(ImageAsset ",<" (Nothing))], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))) (read "1864-05-10 14:25:39.693 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole}))))}
testObject_AddBotResponse_user_20 :: AddBotResponse
testObject_AddBotResponse_user_20 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000003"))), rsAddBotClient = ClientId {client = "1"}, rsAddBotName = Name {fromName = "0\42306\169201\SI\v\1007104\1056763\NUL\170320\39626\NUL\126597\n\n\DELf\EM8Y)\r\1033645)3#\1101397\DEL<\1080952\"W\51401\998686\1030080Q\NAK\1069850}\r1V\1103159'\vS6\170974q\SUB\SI \1022853;l\n\b\98291\1110776R`\f\137447nT[\18912c\1064588\148866\78641$b^\149355\1007018\1093988\a^{3F\984837\&1Ma\SYN1\190360 6\DC4\US\17785\996150\1048006\&1X8\139801"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000003")))) (read "1864-05-13 00:52:41.731 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 286902458053010})}))))}
