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
testObject_AddBotResponse_user_1 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000002"))), rsAddBotClient = ClientId {client = "7"}, rsAddBotName = Name {fromName = "\185128\f\46500\1077845;\990962\997433#^8\1114033w\CAN\GSi"}, rsAddBotColour = ColourId {fromColourId = -4}, rsAddBotAssets = [(ImageAsset "F" (Just AssetComplete)),(ImageAsset "i" (Just AssetComplete))], rsAddBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000002")))) (read "1864-05-14 14:01:58.533 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 6458693660242853})}))))}
testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0003-0000-000200000004"))), rsAddBotClient = ClientId {client = "d"}, rsAddBotName = Name {fromName = "\1105675;8V!W*Z\fv\993736[{H(\132620.\1112836m\173409\\\1004060U0\ENQR\95775n&jy\SOH\1102112\GSywE\1090044M\1036882,J\175364p\1017733]\1053776nK\n\48262e=VAj\95496`Y\b\995762\DLE\NULApAb\98479\149713\1113224 \133287r\990647p+\EOT5Z\1096600\59900\b\47150\1089696\SUB\149694g\SYNE/7\n\1039632Yo\a.\1062056\21291,lH\USR\1024255X\ETBqx\161110\1022927,\1045000\184818\fF\1075966"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "" (Nothing)),(ImageAsset "p" (Nothing))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000001")))) (read "1864-05-13 09:12:39.87 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -1}}))))}
testObject_AddBotResponse_user_3 :: AddBotResponse
testObject_AddBotResponse_user_3 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), rsAddBotClient = ClientId {client = "d"}, rsAddBotName = Name {fromName = "^ miw\44310j\US\1013270\b\1042315s\SI\SIKG\1081797\48309*6\tm\1071828T\v\ESC-:\SYN\96253\&0s\STX\DLE{+\1432\&6\ENQ\133075\167649\1041359LfWc\DC4;\120684\ESCe\1055672\&3QJ\1007409\1082060Z\144232\181258\&3\1061061Z\1066670UT!\ETB\1002276\USV9\"\1056482mB[.\20769l\168473z/u-\RS\ETB:\n #\65156\&9{"}, rsAddBotColour = ColourId {fromColourId = 4}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview))], rsAddBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000300000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000002")))) (read "1864-05-08 00:06:46.081 UTC") (Nothing))}
testObject_AddBotResponse_user_4 :: AddBotResponse
testObject_AddBotResponse_user_4 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0004-0000-000000000003"))), rsAddBotClient = ClientId {client = "7"}, rsAddBotName = Name {fromName = "\1060674~\CANW\176528\SI-\78806Og\10891\985615\150407\1013509\&7\ACK^o+\46569^\STX\94255\1018991J\1055788 \1091080<\EOTN\ETX=8\1101327\f\1070983c.\147983]m\v?]6\1054632\r\153834Y(\1057540xX\rk\153978(\1053244\179539\1048562\&5v3\154518\1093002bh_Sx7:}\1007123&5H\n\SOH\DLE\1062487&uc"}, rsAddBotColour = ColourId {fromColourId = -4}, rsAddBotAssets = [(ImageAsset "\12315" (Just AssetPreview))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002")))) (read "1864-05-12 17:11:56.908 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -3}}))))}
testObject_AddBotResponse_user_5 :: AddBotResponse
testObject_AddBotResponse_user_5 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = " /&Z\ACK\SOKO\1023780Wn\SYN\DLEz\bk\RSOU2\DLE\1003396\CAN\SI\DEL{<|F\1030850\ETX%g\EM\SI\1046941\1010342jOd\185911s`hCx\1084147\SOHAp\DC1\51585]O\NAKS\FSL$ ;\ENQ\1030661W2\SYN2\ESC"}, rsAddBotColour = ColourId {fromColourId = -4}, rsAddBotAssets = [], rsAddBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000400000003")))) (read "1864-05-06 13:43:31.068 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), misOtrMuted = Just True, misOtrMutedStatus = Just (MutedStatus {fromMutedStatus = 1}), misOtrMutedRef = Nothing, misOtrArchived = Just True, misOtrArchivedRef = Nothing, misHidden = Just True, misHiddenRef = Nothing, misConvRoleName = Just (fromJust (parseRoleName "vwefg0lciplhbak5qol976h8kdbze34ynvjd"))}))))}
testObject_AddBotResponse_user_6 :: AddBotResponse
testObject_AddBotResponse_user_6 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000000000002"))), rsAddBotClient = ClientId {client = "d"}, rsAddBotName = Name {fromName = "lR\1004750\"S\45205t3@\f\140835\SI6,\NAK_8EF\1040248\NAK\1068879nzft\SI\DEL*IE'Z.\ETX\NAK\DLE\ETX\SUBmn-Mt\ACK\49210*:~\ETB)\b"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [], rsAddBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000100000003")))) (read "1864-05-12 12:32:29.681 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_AddBotResponse_user_7 :: AddBotResponse
testObject_AddBotResponse_user_7 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000001"))), rsAddBotClient = ClientId {client = "2"}, rsAddBotName = Name {fromName = "\162661P\vf\8720\\,\ACK\STX\STX\164397\DC4XJj\989811\73008U5\983139@WV\50595hZ\1014961\DEL8+\GSeb 5G\DEL@\EM}\bd\ESC`s26\ETX)\ETX`\165748w\DC2X|'\1034295\NUL,)[YC\142252pg\DLED\1002582\RSO\STX\1064260\73897\STXU.k7DV$c\97140o9\a{'\DEL_\1035774\b"}, rsAddBotColour = ColourId {fromColourId = -1}, rsAddBotAssets = [(ImageAsset "," (Just AssetComplete)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002")))) (read "1864-05-13 02:50:16.613 UTC") (Just (EdConvRename (ConversationRename {cupName = "\STX\177798\CAN_1"}))))}
testObject_AddBotResponse_user_8 :: AddBotResponse
testObject_AddBotResponse_user_8 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0004-0000-000300000000"))), rsAddBotClient = ClientId {client = "f"}, rsAddBotName = Name {fromName = "\49107Z/\1017460r\178286X\187464\1094772]~\1023663\NAK\1080967J\DELd\CANQ:Vw\SUB3[Y\145201/cQ\SO\EOTT%\58257\49785n/bo9Z\132112\1102893\EM\168750%\61423O\141626\165104\179805OB\1113399\15190\&9\1022572\988594\SUBz \GSM\DC1\v_f<"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "c\1072194" (Just AssetPreview))], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))) (read "1864-05-14 05:02:00.497 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [], cupAccessRole = ActivatedAccessRole}))))}
testObject_AddBotResponse_user_9 :: AddBotResponse
testObject_AddBotResponse_user_9 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000001"))), rsAddBotClient = ClientId {client = "a"}, rsAddBotName = Name {fromName = ")\ESC\161984z\1038791\\7\154504:\SO:\141567\1110279\DC4\1076539\&6|ey:)i\b\1014629X]{+r{E\191301{\1098097\\{\DC3\997014_\v\176654{&'\128300\CANyx\185316J\133753C"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000003")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000002")))) (read "1864-05-04 16:44:52.154 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = 2}}))))}
testObject_AddBotResponse_user_10 :: AddBotResponse
testObject_AddBotResponse_user_10 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000002"))), rsAddBotClient = ClientId {client = "b"}, rsAddBotName = Name {fromName = "\RS\ACKN<\RSG\EOT"}, rsAddBotColour = ColourId {fromColourId = -3}, rsAddBotAssets = [(ImageAsset "" (Nothing)),(ImageAsset "\ETX" (Just AssetPreview))], rsAddBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000001")))) (read "1864-05-14 16:51:52.882 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), cMessage = Just "\t", cName = Just "", cEmail = Just ""}))))}
testObject_AddBotResponse_user_11 :: AddBotResponse
testObject_AddBotResponse_user_11 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000004"))), rsAddBotClient = ClientId {client = "8"}, rsAddBotName = Name {fromName = "\986397\&2l\25879e+]\DC3E\173079!VL\ETBD3\DELeohCu@\1000958\983157\r>ya\\\1098643\GS\1066793\113770\aF"}, rsAddBotColour = ColourId {fromColourId = -1}, rsAddBotAssets = [], rsAddBotEvent = (Event (Typing) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000002")))) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000000000000")))) (read "1864-05-10 13:30:33.118 UTC") (Just (EdTyping (TypingData {tdStatus = StoppedTyping}))))}
testObject_AddBotResponse_user_12 :: AddBotResponse
testObject_AddBotResponse_user_12 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000004"))), rsAddBotClient = ClientId {client = "b"}, rsAddBotName = Name {fromName = "V?P\NUL!\DC4B\1032999\&4\US&\bb\DC3s\t\65492D\STX&"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000000")))) (read "1864-05-08 21:40:55.879 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "", otrData = Just ""}))))}
testObject_AddBotResponse_user_13 :: AddBotResponse
testObject_AddBotResponse_user_13 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000002"))), rsAddBotClient = ClientId {client = "c"}, rsAddBotName = Name {fromName = "/Sg`)2l\r8\DLET2\13945n\SUB\v\1035323HQ\US\1064654\SO\22041\SOUvo[59\98529p\DC4\STXS\DEL'mY\nj\1052862a\1082658[ &\1023823^\39154\1015739\&5\175606?j+\SOH\39178O\1040543K\77942en\42509\SUB\39670w\RS-\NAKI\NAK\986436-:R'q\1021594\1010837w@\fAN~e-"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvCreate) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000300000002")))) (read "1864-05-07 14:17:04.775 UTC") (Just (EdConversation (Conversation {cnvId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), cnvType = SelfConv, cnvCreator = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), cnvAccess = [], cnvAccessRole = TeamAccessRole, cnvName = Just "", cnvMembers = ConvMembers {cmSelf = Member {memId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), memService = Nothing, memOtrMuted = True, memOtrMutedStatus = Just (MutedStatus {fromMutedStatus = -1}), memOtrMutedRef = Just "", memOtrArchived = False, memOtrArchivedRef = Just "", memHidden = True, memHiddenRef = Just "", memConvRoleName = (fromJust (parseRoleName "vknwoo0uvnm1tgwfds1khlypqzr_a9fkg40buoulml25dywyp1cxvleuitb477h7efdnju0"))}, cmOthers = []}, cnvTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), cnvMessageTimer = Just (Ms {ms = 8849489546582705}), cnvReceiptMode = Just (ReceiptMode {unReceiptMode = 1})}))))}
testObject_AddBotResponse_user_14 :: AddBotResponse
testObject_AddBotResponse_user_14 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0003-0000-000000000001"))), rsAddBotClient = ClientId {client = "b"}, rsAddBotName = Name {fromName = "z\ETX*\SOHQ>\DLE]6\1017167&\DLEK[\1016403\DEL(\12405g\986368\nX\1055119.*13Y`\996135u\DLE\9181.\983955\168215\1062253z\1021456\147857\1010332\SUB"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000004")))) (read "1864-05-09 20:54:15.608 UTC") (Nothing))}
testObject_AddBotResponse_user_15 :: AddBotResponse
testObject_AddBotResponse_user_15 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0004-0000-000300000003"))), rsAddBotClient = ClientId {client = "8"}, rsAddBotName = Name {fromName = "\GSE[%\69654\DC1"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [(ImageAsset "\94786h" (Just AssetPreview))], rsAddBotEvent = (Event (MemberJoin) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000000000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000002")))) (read "1864-05-08 10:23:57.478 UTC") (Just (EdMembersJoin (SimpleMembers {mMembers = [SimpleMember {smId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000001"))), smConvRoleName = (fromJust (parseRoleName "6glz5zhn0rt8q45lkdpzc590o84l7xsqo26pd2mv6z0bva9q2iwz7k5lj5mxx0084277smdztmm21wkt98os9xv3ggof512vfugwvfucox1yf6tzsbiqicc"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000000"))), smConvRoleName = (fromJust (parseRoleName "wgefwos6xvdqzrvtlcaf6_hly7it_vuykrbb1umm"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), smConvRoleName = (fromJust (parseRoleName "pc8nnz6domgyfz7iu2j1laqnouj6uyabzd9cl082485lhpattzddfsr82_0uecuh1fsxjc344pi9x6x6ad_f4v8fwg2crfyi5rs9v"))},SimpleMember {smId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), smConvRoleName = (fromJust (parseRoleName "_99vohuf011s86lw6hc3z881d55k77r3qriwru_z"))}]}))))}
testObject_AddBotResponse_user_16 :: AddBotResponse
testObject_AddBotResponse_user_16 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0004-0000-000100000002"))), rsAddBotClient = ClientId {client = "0"}, rsAddBotName = Name {fromName = "{c?$\1018358e@\FSJiSC"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing))], rsAddBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000400000002")))) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000003")))) (read "1864-05-07 20:20:20.59 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Nothing, misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "l3h3yjslk2mslr63vnc_i7s3xudaba8loaq6_9elvp9tkta0yooazgtmqb8l68l593qc2emp2mmymyklsbfgqb3qx2h4kffxr3jf1k0850ehp__"))}))))}
testObject_AddBotResponse_user_17 :: AddBotResponse
testObject_AddBotResponse_user_17 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000002"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "\RSkdqFj{\12824}k,k#\49741\&1N\SYNxh-P\1098402q\148371\&8q\1048359\1077218\SO\\\1005998$\154840H\40087\1019290\1002370&)zf)a\SOQ!k\SI}0\185324\ETX\ra\160517\SI\161377\&2\128820\"hUY\6583\1069118P\1011653\NAK\985272\1005852\NAK\ETX>e\SYN"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvCodeUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000100000004")))) (read "1864-05-08 10:54:44.42 UTC") (Just (EdConvCodeUpdate (ConversationCode {conversationKey = Key {asciiKey = (unsafeRange ((fromRight undefined (validate ("=4n0GSIzDPIavq2GXaFw")))))}, conversationCode = Value {asciiValue = (unsafeRange ((fromRight undefined (validate ("2mRdhH_9ZkqCjgr")))))}, conversationUri = Nothing}))))}
testObject_AddBotResponse_user_18 :: AddBotResponse
testObject_AddBotResponse_user_18 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000004"))), rsAddBotClient = ClientId {client = "3"}, rsAddBotName = Name {fromName = ")U^f\STX]\RS\1090434L\120514\1016016\n'\SO\STX@\DELD1\"'\EOT=(s\57536\1005894\6376_yz\987779(N<\DEL|$Jd2P\1056451H\FS\ACK\EOT,]\STX6\ETBy#\EM"}, rsAddBotColour = ColourId {fromColourId = -4}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000300000004")))) ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000")))) (read "1864-05-04 18:39:38.469 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000003"))),(Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000003"))),(Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000002")))]}))))}
testObject_AddBotResponse_user_19 :: AddBotResponse
testObject_AddBotResponse_user_19 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "\r\1067556JG \182411\DC2\153726+l6N\NAKGU\ESC{\f\1057827,8\DC3\5431\1034070\SOH\DC1\SOHO$\1071901B!Th%9G\986636O\CAN\78273\31991NO\1074000\ENQ)rbdI\EM=R\1060598\CAN \ETB\"U#@R\143804u@{\ACKk\SOHj"}, rsAddBotColour = ColourId {fromColourId = -1}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000100000004")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000300000000")))) (read "1864-05-09 08:31:14.474 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), cMessage = Just "5", cName = Just "&", cEmail = Nothing}))))}
testObject_AddBotResponse_user_20 :: AddBotResponse
testObject_AddBotResponse_user_20 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0004-0000-000400000002"))), rsAddBotClient = ClientId {client = "3"}, rsAddBotName = Name {fromName = "'\DC2\ESC\184886\\\1079299\DC4\SI\ETX\73043}o\GS\38883u|\SUB\142689\&1-"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "" (Just AssetPreview)),(ImageAsset "\62493" (Just AssetComplete))], rsAddBotEvent = (Event (ConvAccessUpdate) ((Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000200000002")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000003")))) (read "1864-05-13 00:00:34.942 UTC") (Just (EdConvAccessUpdate (ConversationAccessUpdate {cupAccess = [InviteAccess,LinkAccess,CodeAccess], cupAccessRole = NonActivatedAccessRole}))))}
