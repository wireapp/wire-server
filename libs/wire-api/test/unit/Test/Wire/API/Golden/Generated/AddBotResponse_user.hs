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
testObject_AddBotResponse_user_1 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0004-0000-000100000004"))), rsAddBotClient = ClientId {client = "5"}, rsAddBotName = Name {fromName = "K)n\31083\146031Iq\1081420A\rstpe\996085YX\v[vc>}\DELah8\1008497\GSPC t9<\82984\EOT$Kd\f\122891G;\NAK\1040923SjCm&"}, rsAddBotColour = ColourId {fromColourId = 2}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000000000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000200000004")))) (read "1864-05-05 22:05:26.905316404619 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), cMessage = Just "", cName = Nothing, cEmail = Just "\1074377"}))))}
testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0003-0000-000100000003"))), rsAddBotClient = ClientId {client = "2"}, rsAddBotName = Name {fromName = "g\\\ENQ\31462\190554N\STX5\t\f\DLE<\ETB[\40000X\995458\&2\1037507\DC3\111024\1059306+i\ENQ\b\EOTq(*\DC4\ESC\175941'\993429oQ<\172836\US\164855\1021429\STX\101057\1015654o^8\EM\1040028\SIFtD\1065107\EOT @\1027919\&4\53701\DC4F\b\SI\ENQO>\1107971\165678\SO\178945),\142588N"}, rsAddBotColour = ColourId {fromColourId = 3}, rsAddBotAssets = [], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000000")))) (read "1864-05-09 05:47:06.743827450354 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "1"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "", otrData = Nothing}))))}
testObject_AddBotResponse_user_3 :: AddBotResponse
testObject_AddBotResponse_user_3 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000002-0000-0002-0000-000300000000"))), rsAddBotClient = ClientId {client = "8"}, rsAddBotName = Name {fromName = "2l\1040279\1089483|_\31989\995615#\ACK\1005046]Gn$\CAN{+l1dq\1112841\ETB\f\988341=S\DLEN\1080818X\\\DC3nP.F\83252\"Q\b\a8\1073247\v\95492D,w\DC4Bx\58614E\a8\USz(\140495\rp_\1109978\SOH\"O\997718\DC2\33241r]o;\6286Sk\12635\&3\95053\1004779f\998680H\1044783=\1074638b\STXc\64538m{8=\SOHN\f/R;}\ax2\DC1\1037709\DEL.\1048745L&\1064408\vK/`b\1007124D\r\177894\DC3\""}, rsAddBotColour = ColourId {fromColourId = 3}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002")))) (read "1864-05-12 22:00:26.044040269407 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -4}}))))}
testObject_AddBotResponse_user_4 :: AddBotResponse
testObject_AddBotResponse_user_4 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), rsAddBotClient = ClientId {client = "e"}, rsAddBotName = Name {fromName = "k2x\v\nC\22209W\68866N\NAKY|FW9\1003669\&7\vml~\bo\1461e\SUB\70200Ai\97782\152005Y\994244A\fzg\1001204Nn\SUB:"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000003")))) (read "1864-05-10 02:15:03.802919261101 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Nothing, misOtrMuted = Just True, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Nothing, misOtrArchivedRef = Just "", misHidden = Just False, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "olgy74mrb2b877thp5j7k5b5kd2fookam6maejh64oup3hv8l4zjgxnf3jiilpt1z6"))}))))}
testObject_AddBotResponse_user_5 :: AddBotResponse
testObject_AddBotResponse_user_5 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), rsAddBotClient = ClientId {client = "5"}, rsAddBotName = Name {fromName = "\986099<]\DEL\STX\STX\v\FS\158368:\DC3E*g%\94053\v\DELLS<CQ|B\ESCBZEDHsL\127789\&1\DC4N6W#I?\175676O"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000200000004")))) ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000300000000")))) (read "1864-05-05 06:00:34.187513398108 UTC") (Just (EdConvRename (ConversationRename {cupName = "\\\v\1088917"}))))}
