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
testObject_AddBotResponse_user_1 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000004-0000-0000-0000-000400000003"))), rsAddBotClient = ClientId {client = "a"}, rsAddBotName = Name {fromName = "\154399\58756\39317_\DEL.\21059N\1099115]E\153738$\1038266\EM\SYN\10285V\983206*W\SIl%Sh\SUB\992919\1049414\NAK\NULPE\GS|1\1062338\158401\GS_%\188210i\1049852\SUB\ACKW\68002\ENQ\1036390TRc\SYNbz:\1048549{[}\1006142\FS\135925\&2Lje]\ACK)\987140"}, rsAddBotColour = ColourId {fromColourId = -2}, rsAddBotAssets = [(ImageAsset "\ESC\r" (Just AssetComplete))], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))) ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000002")))) (read "1864-05-04 06:58:53.65 UTC") (Just (EdConvRename (ConversationRename {cupName = ""}))))}
testObject_AddBotResponse_user_2 :: AddBotResponse
testObject_AddBotResponse_user_2 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000002"))), rsAddBotClient = ClientId {client = "c"}, rsAddBotName = Name {fromName = "\DEL}^ \1097807'=L\ACK\22141{(\GS>(M82\DC4\1005411YM&$\152127^"}, rsAddBotColour = ColourId {fromColourId = -4}, rsAddBotAssets = [(ImageAsset "" (Just AssetComplete)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Just AssetPreview)),(ImageAsset "" (Nothing)),(ImageAsset "" (Nothing)),(ImageAsset "" (Just AssetComplete))], rsAddBotEvent = (Event (OtrMessageAdd) ((Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000001")))) ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000003")))) (read "1864-05-04 04:47:58.159 UTC") (Just (EdOtrMessage (OtrMessage {otrSender = ClientId {client = "0"}, otrRecipient = ClientId {client = "0"}, otrCiphertext = "", otrData = Just "E"}))))}
testObject_AddBotResponse_user_3 :: AddBotResponse
testObject_AddBotResponse_user_3 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000003"))), rsAddBotClient = ClientId {client = "3"}, rsAddBotName = Name {fromName = "$\1100488\SUB\ACK\GSf\tgV~d#k\1074531D\120660\155122\USO\1081887\&7\DEL\SUB\1090502a\DC1\b\FS\"\SOH7\1064678\EOTq\1097755u\996006\1019795\ACK7w\ETX\6584\&0{\DC4;F\998644\7713\1018902\1041184Z\156331\&6\SOZ\\M\v\100592W\41530#e\43397z\145618\FSk4[X\SIr\fC\DC1\1025872\1097748[M\FSd\a\RS\94544w9\1038530\&8R\SYN\17795E]"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [], rsAddBotEvent = (Event (ConvCodeDelete) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000001")))) ((Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000200000000")))) (read "1864-05-08 05:17:01.389 UTC") (Nothing))}
testObject_AddBotResponse_user_4 :: AddBotResponse
testObject_AddBotResponse_user_4 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000001-0000-0001-0000-000400000002"))), rsAddBotClient = ClientId {client = "5"}, rsAddBotName = Name {fromName = "\998829%\1050427m\25472\SOHX%I\44082r\1091890\USo3W\1029435e\1104664\&5\"Cx*\1031860\34444\SYN\NAK\140745\6419\1056449n?*Ko\36021k-e%\1001862>G+\44842\119125\1005537\18310\&9\DELqW&\EOTF$\1095682\&5\1025286\35812\153571\1024684gN\35465V\1042821yN\48608\&4\US\1094670aI\US\RSr\63397:#ZU\1087218\1073360\&3\1050547#\177336p"}, rsAddBotColour = ColourId {fromColourId = 0}, rsAddBotAssets = [(ImageAsset "" (Just AssetPreview))], rsAddBotEvent = (Event (ConvRename) ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000400000000")))) ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000000")))) (read "1864-05-11 20:48:06.993 UTC") (Just (EdConvRename (ConversationRename {cupName = "i[#"}))))}
testObject_AddBotResponse_user_5 :: AddBotResponse
testObject_AddBotResponse_user_5 = AddBotResponse {rsAddBotId = ((BotId . Id) (fromJust (UUID.fromString "00000003-0000-0003-0000-000100000003"))), rsAddBotClient = ClientId {client = "2"}, rsAddBotName = Name {fromName = "&abi\ETB\SOH`\187946\RSYP\67822psV\\\1021139n\ENQ\987160U\162712b57\SIC\SI\37069\DC4\40186J\DC2+\ACK5}\184393\&6\NAK\FS~\986833\r\NAK3\STX\DC4\SO\\X\ETB0?A\ETB2{\140589\&5\DC4<}\ESC\1020457m\SUB\US>P?sqpTP\65039\1011897E <D1l}^\ACK\989959\1084501\1107853Ac\1113815\&5-\1026685\135288\64083\ETXz\1097587\FSwA\DC1\t(\4102o4uDFIA{\1094112zS\1036990\183497"}, rsAddBotColour = ColourId {fromColourId = 1}, rsAddBotAssets = [(ImageAsset "\1084703" (Just AssetComplete))], rsAddBotEvent = (Event (MemberStateUpdate) ((Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000100000003")))) ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000004")))) (read "1864-05-07 04:49:32.566 UTC") (Just (EdMemberUpdate (MemberUpdateData {misTarget = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), misOtrMuted = Just False, misOtrMutedStatus = Nothing, misOtrMutedRef = Just "", misOtrArchived = Just False, misOtrArchivedRef = Just "", misHidden = Nothing, misHiddenRef = Just "", misConvRoleName = Just (fromJust (parseRoleName "13lubtv694ph4f_p5og69vxvz0gv02fi1y4txsknsns_8p8qemp502cdbadqe43tp9yuopc8811qx_pig81swr5ewcy9tfm3apv"))}))))}
