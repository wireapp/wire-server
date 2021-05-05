{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.RemoveBotResponse_user where

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
testObject_RemoveBotResponse_1 :: RemoveBotResponse
testObject_RemoveBotResponse_1 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvMessageTimerUpdate) ((Id (fromJust (UUID.fromString "00001116-0000-5561-0000-369f000071cd")))) ((Id (fromJust (UUID.fromString "0000047e-0000-2fab-0000-0c17000068ae")))) (read "1864-05-02 03:58:32.487708890565 UTC") (Just (EdConvMessageTimerUpdate (ConversationMessageTimerUpdate {cupMessageTimer = Just (Ms {ms = 3761234})}))))}
testObject_RemoveBotResponse_2 :: RemoveBotResponse
testObject_RemoveBotResponse_2 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvDelete) ((Id (fromJust (UUID.fromString "00004c33-0000-7586-0000-382a00007a13")))) ((Id (fromJust (UUID.fromString "000078ab-0000-7ae2-0000-34bc000045c4")))) (read "1864-05-14 09:27:47.778986307137 UTC") (Nothing))}
testObject_RemoveBotResponse_3 :: RemoveBotResponse
testObject_RemoveBotResponse_3 = RemoveBotResponse {rsRemoveBotEvent = (Event (MemberLeave) ((Id (fromJust (UUID.fromString "00001583-0000-3576-0000-6d0f000075e5")))) ((Id (fromJust (UUID.fromString "00004e5c-0000-29cd-0000-257800001ebb")))) (read "1864-04-14 10:07:06.879459466137 UTC") (Just (EdMembersLeave (UserIdList {mUsers = [(Id (fromJust (UUID.fromString "0000600a-0000-5e76-0000-7b0f000051fa"))),(Id (fromJust (UUID.fromString "00007f2e-0000-71e6-0000-5d77000045cf"))),(Id (fromJust (UUID.fromString "000002ca-0000-3d6b-0000-5b2100003d87"))),(Id (fromJust (UUID.fromString "0000386c-0000-01a6-0000-5f49000027dc"))),(Id (fromJust (UUID.fromString "000033d7-0000-4a7e-0000-052c00007530"))),(Id (fromJust (UUID.fromString "00000dfe-0000-4983-0000-6d4f0000721e"))),(Id (fromJust (UUID.fromString "00004e76-0000-72d8-0000-6c63000038af"))),(Id (fromJust (UUID.fromString "00001809-0000-10a8-0000-549c000056b9"))),(Id (fromJust (UUID.fromString "0000627a-0000-01c5-0000-3c3700001a73"))),(Id (fromJust (UUID.fromString "000020cd-0000-3533-0000-17e900007713"))),(Id (fromJust (UUID.fromString "00006340-0000-5b4d-0000-6f7300004953"))),(Id (fromJust (UUID.fromString "000029c5-0000-4a8c-0000-28030000093d"))),(Id (fromJust (UUID.fromString "00006ae8-0000-133f-0000-1a1a000072df"))),(Id (fromJust (UUID.fromString "00003e45-0000-008d-0000-20f000004b6a"))),(Id (fromJust (UUID.fromString "00005c3e-0000-56bc-0000-13a100002d5c"))),(Id (fromJust (UUID.fromString "00004d16-0000-01d8-0000-7b26000025ae"))),(Id (fromJust (UUID.fromString "0000716a-0000-47e5-0000-665c00000e43"))),(Id (fromJust (UUID.fromString "000063c8-0000-04ee-0000-6d1a00006ca8"))),(Id (fromJust (UUID.fromString "00005df8-0000-0aab-0000-054300000cdc"))),(Id (fromJust (UUID.fromString "00000967-0000-7594-0000-5d4300005940"))),(Id (fromJust (UUID.fromString "00006280-0000-6734-0000-66f500002aca"))),(Id (fromJust (UUID.fromString "0000714f-0000-52f4-0000-1c07000035b6"))),(Id (fromJust (UUID.fromString "00003f36-0000-3589-0000-445100005f7f"))),(Id (fromJust (UUID.fromString "00003c8b-0000-2a9c-0000-0d0700003f64"))),(Id (fromJust (UUID.fromString "000053b6-0000-6ccc-0000-1c3700004868")))]}))))}
testObject_RemoveBotResponse_4 :: RemoveBotResponse
testObject_RemoveBotResponse_4 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvConnect) ((Id (fromJust (UUID.fromString "00000629-0000-4495-0000-2f5400004c57")))) ((Id (fromJust (UUID.fromString "00001663-0000-7d8b-0000-71ac0000563f")))) (read "1864-05-10 05:26:09.719656465902 UTC") (Just (EdConnect (Connect {cRecipient = (Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000800000005"))), cMessage = Just "\FS\EM|\DC3|", cName = Just "\GSG\tc\139758d5", cEmail = Just "\1025122J7K"}))))}
testObject_RemoveBotResponse_5 :: RemoveBotResponse
testObject_RemoveBotResponse_5 = RemoveBotResponse {rsRemoveBotEvent = (Event (ConvReceiptModeUpdate) ((Id (fromJust (UUID.fromString "00003116-0000-59cf-0000-7da20000093f")))) ((Id (fromJust (UUID.fromString "00000f1d-0000-77a2-0000-685700004d38")))) (read "1864-06-05 01:40:50.469364801146 UTC") (Just (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate {cruReceiptMode = ReceiptMode {unReceiptMode = -8901}}))))}
