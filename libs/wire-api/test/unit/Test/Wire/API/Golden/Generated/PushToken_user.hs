{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.PushToken_user where

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
testObject_PushToken_user_1 :: PushToken
testObject_PushToken_user_1 = (pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "\53619H\1067504ncS\37501"}) (ClientId {client = "11"}))
testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNS) (AppName {appNameText = "i?"}) (Token {tokenText = "G"}) (ClientId {client = "18"}))
testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (APNSVoIP) (AppName {appNameText = "f\985401\rU\1036898\31726"}) (Token {tokenText = "\t"}) (ClientId {client = "15"}))
testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\FSk\67279"}) (Token {tokenText = ""}) (ClientId {client = "12"}))
testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (APNSSandbox) (AppName {appNameText = "/<"}) (Token {tokenText = "C\v"}) (ClientId {client = "18"}))
testObject_PushToken_user_6 :: PushToken
testObject_PushToken_user_6 = (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "\1100004\1072684B\ACK"}) (ClientId {client = "1f"}))
testObject_PushToken_user_7 :: PushToken
testObject_PushToken_user_7 = (pushToken (GCM) (AppName {appNameText = "\CAN\1087083\55140\47423\CAN"}) (Token {tokenText = "\DC3}\40346&'?"}) (ClientId {client = "a"}))
testObject_PushToken_user_8 :: PushToken
testObject_PushToken_user_8 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\SUB\991768\n\2664`e"}) (Token {tokenText = "\1019538\184843r"}) (ClientId {client = "1"}))
testObject_PushToken_user_9 :: PushToken
testObject_PushToken_user_9 = (pushToken (GCM) (AppName {appNameText = "l\189037"}) (Token {tokenText = "Hc"}) (ClientId {client = "19"}))
testObject_PushToken_user_10 :: PushToken
testObject_PushToken_user_10 = (pushToken (GCM) (AppName {appNameText = "\DLE"}) (Token {tokenText = ":\SIQu"}) (ClientId {client = "16"}))
testObject_PushToken_user_11 :: PushToken
testObject_PushToken_user_11 = (pushToken (APNSVoIP) (AppName {appNameText = "\ENQs\1082866\&2\n~"}) (Token {tokenText = "+"}) (ClientId {client = "13"}))
testObject_PushToken_user_12 :: PushToken
testObject_PushToken_user_12 = (pushToken (GCM) (AppName {appNameText = "7e\1103739"}) (Token {tokenText = "+c\DEL"}) (ClientId {client = "9"}))
testObject_PushToken_user_13 :: PushToken
testObject_PushToken_user_13 = (pushToken (GCM) (AppName {appNameText = "\ETX\3352\DC4_"}) (Token {tokenText = "z\1077394+@yU"}) (ClientId {client = "d"}))
testObject_PushToken_user_14 :: PushToken
testObject_PushToken_user_14 = (pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = ""}) (ClientId {client = "e"}))
testObject_PushToken_user_15 :: PushToken
testObject_PushToken_user_15 = (pushToken (APNSVoIP) (AppName {appNameText = "e\156040\&63%"}) (Token {tokenText = "}u\177112]w"}) (ClientId {client = "8"}))
testObject_PushToken_user_16 :: PushToken
testObject_PushToken_user_16 = (pushToken (GCM) (AppName {appNameText = "\US\ETX"}) (Token {tokenText = ""}) (ClientId {client = "1a"}))
testObject_PushToken_user_17 :: PushToken
testObject_PushToken_user_17 = (pushToken (APNS) (AppName {appNameText = "k\9795d"}) (Token {tokenText = "Yl\\"}) (ClientId {client = "1a"}))
testObject_PushToken_user_18 :: PushToken
testObject_PushToken_user_18 = (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\r\ACK/\1053404\1045157"}) (ClientId {client = "1c"}))
testObject_PushToken_user_19 :: PushToken
testObject_PushToken_user_19 = (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "!!\DLER]2\189923"}) (ClientId {client = "14"}))
testObject_PushToken_user_20 :: PushToken
testObject_PushToken_user_20 = (pushToken (APNS) (AppName {appNameText = "7u\ETBD"}) (Token {tokenText = ""}) (ClientId {client = "6"}))
