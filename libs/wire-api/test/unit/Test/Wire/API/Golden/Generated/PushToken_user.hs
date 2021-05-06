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
testObject_PushToken_user_1 = (pushToken (GCM) (AppName {appNameText = "b\DELk"}) (Token {tokenText = "M1\170241\989337}B\SOH"}) (ClientId {client = "1d"}))
testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNSVoIP) (AppName {appNameText = "?^\128598Su\1091027"}) (Token {tokenText = "\EOTO\t\72346I\SUB\74267"}) (ClientId {client = "9"}))
testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (APNS) (AppName {appNameText = "\vK9|!\ENQP"}) (Token {tokenText = "+\1061016BKou"}) (ClientId {client = "14"}))
testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (APNS) (AppName {appNameText = "\SUBr?"}) (Token {tokenText = "k\1077309\CAN\GS\ACK\1003860"}) (ClientId {client = "6"}))
testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (GCM) (AppName {appNameText = "\CANi{a"}) (Token {tokenText = ""}) (ClientId {client = "1c"}))
testObject_PushToken_user_6 :: PushToken
testObject_PushToken_user_6 = (pushToken (GCM) (AppName {appNameText = "Z\SUBxmH0"}) (Token {tokenText = "\GSr\SOm"}) (ClientId {client = "a"}))
testObject_PushToken_user_7 :: PushToken
testObject_PushToken_user_7 = (pushToken (APNS) (AppName {appNameText = "U\1086088!,!9\161330"}) (Token {tokenText = "t'"}) (ClientId {client = "7"}))
testObject_PushToken_user_8 :: PushToken
testObject_PushToken_user_8 = (pushToken (GCM) (AppName {appNameText = "B"}) (Token {tokenText = "\DC22"}) (ClientId {client = "4"}))
testObject_PushToken_user_9 :: PushToken
testObject_PushToken_user_9 = (pushToken (APNSSandbox) (AppName {appNameText = "\15748"}) (Token {tokenText = "\ACK(\998874\SUB5<"}) (ClientId {client = "19"}))
testObject_PushToken_user_10 :: PushToken
testObject_PushToken_user_10 = (pushToken (APNSVoIP) (AppName {appNameText = "+\DC4\187401D"}) (Token {tokenText = "5"}) (ClientId {client = "6"}))
testObject_PushToken_user_11 :: PushToken
testObject_PushToken_user_11 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\ENQj\NUL\99087uk\1086999"}) (Token {tokenText = "<\43682"}) (ClientId {client = "4"}))
testObject_PushToken_user_12 :: PushToken
testObject_PushToken_user_12 = (pushToken (APNSVoIP) (AppName {appNameText = "\ACK\RS]\159393R\DLE"}) (Token {tokenText = "\SOHl"}) (ClientId {client = "1b"}))
testObject_PushToken_user_13 :: PushToken
testObject_PushToken_user_13 = (pushToken (APNSSandbox) (AppName {appNameText = "Np\188351r"}) (Token {tokenText = "\26140\n7\1046142>"}) (ClientId {client = "c"}))
testObject_PushToken_user_14 :: PushToken
testObject_PushToken_user_14 = (pushToken (APNSVoIP) (AppName {appNameText = "DVK\SOHD\1000362T"}) (Token {tokenText = "M8-q\f\31046"}) (ClientId {client = "4"}))
testObject_PushToken_user_15 :: PushToken
testObject_PushToken_user_15 = (pushToken (APNS) (AppName {appNameText = "\DLE\SI\DC23"}) (Token {tokenText = "i\1092860\&8t\141207\DLE"}) (ClientId {client = "1c"}))
testObject_PushToken_user_16 :: PushToken
testObject_PushToken_user_16 = (pushToken (APNSVoIP) (AppName {appNameText = "\1741\DLE-*\41290\SYN"}) (Token {tokenText = ""}) (ClientId {client = "12"}))
testObject_PushToken_user_17 :: PushToken
testObject_PushToken_user_17 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "8"}) (Token {tokenText = "\1044734U\160141\1072565\ENQ"}) (ClientId {client = "1c"}))
testObject_PushToken_user_18 :: PushToken
testObject_PushToken_user_18 = (pushToken (APNSVoIP) (AppName {appNameText = "\989849\14598\29240"}) (Token {tokenText = "\1038003\1104944\142763D\989747\&7C"}) (ClientId {client = "a"}))
testObject_PushToken_user_19 :: PushToken
testObject_PushToken_user_19 = (pushToken (APNSSandbox) (AppName {appNameText = "_"}) (Token {tokenText = "Rr"}) (ClientId {client = "7"}))
testObject_PushToken_user_20 :: PushToken
testObject_PushToken_user_20 = (pushToken (APNSVoIP) (AppName {appNameText = "\\l"}) (Token {tokenText = ";q\1058779\1083490\&0"}) (ClientId {client = "1"}))
