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
testObject_PushToken_user_1 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\SYN\DC1t\DEL"}) (Token {tokenText = "6o\ENQ%\69972g("}) (ClientId {client = "b"}))
testObject_PushToken_user_2 :: PushToken
testObject_PushToken_user_2 = (pushToken (APNSVoIP) (AppName {appNameText = ""}) (Token {tokenText = "u\STX9\989339\188554?"}) (ClientId {client = "4"}))
testObject_PushToken_user_3 :: PushToken
testObject_PushToken_user_3 = (pushToken (GCM) (AppName {appNameText = "\aw\1052308\SUB"}) (Token {tokenText = "\1097863f\1065539\US"}) (ClientId {client = "1e"}))
testObject_PushToken_user_4 :: PushToken
testObject_PushToken_user_4 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "p\1041424\1038399RO\ETX"}) (Token {tokenText = "\EM\8889`.\ENQ4"}) (ClientId {client = "16"}))
testObject_PushToken_user_5 :: PushToken
testObject_PushToken_user_5 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\ENQ)\FS["}) (Token {tokenText = "'\1002243\172219"}) (ClientId {client = "e"}))
testObject_PushToken_user_6 :: PushToken
testObject_PushToken_user_6 = (pushToken (APNSVoIP) (AppName {appNameText = "\182953\SYN$t\43144\RS"}) (Token {tokenText = "\1020642\1033731"}) (ClientId {client = "0"}))
testObject_PushToken_user_7 :: PushToken
testObject_PushToken_user_7 = (pushToken (APNSSandbox) (AppName {appNameText = "mT|/n"}) (Token {tokenText = "eA"}) (ClientId {client = "18"}))
testObject_PushToken_user_8 :: PushToken
testObject_PushToken_user_8 = (pushToken (GCM) (AppName {appNameText = "\92423M\1009981\1099589\149917\63126c"}) (Token {tokenText = "`Y\v"}) (ClientId {client = "e"}))
testObject_PushToken_user_9 :: PushToken
testObject_PushToken_user_9 = (pushToken (APNSSandbox) (AppName {appNameText = "\59849\10837n\984133_W"}) (Token {tokenText = "bh,\b"}) (ClientId {client = "1f"}))
testObject_PushToken_user_10 :: PushToken
testObject_PushToken_user_10 = (pushToken (APNSVoIP) (AppName {appNameText = "W\\T\1073746\1031889#"}) (Token {tokenText = "\ETX\167811"}) (ClientId {client = "c"}))
testObject_PushToken_user_11 :: PushToken
testObject_PushToken_user_11 = (pushToken (APNSVoIPSandbox) (AppName {appNameText = "\RS20\GS"}) (Token {tokenText = "5\SYNGv\1052281\1009675\1028309"}) (ClientId {client = "13"}))
testObject_PushToken_user_12 :: PushToken
testObject_PushToken_user_12 = (pushToken (APNSSandbox) (AppName {appNameText = "\163279\180808\fJ"}) (Token {tokenText = "\b\ETX"}) (ClientId {client = "1d"}))
testObject_PushToken_user_13 :: PushToken
testObject_PushToken_user_13 = (pushToken (APNSVoIP) (AppName {appNameText = "1\SYN~>"}) (Token {tokenText = ""}) (ClientId {client = "9"}))
testObject_PushToken_user_14 :: PushToken
testObject_PushToken_user_14 = (pushToken (APNSSandbox) (AppName {appNameText = ""}) (Token {tokenText = "\1055005`\RS\150952\1079269\39141"}) (ClientId {client = "e"}))
testObject_PushToken_user_15 :: PushToken
testObject_PushToken_user_15 = (pushToken (GCM) (AppName {appNameText = "\r"}) (Token {tokenText = "g0&u"}) (ClientId {client = "2"}))
testObject_PushToken_user_16 :: PushToken
testObject_PushToken_user_16 = (pushToken (APNSVoIP) (AppName {appNameText = "\74471z"}) (Token {tokenText = "X"}) (ClientId {client = "9"}))
testObject_PushToken_user_17 :: PushToken
testObject_PushToken_user_17 = (pushToken (GCM) (AppName {appNameText = ""}) (Token {tokenText = "l=.\1061017\GSW"}) (ClientId {client = "1c"}))
testObject_PushToken_user_18 :: PushToken
testObject_PushToken_user_18 = (pushToken (APNSSandbox) (AppName {appNameText = "\SUB,'"}) (Token {tokenText = "\1090194\a7\39408"}) (ClientId {client = "1f"}))
testObject_PushToken_user_19 :: PushToken
testObject_PushToken_user_19 = (pushToken (GCM) (AppName {appNameText = "\EM"}) (Token {tokenText = "!w*\SOHuGe"}) (ClientId {client = "9"}))
testObject_PushToken_user_20 :: PushToken
testObject_PushToken_user_20 = (pushToken (GCM) (AppName {appNameText = "2\US\49531"}) (Token {tokenText = "\SI\1088775\n"}) (ClientId {client = "2"}))
