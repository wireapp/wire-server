{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TurnHost_user where

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
testObject_TurnHost_user_1 :: TurnHost
testObject_TurnHost_user_1 = TurnHostIp (IpAddr (read "8475:175b:994a:694b:cdae:2904:942a:129f"))
testObject_TurnHost_user_2 :: TurnHost
testObject_TurnHost_user_2 = TurnHostIp (IpAddr (read "116.175.252.201"))
testObject_TurnHost_user_3 :: TurnHost
testObject_TurnHost_user_3 = TurnHostName "007.com"
testObject_TurnHost_user_4 :: TurnHost
testObject_TurnHost_user_4 = TurnHostName "xn--mgbh0fb.xn--kgbechtv"
testObject_TurnHost_user_5 :: TurnHost
testObject_TurnHost_user_5 = TurnHostName "123"
testObject_TurnHost_user_6 :: TurnHost
testObject_TurnHost_user_6 = TurnHostName "a-c"
testObject_TurnHost_user_7 :: TurnHost
testObject_TurnHost_user_7 = TurnHostName "007.com"
testObject_TurnHost_user_8 :: TurnHost
testObject_TurnHost_user_8 = TurnHostName "host.name"
testObject_TurnHost_user_9 :: TurnHost
testObject_TurnHost_user_9 = TurnHostIp (IpAddr (read "7120:ce25:5493:e3ef:76fa:d8d2:b539:94d4"))
testObject_TurnHost_user_10 :: TurnHost
testObject_TurnHost_user_10 = TurnHostIp (IpAddr (read "ba9:2b30:a43b:fa37:bbb:970c:5412:7164"))
testObject_TurnHost_user_11 :: TurnHost
testObject_TurnHost_user_11 = TurnHostIp (IpAddr (read "f97:5da9:6011:11e8:c82c:d54c:1439:188f"))
testObject_TurnHost_user_12 :: TurnHost
testObject_TurnHost_user_12 = TurnHostIp (IpAddr (read "4667:7e78:e711:47e8:b194:ff74:fc0c:51d2"))
testObject_TurnHost_user_13 :: TurnHost
testObject_TurnHost_user_13 = TurnHostIp (IpAddr (read "d8e5:36cb:90ae:2d72:ffa2:98d9:3435:34db"))
testObject_TurnHost_user_14 :: TurnHost
testObject_TurnHost_user_14 = TurnHostName "123"
testObject_TurnHost_user_15 :: TurnHost
testObject_TurnHost_user_15 = TurnHostIp (IpAddr (read "3662:81b4:9ddb:182f:9928:dbfd:6dcc:e41a"))
testObject_TurnHost_user_16 :: TurnHost
testObject_TurnHost_user_16 = TurnHostName "xn--mgbh0fb.xn--kgbechtv"
testObject_TurnHost_user_17 :: TurnHost
testObject_TurnHost_user_17 = TurnHostIp (IpAddr (read "178.121.140.242"))
testObject_TurnHost_user_18 :: TurnHost
testObject_TurnHost_user_18 = TurnHostIp (IpAddr (read "3.79.113.110"))
testObject_TurnHost_user_19 :: TurnHost
testObject_TurnHost_user_19 = TurnHostIp (IpAddr (read "44ce:6c77:9e7f:c9fa:4282:72ef:804e:b0a3"))
testObject_TurnHost_user_20 :: TurnHost
testObject_TurnHost_user_20 = TurnHostName "a-c"
