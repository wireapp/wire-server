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
testObject_TurnHost_user_1 = TurnHostName "007.com"
testObject_TurnHost_user_2 :: TurnHost
testObject_TurnHost_user_2 = TurnHostIp (IpAddr (read "44:dbae:813:b53a:8a5f:c290:31ac:cdf8"))
testObject_TurnHost_user_3 :: TurnHost
testObject_TurnHost_user_3 = TurnHostIp (IpAddr (read "160.252.104.21"))
testObject_TurnHost_user_4 :: TurnHost
testObject_TurnHost_user_4 = TurnHostIp (IpAddr (read "73.205.40.128"))
testObject_TurnHost_user_5 :: TurnHost
testObject_TurnHost_user_5 = TurnHostName "xn--mgbh0fb.xn--kgbechtv"
testObject_TurnHost_user_6 :: TurnHost
testObject_TurnHost_user_6 = TurnHostIp (IpAddr (read "ff8d:6e73:393e:3034:63a0:ee31:11a9:920f"))
testObject_TurnHost_user_7 :: TurnHost
testObject_TurnHost_user_7 = TurnHostIp (IpAddr (read "151.85.197.118"))
testObject_TurnHost_user_8 :: TurnHost
testObject_TurnHost_user_8 = TurnHostIp (IpAddr (read "224.69.92.182"))
testObject_TurnHost_user_9 :: TurnHost
testObject_TurnHost_user_9 = TurnHostIp (IpAddr (read "229.64.202.76"))
testObject_TurnHost_user_10 :: TurnHost
testObject_TurnHost_user_10 = TurnHostIp (IpAddr (read "4c5a:79e6:6090:b64d:f883:d732:f066:98d"))
testObject_TurnHost_user_11 :: TurnHost
testObject_TurnHost_user_11 = TurnHostName "123"
testObject_TurnHost_user_12 :: TurnHost
testObject_TurnHost_user_12 = TurnHostName "a-c"
testObject_TurnHost_user_13 :: TurnHost
testObject_TurnHost_user_13 = TurnHostName "007.com"
testObject_TurnHost_user_14 :: TurnHost
testObject_TurnHost_user_14 = TurnHostIp (IpAddr (read "194.80.240.196"))
testObject_TurnHost_user_15 :: TurnHost
testObject_TurnHost_user_15 = TurnHostIp (IpAddr (read "210.14.40.15"))
testObject_TurnHost_user_16 :: TurnHost
testObject_TurnHost_user_16 = TurnHostIp (IpAddr (read "6bc:7e8:9b49:ebe:2aa9:5f66:48e2:1819"))
testObject_TurnHost_user_17 :: TurnHost
testObject_TurnHost_user_17 = TurnHostIp (IpAddr (read "126.26.68.22"))
testObject_TurnHost_user_18 :: TurnHost
testObject_TurnHost_user_18 = TurnHostIp (IpAddr (read "8c8e:9be7:d534:d16c:42a6:534d:50e6:794e"))
testObject_TurnHost_user_19 :: TurnHost
testObject_TurnHost_user_19 = TurnHostName "007.com"
testObject_TurnHost_user_20 :: TurnHost
testObject_TurnHost_user_20 = TurnHostName "host.name"
