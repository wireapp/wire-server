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
testObject_TurnHost_user_1 = TurnHostIp (IpAddr (read "142.88.198.205"))
testObject_TurnHost_user_2 :: TurnHost
testObject_TurnHost_user_2 = TurnHostName "host.name"
testObject_TurnHost_user_3 :: TurnHost
testObject_TurnHost_user_3 = TurnHostIp (IpAddr (read "1865:56cd:b13f:d86f:4025:cfad:ab48:e163"))
testObject_TurnHost_user_4 :: TurnHost
testObject_TurnHost_user_4 = TurnHostIp (IpAddr (read "60.24.63.21"))
testObject_TurnHost_user_5 :: TurnHost
testObject_TurnHost_user_5 = TurnHostName "007.com"
testObject_TurnHost_user_6 :: TurnHost
testObject_TurnHost_user_6 = TurnHostIp (IpAddr (read "232.161.14.242"))
testObject_TurnHost_user_7 :: TurnHost
testObject_TurnHost_user_7 = TurnHostIp (IpAddr (read "201.242.72.146"))
testObject_TurnHost_user_8 :: TurnHost
testObject_TurnHost_user_8 = TurnHostIp (IpAddr (read "7cf4:12ee:519e:ec14:3cc:7565:188d:936b"))
testObject_TurnHost_user_9 :: TurnHost
testObject_TurnHost_user_9 = TurnHostName "host.name"
testObject_TurnHost_user_10 :: TurnHost
testObject_TurnHost_user_10 = TurnHostIp (IpAddr (read "173.243.23.40"))
testObject_TurnHost_user_11 :: TurnHost
testObject_TurnHost_user_11 = TurnHostIp (IpAddr (read "330f:79c1:316a:f3a3:815b:c387:d793:9270"))
testObject_TurnHost_user_12 :: TurnHost
testObject_TurnHost_user_12 = TurnHostIp (IpAddr (read "165.19.178.127"))
testObject_TurnHost_user_13 :: TurnHost
testObject_TurnHost_user_13 = TurnHostIp (IpAddr (read "232.21.84.72"))
testObject_TurnHost_user_14 :: TurnHost
testObject_TurnHost_user_14 = TurnHostIp (IpAddr (read "993f:c260:32e6:a3d2:e42c:4267:a084:c4a"))
testObject_TurnHost_user_15 :: TurnHost
testObject_TurnHost_user_15 = TurnHostIp (IpAddr (read "223.59.103.126"))
testObject_TurnHost_user_16 :: TurnHost
testObject_TurnHost_user_16 = TurnHostName "host.name"
testObject_TurnHost_user_17 :: TurnHost
testObject_TurnHost_user_17 = TurnHostIp (IpAddr (read "2.216.113.36"))
testObject_TurnHost_user_18 :: TurnHost
testObject_TurnHost_user_18 = TurnHostIp (IpAddr (read "4ea3:e7e4:f09d:74fe:d15e:4893:2a7b:f23d"))
testObject_TurnHost_user_19 :: TurnHost
testObject_TurnHost_user_19 = TurnHostName "xn--mgbh0fb.xn--kgbechtv"
testObject_TurnHost_user_20 :: TurnHost
testObject_TurnHost_user_20 = TurnHostIp (IpAddr (read "6b24:65aa:6ba8:f1a3:25f0:2ace:413b:4b3a"))
