{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.UserHandleInfo_user where

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
testObject_UserHandleInfo_user_1 :: UserHandleInfo
testObject_UserHandleInfo_user_1 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003150-0000-49a7-0000-7ec8000056a9"))), qDomain = Domain {_domainText = "53di-z.pj4-s623.zs.f.c-6pc-j.sx"}}}
testObject_UserHandleInfo_user_2 :: UserHandleInfo
testObject_UserHandleInfo_user_2 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004d85-0000-6f24-0000-299d0000358f"))), qDomain = Domain {_domainText = "3c285l5.l6o.a2if9bm4-3sgl7"}}}
testObject_UserHandleInfo_user_3 :: UserHandleInfo
testObject_UserHandleInfo_user_3 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000022a6-0000-6dee-0000-1a6d000023a4"))), qDomain = Domain {_domainText = "d.8.oz"}}}
testObject_UserHandleInfo_user_4 :: UserHandleInfo
testObject_UserHandleInfo_user_4 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002d48-0000-614d-0000-3a6c000056ba"))), qDomain = Domain {_domainText = "as3qzz.t--e.bz-7137r"}}}
testObject_UserHandleInfo_user_5 :: UserHandleInfo
testObject_UserHandleInfo_user_5 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000153d-0000-0038-0000-481a00005ff5"))), qDomain = Domain {_domainText = "k-8.po5c"}}}
testObject_UserHandleInfo_user_6 :: UserHandleInfo
testObject_UserHandleInfo_user_6 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000944-0000-2c6f-0000-6a9000001dd8"))), qDomain = Domain {_domainText = "0kro7.17.wa2130"}}}
testObject_UserHandleInfo_user_7 :: UserHandleInfo
testObject_UserHandleInfo_user_7 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000192a-0000-6c55-0000-650400006443"))), qDomain = Domain {_domainText = "xor7pjwc6oh-von.p-g"}}}
testObject_UserHandleInfo_user_8 :: UserHandleInfo
testObject_UserHandleInfo_user_8 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000c01-0000-66e1-0000-00d60000260c"))), qDomain = Domain {_domainText = "0b-2.xz2iqx6"}}}
testObject_UserHandleInfo_user_9 :: UserHandleInfo
testObject_UserHandleInfo_user_9 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006b45-0000-47b4-0000-2a310000363d"))), qDomain = Domain {_domainText = "il1x.9d.o"}}}
testObject_UserHandleInfo_user_10 :: UserHandleInfo
testObject_UserHandleInfo_user_10 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000678-0000-75bd-0000-18bd000013ae"))), qDomain = Domain {_domainText = "su.h-9ez"}}}
testObject_UserHandleInfo_user_11 :: UserHandleInfo
testObject_UserHandleInfo_user_11 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006da3-0000-030a-0000-705a000021a2"))), qDomain = Domain {_domainText = "09-d0.p87l"}}}
testObject_UserHandleInfo_user_12 :: UserHandleInfo
testObject_UserHandleInfo_user_12 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000044d2-0000-2c86-0000-01de00006707"))), qDomain = Domain {_domainText = "oj.xl3ushx"}}}
testObject_UserHandleInfo_user_13 :: UserHandleInfo
testObject_UserHandleInfo_user_13 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006ed5-0000-34ba-0000-1ce40000193b"))), qDomain = Domain {_domainText = "9-7.nieu"}}}
testObject_UserHandleInfo_user_14 :: UserHandleInfo
testObject_UserHandleInfo_user_14 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004a5c-0000-2983-0000-39ba00001271"))), qDomain = Domain {_domainText = "2twr-wm.ac"}}}
testObject_UserHandleInfo_user_15 :: UserHandleInfo
testObject_UserHandleInfo_user_15 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000027d9-0000-2547-0000-2e1700006e5f"))), qDomain = Domain {_domainText = "56--w.5qz5.jv8ug"}}}
testObject_UserHandleInfo_user_16 :: UserHandleInfo
testObject_UserHandleInfo_user_16 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004007-0000-18e1-0000-0aa1000057ab"))), qDomain = Domain {_domainText = "7d.0.4t73.3.k7"}}}
testObject_UserHandleInfo_user_17 :: UserHandleInfo
testObject_UserHandleInfo_user_17 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000963-0000-5b4a-0000-418700001b26"))), qDomain = Domain {_domainText = "p-v.vz"}}}
testObject_UserHandleInfo_user_18 :: UserHandleInfo
testObject_UserHandleInfo_user_18 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004814-0000-33f6-0000-7a4e00006649"))), qDomain = Domain {_domainText = "x.l"}}}
testObject_UserHandleInfo_user_19 :: UserHandleInfo
testObject_UserHandleInfo_user_19 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007009-0000-5da6-0000-7c5500007a7b"))), qDomain = Domain {_domainText = "0y.56.n8a"}}}
testObject_UserHandleInfo_user_20 :: UserHandleInfo
testObject_UserHandleInfo_user_20 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000080e-0000-408e-0000-2cff00005870"))), qDomain = Domain {_domainText = "2-h.the.22on5.mn7-36.2.3-5.d"}}}
