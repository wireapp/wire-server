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
testObject_UserHandleInfo_1 :: UserHandleInfo
testObject_UserHandleInfo_1 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001577-0000-7d38-0000-7b400000501a"))), qDomain = Domain {_domainText = "87.e0yv2k"}}}
testObject_UserHandleInfo_2 :: UserHandleInfo
testObject_UserHandleInfo_2 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006803-0000-4dae-0000-59ab00001199"))), qDomain = Domain {_domainText = "o.b.435q70x-1-pdq.u2um1-4"}}}
testObject_UserHandleInfo_3 :: UserHandleInfo
testObject_UserHandleInfo_3 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000052d2-0000-6a89-0000-0c8600002d91"))), qDomain = Domain {_domainText = "49yz.xeab9465.s.m.ny8u0.a4"}}}
testObject_UserHandleInfo_4 :: UserHandleInfo
testObject_UserHandleInfo_4 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002146-0000-44a1-0000-2151000043bc"))), qDomain = Domain {_domainText = "84-e3p-3r.6.h.u"}}}
testObject_UserHandleInfo_5 :: UserHandleInfo
testObject_UserHandleInfo_5 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006e1e-0000-7256-0000-5ac100006bb5"))), qDomain = Domain {_domainText = "ila-xb5-4.c8-39m"}}}
