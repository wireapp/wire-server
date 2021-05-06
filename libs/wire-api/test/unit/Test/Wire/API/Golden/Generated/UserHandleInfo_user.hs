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
testObject_UserHandleInfo_user_1 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001436-0000-12a5-0000-2176000025da"))), qDomain = Domain {_domainText = "30jx.4eq8ttiv.q"}}}
testObject_UserHandleInfo_user_2 :: UserHandleInfo
testObject_UserHandleInfo_user_2 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000747a-0000-76a9-0000-60c400007030"))), qDomain = Domain {_domainText = "6.f-9pg-z"}}}
testObject_UserHandleInfo_user_3 :: UserHandleInfo
testObject_UserHandleInfo_user_3 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000574e-0000-2f35-0000-5b1900006eb9"))), qDomain = Domain {_domainText = "9xnd.flv39r9-8u"}}}
testObject_UserHandleInfo_user_4 :: UserHandleInfo
testObject_UserHandleInfo_user_4 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007604-0000-645c-0000-47ec000027bf"))), qDomain = Domain {_domainText = "393.2t-ba7w.37.v-9-08r3"}}}
testObject_UserHandleInfo_user_5 :: UserHandleInfo
testObject_UserHandleInfo_user_5 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000012a7-0000-379a-0000-6ec7000015c2"))), qDomain = Domain {_domainText = "53-3m.zfh-14--6-2h60"}}}
