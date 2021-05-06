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
testObject_UserHandleInfo_user_1 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005099-0000-41e9-0000-76b200001cac"))), qDomain = Domain {_domainText = "d-s0.om0"}}}
testObject_UserHandleInfo_user_2 :: UserHandleInfo
testObject_UserHandleInfo_user_2 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000065bf-0000-112f-0000-544500000db5"))), qDomain = Domain {_domainText = "tz.6-a.x83441-7.vh1.v-3j4.q6-rp68k5-9"}}}
testObject_UserHandleInfo_user_3 :: UserHandleInfo
testObject_UserHandleInfo_user_3 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003ab7-0000-42df-0000-63bc00001623"))), qDomain = Domain {_domainText = "rt--y.nt1j"}}}
testObject_UserHandleInfo_user_4 :: UserHandleInfo
testObject_UserHandleInfo_user_4 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006e1b-0000-3425-0000-30980000788e"))), qDomain = Domain {_domainText = "19lfuyh2.9c-qoa5y78.v5"}}}
testObject_UserHandleInfo_user_5 :: UserHandleInfo
testObject_UserHandleInfo_user_5 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002161-0000-1cf3-0000-176100000695"))), qDomain = Domain {_domainText = "45j.i"}}}
testObject_UserHandleInfo_user_6 :: UserHandleInfo
testObject_UserHandleInfo_user_6 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005b1e-0000-7ae9-0000-033d0000330c"))), qDomain = Domain {_domainText = "l-z5l9ju00.l1.ei3"}}}
testObject_UserHandleInfo_user_7 :: UserHandleInfo
testObject_UserHandleInfo_user_7 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000069d3-0000-2813-0000-42be0000113e"))), qDomain = Domain {_domainText = "p04.9.3ud6-v-6.dudq96"}}}
testObject_UserHandleInfo_user_8 :: UserHandleInfo
testObject_UserHandleInfo_user_8 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000650d-0000-2b98-0000-537100000809"))), qDomain = Domain {_domainText = "8n1--5.rm"}}}
testObject_UserHandleInfo_user_9 :: UserHandleInfo
testObject_UserHandleInfo_user_9 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004584-0000-742a-0000-535500007ce6"))), qDomain = Domain {_domainText = "08.i1.n0oh.da9iiu6df292647a7"}}}
testObject_UserHandleInfo_user_10 :: UserHandleInfo
testObject_UserHandleInfo_user_10 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006c8a-0000-739a-0000-048600006abe"))), qDomain = Domain {_domainText = "v.c9"}}}
testObject_UserHandleInfo_user_11 :: UserHandleInfo
testObject_UserHandleInfo_user_11 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000002ee-0000-763e-0000-57f800002d7c"))), qDomain = Domain {_domainText = "7.j807.q.mkk"}}}
testObject_UserHandleInfo_user_12 :: UserHandleInfo
testObject_UserHandleInfo_user_12 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000191c-0000-3616-0000-244400003dd8"))), qDomain = Domain {_domainText = "0h0ee72.lyj.xyd-d4.n3wiii-7w3-jh-b5.v970"}}}
testObject_UserHandleInfo_user_13 :: UserHandleInfo
testObject_UserHandleInfo_user_13 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000067dd-0000-2256-0000-6b0800001179"))), qDomain = Domain {_domainText = "37uc.d-vv0f.56271-q.s5xl6nj1a1"}}}
testObject_UserHandleInfo_user_14 :: UserHandleInfo
testObject_UserHandleInfo_user_14 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000017bb-0000-23e2-0000-061300000650"))), qDomain = Domain {_domainText = "5809453cpd-3-px6.b522qk3x-ot"}}}
testObject_UserHandleInfo_user_15 :: UserHandleInfo
testObject_UserHandleInfo_user_15 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000058f-0000-0ed0-0000-308e0000642f"))), qDomain = Domain {_domainText = "t0.g.m5799x4t0"}}}
testObject_UserHandleInfo_user_16 :: UserHandleInfo
testObject_UserHandleInfo_user_16 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000a81-0000-303a-0000-4f6f00003176"))), qDomain = Domain {_domainText = "6.xe.sb--6"}}}
testObject_UserHandleInfo_user_17 :: UserHandleInfo
testObject_UserHandleInfo_user_17 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001a45-0000-435f-0000-4fbf00006449"))), qDomain = Domain {_domainText = "3-760.vm8"}}}
testObject_UserHandleInfo_user_18 :: UserHandleInfo
testObject_UserHandleInfo_user_18 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003b68-0000-195c-0000-356000006296"))), qDomain = Domain {_domainText = "ql4ba.y5uonw11pz"}}}
testObject_UserHandleInfo_user_19 :: UserHandleInfo
testObject_UserHandleInfo_user_19 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000545a-0000-7cfb-0000-311900001738"))), qDomain = Domain {_domainText = "55fca.f085--j"}}}
testObject_UserHandleInfo_user_20 :: UserHandleInfo
testObject_UserHandleInfo_user_20 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000018cd-0000-2d19-0000-53070000345f"))), qDomain = Domain {_domainText = "4-am.65.h25k"}}}
