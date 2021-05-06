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
testObject_UserHandleInfo_user_1 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000026a3-0000-5a0f-0000-5cf600007a23"))), qDomain = Domain {_domainText = "jr8hab-r.95.eo516g39-5g08"}}}
testObject_UserHandleInfo_user_2 :: UserHandleInfo
testObject_UserHandleInfo_user_2 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001243-0000-31cb-0000-1511000015b0"))), qDomain = Domain {_domainText = "3uu5f.7--6wm.n0mq"}}}
testObject_UserHandleInfo_user_3 :: UserHandleInfo
testObject_UserHandleInfo_user_3 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004042-0000-2626-0000-2501000026ac"))), qDomain = Domain {_domainText = "5-2.xbo"}}}
testObject_UserHandleInfo_user_4 :: UserHandleInfo
testObject_UserHandleInfo_user_4 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000678c-0000-5484-0000-1243000055ed"))), qDomain = Domain {_domainText = "h0.n3"}}}
testObject_UserHandleInfo_user_5 :: UserHandleInfo
testObject_UserHandleInfo_user_5 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007a56-0000-1456-0000-514300000e96"))), qDomain = Domain {_domainText = "8-4.mv-e2nq-25.c64-60.f"}}}
testObject_UserHandleInfo_user_6 :: UserHandleInfo
testObject_UserHandleInfo_user_6 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000501c-0000-5f0e-0000-50ef00005db3"))), qDomain = Domain {_domainText = "9-d.9l5.48173f5.489954.z50kx4-4-7b.f3"}}}
testObject_UserHandleInfo_user_7 :: UserHandleInfo
testObject_UserHandleInfo_user_7 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000126e-0000-6847-0000-641f00004718"))), qDomain = Domain {_domainText = "y.x"}}}
testObject_UserHandleInfo_user_8 :: UserHandleInfo
testObject_UserHandleInfo_user_8 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004c39-0000-6263-0000-5feb00005e72"))), qDomain = Domain {_domainText = "q-vq46r6.xh95nvb57"}}}
testObject_UserHandleInfo_user_9 :: UserHandleInfo
testObject_UserHandleInfo_user_9 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005a05-0000-5a68-0000-0f1100000d1f"))), qDomain = Domain {_domainText = "v8dv.h-614n"}}}
testObject_UserHandleInfo_user_10 :: UserHandleInfo
testObject_UserHandleInfo_user_10 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000800-0000-0216-0000-5d2b00007efd"))), qDomain = Domain {_domainText = "hm.04821.c"}}}
testObject_UserHandleInfo_user_11 :: UserHandleInfo
testObject_UserHandleInfo_user_11 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000732d-0000-6c6a-0000-5eba0000600e"))), qDomain = Domain {_domainText = "v9-8a-8.97r3.xh38c4-9.h.oxq.xk0r738wbw0"}}}
testObject_UserHandleInfo_user_12 :: UserHandleInfo
testObject_UserHandleInfo_user_12 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000024a6-0000-324a-0000-2d1300003946"))), qDomain = Domain {_domainText = "kuiq.h"}}}
testObject_UserHandleInfo_user_13 :: UserHandleInfo
testObject_UserHandleInfo_user_13 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006af7-0000-7435-0000-476500006206"))), qDomain = Domain {_domainText = "z.ua.i-h"}}}
testObject_UserHandleInfo_user_14 :: UserHandleInfo
testObject_UserHandleInfo_user_14 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001755-0000-66f2-0000-728600000ff6"))), qDomain = Domain {_domainText = "1j-2.eq29143-oj.247ld7.j5xm"}}}
testObject_UserHandleInfo_user_15 :: UserHandleInfo
testObject_UserHandleInfo_user_15 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000011bc-0000-5cfe-0000-658700006590"))), qDomain = Domain {_domainText = "8q3.5680i.8-36-j8.if.47fnsq87-6.5.m"}}}
testObject_UserHandleInfo_user_16 :: UserHandleInfo
testObject_UserHandleInfo_user_16 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003c8f-0000-0256-0000-185900004db5"))), qDomain = Domain {_domainText = "3p59.i-x3g2m40-2"}}}
testObject_UserHandleInfo_user_17 :: UserHandleInfo
testObject_UserHandleInfo_user_17 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002370-0000-0d4a-0000-55940000619a"))), qDomain = Domain {_domainText = "7-qu94o-p.h"}}}
testObject_UserHandleInfo_user_18 :: UserHandleInfo
testObject_UserHandleInfo_user_18 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000065a5-0000-1bb5-0000-347f00003ac4"))), qDomain = Domain {_domainText = "w.5.y9p2-18"}}}
testObject_UserHandleInfo_user_19 :: UserHandleInfo
testObject_UserHandleInfo_user_19 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006926-0000-531d-0000-1ddc00005bc0"))), qDomain = Domain {_domainText = "69id5p.b5k.k411-75kw8"}}}
testObject_UserHandleInfo_user_20 :: UserHandleInfo
testObject_UserHandleInfo_user_20 = UserHandleInfo {userHandleId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000073d0-0000-3c95-0000-3af10000369e"))), qDomain = Domain {_domainText = "7b866d-y.03rj6p.z0"}}}
