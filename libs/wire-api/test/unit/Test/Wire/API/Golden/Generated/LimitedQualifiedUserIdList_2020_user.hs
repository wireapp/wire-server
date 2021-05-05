{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.LimitedQualifiedUserIdList_2020_user where

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
testObject_LimitedQualifiedUserIdList_2020_1 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_1 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000006ef-0000-07fd-0000-0efe000067be"))), qDomain = Domain {_domainText = "355id.ih-666"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000677d-0000-2609-0000-161f00005e82"))), qDomain = Domain {_domainText = "5.q.9t--4.5c-e0780b.58-6q.c"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000068ad-0000-6ca6-0000-18ee00006019"))), qDomain = Domain {_domainText = "4-2l700.20.mn"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006eae-0000-4885-0000-08290000064d"))), qDomain = Domain {_domainText = "0-xsk.2w0.o"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006863-0000-2377-0000-4cf200003778"))), qDomain = Domain {_domainText = "kq5s.gg.59.v.i69"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001ec2-0000-5ad0-0000-41b30000736d"))), qDomain = Domain {_domainText = "e-504.xtp-2r09"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000009a4-0000-460d-0000-0ca300002bf0"))), qDomain = Domain {_domainText = "x.pbz2n"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000045e3-0000-3a9e-0000-37fb00005460"))), qDomain = Domain {_domainText = "0-i.8.iv8"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003431-0000-3aec-0000-222b000002b0"))), qDomain = Domain {_domainText = "974o-72l4th8.n3k-3.58.n.io---ui570b5m"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000047a-0000-31b8-0000-0ac90000298d"))), qDomain = Domain {_domainText = "9u9.e7fz200-w"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005cb1-0000-44fe-0000-449400002cc8"))), qDomain = Domain {_domainText = "6i-r---m9du9k.cz-md9.g-8-6diqmk57m.t"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001cbb-0000-42d1-0000-01d200004e19"))), qDomain = Domain {_domainText = "65n49s1j.a"}}]))}
testObject_LimitedQualifiedUserIdList_2020_2 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_2 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002d24-0000-6b55-0000-33c00000234f"))), qDomain = Domain {_domainText = "1.7qj59m-6.8.hye0om5"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004fbb-0000-7c16-0000-6967000042ae"))), qDomain = Domain {_domainText = "8974p0-6l.s-h"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000032f3-0000-734a-0000-1a2800006882"))), qDomain = Domain {_domainText = "c-wel580.j362-m1"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000787e-0000-437f-0000-44d700003f14"))), qDomain = Domain {_domainText = "b520kol.j04i7.32zhab.lq0.pe.sn2tx"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001c78-0000-2674-0000-1d180000175f"))), qDomain = Domain {_domainText = "v-9-h5331nbg3.7.87rs3i1-6.z4np2.r6l3m7-8.nmr-ti"}}]))}
testObject_LimitedQualifiedUserIdList_2020_3 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_3 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000059ed-0000-7204-0000-1790000063d8"))), qDomain = Domain {_domainText = "5.m"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007c44-0000-3cea-0000-3e3a00001ed7"))), qDomain = Domain {_domainText = "8a3jlv.n-r-7zl"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000273b-0000-241f-0000-65e0000019e6"))), qDomain = Domain {_domainText = "b.o92069.5-i.xuey.g834"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005040-0000-1344-0000-331a0000438b"))), qDomain = Domain {_domainText = "oa0sr-pye0.4mogs-0-31076iq.gp-3"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006ffc-0000-6892-0000-1ced000053b1"))), qDomain = Domain {_domainText = "72agf6.2n1q.x69"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007571-0000-0733-0000-475200001134"))), qDomain = Domain {_domainText = "mf7fl.c.0c103p6-v8874-f.j0"}}]))}
testObject_LimitedQualifiedUserIdList_2020_4 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_4 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000027af-0000-0b88-0000-7eb700003e83"))), qDomain = Domain {_domainText = "i4.1.ik6-2c"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000e9d-0000-0f96-0000-2714000056fc"))), qDomain = Domain {_domainText = "0eb39-vu.m2q-7q"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000073e7-0000-7047-0000-3b79000033ff"))), qDomain = Domain {_domainText = "40j.r9wq.j4q7c90"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000055b9-0000-63e4-0000-11ef00007fdf"))), qDomain = Domain {_domainText = "hz7c.c"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006ed7-0000-0c61-0000-7a8800007540"))), qDomain = Domain {_domainText = "tp-tuk6.k"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001d0b-0000-5575-0000-06a600001197"))), qDomain = Domain {_domainText = "87-r.033t.gu"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000129e-0000-5f5a-0000-6b2200001f85"))), qDomain = Domain {_domainText = "2-fk-61.b-6.k"}}]))}
testObject_LimitedQualifiedUserIdList_2020_5 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_5 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004a79-0000-039c-0000-192300000907"))), qDomain = Domain {_domainText = "6mgp-0p.s-5iomdp--jn3q7y"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007ac6-0000-5f91-0000-500e00000e13"))), qDomain = Domain {_domainText = "p.g"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003976-0000-3239-0000-215400003caa"))), qDomain = Domain {_domainText = "0rm562.c7-z"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000436e-0000-4b27-0000-073600000dec"))), qDomain = Domain {_domainText = "n4lb8.wro1v0j.p77r"}}]))}
