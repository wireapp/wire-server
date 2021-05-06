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
testObject_LimitedQualifiedUserIdList_2020_user_1 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_1 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000067d0-0000-7e47-0000-0a3c0000582e"))), qDomain = Domain {_domainText = "92.x"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000284b-0000-0176-0000-3fb700002b80"))), qDomain = Domain {_domainText = "6-9a5.bzwdu9.yk1-8bj81n3k2-75k4-ojjg1b"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000035ea-0000-320f-0000-03e30000387a"))), qDomain = Domain {_domainText = "8x--85k.moz-ya-th.j-6-f--m-1o"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_2 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_2 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000184-0000-1872-0000-7caf00007c73"))), qDomain = Domain {_domainText = "2vd0.895e.ny-h3a-64u.p9x9.g15.f3"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000043b3-0000-4c80-0000-36b700000231"))), qDomain = Domain {_domainText = "j1ns9kew.4-x.t4f"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006ce3-0000-7527-0000-37ec00003eb2"))), qDomain = Domain {_domainText = "d-l1.v-x"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000007fb-0000-78c1-0000-7653000039a5"))), qDomain = Domain {_domainText = "j60k0.z70s"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000307a-0000-2ac3-0000-529f000066cd"))), qDomain = Domain {_domainText = "6c-11tj9479.o"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002946-0000-57b7-0000-2d5000003348"))), qDomain = Domain {_domainText = "uc8f-63.9b5.624.q29lr.e"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002581-0000-79ea-0000-11ab0000211f"))), qDomain = Domain {_domainText = "nfk-wx.ds9-j92u4-1.k8.h-d56o0df1o"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000023a4-0000-1dc7-0000-38a200001ab6"))), qDomain = Domain {_domainText = "go2.53-8-7-x3.85o0-6.r-t4vk6pz0rg1009.0t-7y.dy-9-q.z"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006da0-0000-376a-0000-26b60000359c"))), qDomain = Domain {_domainText = "b.02.n03"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007af6-0000-7172-0000-7d6200001b33"))), qDomain = Domain {_domainText = "8r3ary.21z3511.g0zwv5i143x"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000022cd-0000-71f9-0000-4793000006e2"))), qDomain = Domain {_domainText = "r2567.nu"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000650-0000-0b87-0000-1a9d00004956"))), qDomain = Domain {_domainText = "376.kg-mlke230b"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007839-0000-13dd-0000-6cb200000d4f"))), qDomain = Domain {_domainText = "3-6c.r"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000212c-0000-4f25-0000-16d100007c77"))), qDomain = Domain {_domainText = "41.l.e35.l9.nt"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000019ee-0000-7908-0000-5f2e00007cd1"))), qDomain = Domain {_domainText = "3q4j.r8i4i4h.c0.i2sa"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_3 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_3 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000809-0000-65ea-0000-370d00007139"))), qDomain = Domain {_domainText = "7-e1.r4r.c88.9b2.w5-c7i-j-a995"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000052b2-0000-6a9c-0000-575300005e4b"))), qDomain = Domain {_domainText = "15gw.ut"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007e04-0000-0d60-0000-2afc00004dee"))), qDomain = Domain {_domainText = "5-76-f.szs.cx13.lj94r"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000049af-0000-5c5e-0000-0f4300002e3e"))), qDomain = Domain {_domainText = "n.y"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007b46-0000-2dcb-0000-42a300003d99"))), qDomain = Domain {_domainText = "t0.c29"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000663-0000-596f-0000-6f2c00006a55"))), qDomain = Domain {_domainText = "2c.p-t48.itd5"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004a9a-0000-0414-0000-67c700002b59"))), qDomain = Domain {_domainText = "8-z4ytz.fr"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000071b6-0000-2f8f-0000-300a00000452"))), qDomain = Domain {_domainText = "05r.c"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000211e-0000-7789-0000-19e50000064b"))), qDomain = Domain {_domainText = "z.81049.j"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000187d-0000-4d55-0000-3994000034b4"))), qDomain = Domain {_domainText = "2-6.vjszp-t9"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000034ab-0000-7b83-0000-27c300006896"))), qDomain = Domain {_domainText = "4r.l1z.fc-0z"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001af5-0000-7feb-0000-7aae00007015"))), qDomain = Domain {_domainText = "0c1.qi1u8"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000025d7-0000-0fef-0000-1d9d00003a77"))), qDomain = Domain {_domainText = "f.3300.ba"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000134a-0000-3dd0-0000-76c100005e3e"))), qDomain = Domain {_domainText = "3k.gwzwz6"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000464f-0000-1408-0000-2e6c000024a5"))), qDomain = Domain {_domainText = "2992.re-6"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000000e3-0000-6ec4-0000-60ad000030df"))), qDomain = Domain {_domainText = "4z--5.9-e.m-i2.38.pi--14i239.96.c9-5z"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001990-0000-5c5a-0000-00f40000031d"))), qDomain = Domain {_domainText = "f.0-j.9-x--pa7h0.l7we31"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_4 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_4 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007a9d-0000-631e-0000-374d00001769"))), qDomain = Domain {_domainText = "gb.7.n8-ogf"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004471-0000-3ae0-0000-50a7000030eb"))), qDomain = Domain {_domainText = "n.q1t8jg7"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001b96-0000-368c-0000-61ad0000114c"))), qDomain = Domain {_domainText = "88-36.g-pdu4-4q38.82x.q"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003d3e-0000-7802-0000-59f300000dd5"))), qDomain = Domain {_domainText = "suz.e41d"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006177-0000-349c-0000-6ba90000324a"))), qDomain = Domain {_domainText = "f.dyll"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_5 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_5 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005169-0000-2833-0000-20c700000233"))), qDomain = Domain {_domainText = "62e.s-6"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000049bd-0000-1ff5-0000-479500004354"))), qDomain = Domain {_domainText = "59.9-c7.xf7i"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000677e-0000-642c-0000-23760000234a"))), qDomain = Domain {_domainText = "lv63-mde.86.p-nykw01.y67h"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007771-0000-6b6e-0000-40ed00007e30"))), qDomain = Domain {_domainText = "uo.zbll.r1"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004e4f-0000-530e-0000-4a5500007ed7"))), qDomain = Domain {_domainText = "25u.v"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007e07-0000-775d-0000-5a21000030e5"))), qDomain = Domain {_domainText = "r3684.p-1k1z1a43k8-b"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003a77-0000-1bd6-0000-00f500004473"))), qDomain = Domain {_domainText = "ue.fc7r"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006db6-0000-0b21-0000-585000003d40"))), qDomain = Domain {_domainText = "pbi.vy73"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000026b5-0000-27b2-0000-40ab00005b55"))), qDomain = Domain {_domainText = "s.t"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003c19-0000-690d-0000-61c200006713"))), qDomain = Domain {_domainText = "8.j-589g.zh"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000045da-0000-6b6d-0000-1f9800006130"))), qDomain = Domain {_domainText = "6m9i2.ta6v2"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000419f-0000-5dae-0000-667e00002019"))), qDomain = Domain {_domainText = "alpa.k3-n4h.h-av"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000709b-0000-15d5-0000-058900002b0e"))), qDomain = Domain {_domainText = "w12.w7-804b-1-z6mes87dy654"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004a08-0000-7c61-0000-1d3500006cec"))), qDomain = Domain {_domainText = "40f--t1.jg13"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001aa2-0000-1e3f-0000-41b300004921"))), qDomain = Domain {_domainText = "xy.1-8o6.np2"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004a6f-0000-6813-0000-676f00006a40"))), qDomain = Domain {_domainText = "5dwb.7-pf151.g49rc2u8i"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000009fe-0000-14b0-0000-520b00004ae9"))), qDomain = Domain {_domainText = "8p76c.v6-696j0.n-n2i8r0v--802"}}]))}
