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
testObject_LimitedQualifiedUserIdList_2020_user_1 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007e09-0000-1f61-0000-076600000485"))), qDomain = Domain {_domainText = "7y.p.i860o--i-g4ns.3.m1.p8-rr-y"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004ff0-0000-3841-0000-3e0600003756"))), qDomain = Domain {_domainText = "q-o-b.ybf"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004803-0000-65cb-0000-609d0000716c"))), qDomain = Domain {_domainText = "71s.j008"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006396-0000-2260-0000-186100006b1f"))), qDomain = Domain {_domainText = "z2.98nqanq5u9.25e0-3m3p.z61x"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007cb5-0000-12de-0000-7f4500006647"))), qDomain = Domain {_domainText = "4o.jv291-0.5-0.4.pbh"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006921-0000-0420-0000-4847000079d5"))), qDomain = Domain {_domainText = "o705.l1--x7.sd"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007366-0000-0f92-0000-7009000062f7"))), qDomain = Domain {_domainText = "978.fa786ui6dy3"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004d06-0000-34cb-0000-584c00002670"))), qDomain = Domain {_domainText = "l.a.r5bi"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006f76-0000-4735-0000-313c00001ea5"))), qDomain = Domain {_domainText = "4g-i.l3ar-w"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007895-0000-145f-0000-4676000059d1"))), qDomain = Domain {_domainText = "89zjm.7qzy.kw"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002b7a-0000-4f12-0000-632200001198"))), qDomain = Domain {_domainText = "x.7.3275.n2-36-nl"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000379a-0000-78f6-0000-457a00000b61"))), qDomain = Domain {_domainText = "9pzy.qd28"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000247f-0000-7c94-0000-5dce0000405a"))), qDomain = Domain {_domainText = "e38.ljuq"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000062de-0000-64fa-0000-739900006e31"))), qDomain = Domain {_domainText = "8xaz5.d8n4"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000016dd-0000-380b-0000-4b46000063c0"))), qDomain = Domain {_domainText = "3.fs"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_2 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_2 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000249a-0000-6d07-0000-2f6c0000095b"))), qDomain = Domain {_domainText = "238-07-7.u.5q6im21.as"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004335-0000-5261-0000-3fca00000590"))), qDomain = Domain {_domainText = "4pbv.cde33.lj-p-8k-a-j-t"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000032f6-0000-36b8-0000-51f4000005a0"))), qDomain = Domain {_domainText = "03v.br614"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007d10-0000-7ead-0000-42cc00006b14"))), qDomain = Domain {_domainText = "1.3.bv71g8.wn--y1"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_3 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_3 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003ff5-0000-1b0e-0000-166d0000049c"))), qDomain = Domain {_domainText = "6.kb4dk.8n3e9.u3--xm"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007bf8-0000-25f9-0000-322100002eb4"))), qDomain = Domain {_domainText = "2.nan"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000025d3-0000-3281-0000-5ed20000698f"))), qDomain = Domain {_domainText = "c.eq7fb0"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00003272-0000-220b-0000-7fd500006ca3"))), qDomain = Domain {_domainText = "4fno.ddv"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000078a2-0000-75bc-0000-761a00000642"))), qDomain = Domain {_domainText = "j1x3331.x7q"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000010c1-0000-698e-0000-04d9000032b2"))), qDomain = Domain {_domainText = "6rh7yk.t--c92s9l"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000464-0000-72bd-0000-08120000595d"))), qDomain = Domain {_domainText = "t.66r.kzq"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000a1a-0000-7067-0000-0a82000025ba"))), qDomain = Domain {_domainText = "ytp.mp"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_4 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_4 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00006661-0000-0be1-0000-3b1e00002968"))), qDomain = Domain {_domainText = "12.vg3"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000054dc-0000-06f7-0000-551c00003ed7"))), qDomain = Domain {_domainText = "lzu3.i.y"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000031c5-0000-4c77-0000-753800002b32"))), qDomain = Domain {_domainText = "m.i49x88-8"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00001550-0000-370e-0000-69ae00004e87"))), qDomain = Domain {_domainText = "f.4.1a-2.q2h7q4"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000073d-0000-155f-0000-4c4700004075"))), qDomain = Domain {_domainText = "t-ce4wi.sad1mh.w6s"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000188d-0000-10ca-0000-3c8d000016ea"))), qDomain = Domain {_domainText = "a-q.n6-26ais8.x7.u1g-3-917"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002455-0000-3f09-0000-2c220000299c"))), qDomain = Domain {_domainText = "9-o4t4mj.z0pblgf"}}]))}
testObject_LimitedQualifiedUserIdList_2020_user_5 :: LimitedQualifiedUserIdList 20
testObject_LimitedQualifiedUserIdList_2020_user_5 = LimitedQualifiedUserIdList {qualifiedUsers = (unsafeRange ([Qualified {qUnqualified = (Id (fromJust (UUID.fromString "000018a7-0000-2c45-0000-78bb000077aa"))), qDomain = Domain {_domainText = "5h8m.92.3k5-9fv.22j--2.vki5"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00002c80-0000-7b39-0000-68d500001e55"))), qDomain = Domain {_domainText = "ufhko45o5ezwc.r"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004b66-0000-018c-0000-4dda0000616b"))), qDomain = Domain {_domainText = "8.zt405rka49.i"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000355-0000-1d19-0000-26cf00002d5a"))), qDomain = Domain {_domainText = "7b471.19f.tf1"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000695d-0000-7e56-0000-0dc600005f49"))), qDomain = Domain {_domainText = "6357t30-17qc083.zqy6.n5"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007656-0000-7b6e-0000-139500007617"))), qDomain = Domain {_domainText = "646-bxbmt.j201-yv"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00007525-0000-284f-0000-1f3200006544"))), qDomain = Domain {_domainText = "78.k967.5w2-s.s7"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00005859-0000-19ba-0000-3c6300006df1"))), qDomain = Domain {_domainText = "e1--qwx5.3o39.k87z2-123"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004ae6-0000-01ec-0000-42a600003c98"))), qDomain = Domain {_domainText = "m76915.bgr2"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000117a-0000-16e1-0000-494200003939"))), qDomain = Domain {_domainText = "959qra15536.e3r9"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000509-0000-6f87-0000-080300002a5d"))), qDomain = Domain {_domainText = "90cgw.q0"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00004ca1-0000-26dd-0000-4b8600003bda"))), qDomain = Domain {_domainText = "00x-g8.b41047a"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "0000531c-0000-3e4d-0000-11c200003d41"))), qDomain = Domain {_domainText = "0o9.6b-9y5.d3.v.r8-dr-mo946-y-xo.603nr.z"}},Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000cd3-0000-6f50-0000-422e000062fd"))), qDomain = Domain {_domainText = "s78.8m6.5r.12.5.ytl17"}}]))}
