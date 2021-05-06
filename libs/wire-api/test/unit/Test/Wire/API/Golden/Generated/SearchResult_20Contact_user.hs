{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SearchResult_20Contact_user where

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
testObject_SearchResult_20Contact_user_1 :: SearchResult Contact
testObject_SearchResult_20Contact_user_1 = SearchResult {searchFound = 7, searchReturned = -5, searchTook = 7, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "2.jg.tv"}}, contactName = "", contactColorId = Just 1, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_2 :: SearchResult Contact
testObject_SearchResult_20Contact_user_2 = SearchResult {searchFound = -1, searchReturned = -7, searchTook = -2, searchResults = []}
testObject_SearchResult_20Contact_user_3 :: SearchResult Contact
testObject_SearchResult_20Contact_user_3 = SearchResult {searchFound = -5, searchReturned = 4, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "7.9.ff-1"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "1448.j25w8.j-5"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "7t0o-5.2hue45k.301h03.o.re-9d5nfn"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "1-v293.sl.sr8cul"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "aua1.2by-n4.35.zf-5e"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "ti0j.l--v85"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "71n3r0.er-f.xm127-m"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}]}
testObject_SearchResult_20Contact_user_4 :: SearchResult Contact
testObject_SearchResult_20Contact_user_4 = SearchResult {searchFound = 5, searchReturned = 3, searchTook = 7, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "1h.5v.v46"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "885g7w.2j6o40c.y16e2dt-28.5e-f0.23p.u4f33c"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "q8.umm0x"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "m.hnl698aw7.o8l"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "v3hqk5.t"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_5 :: SearchResult Contact
testObject_SearchResult_20Contact_user_5 = SearchResult {searchFound = -5, searchReturned = -6, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "rfo2v-8s3.s-67.9f0uaox.rlh05k.j"}}, contactName = "\23876", contactColorId = Just (-1), contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_6 :: SearchResult Contact
testObject_SearchResult_20Contact_user_6 = SearchResult {searchFound = 6, searchReturned = -6, searchTook = 5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "66b.dd8d"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "p.du75-c-t2y"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_7 :: SearchResult Contact
testObject_SearchResult_20Contact_user_7 = SearchResult {searchFound = -3, searchReturned = -4, searchTook = -3, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "707dg-uu-r73.x.15c0qi4.b8lfdx"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "w", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_8 :: SearchResult Contact
testObject_SearchResult_20Contact_user_8 = SearchResult {searchFound = 5, searchReturned = -4, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "39.ct1"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "h4x.ik6w5m4inl796g7.8.x4"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_9 :: SearchResult Contact
testObject_SearchResult_20Contact_user_9 = SearchResult {searchFound = -3, searchReturned = 5, searchTook = 6, searchResults = []}
testObject_SearchResult_20Contact_user_10 :: SearchResult Contact
testObject_SearchResult_20Contact_user_10 = SearchResult {searchFound = 0, searchReturned = 5, searchTook = 1, searchResults = []}
testObject_SearchResult_20Contact_user_11 :: SearchResult Contact
testObject_SearchResult_20Contact_user_11 = SearchResult {searchFound = 5, searchReturned = 2, searchTook = 1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "u67.24-uomoo5.u2m91"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "c2-g.rt52.0--5a.pw-5"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "2x.d59r.w"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "9m.9.qw1"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_12 :: SearchResult Contact
testObject_SearchResult_20Contact_user_12 = SearchResult {searchFound = -1, searchReturned = 0, searchTook = 2, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "vq.v-a1-z1vu09nk.fx2.r.pk-4"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "h.597d.j512k02"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "a8hvh.gc"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "1pu.euz.t9p8--ngy368n61"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_13 :: SearchResult Contact
testObject_SearchResult_20Contact_user_13 = SearchResult {searchFound = 4, searchReturned = 3, searchTook = -5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "2-5.e23-z-0"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "f10i.588hf.xr5yc9"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_14 :: SearchResult Contact
testObject_SearchResult_20Contact_user_14 = SearchResult {searchFound = -5, searchReturned = -1, searchTook = 5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "9xg.stj-8"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "c-2.wtlt89w55-3x-962"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_15 :: SearchResult Contact
testObject_SearchResult_20Contact_user_15 = SearchResult {searchFound = -5, searchReturned = -5, searchTook = 5, searchResults = []}
testObject_SearchResult_20Contact_user_16 :: SearchResult Contact
testObject_SearchResult_20Contact_user_16 = SearchResult {searchFound = -2, searchReturned = 6, searchTook = -6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "jr-57.vk7xe5.l00"}}, contactName = "", contactColorId = Just 1, contactHandle = Just ":", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_17 :: SearchResult Contact
testObject_SearchResult_20Contact_user_17 = SearchResult {searchFound = 3, searchReturned = -2, searchTook = -1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "o.1p059.tl2667"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "94v69e.spw"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "u.4h38k-9lw7m.m6.v.q6w54v1h.umi"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "4dj.kyl.n-r-8s"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_18 :: SearchResult Contact
testObject_SearchResult_20Contact_user_18 = SearchResult {searchFound = -6, searchReturned = -1, searchTook = -7, searchResults = []}
testObject_SearchResult_20Contact_user_19 :: SearchResult Contact
testObject_SearchResult_20Contact_user_19 = SearchResult {searchFound = -4, searchReturned = -7, searchTook = -1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "29-k.yq-86b"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "066057.lk"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_20 :: SearchResult Contact
testObject_SearchResult_20Contact_user_20 = SearchResult {searchFound = -1, searchReturned = -5, searchTook = -3, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "sg.o0oz4hs1.xe-38-73f7"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "441-fy.7.cc.d73m2-f8h--17"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))}]}
