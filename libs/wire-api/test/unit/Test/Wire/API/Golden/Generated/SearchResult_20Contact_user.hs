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
testObject_SearchResult_20Contact_user_1 = SearchResult {searchFound = 3, searchReturned = -7, searchTook = -7, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "3g3j8-0399.x.656.i2"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "9.1.bu.f1nn3"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "8-01.7e.dh.d"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "e6l.uz"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "u1.k-201"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "r8c630k1.ls-b"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_2 :: SearchResult Contact
testObject_SearchResult_20Contact_user_2 = SearchResult {searchFound = 2, searchReturned = 5, searchTook = 5, searchResults = []}
testObject_SearchResult_20Contact_user_3 :: SearchResult Contact
testObject_SearchResult_20Contact_user_3 = SearchResult {searchFound = -3, searchReturned = -2, searchTook = -5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "97.1k.x-76p"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "6pk1lnj.c"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "23.v020"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "n9n-42p-7-n6w7-kf.7o97m.1d.b.pm9-6564.e9v94.f"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "24z.t"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "ln.d"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_4 :: SearchResult Contact
testObject_SearchResult_20Contact_user_4 = SearchResult {searchFound = 7, searchReturned = 6, searchTook = 5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "4qa-8.l-b4xt--4"}}, contactName = "", contactColorId = Just (-1), contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_5 :: SearchResult Contact
testObject_SearchResult_20Contact_user_5 = SearchResult {searchFound = 7, searchReturned = 0, searchTook = -1, searchResults = []}
testObject_SearchResult_20Contact_user_6 :: SearchResult Contact
testObject_SearchResult_20Contact_user_6 = SearchResult {searchFound = -1, searchReturned = -1, searchTook = 1, searchResults = []}
testObject_SearchResult_20Contact_user_7 :: SearchResult Contact
testObject_SearchResult_20Contact_user_7 = SearchResult {searchFound = 1, searchReturned = 3, searchTook = 6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "q-21x0c12g.tpu8.4-as1.m-i.xn-773kcn.n3v46-9csr"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "g622.hd2f"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "5wpz-r4fc.8-84.a8e77h5.p"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_8 :: SearchResult Contact
testObject_SearchResult_20Contact_user_8 = SearchResult {searchFound = -4, searchReturned = 7, searchTook = -6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "pc97.u13g"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "qd.b-r769"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_9 :: SearchResult Contact
testObject_SearchResult_20Contact_user_9 = SearchResult {searchFound = 1, searchReturned = 5, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "rf-89.z3637608"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "r82.t-32no4w"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}]}
testObject_SearchResult_20Contact_user_10 :: SearchResult Contact
testObject_SearchResult_20Contact_user_10 = SearchResult {searchFound = 1, searchReturned = -3, searchTook = 1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "sji-9.vr.jk"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "ex.s260"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "1f-p.s"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "04--43.d2hvm"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "fr.yuom"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "b0029j8.s92h.4j.k.28my.si--7-23n"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "ln2.u.g-84s--1-v.7u-2i2.cp"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "t8.od-3.r08.k390m-02.b"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "2-27-a30i.p-x12.j34z1tip"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "jo6320.6681n.93.1-kum.9.54.i65p68--y9.ppr"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "343.1h44.u"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "j-6.d-w-ou4.3-7-2-i3.j5p35ip9--x9"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "76m2of76x9.2zdh.r6q7"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "9.4x26x8.gx-1"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "l628.e8n"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_11 :: SearchResult Contact
testObject_SearchResult_20Contact_user_11 = SearchResult {searchFound = 6, searchReturned = -3, searchTook = 5, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "fv3.j4w5"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "42f.rb8w9"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "xjr.4u5.i7-t03.43.k0p.uix2"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "rez-12d.j"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001")))}]}
testObject_SearchResult_20Contact_user_12 :: SearchResult Contact
testObject_SearchResult_20Contact_user_12 = SearchResult {searchFound = 7, searchReturned = 0, searchTook = 4, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "6--j-d7.ay2-0.ij43"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "7dx1.3.b"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "r87.ql"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))}]}
testObject_SearchResult_20Contact_user_13 :: SearchResult Contact
testObject_SearchResult_20Contact_user_13 = SearchResult {searchFound = 3, searchReturned = -6, searchTook = 4, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "psa.f--z.d810"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "t.8-pbgjc8m.of7"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_14 :: SearchResult Contact
testObject_SearchResult_20Contact_user_14 = SearchResult {searchFound = -2, searchReturned = 2, searchTook = -1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "7-3.z-9z"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "0.666n.d4umx26"}}, contactName = "", contactColorId = Nothing, contactHandle = Nothing, contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_15 :: SearchResult Contact
testObject_SearchResult_20Contact_user_15 = SearchResult {searchFound = 3, searchReturned = 3, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "811.o6-e3r"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "g6.fyo"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "0.qtm2ll"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "313.x-3b7-5"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "au.wtgk"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "32-9whch.rve6-4"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}]}
testObject_SearchResult_20Contact_user_16 :: SearchResult Contact
testObject_SearchResult_20Contact_user_16 = SearchResult {searchFound = 1, searchReturned = -5, searchTook = -7, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "9k0q.9f4q.p-5a"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "6h-54o.8--6.2-s.m9"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_17 :: SearchResult Contact
testObject_SearchResult_20Contact_user_17 = SearchResult {searchFound = 1, searchReturned = -3, searchTook = 3, searchResults = []}
testObject_SearchResult_20Contact_user_18 :: SearchResult Contact
testObject_SearchResult_20Contact_user_18 = SearchResult {searchFound = 0, searchReturned = -1, searchTook = -6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "6--t2w.q68j-hy4ql"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "nf87.zm5"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "wkpq--b.zm-a"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "0bpz.mlu"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))}]}
testObject_SearchResult_20Contact_user_19 :: SearchResult Contact
testObject_SearchResult_20Contact_user_19 = SearchResult {searchFound = 5, searchReturned = 6, searchTook = -6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "j11h2a87g13.31t.v.h3-9.yx-8"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "q50s7913-u.3s5.e.w6k"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "y35vek-371.0.yzhl"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "69a1-7.p.z8951"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_20 :: SearchResult Contact
testObject_SearchResult_20Contact_user_20 = SearchResult {searchFound = -2, searchReturned = -3, searchTook = 1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "km79v.r4.m-b"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "f-3-5hvp.q2x-1o"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
