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
testObject_SearchResult_20Contact_user_1 = SearchResult {searchFound = -6, searchReturned = -2, searchTook = 4, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "b.t10.r8r"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "n7-s.ay"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), qDomain = Domain {_domainText = "qf157-o7-9704r.l.u0j3h1"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "4boh-83.d"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "7p.9gd97.w4p3i4"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "4.neg"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "3csk.op0p6"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "9e.hp"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "u4ph.u4-d5"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))}]}
testObject_SearchResult_20Contact_user_2 :: SearchResult Contact
testObject_SearchResult_20Contact_user_2 = SearchResult {searchFound = 5, searchReturned = 2, searchTook = 0, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "1-k.jb-p"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "e8y.w8t8-d"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "fw0w.v.ity501"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "8b4.u91-5"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Nothing}]}
testObject_SearchResult_20Contact_user_3 :: SearchResult Contact
testObject_SearchResult_20Contact_user_3 = SearchResult {searchFound = -4, searchReturned = 2, searchTook = -6, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "39.ov.ktcw"}}, contactName = "t", contactColorId = Just 0, contactHandle = Just "}", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_user_4 :: SearchResult Contact
testObject_SearchResult_20Contact_user_4 = SearchResult {searchFound = 0, searchReturned = -1, searchTook = -3, searchResults = []}
testObject_SearchResult_20Contact_user_5 :: SearchResult Contact
testObject_SearchResult_20Contact_user_5 = SearchResult {searchFound = -3, searchReturned = -3, searchTook = 1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "09.31-z0.57.h-b6ld.v8.y7vr4-1o60w"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "rx0zn.d--89.7rizlx0.95lz-y4.nrgzz68-v6-0.66l.w30c"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "8.d3s"}}, contactName = "", contactColorId = Just 0, contactHandle = Nothing, contactTeam = Nothing}]}
