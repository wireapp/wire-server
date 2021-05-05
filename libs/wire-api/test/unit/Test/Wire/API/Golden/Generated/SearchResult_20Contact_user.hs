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
testObject_SearchResult_20Contact_1 :: SearchResult Contact
testObject_SearchResult_20Contact_1 = SearchResult {searchFound = 2, searchReturned = -3, searchTook = 4, searchResults = []}
testObject_SearchResult_20Contact_2 :: SearchResult Contact
testObject_SearchResult_20Contact_2 = SearchResult {searchFound = -4, searchReturned = 5, searchTook = -2, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "c30.23pk-n.v.f31x"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), qDomain = Domain {_domainText = "b70ac4.rur1iu.68t-u.843.m.k--a0e.9o.h3j.1-t.8.d9.d67"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), qDomain = Domain {_domainText = "ne.2.we--kq"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))}]}
testObject_SearchResult_20Contact_3 :: SearchResult Contact
testObject_SearchResult_20Contact_3 = SearchResult {searchFound = -3, searchReturned = 5, searchTook = -3, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "444.ml5-v"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), qDomain = Domain {_domainText = "j.i"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), qDomain = Domain {_domainText = "05w0q81.d.d"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), qDomain = Domain {_domainText = "5.o4"}}, contactName = "", contactColorId = Nothing, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000")))}]}
testObject_SearchResult_20Contact_4 :: SearchResult Contact
testObject_SearchResult_20Contact_4 = SearchResult {searchFound = -1, searchReturned = -6, searchTook = -3, searchResults = []}
testObject_SearchResult_20Contact_5 :: SearchResult Contact
testObject_SearchResult_20Contact_5 = SearchResult {searchFound = 4, searchReturned = -2, searchTook = -1, searchResults = [Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), qDomain = Domain {_domainText = "4c.91.ja6py--rr7"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000")))},Contact {contactQualifiedId = Qualified {qUnqualified = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), qDomain = Domain {_domainText = "0yb04fzi.a4o"}}, contactName = "", contactColorId = Just 0, contactHandle = Just "", contactTeam = Nothing}]}
