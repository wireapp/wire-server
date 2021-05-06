{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.SearchResult_20TeamContact_user where

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
testObject_SearchResult_20TeamContact_user_1 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_1 = SearchResult {searchFound = -4, searchReturned = 5, searchTook = 3, searchResults = []}
testObject_SearchResult_20TeamContact_user_2 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_2 = SearchResult {searchFound = 4, searchReturned = 7, searchTook = -7, searchResults = []}
testObject_SearchResult_20TeamContact_user_3 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_3 = SearchResult {searchFound = 5, searchReturned = -1, searchTook = -6, searchResults = [TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T06:16:17.984Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleExternalPartner},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Nothing, teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T13:56:32.733Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Nothing}]}
testObject_SearchResult_20TeamContact_user_4 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_4 = SearchResult {searchFound = 0, searchReturned = -6, searchTook = -2, searchResults = [TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T03:18:04.458Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleAdmin},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleAdmin},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T16:59:36.092Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleMember},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T16:22:50.852Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner}]}
testObject_SearchResult_20TeamContact_user_5 :: SearchResult TeamContact
testObject_SearchResult_20TeamContact_user_5 = SearchResult {searchFound = 2, searchReturned = 1, searchTook = 2, searchResults = [TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleAdmin},TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleExternalPartner}]}
