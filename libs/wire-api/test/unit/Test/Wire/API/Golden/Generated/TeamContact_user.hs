{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamContact_user where

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
testObject_TeamContact_1 :: TeamContact
testObject_TeamContact_1 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), teamContactName = "5\a", teamContactColorId = Just (-1), teamContactHandle = Just ">", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), teamContactEmail = Just (Email {emailLocal = "\1112339", emailDomain = "\10417"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T07:04:05.607Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleMember}
testObject_TeamContact_2 :: TeamContact
testObject_TeamContact_2 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), teamContactName = "};A", teamContactColorId = Just (-2), teamContactHandle = Just "\135678", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T21:56:22.918Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "!2", teamContactRole = Just RoleMember}
testObject_TeamContact_3 :: TeamContact
testObject_TeamContact_3 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), teamContactName = "\38919\ACK1", teamContactColorId = Just 2, teamContactHandle = Just "$", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T10:07:46.572Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "j)t", teamContactRole = Nothing}
testObject_TeamContact_4 :: TeamContact
testObject_TeamContact_4 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), teamContactName = "", teamContactColorId = Just (-3), teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T19:20:26.374Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "Y", teamContactRole = Nothing}
testObject_TeamContact_5 :: TeamContact
testObject_TeamContact_5 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000002"))), teamContactName = "\1067014", teamContactColorId = Nothing, teamContactHandle = Just "R", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "y", teamContactRole = Just RoleAdmin}
