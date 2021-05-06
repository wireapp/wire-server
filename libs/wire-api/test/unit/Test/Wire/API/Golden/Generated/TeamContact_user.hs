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
testObject_TeamContact_user_1 :: TeamContact
testObject_TeamContact_user_1 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), teamContactName = "qb\f", teamContactColorId = Just 3, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T05:13:38.126Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "Er", teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_2 :: TeamContact
testObject_TeamContact_user_2 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), teamContactName = "\FS^\SUB", teamContactColorId = Just (-3), teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), teamContactEmail = Just (Email {emailLocal = "\NAK\41189", emailDomain = "9\ETBz"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "O\DC4/", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_3 :: TeamContact
testObject_TeamContact_user_3 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), teamContactName = "2\190386", teamContactColorId = Nothing, teamContactHandle = Just "O_", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T02:36:13.643Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "\11536@", teamContactRole = Just RoleMember}
testObject_TeamContact_user_4 :: TeamContact
testObject_TeamContact_user_4 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), teamContactName = ".\SOH\FS", teamContactColorId = Just (-1), teamContactHandle = Just "\\7v", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T20:00:29.422Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "YJ", teamContactRole = Nothing}
testObject_TeamContact_user_5 :: TeamContact
testObject_TeamContact_user_5 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), teamContactName = "B", teamContactColorId = Just (-2), teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), teamContactEmail = Just (Email {emailLocal = "\1014585", emailDomain = "\1052389"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T18:33:18.005Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "g", teamContactRole = Just RoleAdmin}
