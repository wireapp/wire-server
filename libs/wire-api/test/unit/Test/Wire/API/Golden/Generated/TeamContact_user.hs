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
testObject_TeamContact_user_1 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), teamContactName = "", teamContactColorId = Nothing, teamContactHandle = Just "\1009081", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = " "}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember}
testObject_TeamContact_user_2 :: TeamContact
testObject_TeamContact_user_2 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), teamContactName = "\984740", teamContactColorId = Just 2, teamContactHandle = Just "U", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T14:02:12.070Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
testObject_TeamContact_user_3 :: TeamContact
testObject_TeamContact_user_3 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 0, teamContactHandle = Just ";\1093880\1094709", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "\37180R", emailDomain = "\1026273"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember}
testObject_TeamContact_user_4 :: TeamContact
testObject_TeamContact_user_4 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactName = "9x", teamContactColorId = Just (-1), teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\SOH\1067939", emailDomain = "P:"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember}
testObject_TeamContact_user_5 :: TeamContact
testObject_TeamContact_user_5 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), teamContactName = "", teamContactColorId = Nothing, teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "a"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T05:26:33.039Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\1069977", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_6 :: TeamContact
testObject_TeamContact_user_6 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), teamContactName = "w\1080984", teamContactColorId = Just 1, teamContactHandle = Just "\61138\&9", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "{\RS\1060259", emailDomain = "!"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T19:52:03.220Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "\DEL\183957\"", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_7 :: TeamContact
testObject_TeamContact_user_7 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000000"))), teamContactName = "\r", teamContactColorId = Just 1, teamContactHandle = Just "\ETBe", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_8 :: TeamContact
testObject_TeamContact_user_8 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), teamContactName = "G}3", teamContactColorId = Just 1, teamContactHandle = Just "O;", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "S", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_9 :: TeamContact
testObject_TeamContact_user_9 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), teamContactName = "hj\DEL", teamContactColorId = Just 3, teamContactHandle = Just "SiF", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "$q\37904", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T04:04:35.736Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_10 :: TeamContact
testObject_TeamContact_user_10 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), teamContactName = "-\a", teamContactColorId = Just 1, teamContactHandle = Nothing, teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "n\134085", emailDomain = ",\165854"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T22:16:47.128Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "\167249\NAK", teamContactRole = Just RoleMember}
testObject_TeamContact_user_11 :: TeamContact
testObject_TeamContact_user_11 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), teamContactName = "", teamContactColorId = Nothing, teamContactHandle = Just "5", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T16:08:18.340Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_12 :: TeamContact
testObject_TeamContact_user_12 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000200000001"))), teamContactName = "", teamContactColorId = Just (-2), teamContactHandle = Just "\US", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = "^\153472"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T13:11:01.496Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "2", teamContactRole = Nothing}
testObject_TeamContact_user_13 :: TeamContact
testObject_TeamContact_user_13 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), teamContactName = "\1038800", teamContactColorId = Nothing, teamContactHandle = Just "\7137", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), teamContactEmail = Nothing, teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleMember}
testObject_TeamContact_user_14 :: TeamContact
testObject_TeamContact_user_14 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 3, teamContactHandle = Nothing, teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T09:01:43.316Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "U\r", teamContactRole = Just RoleMember}
testObject_TeamContact_user_15 :: TeamContact
testObject_TeamContact_user_15 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), teamContactName = "\vh\1007458", teamContactColorId = Just 3, teamContactHandle = Just "\ENQ", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "%E", emailDomain = "\1096523\\'"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T21:20:09.859Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "U/", teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_16 :: TeamContact
testObject_TeamContact_user_16 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), teamContactName = "", teamContactColorId = Just 3, teamContactHandle = Just "M\1076017\135625", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\t", emailDomain = "'D%"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T13:17:03.440Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "B", teamContactRole = Nothing}
testObject_TeamContact_user_17 :: TeamContact
testObject_TeamContact_user_17 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), teamContactName = "Y\1080894s", teamContactColorId = Just 2, teamContactHandle = Just "G\183265\1079537", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "'\37212"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleOwner}
testObject_TeamContact_user_18 :: TeamContact
testObject_TeamContact_user_18 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), teamContactName = "g\1052985", teamContactColorId = Just 0, teamContactHandle = Just "\t", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "&\US;"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T15:44:42.985Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\EOT", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_19 :: TeamContact
testObject_TeamContact_user_19 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000000"))), teamContactName = "|\166594r", teamContactColorId = Just 3, teamContactHandle = Nothing, teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "Z", emailDomain = "\SIgS"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T13:26:19.292Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_20 :: TeamContact
testObject_TeamContact_user_20 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), teamContactName = "\ACK", teamContactColorId = Just 0, teamContactHandle = Just "o\n", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000001"))), teamContactEmail = Just (Email {emailLocal = "2", emailDomain = "t"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T21:21:45.507Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "q", teamContactRole = Just RoleExternalPartner}
