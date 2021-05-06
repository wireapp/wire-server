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
testObject_TeamContact_user_1 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000001"))), teamContactName = "8", teamContactColorId = Just (-2), teamContactHandle = Just "\35066", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T17:25:37.908Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_2 :: TeamContact
testObject_TeamContact_user_2 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), teamContactName = "\r\100269", teamContactColorId = Just (-2), teamContactHandle = Just "\1000293O", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T14:46:30.656Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_3 :: TeamContact
testObject_TeamContact_user_3 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), teamContactName = "e9", teamContactColorId = Just 2, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "m", emailDomain = "\ESC\1023008W"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "\\\1042885", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_4 :: TeamContact
testObject_TeamContact_user_4 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), teamContactName = "\DC3$\ETB", teamContactColorId = Just 2, teamContactHandle = Just "k\t\99607", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\t\DC3\r", emailDomain = "\1025286\RS="}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T17:51:25.278Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
testObject_TeamContact_user_5 :: TeamContact
testObject_TeamContact_user_5 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactName = "T", teamContactColorId = Just 3, teamContactHandle = Just "<", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T23:07:42.774Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "\134660k\DC4", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_6 :: TeamContact
testObject_TeamContact_user_6 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000000"))), teamContactName = "\EOTA", teamContactColorId = Just (-3), teamContactHandle = Just "~\ETX\1018616", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T08:17:14.909Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "P", teamContactRole = Just RoleMember}
testObject_TeamContact_user_7 :: TeamContact
testObject_TeamContact_user_7 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), teamContactName = "", teamContactColorId = Just 2, teamContactHandle = Just "v8\1056677", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000002"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "d"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-10T11:00:24.893Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleOwner}
testObject_TeamContact_user_8 :: TeamContact
testObject_TeamContact_user_8 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000002"))), teamContactName = "\n", teamContactColorId = Just (-3), teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000002"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T15:33:50.438Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_9 :: TeamContact
testObject_TeamContact_user_9 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), teamContactName = "", teamContactColorId = Just (-1), teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), teamContactEmail = Just (Email {emailLocal = "o\154394", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\121006", teamContactRole = Nothing}
testObject_TeamContact_user_10 :: TeamContact
testObject_TeamContact_user_10 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000000"))), teamContactName = "\DC3", teamContactColorId = Nothing, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T10:21:30.240Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "\DC3t", teamContactRole = Just RoleMember}
testObject_TeamContact_user_11 :: TeamContact
testObject_TeamContact_user_11 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 3, teamContactHandle = Just ".\162957", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), teamContactEmail = Just (Email {emailLocal = "C{", emailDomain = "x2"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T00:33:44.826Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "NWJ", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_12 :: TeamContact
testObject_TeamContact_user_12 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000002"))), teamContactName = "^\1084367", teamContactColorId = Just 1, teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000001"))), teamContactEmail = Just (Email {emailLocal = "\132227", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T11:57:39.245Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "\SUB", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_13 :: TeamContact
testObject_TeamContact_user_13 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))), teamContactName = "$n\r", teamContactColorId = Just 1, teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))), teamContactEmail = Just (Email {emailLocal = "D%", emailDomain = ";I"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T02:04:02.386Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_14 :: TeamContact
testObject_TeamContact_user_14 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), teamContactName = "\NAK\1020558\"", teamContactColorId = Nothing, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "5", emailDomain = "\165643x\131647"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T20:50:52.491Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "$", teamContactRole = Just RoleMember}
testObject_TeamContact_user_15 :: TeamContact
testObject_TeamContact_user_15 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), teamContactName = "\121278\1056973_", teamContactColorId = Just 3, teamContactHandle = Just "U\1030482D", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T20:52:35.467Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleMember}
testObject_TeamContact_user_16 :: TeamContact
testObject_TeamContact_user_16 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000001"))), teamContactName = "\FS(*", teamContactColorId = Just 3, teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))), teamContactEmail = Just (Email {emailLocal = "\17720\&1", emailDomain = "b\DC3"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T18:26:27.865Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
testObject_TeamContact_user_17 :: TeamContact
testObject_TeamContact_user_17 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000100000001"))), teamContactName = "\141699\1028842", teamContactColorId = Just 0, teamContactHandle = Just "q1", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "O>>"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "4", teamContactRole = Just RoleMember}
testObject_TeamContact_user_18 :: TeamContact
testObject_TeamContact_user_18 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), teamContactName = "\53879\CAN", teamContactColorId = Just 1, teamContactHandle = Nothing, teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\11609\DEL", emailDomain = "]"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_19 :: TeamContact
testObject_TeamContact_user_19 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000002"))), teamContactName = "\1027314\1052893\FS", teamContactColorId = Just (-1), teamContactHandle = Just "|", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T14:10:41.235Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_20 :: TeamContact
testObject_TeamContact_user_20 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000001"))), teamContactName = "y\aU", teamContactColorId = Just (-1), teamContactHandle = Just "+\186129\t", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), teamContactEmail = Just (Email {emailLocal = "P\1091367k", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T10:46:57.003Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
