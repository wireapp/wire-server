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
testObject_TeamContact_user_1 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000000000001"))), teamContactName = "+", teamContactColorId = Just 1, teamContactHandle = Just "\1083516m", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000000"))), teamContactEmail = Just (Email {emailLocal = "Y5", emailDomain = "J\68464~"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-12T07:33:27.243Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "F1", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_2 :: TeamContact
testObject_TeamContact_user_2 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactName = "xv+", teamContactColorId = Just (-1), teamContactHandle = Just "d", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "^"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T20:51:27.043Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
testObject_TeamContact_user_3 :: TeamContact
testObject_TeamContact_user_3 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000002"))), teamContactName = "\b+\STX", teamContactColorId = Just (-3), teamContactHandle = Just "\RS\45151", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T16:54:48.316Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "A", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_4 :: TeamContact
testObject_TeamContact_user_4 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000001"))), teamContactName = "", teamContactColorId = Just (-2), teamContactHandle = Just "c", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), teamContactEmail = Just (Email {emailLocal = "^\FS", emailDomain = "-\SI\f"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T02:43:45.292Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\1075741v+", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_5 :: TeamContact
testObject_TeamContact_user_5 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactName = "ot", teamContactColorId = Just (-2), teamContactHandle = Just "\GS", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000002"))), teamContactEmail = Just (Email {emailLocal = "\STX\1087597$", emailDomain = "\EOTy"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T21:38:42.110Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_6 :: TeamContact
testObject_TeamContact_user_6 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), teamContactName = "^\GSm", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:52:35.778Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_7 :: TeamContact
testObject_TeamContact_user_7 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000001"))), teamContactName = "n", teamContactColorId = Just (-1), teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000000"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T15:43:43.923Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "\DC4", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_8 :: TeamContact
testObject_TeamContact_user_8 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000002"))), teamContactName = "\1022903", teamContactColorId = Just 0, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "\1045133\ETX"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T21:17:33.423Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleExternalPartner}
testObject_TeamContact_user_9 :: TeamContact
testObject_TeamContact_user_9 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000200000001"))), teamContactName = "|", teamContactColorId = Just (-3), teamContactHandle = Nothing, teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), teamContactEmail = Just (Email {emailLocal = "\1049691\EOT", emailDomain = "\1036002\139268\DC1"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T14:10:48.008Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_10 :: TeamContact
testObject_TeamContact_user_10 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000002"))), teamContactName = "", teamContactColorId = Just 2, teamContactHandle = Just "\DLENO", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = "]\v"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-06T20:37:42.784Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "", teamContactRole = Nothing}
testObject_TeamContact_user_11 :: TeamContact
testObject_TeamContact_user_11 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000002"))), teamContactName = "c", teamContactColorId = Just (-3), teamContactHandle = Just "fR\44835", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000002"))), teamContactEmail = Just (Email {emailLocal = "\ETB\988995", emailDomain = "\159009"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T15:31:41.135Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember}
testObject_TeamContact_user_12 :: TeamContact
testObject_TeamContact_user_12 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000100000000"))), teamContactName = "", teamContactColorId = Just 3, teamContactHandle = Just "P3", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "j56", emailDomain = "w\SUB"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_13 :: TeamContact
testObject_TeamContact_user_13 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000002"))), teamContactName = "\n", teamContactColorId = Nothing, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), teamContactEmail = Nothing, teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-08T01:27:53.644Z")), teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "", teamContactRole = Just RoleMember}
testObject_TeamContact_user_14 :: TeamContact
testObject_TeamContact_user_14 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), teamContactName = "q\1035126\DEL", teamContactColorId = Nothing, teamContactHandle = Just "", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000200000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleOwner}
testObject_TeamContact_user_15 :: TeamContact
testObject_TeamContact_user_15 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), teamContactName = "\7216\160436\&8", teamContactColorId = Just 2, teamContactHandle = Just "o\184236", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "", emailDomain = "*"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-09T17:27:55.901Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\5164\73812\b", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_16 :: TeamContact
testObject_TeamContact_user_16 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000200000002"))), teamContactName = "\171558\a", teamContactColorId = Just 2, teamContactHandle = Just "", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "|\FS$", emailDomain = ""}), teamContactCreatedAt = Nothing, teamContactManagedBy = Nothing, teamContactSAMLIdp = Nothing, teamContactRole = Nothing}
testObject_TeamContact_user_17 :: TeamContact
testObject_TeamContact_user_17 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000000000002"))), teamContactName = "e\1056939\1082242", teamContactColorId = Just (-1), teamContactHandle = Just "E\33786", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\ENQ", emailDomain = "8S"}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-07T20:57:49.164Z")), teamContactManagedBy = Nothing, teamContactSAMLIdp = Just "\99275\68817R", teamContactRole = Just RoleOwner}
testObject_TeamContact_user_18 :: TeamContact
testObject_TeamContact_user_18 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), teamContactName = "LV", teamContactColorId = Just 3, teamContactHandle = Just ",", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000000"))), teamContactEmail = Just (Email {emailLocal = "k", emailDomain = "\1004825_"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByWire, teamContactSAMLIdp = Just "E", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_19 :: TeamContact
testObject_TeamContact_user_19 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), teamContactName = "#~", teamContactColorId = Just (-1), teamContactHandle = Just "&", teamContactTeam = Nothing, teamContactEmail = Just (Email {emailLocal = "\186707m", emailDomain = "\SI"}), teamContactCreatedAt = Nothing, teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Just "\nT", teamContactRole = Just RoleAdmin}
testObject_TeamContact_user_20 :: TeamContact
testObject_TeamContact_user_20 = TeamContact {teamContactUserId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000001"))), teamContactName = "\DC2\US", teamContactColorId = Just 3, teamContactHandle = Just "\1052227", teamContactTeam = Just (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000100000000"))), teamContactEmail = Just (Email {emailLocal = "", emailDomain = ""}), teamContactCreatedAt = Just (fromJust (readUTCTimeMillis "1864-05-11T05:35:22.709Z")), teamContactManagedBy = Just ManagedByScim, teamContactSAMLIdp = Nothing, teamContactRole = Just RoleAdmin}
