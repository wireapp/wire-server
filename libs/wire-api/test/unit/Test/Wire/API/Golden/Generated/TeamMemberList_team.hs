{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamMemberList_team where

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
import Data.LegalHold
import Wire.API.Conversation.Role
import Wire.API.Event.Team
import Wire.API.Provider.Service
import Wire.API.Team
import Wire.API.Team.Conversation
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold.External
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User.Client.Prekey
import Wire.API.User.Identity
import Wire.API.User.Profile
testObject_TeamMemberList_1 :: TeamMemberList
testObject_TeamMemberList_1 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-10T13:01:57.021Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-08T19:00:52.159Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListTruncated))
testObject_TeamMemberList_2 :: TeamMemberList
testObject_TeamMemberList_2 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:40:54.301Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T21:57:37.101Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T11:32:25.471Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T20:17:21.207Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}]) (ListTruncated))
testObject_TeamMemberList_3 :: TeamMemberList
testObject_TeamMemberList_3 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T17:08:53.228Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T22:10:59.852Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T06:26:06.653Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T22:32:25.917Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T14:07:35.004Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T15:01:34.298Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:41:58.776Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_4 :: TeamMemberList
testObject_TeamMemberList_4 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:51:46.118Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T16:45:44.994Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:26:30.249Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_5 :: TeamMemberList
testObject_TeamMemberList_5 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T21:32:09.273Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T01:13:54.369Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T03:24:48.783Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T18:13:49.654Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T05:12:13.833Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T23:43:59.405Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T19:14:51.782Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}]) (ListTruncated))
