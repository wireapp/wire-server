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
testObject_TeamMemberList_team_1 :: TeamMemberList
testObject_TeamMemberList_team_1 = (newTeamMemberList ([]) (ListTruncated))
testObject_TeamMemberList_team_2 :: TeamMemberList
testObject_TeamMemberList_team_2 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T00:25:39.619Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T07:17:24.823Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T06:13:00.597Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T19:22:12.323Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListComplete))
testObject_TeamMemberList_team_3 :: TeamMemberList
testObject_TeamMemberList_team_3 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T16:03:41.878Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T14:42:07.490Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:19:21.509Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T21:13:04.450Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:20:28.065Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T13:42:10.897Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T18:17:43.105Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListComplete))
testObject_TeamMemberList_team_4 :: TeamMemberList
testObject_TeamMemberList_team_4 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T11:33:29.644Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T20:54:30.726Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T12:57:24.896Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T15:59:30.227Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T22:36:15.232Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T00:55:10.864Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T03:40:19.802Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T06:05:28.849Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T10:18:58.067Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T02:34:14.175Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T16:48:07.454Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T18:15:57.859Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T06:47:48.538Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T03:04:01.445Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T12:09:36.182Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T01:56:36.674Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T02:08:42.652Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T00:33:33.058Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListComplete))
testObject_TeamMemberList_team_5 :: TeamMemberList
testObject_TeamMemberList_team_5 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-10T09:34:37.521Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T07:57:32.849Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [AddTeamMember], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-08T13:13:36.363Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListComplete))
