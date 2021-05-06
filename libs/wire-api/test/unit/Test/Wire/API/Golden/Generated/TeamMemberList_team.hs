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
testObject_TeamMemberList_team_1 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T05:52:37.213Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:51:23.804Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T02:10:10.874Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T02:14:36.854Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T13:53:34.089Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T13:56:07.913Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T18:07:30.817Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T08:07:03.497Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T03:55:52.940Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListComplete))
testObject_TeamMemberList_team_2 :: TeamMemberList
testObject_TeamMemberList_team_2 = (newTeamMemberList ([]) (ListComplete))
testObject_TeamMemberList_team_3 :: TeamMemberList
testObject_TeamMemberList_team_3 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-10T22:07:59.018Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListTruncated))
testObject_TeamMemberList_team_4 :: TeamMemberList
testObject_TeamMemberList_team_4 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T16:25:53.409Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T18:19:59.556Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T07:09:34.929Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListComplete))
testObject_TeamMemberList_team_5 :: TeamMemberList
testObject_TeamMemberList_team_5 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T20:56:19.910Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
