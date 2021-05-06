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
testObject_TeamMemberList_team_1 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:30:34.944Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T13:45:31.048Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T23:50:40.670Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T04:38:05.642Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
testObject_TeamMemberList_team_2 :: TeamMemberList
testObject_TeamMemberList_team_2 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-08T13:15:46.635Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_team_3 :: TeamMemberList
testObject_TeamMemberList_team_3 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T11:49:32.863Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T06:44:36.917Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T20:40:42.919Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T02:58:31.577Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T03:49:37.678Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T21:55:54.653Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T16:43:01.804Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T19:05:02.697Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T08:19:45.301Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_team_4 :: TeamMemberList
testObject_TeamMemberList_team_4 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T08:59:04.855Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T22:11:14.936Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T10:44:38.385Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T09:40:10.618Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T03:18:24.739Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T19:39:35.357Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
testObject_TeamMemberList_team_5 :: TeamMemberList
testObject_TeamMemberList_team_5 = (newTeamMemberList ([]) (ListTruncated))
testObject_TeamMemberList_team_6 :: TeamMemberList
testObject_TeamMemberList_team_6 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-08T00:40:47.138Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [RemoveTeamMember], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
testObject_TeamMemberList_team_7 :: TeamMemberList
testObject_TeamMemberList_team_7 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000002"))), _permissions = Permissions {_self = fromList [AddTeamMember,GetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000200000002"))),(fromJust (readUTCTimeMillis "1864-05-08T06:34:19.375Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_team_8 :: TeamMemberList
testObject_TeamMemberList_team_8 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T13:34:17.207Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:19:24.318Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T09:25:39.250Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T18:53:24.297Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListTruncated))
testObject_TeamMemberList_team_9 :: TeamMemberList
testObject_TeamMemberList_team_9 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),(fromJust (readUTCTimeMillis "1864-05-08T22:21:34.435Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_team_10 :: TeamMemberList
testObject_TeamMemberList_team_10 = (newTeamMemberList ([]) (ListComplete))
testObject_TeamMemberList_team_11 :: TeamMemberList
testObject_TeamMemberList_team_11 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000200000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
testObject_TeamMemberList_team_12 :: TeamMemberList
testObject_TeamMemberList_team_12 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T22:28:01.034Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-10T03:50:53.244Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
testObject_TeamMemberList_team_13 :: TeamMemberList
testObject_TeamMemberList_team_13 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000200000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,GetTeamConversations], _copy = fromList [GetTeamConversations]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}]) (ListComplete))
testObject_TeamMemberList_team_14 :: TeamMemberList
testObject_TeamMemberList_team_14 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T03:51:30.111Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-10T02:22:18.067Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListTruncated))
testObject_TeamMemberList_team_15 :: TeamMemberList
testObject_TeamMemberList_team_15 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:29:12.965Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T02:30:23.985Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T12:38:25.089Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}]) (ListTruncated))
testObject_TeamMemberList_team_16 :: TeamMemberList
testObject_TeamMemberList_team_16 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T08:01:31.660Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T02:55:16.046Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T12:09:25.267Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T14:25:47.190Z"))), _legalHoldStatus = UserLegalHoldEnabled}]) (ListComplete))
testObject_TeamMemberList_team_17 :: TeamMemberList
testObject_TeamMemberList_team_17 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T23:43:27.140Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-09T22:47:27.498Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T12:23:44.425Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T00:38:17.344Z"))), _legalHoldStatus = UserLegalHoldDisabled}]) (ListComplete))
testObject_TeamMemberList_team_18 :: TeamMemberList
testObject_TeamMemberList_team_18 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))), _permissions = Permissions {_self = fromList [GetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-08T13:44:34.635Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),(fromJust (readUTCTimeMillis "1864-05-08T04:34:33.112Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [RemoveTeamMember], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}]) (ListComplete))
testObject_TeamMemberList_team_19 :: TeamMemberList
testObject_TeamMemberList_team_19 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-10T06:53:41.627Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-08T01:30:07.499Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [SetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}]) (ListComplete))
testObject_TeamMemberList_team_20 :: TeamMemberList
testObject_TeamMemberList_team_20 = (newTeamMemberList ([TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T19:43:03.209Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-09T17:52:37.051Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T12:46:33.304Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T14:52:50.626Z"))), _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T16:28:10.781Z"))), _legalHoldStatus = UserLegalHoldEnabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-09T14:25:17.068Z"))), _legalHoldStatus = UserLegalHoldDisabled},TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-09T04:31:33.155Z"))), _legalHoldStatus = UserLegalHoldPending}]) (ListTruncated))
