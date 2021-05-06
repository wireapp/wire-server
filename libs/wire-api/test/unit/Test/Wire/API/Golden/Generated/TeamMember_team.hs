{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamMember_team where

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
testObject_TeamMember_team_1 :: TeamMember
testObject_TeamMember_team_1 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000400000007"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,GetBilling,SetBilling,GetTeamConversations,DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000200000004"))),(fromJust (readUTCTimeMillis "1864-05-12T21:43:48.320Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000800000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,SetTeamData,GetMemberPermissions,DeleteTeam], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000100000006"))),(fromJust (readUTCTimeMillis "1864-05-14T05:47:44.019Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0005-0000-000500000002"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000500000005"))),(fromJust (readUTCTimeMillis "1864-05-13T06:54:34.163Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000700000007"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000400000007"))),(fromJust (readUTCTimeMillis "1864-05-03T23:56:43.182Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000200000004"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,SetTeamData,GetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,RemoveTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000000000008"))),(fromJust (readUTCTimeMillis "1864-05-04T10:12:30.386Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_6 :: TeamMember
testObject_TeamMember_team_6 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000500000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,GetBilling], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_7 :: TeamMember
testObject_TeamMember_team_7 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000000000003"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,GetTeamConversations], _copy = fromList [SetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000500000000"))),(fromJust (readUTCTimeMillis "1864-05-12T22:26:57.431Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_8 :: TeamMember
testObject_TeamMember_team_8 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000600000004"))), _permissions = Permissions {_self = fromList [AddTeamMember,GetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000400000005"))),(fromJust (readUTCTimeMillis "1864-05-16T16:04:38.390Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_9 :: TeamMember
testObject_TeamMember_team_9 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000400000001"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000700000001"))),(fromJust (readUTCTimeMillis "1864-05-13T08:27:05.775Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_10 :: TeamMember
testObject_TeamMember_team_10 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000600000004"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetBilling,GetMemberPermissions], _copy = fromList [RemoveTeamMember,GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000100000006"))),(fromJust (readUTCTimeMillis "1864-05-14T19:23:21.045Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_11 :: TeamMember
testObject_TeamMember_team_11 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000700000002"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0005-0000-000000000001"))),(fromJust (readUTCTimeMillis "1864-05-04T18:45:28.302Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_12 :: TeamMember
testObject_TeamMember_team_12 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0004-0000-000600000004"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetTeamData], _copy = fromList [CreateConversation]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000700000002"))),(fromJust (readUTCTimeMillis "1864-05-09T20:46:40.071Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_13 :: TeamMember
testObject_TeamMember_team_13 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000200000002"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetTeamData], _copy = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000500000002"))),(fromJust (readUTCTimeMillis "1864-05-02T05:12:27.454Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_14 :: TeamMember
testObject_TeamMember_team_14 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000300000004"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000700000003"))),(fromJust (readUTCTimeMillis "1864-05-03T10:14:02.195Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_15 :: TeamMember
testObject_TeamMember_team_15 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000003"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-09T23:28:30.416Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_16 :: TeamMember
testObject_TeamMember_team_16 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0008-0000-000800000002"))), _permissions = Permissions {_self = fromList [AddTeamMember,GetMemberPermissions], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000500000005"))),(fromJust (readUTCTimeMillis "1864-05-08T06:05:42.075Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_17 :: TeamMember
testObject_TeamMember_team_17 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000000000005"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetBilling,DeleteTeam], _copy = fromList [RemoveTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000500000002"))),(fromJust (readUTCTimeMillis "1864-05-03T00:17:44.712Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_18 :: TeamMember
testObject_TeamMember_team_18 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000500000006"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,GetMemberPermissions,DeleteTeam], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000500000007"))),(fromJust (readUTCTimeMillis "1864-05-05T00:37:15.707Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_19 :: TeamMember
testObject_TeamMember_team_19 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000500000000"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetMemberPermissions], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetMemberPermissions]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_20 :: TeamMember
testObject_TeamMember_team_20 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000200000008"))),(fromJust (readUTCTimeMillis "1864-05-08T02:54:21.139Z"))), _legalHoldStatus = UserLegalHoldEnabled}
