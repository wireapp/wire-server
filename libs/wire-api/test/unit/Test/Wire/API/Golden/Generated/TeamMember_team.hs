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
testObject_TeamMember_team_1 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0006-0000-000600000002"))), _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList [GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000400000004"))),(fromJust (readUTCTimeMillis "1864-05-13T08:21:06.955Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000500000005"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,SetTeamData], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000800000006"))),(fromJust (readUTCTimeMillis "1864-05-02T06:23:07.029Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000000000005"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000005"))),(fromJust (readUTCTimeMillis "1864-05-02T12:42:07.073Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000600000006"))), _permissions = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetTeamConversations]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000500000000"))),(fromJust (readUTCTimeMillis "1864-05-10T01:16:08.598Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000500000006"))), _permissions = Permissions {_self = fromList [GetBilling], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_6 :: TeamMember
testObject_TeamMember_team_6 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000500000002"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,GetBilling,SetBilling], _copy = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,SetBilling]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_7 :: TeamMember
testObject_TeamMember_team_7 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0002-0000-000400000004"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0007-0000-000100000006"))),(fromJust (readUTCTimeMillis "1864-05-09T01:21:40.405Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_8 :: TeamMember
testObject_TeamMember_team_8 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000800000004"))), _permissions = Permissions {_self = fromList [CreateConversation,SetBilling,SetTeamData,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000400000000"))),(fromJust (readUTCTimeMillis "1864-05-11T13:10:38.729Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_9 :: TeamMember
testObject_TeamMember_team_9 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000200000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000600000007"))),(fromJust (readUTCTimeMillis "1864-05-15T05:24:16.986Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_10 :: TeamMember
testObject_TeamMember_team_10 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000300000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000400000004"))),(fromJust (readUTCTimeMillis "1864-05-05T08:57:27.368Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_11 :: TeamMember
testObject_TeamMember_team_11 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000800000003"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetMemberPermissions,DeleteTeam], _copy = fromList [CreateConversation,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000800000001"))),(fromJust (readUTCTimeMillis "1864-05-14T20:26:48.019Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_12 :: TeamMember
testObject_TeamMember_team_12 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000600000007"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000700000005"))),(fromJust (readUTCTimeMillis "1864-05-09T12:48:06.968Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_13 :: TeamMember
testObject_TeamMember_team_13 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000008"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_14 :: TeamMember
testObject_TeamMember_team_14 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000300000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,SetBilling,GetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000000000000"))),(fromJust (readUTCTimeMillis "1864-05-16T01:53:01.312Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_15 :: TeamMember
testObject_TeamMember_team_15 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000007"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedModifyConvName,SetTeamData,GetMemberPermissions,DeleteTeam], _copy = fromList [CreateConversation,SetTeamData,GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000300000003"))),(fromJust (readUTCTimeMillis "1864-05-04T23:35:44.363Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_16 :: TeamMember
testObject_TeamMember_team_16 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000800000003"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,SetTeamData,GetMemberPermissions,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0002-0000-000000000008"))),(fromJust (readUTCTimeMillis "1864-05-10T19:54:20.251Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_17 :: TeamMember
testObject_TeamMember_team_17 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000700000005"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,GetMemberPermissions,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000400000001"))),(fromJust (readUTCTimeMillis "1864-05-08T04:54:34.382Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_18 :: TeamMember
testObject_TeamMember_team_18 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000200000008"))), _permissions = Permissions {_self = fromList [SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0007-0000-000800000002"))),(fromJust (readUTCTimeMillis "1864-05-10T06:56:07.118Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_19 :: TeamMember
testObject_TeamMember_team_19 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000700000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions], _copy = fromList [GetBilling,SetMemberPermissions]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_20 :: TeamMember
testObject_TeamMember_team_20 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000500000000"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,GetTeamConversations,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000100000004"))),(fromJust (readUTCTimeMillis "1864-05-14T11:48:16.202Z"))), _legalHoldStatus = UserLegalHoldEnabled}
