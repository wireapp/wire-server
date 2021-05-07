{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.NewTeamMember_team where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.LegalHold
  ( UserLegalHoldStatus
      ( UserLegalHoldDisabled,
        UserLegalHoldEnabled,
        UserLegalHoldPending
      ),
  )
import qualified Data.UUID as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Team.Member
  ( NewTeamMember,
    TeamMember
      ( TeamMember,
        _invitation,
        _legalHoldStatus,
        _permissions,
        _userId
      ),
    newNewTeamMember,
  )
import Wire.API.Team.Permission
  ( Perm
      ( AddTeamMember,
        CreateConversation,
        DeleteTeam,
        DoNotUseDeprecatedAddRemoveConvMember,
        DoNotUseDeprecatedDeleteConversation,
        DoNotUseDeprecatedModifyConvName,
        GetBilling,
        GetMemberPermissions,
        GetTeamConversations,
        RemoveTeamMember,
        SetBilling,
        SetMemberPermissions,
        SetTeamData
      ),
    Permissions (Permissions, _copy, _self),
  )

testObject_NewTeamMember_team_1 :: NewTeamMember
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000200000002"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000004"))), (fromJust (readUTCTimeMillis "1864-05-04T12:59:54.182Z"))), _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000003"))), _permissions = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedDeleteConversation, DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000500000008"))), (fromJust (readUTCTimeMillis "1864-05-16T00:49:15.576Z"))), _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000700000005"))), _permissions = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, RemoveTeamMember, GetBilling, DeleteTeam], _copy = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, GetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000500000002"))), (fromJust (readUTCTimeMillis "1864-05-08T07:57:50.660Z"))), _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000700000005"))), _permissions = Permissions {_self = fromList [CreateConversation, AddTeamMember, SetTeamData], _copy = fromList [CreateConversation, SetTeamData]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002"))), _permissions = Permissions {_self = fromList [AddTeamMember, SetBilling, GetTeamConversations], _copy = fromList [AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000600000006"))), (fromJust (readUTCTimeMillis "1864-05-12T23:29:05.832Z"))), _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000400000003"))), _permissions = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, GetBilling, SetTeamData, SetMemberPermissions], _copy = fromList [CreateConversation, GetBilling]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000800000003"))), (fromJust (readUTCTimeMillis "1864-05-16T01:49:44.477Z"))), _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000500000005"))), _permissions = Permissions {_self = fromList [AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetTeamConversations, DeleteTeam], _copy = fromList [AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000007"))), (fromJust (readUTCTimeMillis "1864-05-08T14:17:14.531Z"))), _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000200000003"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedModifyConvName]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000200000002"))), (fromJust (readUTCTimeMillis "1864-05-16T06:33:31.445Z"))), _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000300000004"))), _permissions = Permissions {_self = fromList [SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000700000000"))), (fromJust (readUTCTimeMillis "1864-05-08T10:27:23.240Z"))), _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000600000003"))), _permissions = Permissions {_self = fromList [GetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000600000008"))), (fromJust (readUTCTimeMillis "1864-05-15T10:49:54.418Z"))), _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000000000002"))), _permissions = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedModifyConvName, SetTeamData], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000002"))), (fromJust (readUTCTimeMillis "1864-05-14T12:23:51.061Z"))), _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000007"))), _permissions = Permissions {_self = fromList [SetBilling, SetTeamData, GetTeamConversations], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))

testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000600000001"))), _permissions = Permissions {_self = fromList [AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetTeamData, GetTeamConversations], _copy = fromList [AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, GetTeamConversations]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000500000004"))), _permissions = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, DoNotUseDeprecatedModifyConvName, GetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000000000003"))), (fromJust (readUTCTimeMillis "1864-05-16T00:23:45.641Z"))), _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000800000007"))), _permissions = Permissions {_self = fromList [RemoveTeamMember, GetMemberPermissions, DeleteTeam], _copy = fromList [RemoveTeamMember, GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000300000006"))), (fromJust (readUTCTimeMillis "1864-05-02T08:10:15.332Z"))), _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000300000005"))), _permissions = Permissions {_self = fromList [CreateConversation, RemoveTeamMember, GetBilling, GetTeamConversations, DeleteTeam], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000400000005"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000800000007"))), (fromJust (readUTCTimeMillis "1864-05-07T21:53:30.897Z"))), _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000000000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000002"))), (fromJust (readUTCTimeMillis "1864-05-11T12:32:01.417Z"))), _legalHoldStatus = UserLegalHoldEnabled}))

testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000100000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, RemoveTeamMember, SetBilling, SetMemberPermissions], _copy = fromList [DoNotUseDeprecatedDeleteConversation, SetBilling]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}))

testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000000000004"))), _permissions = Permissions {_self = fromList [AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, SetBilling, GetMemberPermissions, GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000400000008"))), (fromJust (readUTCTimeMillis "1864-05-05T07:36:25.213Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
