{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.NewTeamMember_team where
import Data.Id ( Id(Id) )
import Data.Json.Util ( readUTCTimeMillis )
import Imports ( Maybe(Just, Nothing), fromJust )
import qualified Data.UUID as UUID ( fromString )
import GHC.Exts ( IsList(fromList) )
import Data.LegalHold
    ( UserLegalHoldStatus(UserLegalHoldPending, UserLegalHoldEnabled,
                          UserLegalHoldDisabled) )
import Wire.API.Team.Member
    ( TeamMember(TeamMember, _userId, _permissions, _invitation,
                 _legalHoldStatus),
      newNewTeamMember,
      NewTeamMember )
import Wire.API.Team.Permission
    ( Permissions(Permissions, _self, _copy),
      Perm(GetTeamConversations, GetBilling, GetMemberPermissions,
           SetMemberPermissions, CreateConversation, DeleteTeam,
           DoNotUseDeprecatedDeleteConversation, AddTeamMember,
           DoNotUseDeprecatedAddRemoveConvMember,
           DoNotUseDeprecatedModifyConvName, SetBilling, SetTeamData) )

testObject_NewTeamMember_team_1 :: NewTeamMember
testObject_NewTeamMember_team_1 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000600000008"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetMemberPermissions,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000200000006"))),(fromJust (readUTCTimeMillis "1864-05-04T21:28:47.612Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000700000006"))), _permissions = Permissions {_self = fromList [GetBilling,SetBilling,DeleteTeam], _copy = fromList [GetBilling,DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000700000006"))),(fromJust (readUTCTimeMillis "1864-05-05T20:22:02.778Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000300000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling,SetBilling,GetMemberPermissions,DeleteTeam], _copy = fromList [GetBilling,GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000500000007"))),(fromJust (readUTCTimeMillis "1864-05-14T16:51:41.597Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000000000007"))), _permissions = Permissions {_self = fromList [SetMemberPermissions], _copy = fromList [SetMemberPermissions]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000200000002"))), _permissions = Permissions {_self = fromList [SetTeamData,GetMemberPermissions], _copy = fromList [SetTeamData]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-16T11:48:01.066Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000100000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-16T19:51:32.263Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000500000000"))), _permissions = Permissions {_self = fromList [DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0006-0000-000400000006"))),(fromJust (readUTCTimeMillis "1864-05-10T08:06:20.221Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000500000008"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000100000005"))),(fromJust (readUTCTimeMillis "1864-05-12T19:45:30.532Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000500000002"))), _permissions = Permissions {_self = fromList [SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000200000006"))),(fromJust (readUTCTimeMillis "1864-05-05T11:50:22.737Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000500000001"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000004"))),(fromJust (readUTCTimeMillis "1864-05-14T04:50:47.446Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000400000005"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions,SetMemberPermissions], _copy = fromList [CreateConversation,DoNotUseDeprecatedAddRemoveConvMember,GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000000000007"))),(fromJust (readUTCTimeMillis "1864-05-03T07:29:57.141Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000500000000"))), _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000600000002"))),(fromJust (readUTCTimeMillis "1864-05-13T12:59:51.056Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000700000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,SetBilling], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000200000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,SetBilling,GetMemberPermissions,SetMemberPermissions], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000000000004"))),(fromJust (readUTCTimeMillis "1864-05-03T05:21:49.285Z"))), _legalHoldStatus = UserLegalHoldEnabled}))
testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0004-0000-000800000006"))), _permissions = Permissions {_self = fromList [CreateConversation,SetMemberPermissions], _copy = fromList [SetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000800000005"))),(fromJust (readUTCTimeMillis "1864-05-15T07:52:09.558Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000000000003"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,SetBilling,SetTeamData], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000700000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName], _copy = fromList [DoNotUseDeprecatedModifyConvName]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000700000005"))),(fromJust (readUTCTimeMillis "1864-05-16T12:53:18.577Z"))), _legalHoldStatus = UserLegalHoldPending}))
testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000200000002"))), _permissions = Permissions {_self = fromList [AddTeamMember,SetBilling], _copy = fromList [AddTeamMember]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000800000006"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000300000008"))),(fromJust (readUTCTimeMillis "1864-05-12T15:03:44.261Z"))), _legalHoldStatus = UserLegalHoldDisabled}))
testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 = (newNewTeamMember (TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000700000003"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000200000003"))),(fromJust (readUTCTimeMillis "1864-05-06T08:07:33.146Z"))), _legalHoldStatus = UserLegalHoldPending}))
