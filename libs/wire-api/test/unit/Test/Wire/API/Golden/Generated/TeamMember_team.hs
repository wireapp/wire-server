{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.TeamMember_team where
import Data.Id ( Id(Id) )
import Data.Json.Util ( readUTCTimeMillis )
import Imports ( Maybe(Nothing, Just), fromJust )
import qualified Data.UUID as UUID ( fromString )
import GHC.Exts ( IsList(fromList) )
import Data.LegalHold
    ( UserLegalHoldStatus(UserLegalHoldDisabled, UserLegalHoldPending,
                          UserLegalHoldEnabled) )
import Wire.API.Team.Member ( TeamMember(..) )
import Wire.API.Team.Permission
    ( Permissions(Permissions, _self, _copy),
      Perm(GetTeamConversations, SetTeamData,
           DoNotUseDeprecatedAddRemoveConvMember, AddTeamMember,
           DoNotUseDeprecatedDeleteConversation, SetMemberPermissions,
           SetBilling, DeleteTeam, CreateConversation, RemoveTeamMember,
           GetMemberPermissions, DoNotUseDeprecatedModifyConvName,
           GetBilling) )

testObject_TeamMember_team_1 :: TeamMember
testObject_TeamMember_team_1 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000400000000"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000300000000"))), _permissions = Permissions {_self = fromList [SetBilling], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000300000000"))),(fromJust (readUTCTimeMillis "1864-05-05T20:18:12.876Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000600000004"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000100000003"))),(fromJust (readUTCTimeMillis "1864-05-12T14:27:31.755Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0005-0000-000800000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,GetBilling,GetMemberPermissions,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000700000004"))),(fromJust (readUTCTimeMillis "1864-05-03T03:16:29.271Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000200000007"))), _permissions = Permissions {_self = fromList [GetBilling,GetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000200000000"))),(fromJust (readUTCTimeMillis "1864-05-16T03:21:16.748Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_6 :: TeamMember
testObject_TeamMember_team_6 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0007-0000-000600000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,GetTeamConversations], _copy = fromList [RemoveTeamMember]}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_7 :: TeamMember
testObject_TeamMember_team_7 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000400000008"))), _permissions = Permissions {_self = fromList [CreateConversation,AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetTeamData,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000100000001"))),(fromJust (readUTCTimeMillis "1864-05-13T14:00:58.124Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_8 :: TeamMember
testObject_TeamMember_team_8 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000800000007"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000700000006"))),(fromJust (readUTCTimeMillis "1864-05-05T18:30:13.634Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_9 :: TeamMember
testObject_TeamMember_team_9 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0004-0000-000200000004"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,GetBilling,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000008"))),(fromJust (readUTCTimeMillis "1864-05-13T06:40:12.319Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_10 :: TeamMember
testObject_TeamMember_team_10 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0003-0000-000700000004"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [RemoveTeamMember,SetTeamData]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000007-0000-0002-0000-000300000002"))),(fromJust (readUTCTimeMillis "1864-05-08T05:08:05.857Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_11 :: TeamMember
testObject_TeamMember_team_11 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000400000003"))), _permissions = Permissions {_self = fromList [SetTeamData,DeleteTeam], _copy = fromList [DeleteTeam]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000700000006"))),(fromJust (readUTCTimeMillis "1864-05-15T16:54:17.112Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_12 :: TeamMember
testObject_TeamMember_team_12 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0007-0000-000000000007"))), _permissions = Permissions {_self = fromList [AddTeamMember,SetBilling,SetTeamData,GetTeamConversations,DeleteTeam], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_13 :: TeamMember
testObject_TeamMember_team_13 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0002-0000-000400000005"))), _permissions = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000300000003"))),(fromJust (readUTCTimeMillis "1864-05-15T07:27:08.515Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_14 :: TeamMember
testObject_TeamMember_team_14 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0007-0000-000400000001"))), _permissions = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,SetBilling,SetMemberPermissions], _copy = fromList [AddTeamMember]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000200000008"))),(fromJust (readUTCTimeMillis "1864-05-09T21:55:28.380Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_15 :: TeamMember
testObject_TeamMember_team_15 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0000-0000-000100000003"))), _permissions = Permissions {_self = fromList [RemoveTeamMember,SetMemberPermissions,DeleteTeam], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_16 :: TeamMember
testObject_TeamMember_team_16 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000003-0000-0006-0000-000300000001"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000700000006"))),(fromJust (readUTCTimeMillis "1864-05-11T05:13:42.209Z"))), _legalHoldStatus = UserLegalHoldPending}
testObject_TeamMember_team_17 :: TeamMember
testObject_TeamMember_team_17 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000008"))), _permissions = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions,SetMemberPermissions], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000800000005"))),(fromJust (readUTCTimeMillis "1864-05-13T10:13:32.207Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_18 :: TeamMember
testObject_TeamMember_team_18 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000800000001"))), _permissions = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,GetBilling,SetBilling,GetMemberPermissions,DeleteTeam], _copy = fromList [CreateConversation,RemoveTeamMember,GetMemberPermissions]}, _invitation = Just ((Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000003"))),(fromJust (readUTCTimeMillis "1864-05-15T22:11:19.966Z"))), _legalHoldStatus = UserLegalHoldEnabled}
testObject_TeamMember_team_19 :: TeamMember
testObject_TeamMember_team_19 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000400000005"))), _permissions = Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName,GetBilling,GetTeamConversations], _copy = fromList []}, _invitation = Just ((Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000300000008"))),(fromJust (readUTCTimeMillis "1864-05-06T19:03:34.406Z"))), _legalHoldStatus = UserLegalHoldDisabled}
testObject_TeamMember_team_20 :: TeamMember
testObject_TeamMember_team_20 = TeamMember {_userId = (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000500000007"))), _permissions = Permissions {_self = fromList [], _copy = fromList []}, _invitation = Nothing, _legalHoldStatus = UserLegalHoldDisabled}
