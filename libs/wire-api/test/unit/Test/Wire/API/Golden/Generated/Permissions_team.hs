{-# LANGUAGE OverloadedLists #-}
module Test.Wire.API.Golden.Generated.Permissions_team where
import GHC.Exts ( IsList(fromList) )
import Wire.API.Team.Permission
    ( Permissions(..),
      Perm(DeleteTeam, CreateConversation,
           DoNotUseDeprecatedDeleteConversation, GetMemberPermissions,
           AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName,
           GetBilling, SetBilling, SetTeamData, SetMemberPermissions,
           GetTeamConversations, DoNotUseDeprecatedAddRemoveConvMember) )

testObject_Permissions_team_1 :: Permissions
testObject_Permissions_team_1 = Permissions {_self = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling,DeleteTeam], _copy = fromList [AddTeamMember,DoNotUseDeprecatedAddRemoveConvMember,GetBilling,SetBilling]}
testObject_Permissions_team_2 :: Permissions
testObject_Permissions_team_2 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetTeamData,SetMemberPermissions]}
testObject_Permissions_team_3 :: Permissions
testObject_Permissions_team_3 = Permissions {_self = fromList [RemoveTeamMember,SetTeamData,GetTeamConversations], _copy = fromList []}
testObject_Permissions_team_4 :: Permissions
testObject_Permissions_team_4 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetMemberPermissions,GetTeamConversations]}
testObject_Permissions_team_5 :: Permissions
testObject_Permissions_team_5 = Permissions {_self = fromList [CreateConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedModifyConvName,SetTeamData,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_6 :: Permissions
testObject_Permissions_team_6 = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetMemberPermissions,DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,DeleteTeam]}
testObject_Permissions_team_7 :: Permissions
testObject_Permissions_team_7 = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,GetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_8 :: Permissions
testObject_Permissions_team_8 = Permissions {_self = fromList [CreateConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions,SetMemberPermissions], _copy = fromList [CreateConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,GetMemberPermissions,SetMemberPermissions]}
testObject_Permissions_team_9 :: Permissions
testObject_Permissions_team_9 = Permissions {_self = fromList [], _copy = fromList []}
testObject_Permissions_team_10 :: Permissions
testObject_Permissions_team_10 = Permissions {_self = fromList [SetMemberPermissions], _copy = fromList [SetMemberPermissions]}
testObject_Permissions_team_11 :: Permissions
testObject_Permissions_team_11 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_12 :: Permissions
testObject_Permissions_team_12 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData]}
testObject_Permissions_team_13 :: Permissions
testObject_Permissions_team_13 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation]}
testObject_Permissions_team_14 :: Permissions
testObject_Permissions_team_14 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_15 :: Permissions
testObject_Permissions_team_15 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_16 :: Permissions
testObject_Permissions_team_16 = Permissions {_self = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [CreateConversation,DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_17 :: Permissions
testObject_Permissions_team_17 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,GetBilling,SetBilling,SetMemberPermissions,GetTeamConversations,DeleteTeam]}
testObject_Permissions_team_18 :: Permissions
testObject_Permissions_team_18 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation,AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedModifyConvName,SetBilling,SetTeamData,GetMemberPermissions,SetMemberPermissions,DeleteTeam]}
testObject_Permissions_team_19 :: Permissions
testObject_Permissions_team_19 = Permissions {_self = fromList [AddTeamMember,RemoveTeamMember,DoNotUseDeprecatedAddRemoveConvMember,DoNotUseDeprecatedModifyConvName,GetBilling,SetBilling,SetTeamData,SetMemberPermissions,GetTeamConversations], _copy = fromList []}
testObject_Permissions_team_20 :: Permissions
testObject_Permissions_team_20 = Permissions {_self = fromList [DoNotUseDeprecatedAddRemoveConvMember,DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember,DeleteTeam]}
