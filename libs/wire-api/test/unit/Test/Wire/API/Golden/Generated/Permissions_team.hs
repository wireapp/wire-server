{-# LANGUAGE OverloadedLists #-}

module Test.Wire.API.Golden.Generated.Permissions_team where

import GHC.Exts (IsList (fromList))
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
    Permissions (..),
  )

testObject_Permissions_team_1 :: Permissions
testObject_Permissions_team_1 = Permissions {_self = fromList [SetBilling], _copy = fromList [SetBilling]}

testObject_Permissions_team_2 :: Permissions
testObject_Permissions_team_2 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, SetMemberPermissions, GetTeamConversations]}

testObject_Permissions_team_3 :: Permissions
testObject_Permissions_team_3 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam]}

testObject_Permissions_team_4 :: Permissions
testObject_Permissions_team_4 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, GetBilling, SetBilling, GetMemberPermissions, SetMemberPermissions, DeleteTeam], _copy = fromList [GetBilling]}

testObject_Permissions_team_5 :: Permissions
testObject_Permissions_team_5 = Permissions {_self = fromList [CreateConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, GetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [CreateConversation, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, GetMemberPermissions, DeleteTeam]}

testObject_Permissions_team_6 :: Permissions
testObject_Permissions_team_6 = Permissions {_self = fromList [CreateConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, GetMemberPermissions, GetTeamConversations], _copy = fromList [CreateConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, GetMemberPermissions, GetTeamConversations]}

testObject_Permissions_team_7 :: Permissions
testObject_Permissions_team_7 = Permissions {_self = fromList [AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, GetTeamConversations, DeleteTeam], _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember, GetBilling, DeleteTeam]}

testObject_Permissions_team_8 :: Permissions
testObject_Permissions_team_8 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, GetMemberPermissions, SetMemberPermissions, GetTeamConversations], _copy = fromList [AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, GetMemberPermissions, SetMemberPermissions]}

testObject_Permissions_team_9 :: Permissions
testObject_Permissions_team_9 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, DoNotUseDeprecatedAddRemoveConvMember, GetMemberPermissions], _copy = fromList [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember, GetMemberPermissions]}

testObject_Permissions_team_10 :: Permissions
testObject_Permissions_team_10 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, SetMemberPermissions, GetTeamConversations, DeleteTeam]}

testObject_Permissions_team_11 :: Permissions
testObject_Permissions_team_11 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, RemoveTeamMember, GetBilling, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [RemoveTeamMember, GetMemberPermissions, GetTeamConversations]}

testObject_Permissions_team_12 :: Permissions
testObject_Permissions_team_12 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, GetMemberPermissions, GetTeamConversations, DeleteTeam]}

testObject_Permissions_team_13 :: Permissions
testObject_Permissions_team_13 = Permissions {_self = fromList [CreateConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetTeamData, SetMemberPermissions], _copy = fromList [SetTeamData, SetMemberPermissions]}

testObject_Permissions_team_14 :: Permissions
testObject_Permissions_team_14 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, SetBilling, SetTeamData, GetMemberPermissions, SetMemberPermissions], _copy = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, SetBilling, SetTeamData, GetMemberPermissions, SetMemberPermissions]}

testObject_Permissions_team_15 :: Permissions
testObject_Permissions_team_15 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, SetBilling, GetMemberPermissions, SetMemberPermissions, DeleteTeam], _copy = fromList []}

testObject_Permissions_team_16 :: Permissions
testObject_Permissions_team_16 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, DoNotUseDeprecatedAddRemoveConvMember, GetBilling, SetTeamData, SetMemberPermissions, GetTeamConversations], _copy = fromList [DoNotUseDeprecatedDeleteConversation, GetBilling, SetTeamData, SetMemberPermissions, GetTeamConversations]}

testObject_Permissions_team_17 :: Permissions
testObject_Permissions_team_17 = Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, SetTeamData, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedModifyConvName, SetTeamData, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam]}

testObject_Permissions_team_18 :: Permissions
testObject_Permissions_team_18 = Permissions {_self = fromList [CreateConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, GetMemberPermissions, SetMemberPermissions, DeleteTeam], _copy = fromList [CreateConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, GetMemberPermissions, DeleteTeam]}

testObject_Permissions_team_19 :: Permissions
testObject_Permissions_team_19 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, DoNotUseDeprecatedModifyConvName, SetBilling, SetTeamData, GetMemberPermissions, SetMemberPermissions, GetTeamConversations, DeleteTeam], _copy = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, RemoveTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, SetMemberPermissions, GetTeamConversations, DeleteTeam]}

testObject_Permissions_team_20 :: Permissions
testObject_Permissions_team_20 = Permissions {_self = fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, SetMemberPermissions, DeleteTeam], _copy = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetTeamData, SetMemberPermissions, DeleteTeam]}
