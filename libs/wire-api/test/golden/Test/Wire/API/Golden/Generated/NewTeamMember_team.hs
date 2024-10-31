{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Wire.API.Golden.Generated.NewTeamMember_team where

import Data.Id (Id (Id))
import Data.Json.Util (readUTCTimeMillis)
import Data.UUID qualified as UUID (fromString)
import GHC.Exts (IsList (fromList))
import Imports (Maybe (Just, Nothing), fromJust)
import Wire.API.Team.Member (NewTeamMember, mkNewTeamMember)
import Wire.API.Team.Permission
  ( Perm
      ( AddRemoveConvMember,
        AddTeamMember,
        CreateConversation,
        DeleteConversation,
        DeleteTeam,
        GetBilling,
        GetMemberPermissions,
        GetTeamConversations,
        ModifyConvName,
        RemoveTeamMember,
        SetBilling,
        SetMemberPermissions,
        SetTeamData
      ),
    Permissions (Permissions, copy, self),
  )

testObject_NewTeamMember_team_1 :: NewTeamMember
testObject_NewTeamMember_team_1 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000002-0000-0007-0000-000200000002")))
    (Permissions {self = fromList [], copy = fromList []})
    ( Just
        ( Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000004")),
          fromJust (readUTCTimeMillis "1864-05-04T12:59:54.182Z")
        )
    )

testObject_NewTeamMember_team_2 :: NewTeamMember
testObject_NewTeamMember_team_2 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000200000003")))
    ( Permissions
        { self =
            fromList
              [ CreateConversation,
                DeleteConversation,
                AddTeamMember,
                RemoveTeamMember,
                AddRemoveConvMember,
                ModifyConvName
              ],
          copy = fromList [DeleteConversation, AddRemoveConvMember]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000500000008")),
          fromJust (readUTCTimeMillis "1864-05-16T00:49:15.576Z")
        )
    )

testObject_NewTeamMember_team_3 :: NewTeamMember
testObject_NewTeamMember_team_3 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0008-0000-000700000005")))
    ( Permissions
        { self =
            fromList
              [CreateConversation, DeleteConversation, RemoveTeamMember, GetBilling, DeleteTeam],
          copy = fromList [CreateConversation, DeleteConversation, GetBilling]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000005-0000-0004-0000-000500000002")),
          fromJust (readUTCTimeMillis "1864-05-08T07:57:50.660Z")
        )
    )

testObject_NewTeamMember_team_4 :: NewTeamMember
testObject_NewTeamMember_team_4 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000700000005")))
    ( Permissions
        { self = fromList [CreateConversation, AddTeamMember, SetTeamData],
          copy = fromList [CreateConversation, SetTeamData]
        }
    )
    Nothing

testObject_NewTeamMember_team_5 :: NewTeamMember
testObject_NewTeamMember_team_5 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000002")))
    (Permissions {self = fromList [AddTeamMember, SetBilling, GetTeamConversations], copy = fromList [AddTeamMember]})
    ( Just
        ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000600000006")),
          fromJust (readUTCTimeMillis "1864-05-12T23:29:05.832Z")
        )
    )

testObject_NewTeamMember_team_6 :: NewTeamMember
testObject_NewTeamMember_team_6 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000400000003")))
    ( Permissions
        { self =
            fromList
              [CreateConversation, DeleteConversation, GetBilling, SetTeamData, SetMemberPermissions],
          copy = fromList [CreateConversation, GetBilling]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000800000003")),
          fromJust (readUTCTimeMillis "1864-05-16T01:49:44.477Z")
        )
    )

testObject_NewTeamMember_team_7 :: NewTeamMember
testObject_NewTeamMember_team_7 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000007-0000-0004-0000-000500000005")))
    ( Permissions
        { self =
            fromList
              [AddTeamMember, RemoveTeamMember, ModifyConvName, GetTeamConversations, DeleteTeam],
          copy = fromList [AddTeamMember]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000000000007")),
          fromJust (readUTCTimeMillis "1864-05-08T14:17:14.531Z")
        )
    )

testObject_NewTeamMember_team_8 :: NewTeamMember
testObject_NewTeamMember_team_8 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000200000003")))
    ( Permissions
        { self = fromList [ModifyConvName],
          copy = fromList [ModifyConvName]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000200000002")),
          fromJust (readUTCTimeMillis "1864-05-16T06:33:31.445Z")
        )
    )

testObject_NewTeamMember_team_9 :: NewTeamMember
testObject_NewTeamMember_team_9 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000001-0000-0008-0000-000300000004")))
    (Permissions {self = fromList [SetBilling], copy = fromList []})
    ( Just
        ( Id (fromJust (UUID.fromString "00000002-0000-0001-0000-000700000000")),
          fromJust (readUTCTimeMillis "1864-05-08T10:27:23.240Z")
        )
    )

testObject_NewTeamMember_team_10 :: NewTeamMember
testObject_NewTeamMember_team_10 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0003-0000-000600000003")))
    (Permissions {self = fromList [GetBilling], copy = fromList []})
    ( Just
        ( Id (fromJust (UUID.fromString "00000004-0000-0006-0000-000600000008")),
          fromJust (readUTCTimeMillis "1864-05-15T10:49:54.418Z")
        )
    )

testObject_NewTeamMember_team_11 :: NewTeamMember
testObject_NewTeamMember_team_11 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000006-0000-0005-0000-000000000002")))
    ( Permissions
        { self = fromList [CreateConversation, ModifyConvName, SetTeamData],
          copy = fromList []
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000002-0000-0003-0000-000800000002")),
          fromJust (readUTCTimeMillis "1864-05-14T12:23:51.061Z")
        )
    )

testObject_NewTeamMember_team_12 :: NewTeamMember
testObject_NewTeamMember_team_12 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000001-0000-0004-0000-000000000007")))
    (Permissions {self = fromList [SetBilling, SetTeamData, GetTeamConversations], copy = fromList []})
    Nothing

testObject_NewTeamMember_team_13 :: NewTeamMember
testObject_NewTeamMember_team_13 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000002-0000-0004-0000-000600000001")))
    ( Permissions
        { self = fromList [AddTeamMember, AddRemoveConvMember, SetTeamData, GetTeamConversations],
          copy = fromList [AddTeamMember, AddRemoveConvMember, GetTeamConversations]
        }
    )
    Nothing

testObject_NewTeamMember_team_14 :: NewTeamMember
testObject_NewTeamMember_team_14 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000500000004")))
    ( Permissions
        { self =
            fromList
              [CreateConversation, DeleteConversation, ModifyConvName, GetBilling],
          copy = fromList []
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000006-0000-0008-0000-000000000003")),
          fromJust (readUTCTimeMillis "1864-05-16T00:23:45.641Z")
        )
    )

testObject_NewTeamMember_team_15 :: NewTeamMember
testObject_NewTeamMember_team_15 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000800000007")))
    ( Permissions
        { self = fromList [RemoveTeamMember, GetMemberPermissions, DeleteTeam],
          copy = fromList [RemoveTeamMember, GetMemberPermissions]
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000006-0000-0004-0000-000300000006")),
          fromJust (readUTCTimeMillis "1864-05-02T08:10:15.332Z")
        )
    )

testObject_NewTeamMember_team_16 :: NewTeamMember
testObject_NewTeamMember_team_16 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0006-0000-000300000005")))
    ( Permissions
        { self = fromList [CreateConversation, RemoveTeamMember, GetBilling, GetTeamConversations, DeleteTeam],
          copy = fromList []
        }
    )
    Nothing

testObject_NewTeamMember_team_17 :: NewTeamMember
testObject_NewTeamMember_team_17 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000400000005")))
    (Permissions {self = fromList [], copy = fromList []})
    ( Just
        ( Id (fromJust (UUID.fromString "00000004-0000-0008-0000-000800000007")),
          fromJust (readUTCTimeMillis "1864-05-07T21:53:30.897Z")
        )
    )

testObject_NewTeamMember_team_18 :: NewTeamMember
testObject_NewTeamMember_team_18 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000000000001")))
    (Permissions {self = fromList [], copy = fromList []})
    ( Just
        ( Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000500000002")),
          fromJust (readUTCTimeMillis "1864-05-11T12:32:01.417Z")
        )
    )

testObject_NewTeamMember_team_19 :: NewTeamMember
testObject_NewTeamMember_team_19 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000004-0000-0005-0000-000100000008")))
    ( Permissions
        { self = fromList [DeleteConversation, RemoveTeamMember, SetBilling, SetMemberPermissions],
          copy = fromList [DeleteConversation, SetBilling]
        }
    )
    Nothing

testObject_NewTeamMember_team_20 :: NewTeamMember
testObject_NewTeamMember_team_20 =
  mkNewTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000000000004")))
    ( Permissions
        { self =
            fromList
              [ AddTeamMember,
                AddRemoveConvMember,
                ModifyConvName,
                SetBilling,
                GetMemberPermissions,
                GetTeamConversations
              ],
          copy = fromList []
        }
    )
    ( Just
        ( Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000400000008")),
          fromJust (readUTCTimeMillis "1864-05-05T07:36:25.213Z")
        )
    )
