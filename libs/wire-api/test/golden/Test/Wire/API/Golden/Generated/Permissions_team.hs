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

module Test.Wire.API.Golden.Generated.Permissions_team where

import GHC.Exts (IsList (fromList))
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
    Permissions (..),
  )

testObject_Permissions_team_1 :: Permissions
testObject_Permissions_team_1 = Permissions {self = fromList [SetBilling], copy = fromList [SetBilling]}

testObject_Permissions_team_2 :: Permissions
testObject_Permissions_team_2 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            SetMemberPermissions,
            GetTeamConversations
          ]
    }

testObject_Permissions_team_3 :: Permissions
testObject_Permissions_team_3 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            SetBilling,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ]
    }

testObject_Permissions_team_4 :: Permissions
testObject_Permissions_team_4 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            AddRemoveConvMember,
            GetBilling,
            SetBilling,
            GetMemberPermissions,
            SetMemberPermissions,
            DeleteTeam
          ],
      copy = fromList [GetBilling]
    }

testObject_Permissions_team_5 :: Permissions
testObject_Permissions_team_5 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            AddTeamMember,
            RemoveTeamMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            GetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ CreateConversation,
            RemoveTeamMember,
            ModifyConvName,
            GetBilling,
            GetMemberPermissions,
            DeleteTeam
          ]
    }

testObject_Permissions_team_6 :: Permissions
testObject_Permissions_team_6 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            GetTeamConversations
          ],
      copy =
        fromList
          [ CreateConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            GetMemberPermissions,
            GetTeamConversations
          ]
    }

testObject_Permissions_team_7 :: Permissions
testObject_Permissions_team_7 =
  Permissions
    { self =
        fromList
          [ AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            GetTeamConversations,
            DeleteTeam
          ],
      copy = fromList [AddRemoveConvMember, GetBilling, DeleteTeam]
    }

testObject_Permissions_team_8 :: Permissions
testObject_Permissions_team_8 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations
          ],
      copy =
        fromList
          [ AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            GetMemberPermissions,
            SetMemberPermissions
          ]
    }

testObject_Permissions_team_9 :: Permissions
testObject_Permissions_team_9 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddRemoveConvMember,
            GetMemberPermissions
          ],
      copy = fromList [CreateConversation, AddRemoveConvMember, GetMemberPermissions]
    }

testObject_Permissions_team_10 :: Permissions
testObject_Permissions_team_10 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            SetBilling,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            SetBilling,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ]
    }

testObject_Permissions_team_11 :: Permissions
testObject_Permissions_team_11 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            RemoveTeamMember,
            GetBilling,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy = fromList [RemoveTeamMember, GetMemberPermissions, GetTeamConversations]
    }

testObject_Permissions_team_12 :: Permissions
testObject_Permissions_team_12 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ CreateConversation,
            DeleteConversation,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ]
    }

testObject_Permissions_team_13 :: Permissions
testObject_Permissions_team_13 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            AddTeamMember,
            RemoveTeamMember,
            ModifyConvName,
            GetBilling,
            SetTeamData,
            SetMemberPermissions
          ],
      copy = fromList [SetTeamData, SetMemberPermissions]
    }

testObject_Permissions_team_14 :: Permissions
testObject_Permissions_team_14 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions
          ],
      copy =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions
          ]
    }

testObject_Permissions_team_15 :: Permissions
testObject_Permissions_team_15 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            SetBilling,
            GetMemberPermissions,
            SetMemberPermissions,
            DeleteTeam
          ],
      copy = fromList []
    }

testObject_Permissions_team_16 :: Permissions
testObject_Permissions_team_16 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddRemoveConvMember,
            GetBilling,
            SetTeamData,
            SetMemberPermissions,
            GetTeamConversations
          ],
      copy =
        fromList
          [DeleteConversation, GetBilling, SetTeamData, SetMemberPermissions, GetTeamConversations]
    }

testObject_Permissions_team_17 :: Permissions
testObject_Permissions_team_17 =
  Permissions
    { self =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            ModifyConvName,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            ModifyConvName,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ]
    }

testObject_Permissions_team_18 :: Permissions
testObject_Permissions_team_18 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            AddTeamMember,
            AddRemoveConvMember,
            SetBilling,
            GetMemberPermissions,
            SetMemberPermissions,
            DeleteTeam
          ],
      copy =
        fromList
          [ CreateConversation,
            AddTeamMember,
            AddRemoveConvMember,
            SetBilling,
            GetMemberPermissions,
            DeleteTeam
          ]
    }

testObject_Permissions_team_19 :: Permissions
testObject_Permissions_team_19 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            ModifyConvName,
            SetBilling,
            SetTeamData,
            GetMemberPermissions,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ],
      copy =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            RemoveTeamMember,
            AddRemoveConvMember,
            SetBilling,
            SetMemberPermissions,
            GetTeamConversations,
            DeleteTeam
          ]
    }

testObject_Permissions_team_20 :: Permissions
testObject_Permissions_team_20 =
  Permissions
    { self =
        fromList
          [ CreateConversation,
            DeleteConversation,
            AddTeamMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            SetMemberPermissions,
            DeleteTeam
          ],
      copy =
        fromList
          [ DeleteConversation,
            AddTeamMember,
            ModifyConvName,
            GetBilling,
            SetBilling,
            SetTeamData,
            SetMemberPermissions,
            DeleteTeam
          ]
    }
