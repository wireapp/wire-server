{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.TeamMember_team where

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
import Wire.API.Team.Member (TeamMember, mkTeamMember)
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

testObject_TeamMember_team_1 :: TeamMember
testObject_TeamMember_team_1 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000007-0000-0005-0000-000500000002")))
    ( Permissions
        { _self = fromList [GetBilling, GetMemberPermissions, SetMemberPermissions, DeleteTeam],
          _copy = fromList [GetBilling]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000001-0000-0003-0000-000300000004"))),
          (fromJust (readUTCTimeMillis "1864-05-12T22:05:34.634Z"))
        )
    )
    UserLegalHoldPending

testObject_TeamMember_team_2 :: TeamMember
testObject_TeamMember_team_2 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000003-0000-0000-0000-000500000005")))
    (Permissions {_self = fromList [DoNotUseDeprecatedModifyConvName, SetMemberPermissions], _copy = fromList []})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000004"))),
          (fromJust (readUTCTimeMillis "1864-05-03T14:56:52.508Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_3 :: TeamMember
testObject_TeamMember_team_3 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000005-0000-0003-0000-000400000003")))
    ( Permissions
        { _self =
            fromList
              [DoNotUseDeprecatedDeleteConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, GetBilling],
          _copy = fromList [GetBilling]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000000-0000-0005-0000-000200000007"))),
          (fromJust (readUTCTimeMillis "1864-05-06T14:02:04.371Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_4 :: TeamMember
testObject_TeamMember_team_4 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000100000006")))
    ( Permissions
        { _self = fromList [DoNotUseDeprecatedModifyConvName, SetMemberPermissions],
          _copy = fromList [SetMemberPermissions]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000006-0000-0002-0000-000500000001"))),
          (fromJust (readUTCTimeMillis "1864-05-12T15:36:56.285Z"))
        )
    )
    (UserLegalHoldEnabled)

testObject_TeamMember_team_5 :: TeamMember
testObject_TeamMember_team_5 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000200000001")))
    ( Permissions
        { _self = fromList [DoNotUseDeprecatedDeleteConversation, GetBilling, SetBilling, GetMemberPermissions],
          _copy = fromList [DoNotUseDeprecatedDeleteConversation, GetMemberPermissions]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000004-0000-0002-0000-000300000007"))),
          (fromJust (readUTCTimeMillis "1864-05-07T21:02:57.104Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_6 :: TeamMember
testObject_TeamMember_team_6 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000006-0000-0007-0000-000800000005")))
    ( Permissions
        { _self =
            fromList
              [CreateConversation, AddTeamMember, DoNotUseDeprecatedAddRemoveConvMember, SetBilling, SetTeamData],
          _copy = fromList []
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000800000000"))),
          (fromJust (readUTCTimeMillis "1864-05-09T03:11:26.909Z"))
        )
    )
    (UserLegalHoldEnabled)

testObject_TeamMember_team_7 :: TeamMember
testObject_TeamMember_team_7 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000007-0000-0000-0000-000200000001")))
    ( Permissions
        { _self =
            fromList
              [ DoNotUseDeprecatedDeleteConversation,
                DoNotUseDeprecatedAddRemoveConvMember,
                SetBilling,
                SetMemberPermissions,
                GetTeamConversations
              ],
          _copy = fromList []
        }
    )
    (Nothing)
    (UserLegalHoldPending)

testObject_TeamMember_team_8 :: TeamMember
testObject_TeamMember_team_8 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000300000000")))
    ( Permissions
        { _self =
            fromList
              [ DoNotUseDeprecatedAddRemoveConvMember,
                DoNotUseDeprecatedModifyConvName,
                SetTeamData,
                SetMemberPermissions,
                DeleteTeam
              ],
          _copy = fromList []
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000007-0000-0003-0000-000400000003"))),
          (fromJust (readUTCTimeMillis "1864-05-05T18:40:11.956Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_9 :: TeamMember
testObject_TeamMember_team_9 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000008-0000-0006-0000-000300000003")))
    ( Permissions
        { _self = fromList [AddTeamMember, DoNotUseDeprecatedModifyConvName],
          _copy = fromList [DoNotUseDeprecatedModifyConvName]
        }
    )
    (Nothing)
    (UserLegalHoldPending)

testObject_TeamMember_team_10 :: TeamMember
testObject_TeamMember_team_10 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000100000006")))
    (Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember], _copy = fromList []})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000008-0000-0005-0000-000000000002"))),
          (fromJust (readUTCTimeMillis "1864-05-03T19:02:13.669Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_11 :: TeamMember
testObject_TeamMember_team_11 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000004-0000-0001-0000-000400000007")))
    ( Permissions
        { _self =
            fromList [CreateConversation, DoNotUseDeprecatedDeleteConversation, SetTeamData, SetMemberPermissions],
          _copy = fromList []
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000003-0000-0001-0000-000100000005"))),
          (fromJust (readUTCTimeMillis "1864-05-04T18:20:29.420Z"))
        )
    )
    (UserLegalHoldEnabled)

testObject_TeamMember_team_12 :: TeamMember
testObject_TeamMember_team_12 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000002-0000-0006-0000-000200000005")))
    (Permissions {_self = fromList [GetTeamConversations], _copy = fromList []})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000300000003"))),
          (fromJust (readUTCTimeMillis "1864-05-10T22:34:18.259Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_13 :: TeamMember
testObject_TeamMember_team_13 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000800000006")))
    (Permissions {_self = fromList [CreateConversation, GetMemberPermissions], _copy = fromList [CreateConversation]})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000000-0000-0003-0000-000400000007"))),
          (fromJust (readUTCTimeMillis "1864-05-06T08:18:27.514Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_14 :: TeamMember
testObject_TeamMember_team_14 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000004-0000-0000-0000-000300000007")))
    ( Permissions
        { _self = fromList [DoNotUseDeprecatedDeleteConversation, AddTeamMember, GetBilling, GetMemberPermissions],
          _copy = fromList [GetBilling, GetMemberPermissions]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000200000002"))),
          (fromJust (readUTCTimeMillis "1864-05-12T15:53:41.144Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_15 :: TeamMember
testObject_TeamMember_team_15 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000005-0000-0006-0000-000800000006")))
    (Permissions {_self = fromList [DeleteTeam], _copy = fromList [DeleteTeam]})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000008-0000-0000-0000-000500000003"))),
          (fromJust (readUTCTimeMillis "1864-05-04T06:15:13.870Z"))
        )
    )
    (UserLegalHoldEnabled)

testObject_TeamMember_team_16 :: TeamMember
testObject_TeamMember_team_16 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000000-0000-0008-0000-000200000008")))
    (Permissions {_self = fromList [DoNotUseDeprecatedDeleteConversation, GetTeamConversations], _copy = fromList []})
    ( Just
        ( (Id (fromJust (UUID.fromString "00000006-0000-0000-0000-000400000002"))),
          (fromJust (readUTCTimeMillis "1864-05-10T04:27:37.101Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_17 :: TeamMember
testObject_TeamMember_team_17 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000006-0000-0006-0000-000500000007")))
    ( Permissions
        { _self =
            fromList
              [ DoNotUseDeprecatedAddRemoveConvMember,
                DoNotUseDeprecatedModifyConvName,
                GetBilling,
                SetTeamData,
                GetTeamConversations
              ],
          _copy = fromList [DoNotUseDeprecatedAddRemoveConvMember]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000006-0000-0003-0000-000700000004"))),
          (fromJust (readUTCTimeMillis "1864-05-07T23:22:37.991Z"))
        )
    )
    (UserLegalHoldDisabled)

testObject_TeamMember_team_18 :: TeamMember
testObject_TeamMember_team_18 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000005-0000-0005-0000-000200000008")))
    ( Permissions
        { _self =
            fromList [RemoveTeamMember, DoNotUseDeprecatedModifyConvName, GetMemberPermissions, SetMemberPermissions],
          _copy = fromList [SetMemberPermissions]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000007-0000-0008-0000-000500000006"))),
          (fromJust (readUTCTimeMillis "1864-05-15T14:48:55.847Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_19 :: TeamMember
testObject_TeamMember_team_19 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000003-0000-0002-0000-000200000008")))
    ( Permissions
        { _self =
            fromList [AddTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling, SetBilling, SetMemberPermissions],
          _copy = fromList [SetMemberPermissions]
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000006-0000-0001-0000-000200000008"))),
          (fromJust (readUTCTimeMillis "1864-05-12T01:37:35.003Z"))
        )
    )
    (UserLegalHoldPending)

testObject_TeamMember_team_20 :: TeamMember
testObject_TeamMember_team_20 =
  mkTeamMember
    (Id (fromJust (UUID.fromString "00000005-0000-0007-0000-000100000005")))
    ( Permissions
        { _self = fromList [CreateConversation, AddTeamMember, DoNotUseDeprecatedModifyConvName, GetBilling],
          _copy = fromList []
        }
    )
    ( Just
        ( (Id (fromJust (UUID.fromString "00000005-0000-0001-0000-000800000007"))),
          (fromJust (readUTCTimeMillis "1864-05-04T22:12:50.096Z"))
        )
    )
    (UserLegalHoldEnabled)
