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
module Test.Wire.API.Golden.Generated.TeamMemberList_team where

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
  ( ListType (ListComplete, ListTruncated),
    TeamMember
      ( TeamMember,
        _invitation,
        _legalHoldStatus,
        _permissions,
        _userId
      ),
    TeamMemberList,
    newTeamMemberList,
  )
import Wire.API.Team.Permission
  ( Perm
      ( AddTeamMember,
        CreateConversation,
        GetBilling,
        GetMemberPermissions,
        GetTeamConversations,
        SetMemberPermissions,
        SetTeamData
      ),
    Permissions (Permissions, _copy, _self),
  )

testObject_TeamMemberList_team_1 :: TeamMemberList
testObject_TeamMemberList_team_1 = (newTeamMemberList ([]) (ListComplete))

testObject_TeamMemberList_team_2 :: TeamMemberList
testObject_TeamMemberList_team_2 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000002-0000-0000-0000-000000000002"))),
              _permissions = Permissions {_self = fromList [GetBilling, SetMemberPermissions], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000002"))),
                    (fromJust (readUTCTimeMillis "1864-05-10T10:05:44.332Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListComplete)
  )

testObject_TeamMemberList_team_3 :: TeamMemberList
testObject_TeamMemberList_team_3 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T06:07:36.175Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T14:28:10.448Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T16:05:37.642Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:06:20.504Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T16:37:10.774Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T04:36:55.388Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListComplete)
  )

testObject_TeamMemberList_team_4 :: TeamMemberList
testObject_TeamMemberList_team_4 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [GetTeamConversations], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-08T16:05:11.696Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-08T07:09:26.753Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_5 :: TeamMemberList
testObject_TeamMemberList_team_5 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T23:10:04.963Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T15:40:17.119Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:40:38.004Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:30:49.028Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListComplete)
  )

testObject_TeamMemberList_team_6 :: TeamMemberList
testObject_TeamMemberList_team_6 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T17:07:48.156Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:04:10.559Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T10:39:19.860Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:40:56.648Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T12:13:40.273Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:28:04.561Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T02:59:55.584Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T22:57:33.947Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T01:02:39.691Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:39:38.488Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListComplete)
  )

testObject_TeamMemberList_team_7 :: TeamMemberList
testObject_TeamMemberList_team_7 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [SetTeamData], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-10T03:11:36.961Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_8 :: TeamMemberList
testObject_TeamMemberList_team_8 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:35:03.629Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:48:38.818Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T06:12:10.151Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T03:45:53.520Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T17:14:59.798Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T17:51:55.340Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T01:38:35.880Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T18:06:10.660Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:30:46.880Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_9 :: TeamMemberList
testObject_TeamMemberList_team_9 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [AddTeamMember], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-08T22:16:59.050Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [CreateConversation], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-08T21:43:37.550Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_10 :: TeamMemberList
testObject_TeamMemberList_team_10 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T04:44:28.366Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T06:22:04.036Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T12:10:11.701Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T21:54:05.305Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:26:06.221Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T20:12:04.856Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T23:35:44.986Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:36:17.730Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T19:36:57.529Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T19:45:56.914Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:42:17.107Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T03:42:46.106Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T09:41:44.679Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T09:26:44.717Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:40:00.056Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:47:20.635Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T15:58:21.895Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T19:25:51.873Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T03:19:55.569Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListComplete)
  )

testObject_TeamMemberList_team_11 :: TeamMemberList
testObject_TeamMemberList_team_11 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T06:08:50.626Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T08:23:53.653Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T16:28:42.815Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T11:47:57.498Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T17:22:07.538Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T19:14:48.836Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T14:53:49.059Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T10:44:04.209Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T23:34:24.831Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_12 :: TeamMemberList
testObject_TeamMemberList_team_12 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T15:59:09.462Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T00:27:17.631Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_13 :: TeamMemberList
testObject_TeamMemberList_team_13 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [GetMemberPermissions], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-10T04:37:19.686Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T13:22:20.368Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_14 :: TeamMemberList
testObject_TeamMemberList_team_14 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:01:56.077Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T09:34:46.900Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T10:40:24.034Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T10:17:53.056Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T18:37:38.894Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T06:25:10.534Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T02:42:16.433Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T07:25:18.248Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T15:31:36.237Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T15:23:38.616Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_15 :: TeamMemberList
testObject_TeamMemberList_team_15 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T20:33:17.912Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T09:03:59.579Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_16 :: TeamMemberList
testObject_TeamMemberList_team_16 = (newTeamMemberList ([]) (ListComplete))

testObject_TeamMemberList_team_17 :: TeamMemberList
testObject_TeamMemberList_team_17 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T10:04:36.715Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T03:02:37.641Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T23:21:44.944Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T08:47:48.774Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_18 :: TeamMemberList
testObject_TeamMemberList_team_18 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T17:44:12.611Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T05:14:06.040Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000001"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T05:24:40.864Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0001-0000-000000000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T20:09:48.156Z"))
                  ),
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T20:09:31.059Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_19 :: TeamMemberList
testObject_TeamMemberList_team_19 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000000"))),
              _permissions =
                Permissions
                  { _self = fromList [CreateConversation, SetTeamData, SetMemberPermissions],
                    _copy = fromList []
                  },
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0002-0000-000200000002"))),
                    (fromJust (readUTCTimeMillis "1864-05-09T19:12:15.962Z"))
                  ),
              _legalHoldStatus = UserLegalHoldDisabled
            }
        ]
      )
      (ListTruncated)
  )

testObject_TeamMemberList_team_20 :: TeamMemberList
testObject_TeamMemberList_team_20 =
  ( newTeamMemberList
      ( [ TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000100000001"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation = Nothing,
              _legalHoldStatus = UserLegalHoldEnabled
            },
          TeamMember
            { _userId = (Id (fromJust (UUID.fromString "00000001-0000-0001-0000-000100000000"))),
              _permissions = Permissions {_self = fromList [], _copy = fromList []},
              _invitation =
                Just
                  ( (Id (fromJust (UUID.fromString "00000000-0000-0000-0000-000000000000"))),
                    (fromJust (readUTCTimeMillis "1864-05-08T15:41:51.601Z"))
                  ),
              _legalHoldStatus = UserLegalHoldPending
            }
        ]
      )
      (ListComplete)
  )
