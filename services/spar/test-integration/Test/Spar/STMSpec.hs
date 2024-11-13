-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.STMSpec
  ( spec,
  )
where

import Test.Hspec
import Imports

spec :: Spec
spec = pure ()

----------------------------------------------------------------------

-- FUTUREWORK:
-- - team owners
-- - personal accounts

data TeamManage
  = TeamCreate
  | TeamNewIdP
  | TeamUpdateIdP
  | TeamReplaceIdP
  | TeamRemoveIdP
  | TeamNewScim
  | TeamRemoveScim

data CreateTeamMember
  = CreateTeamManagementInv
  | CreateSamlAuto
  | CreateScim

data Auth
  = AuthViaPassword
  | AuthViaSaml

-- | No preconds, except that at least one user exists that can be read.
data ReadUserState
  = ScimGetUser
  | ScimFindUser
  | MembersCsv
  | MembersSearch
  | BrigGetUser
  | CassandraAll -- read brig, galley, spar, and make sure all data in all tables is as predicted by the model.

data UpdateUserState

-- | The model keeps deleted users around so they can still try to authenticate and have the
-- preditced experience (403).
data DeleteUser
  = DeleteViaTeamSettings
  | DeleteViaScim
  | SelfDelete -- never possible for team members?
