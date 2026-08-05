{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.One2OneTeamConv where

import API.Galley
import Control.Retry (constantDelay, limitRetries, retrying)
import Notifications (isTeamMemberJoinNotif)
import SetupHelpers
import Testlib.Prelude

-- | Team member roles that the one2one team conversation test is parametrized
-- over (the standard non-owner roles). Each constructor is enumerated into a
-- separate test case by the test discovery machinery, mirroring the way the
-- original galley test invoked @testCreateOne2OneWithMembers@ once per role.
data One2OneRole = One2OneMember | One2OnePartner
  deriving stock (Generic, Eq, Show)

-- | Map a parametrized role to the team-role string accepted by
-- 'createTeamMember' (see @Wire.API.Team.Role@: @member@, @partner@). The
-- original galley test covered @RoleMember@ and @RoleExternalPartner@.
one2OneRoleName :: One2OneRole -> String
one2OneRoleName = \case
  One2OneMember -> "member"
  One2OnePartner -> "partner"

-- | An owner adds a second team member with the given role and creates a
-- binding one2one team conversation with them. Ported from
-- @services/galley/test/integration/API/Teams.hs@
-- (@testCreateOne2OneWithMembers@).
testCreateOne2OneWithMembers :: (HasCallStack) => One2OneRole -> App ()
testCreateOne2OneWithMembers memberRole = do
  (owner, tid, []) <- createTeam OwnDomain 1
  teamMember <-
    withWebSockets [owner] $ \[wsOwner] -> do
      m <- createTeamMember owner def {role = one2OneRoleName memberRole}
      -- The owner is notified of the new member joining the team. This mirrors
      -- @checkTeamMemberJoin@ in the galley test suite.
      memberJoin <- awaitMatch isTeamMemberJoinNotif wsOwner
      memberJoin %. "payload.0.team" `shouldMatch` tid
      memberJoin %. "payload.0.data.user" `shouldMatch` objId m
      -- The original test additionally asserts a @team.update@ event via the
      -- galley SQS team-event queue (@assertTeamUpdate tid 2 [owner]@). That
      -- queue is galley-test-specific and has no websocket / Testlib
      -- equivalent (galley only pushes @team.member-join@ over websockets on a
      -- member join), so we verify the updated team membership directly.
      bindResponse (getTeamMembers owner tid) $ \resp -> do
        resp.status `shouldMatchInt` 200
        members <- resp.json %. "members" >>= asList
        length members `shouldMatchInt` 2
      pure m
  -- Creating the one2one team conversation is eventually consistent: retry
  -- while the response is not 201, mirroring @retryWhileN 10 repeatIf@ in the
  -- original test.
  bindResponse
    ( retrying
        (constantDelay 500_000 <> limitRetries 10)
        (\_ resp -> pure (resp.status /= 201))
        (const (postOne2OneConversation owner teamMember tid ""))
    )
    $ \resp -> resp.status `shouldMatchInt` 201
  -- Recreating the one2one is a no-op and returns 200.
  bindResponse (postOne2OneConversation owner teamMember tid "") $ \resp ->
    resp.status `shouldMatchInt` 200

-- | Two owners each create their own binding team. Attempting to create a
-- one2one team conversation with a member of a different (binding) team fails
-- with @non-binding-team-members@. Ported from
-- @testCreateOne2OneFailForNonTeamMembers@.
testCreateOne2OneFailForNonTeamMembers :: (HasCallStack) => App ()
testCreateOne2OneFailForNonTeamMembers = do
  (owner1, tid1, []) <- createTeam OwnDomain 1
  (owner2, _tid2, []) <- createTeam OwnDomain 1
  postOne2OneConversation owner1 owner2 tid1 ""
    >>= assertLabel 403 "non-binding-team-members"
