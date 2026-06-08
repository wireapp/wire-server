-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Test.AdminlessGroups where

import API.Galley
import API.GalleyInternal
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testOnLastAdminLeaveReturnEligibleMembers :: (HasCallStack) => App ()
testOnLastAdminLeaveReturnEligibleMembers = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  clients@[alice1, _] <- traverse (createMLSClient def) [alice, bob]
  for_ clients (uploadNewKeyPackage def)

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

  -- Create an MLS team conversation for the owner, then add a second team
  -- member to it. The second member is the eligible fallback if the owner
  -- tries to leave as the last admin.
  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  -- Attempt to leave the conversation as the last admin.
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "adminless-conversation"
    eligibleMembers <- resp.json %. "eligible_members" & asList
    expected <- bob %. "qualified_id"
    eligibleMembers `shouldMatchSet` [expected]
