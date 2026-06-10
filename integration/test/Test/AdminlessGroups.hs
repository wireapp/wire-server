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

import API.Brig (NewApp (..), createApp)
import API.Galley
import API.GalleyInternal hiding (getConversation)
import MLS.Util
import SetupHelpers
import Testlib.Prelude

testOnLastAdminLeaveReturnEligibleMembers :: (HasCallStack) => App ()
testOnLastAdminLeaveReturnEligibleMembers = do
  -- bob is eligible
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

  -- local user is eligible
  localUser <- randomUser OwnDomain def
  connectTwoUsers alice localUser

  -- a remote user is not eligible
  remoteUser <- randomUser OtherDomain def
  connectTwoUsers alice remoteUser

  -- app is not eligible
  let newApp1 :: NewApp
      newApp1 = def {name = "fallback-app-1", description = "non-eligible app member"}
  app <- bindResponse (createApp alice tid newApp1) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  clients@(alice1 : _) <- traverse (createMLSClient def) [alice, bob, localUser, remoteUser, app]
  for_ clients (uploadNewKeyPackage def)

  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [bob, app, localUser, remoteUser] >>= sendAndConsumeCommitBundle

  assertAttemptToLeaveFails conv alice [bob, localUser]

  -- promote bob to admin
  void $ updateRole alice bob "wire_admin" (conv %. "qualified_id") >>= assertSuccess

  -- attempt to leave should succeed now
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

  assertAttemptToLeaveFails conv bob [localUser]

  -- before bob leaves, we make sure the local user is not an admin
  bindResponse (getConversation localUser conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members.self.conversation_role" `shouldMatch` "wire_member"

  -- in V15 it should be possible to leave (autopromotion should be triggered)
  bindResponse (removeMemberV15 bob conv bob) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- assert autopromotion worked and the local user is an admin now
  bindResponse (getConversation localUser conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"
  where
    assertAttemptToLeaveFails conv user eligible =
      bindResponse (removeMember user conv user) $ \resp -> do
        resp.status `shouldMatchInt` 403
        resp.json %. "label" `shouldMatch` "adminless-conversation"
        eligibleMembers <- resp.json %. "eligible_members" & asList
        expected <- for eligible $ \u -> u %. "qualified_id"
        eligibleMembers `shouldMatchSet` expected

    removeMemberV15 :: (HasCallStack, MakesValue remover, MakesValue conv, MakesValue removed) => remover -> conv -> removed -> App Response
    removeMemberV15 remover qcnv removed = do
      (convDomain, convId) <- objQid qcnv
      (removedDomain, removedId) <- objQid removed
      req <- baseRequest remover Galley (ExplicitVersion 15) (joinHttpPath ["conversations", convDomain, convId, "members", removedDomain, removedId])
      submit "DELETE" req

testOnLastAdminLeaveNoEligibleMembersExist :: (HasCallStack) => App ()
testOnLastAdminLeaveNoEligibleMembersExist = do
  (alice, tid, _) <- createTeam OwnDomain 1

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

  alice1 <- createMLSClient def alice
  void $ uploadNewKeyPackage def alice1

  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [] >>= sendAndConsumeCommitBundle

  -- alice leaves the conversation, no error, group will be marked for deletion
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

testOnLastAdminLeaveFeatureDisabled :: (HasCallStack) => App ()
testOnLastAdminLeaveFeatureDisabled = do
  -- bob is eligible
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "disabled"]) >>= assertSuccess

  clients@(alice1 : _) <- traverse (createMLSClient def) [alice, bob]
  for_ clients (uploadNewKeyPackage def)

  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  -- alice leaves the conversation, no error, no autopromotion
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

testOnLastAdminTeamMemberDeletionAutopromotes :: (HasCallStack) => App ()
testOnLastAdminTeamMemberDeletionAutopromotes = do
  (alice, tid, [charlie]) <- createTeam OwnDomain 2

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

  [alice1, charlie1] <- traverse (createMLSClient def) [alice, charlie]
  traverse_ (uploadNewKeyPackage def) [alice1, charlie1]
  aliceId <- alice %. "qualified_id"

  conv <- postConversation charlie defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def charlie1 convId
  void $ createAddCommit charlie1 convId [alice] >>= sendAndConsumeCommitBundle

  bindResponse (getConversation alice conv) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "members.self.conversation_role" `shouldMatch` "wire_member"
    others <- resp.json %. "members.others" & asList
    [other] <- pure others
    other %. "qualified_id" `shouldMatch` objQidObject charlie
    other %. "conversation_role" `shouldMatch` "wire_admin"

  void $ deleteTeamMember tid alice charlie >>= getBody 202

  -- alice is the only eligible member that remains after charlie (the conversastion admin) is removed from the team
  eventually $ do
    bindResponse (getConversation alice conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"
      members <- resp.json %. "members.others" & asList
      shouldBeEmpty members
