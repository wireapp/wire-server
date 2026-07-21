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

import API.Brig
import API.Galley
import API.GalleyInternal hiding (getConversation)
import qualified API.GalleyInternal as GalleyI
import MLS.Util
import Notifications
import SetupHelpers hiding (deleteUser)
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

  -- ephemeral user is not eligible
  tmpUser <- ephemeralUser OwnDomain

  -- a remote user is not eligible
  remoteUser <- randomUser OtherDomain def
  connectTwoUsers alice remoteUser

  -- app is not eligible
  let newApp :: NewApp
      newApp = def {name = "some-app", description = "non-eligible app member"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  clients@(alice1 : tmpUser1 : _) <- traverse (createMLSClient def) [alice, tmpUser, bob, localUser, remoteUser, app]
  for_ clients (uploadNewKeyPackage def)

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [bob, app, localUser, remoteUser] >>= sendAndConsumeCommitBundle

  (key, code) <- bindResponse (postConversationCode alice conv Nothing Nothing) $ \resp -> do
    res <- getJSON 201 resp
    (,) <$> (res %. "data.key" & asString) <*> (res %. "data.code" & asString)
  postJoinCodeConv tmpUser key code >>= assertSuccess
  void $ createExternalCommit convId tmpUser1 Nothing >>= sendAndConsumeCommitBundle

  GalleyI.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members.others" >>= asList
    actual <- traverse (\m -> m %. "qualified_id") members
    expected <- traverse (\m -> m %. "qualified_id") [alice, tmpUser, bob, localUser, remoteUser, app]
    actual `shouldMatchSet` expected

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
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              -- The reminders are due early (+1s and +2s), while deletion is
              -- later (+10s). This gives Arbiter's 1s polling and serial
              -- grouped-job processing enough room to emit both reminders
              -- before the conversation is deleted.
              [ "deletionTimeoutDuration" .= "10s",
                "reminderTimeoutDurations" .= ["9s", "8s"],
                "promotionStrategy" .= "random"
              ]
        ]
    )
    >>= assertSuccess

  let newApp :: NewApp
      newApp = def {name = "adminless-reminder-app", description = "not eligible for promotion"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  tmpUser <- ephemeralUser OwnDomain

  clients@(alice1 : tmpUser1 : _) <- traverse (createMLSClient def) [alice, tmpUser, app]
  traverse_ (uploadNewKeyPackage def) clients

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [app] >>= sendAndConsumeCommitBundle

  (key, code) <- bindResponse (postConversationCode alice conv Nothing Nothing) $ \resp -> do
    res <- getJSON 201 resp
    (,) <$> (res %. "data.key" & asString) <*> (res %. "data.code" & asString)
  postJoinCodeConv tmpUser key code >>= assertSuccess
  void $ createExternalCommit convId tmpUser1 Nothing >>= sendAndConsumeCommitBundle

  GalleyI.getConversation conv `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    members <- resp.json %. "members.others" >>= asList
    actual <- traverse (\m -> m %. "qualified_id") members
    expected <- traverse (\m -> m %. "qualified_id") [app, alice, tmpUser]
    actual `shouldMatchSet` expected

  withWebSockets [app, tmpUser] $ \[wsApp, wsTmpUser] -> do
    -- alice leaves the conversation, no error, group will be marked for deletion
    bindResponse (removeMember alice conv alice) $ \resp -> do
      resp.status `shouldMatchInt` 200

    void $ awaitNMatches 2 isConvAdminlessReminderNotif wsApp
    void $ awaitNMatches 2 isConvAdminlessReminderNotif wsTmpUser

    -- The deletion event is sent after the conversation has been removed. The
    -- suite's local timeout is only 2s, but this job is scheduled 10s ahead.
    -- Use a longer timeout here and avoid racing the final HTTP assertion.
    void $ awaitMatchFor 15 isConvDeleteNotif wsApp
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 404

testAdminlessSetupOnFeatureEnable :: (HasCallStack) => App ()
testAdminlessSetupOnFeatureEnable = do
  (alice, tid, _) <- createTeam OwnDomain 1

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "disabled"]) >>= assertSuccess

  let newApp :: NewApp
      newApp = def {name = "adminless-setup-app", description = "not eligible for promotion"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  [alice1, app1] <- traverse (createMLSClient def) [alice, app]
  traverse_ (uploadNewKeyPackage def) [alice1, app1]

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [app] >>= sendAndConsumeCommitBundle

  -- The feature is disabled, so leaving the conversation must not schedule a
  -- deletion job. Enabling it afterwards exercises the team reconciliation job.
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (GalleyI.getConversation conv) $ \resp -> do
    resp.status `shouldMatchInt` 200

  withWebSockets [app] $ \[wsApp] -> do
    patchTeamFeature
      OwnDomain
      tid
      "preventAdminlessGroups"
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "deletionTimeoutDuration" .= "5s",
                  "reminderTimeoutDurations" .= ["4s"],
                  "promotionStrategy" .= "random"
                ]
          ]
      )
      >>= assertSuccess

    -- Leave enough margin for the setup job to enqueue both jobs and for them
    -- to be picked up when the integration suite is under load.
    reminder <- awaitMatchFor 20 isConvSystemAdminlessReminderNotif wsApp
    reminder %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    reminder %. "payload.0.data.deletion_scheduled_for" & asString
    void $ awaitMatchFor 20 isConvDeleteNotif wsApp
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 404

testAdminlessSetupSystemMemberUpdate :: (HasCallStack) => App ()
testAdminlessSetupSystemMemberUpdate = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "disabled"]) >>= assertSuccess

  [alice1, bob1] <- traverse (createMLSClient def) [alice, bob]
  traverse_ (uploadNewKeyPackage def) [alice1, bob1]

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [bob] >>= sendAndConsumeCommitBundle

  -- Create an adminless conversation while the feature is disabled. The setup
  -- job will later autopromote bob without an originating user ID.
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

  withWebSockets [bob] $ \[wsBob] -> do
    patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

    notif <- awaitMatchFor 20 isConvSystemMemberUpdateNotif wsBob
    notif %. "payload.0.qualified_conversation" `shouldMatch` objQidObject conv
    notif %. "payload.0.data.qualified_target" `shouldMatch` objQidObject bob
    notif %. "payload.0.data.conversation_role" `shouldMatch` "wire_admin"

    bindResponse (getConversation bob conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"

testAdminlessJobsCancelledOnFeatureDisable :: (HasCallStack) => App ()
testAdminlessJobsCancelledOnFeatureDisable = do
  (alice, tid, _) <- createTeam OwnDomain 1

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "deletionTimeoutDuration" .= "5s",
                "reminderTimeoutDurations" .= ([] :: [String]),
                "promotionStrategy" .= "random"
              ]
        ]
    )
    >>= assertSuccess

  let newApp :: NewApp
      newApp = def {name = "adminless-cancel-app", description = "not eligible for promotion"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  [alice1, app1] <- traverse (createMLSClient def) [alice, app]
  traverse_ (uploadNewKeyPackage def) [alice1, app1]

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [app] >>= sendAndConsumeCommitBundle

  withWebSockets [app] $ \[wsApp] -> do
    -- Leaving schedules deletion while the feature is enabled.
    bindResponse (removeMember alice conv alice) $ \resp -> do
      resp.status `shouldMatchInt` 200

    -- Disabling the feature must cancel the pending deletion before its deadline.
    patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "disabled"]) >>= assertSuccess

    -- Wait beyond the original deadline and worker polling window. The
    -- conversation must remain because the pending job was canceled. Feature
    -- update events are ignored by the matcher.
    result <- awaitNMatchesResultFor 15 1 isConvDeleteNotif wsApp
    result.success `shouldMatch` False
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 200

testAdminlessJobsRecreatedOnFeatureConfigChange :: (HasCallStack) => App ()
testAdminlessJobsRecreatedOnFeatureConfigChange = do
  (alice, tid, _) <- createTeam OwnDomain 1

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "deletionTimeoutDuration" .= "10s",
                "reminderTimeoutDurations" .= ([] :: [String]),
                "promotionStrategy" .= "random"
              ]
        ]
    )
    >>= assertSuccess

  let newApp :: NewApp
      newApp = def {name = "adminless-reschedule-app", description = "not eligible for promotion"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  [alice1, app1] <- traverse (createMLSClient def) [alice, app]
  traverse_ (uploadNewKeyPackage def) [alice1, app1]

  conv <- postConversation alice (allowAll defMLS) {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [app] >>= sendAndConsumeCommitBundle

  withWebSockets [app] $ \[wsApp] -> do
    -- Leaving schedules a deletion using the original timeout.
    bindResponse (removeMember alice conv alice) $ \resp -> do
      resp.status `shouldMatchInt` 200

    -- Changing the configuration must cancel the old job and recreate it
    -- using the new timeout.
    patchTeamFeature
      OwnDomain
      tid
      "preventAdminlessGroups"
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "deletionTimeoutDuration" .= "20s",
                  "reminderTimeoutDurations" .= ([] :: [String]),
                  "promotionStrategy" .= "random"
                ]
          ]
      )
      >>= assertSuccess

    -- If the old job was not canceled, it would delete the conversation after
    -- 10s. Wait past that deadline before checking that it still exists.
    oldJobResult <- awaitNMatchesResultFor 15 1 isConvDeleteNotif wsApp
    oldJobResult.success `shouldMatch` False
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 200

    -- The replacement job uses the new 20s timeout.
    void $ awaitMatchFor 30 isConvDeleteNotif wsApp
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 404

testAdminlessJobCancellationIsTeamScoped :: (HasCallStack) => App ()
testAdminlessJobCancellationIsTeamScoped = do
  (alice, canceledTid, _) <- createTeam OwnDomain 1
  (bob, activeTid, _) <- createTeam OwnDomain 1

  let enabledFeature :: Value
      enabledFeature =
        object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "deletionTimeoutDuration" .= "10s",
                  "reminderTimeoutDurations" .= ([] :: [String]),
                  "promotionStrategy" .= "random"
                ]
          ]

  for_ [(alice, canceledTid), (bob, activeTid)] $ \(owner, tid) -> do
    setTeamFeatureLockStatus owner tid "preventAdminlessGroups" "unlocked"
    patchTeamFeature OwnDomain tid "preventAdminlessGroups" enabledFeature >>= assertSuccess

  let newApp :: String -> NewApp
      newApp name = def {name = name, description = "not eligible for promotion"}
  canceledApp <- bindResponse (createApp alice canceledTid (newApp "adminless-cancel-team-app")) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"
  activeApp <- bindResponse (createApp bob activeTid (newApp "adminless-active-team-app")) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user"

  [alice1, canceledApp1] <- traverse (createMLSClient def) [alice, canceledApp]
  [bob1, activeApp1] <- traverse (createMLSClient def) [bob, activeApp]
  traverse_ (uploadNewKeyPackage def) [alice1, canceledApp1, bob1, activeApp1]

  canceledConv <- postConversation alice (allowAll defMLS) {team = Just canceledTid} >>= getJSON 201
  canceledConvId <- objConvId canceledConv
  createGroup def alice1 canceledConvId
  void $ createAddCommit alice1 canceledConvId [canceledApp] >>= sendAndConsumeCommitBundle

  activeConv <- postConversation bob (allowAll defMLS) {team = Just activeTid} >>= getJSON 201
  activeConvId <- objConvId activeConv
  createGroup def bob1 activeConvId
  void $ createAddCommit bob1 activeConvId [activeApp] >>= sendAndConsumeCommitBundle

  withWebSockets [canceledApp, activeApp] $ \[wsCanceled, wsActive] -> do
    -- Schedule one deletion job for each team before disabling only one team.
    bindResponse (removeMember alice canceledConv alice) $ \resp -> do
      resp.status `shouldMatchInt` 200
    bindResponse (removeMember bob activeConv bob) $ \resp -> do
      resp.status `shouldMatchInt` 200

    patchTeamFeature OwnDomain canceledTid "preventAdminlessGroups" (object ["status" .= "disabled"]) >>= assertSuccess

    -- The cancellation query must not affect the other team's job.
    canceledResult <- awaitNMatchesResultFor 20 1 isConvDeleteNotif wsCanceled
    canceledResult.success `shouldMatch` False
    void $ awaitMatchFor 30 isConvDeleteNotif wsActive

    bindResponse (GalleyI.getConversation canceledConv) $ \resp -> do
      resp.status `shouldMatchInt` 200
    bindResponse (GalleyI.getConversation activeConv) $ \resp -> do
      resp.status `shouldMatchInt` 404

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

  -- alice is the only eligible member that remains after charlie (the conversation admin) is removed from the team
  eventually $ do
    bindResponse (getConversation alice conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"
      members <- resp.json %. "members.others" & asList
      shouldBeEmpty members

testOnLastAdminSelfDeletionAutopromotes :: (HasCallStack) => App ()
testOnLastAdminSelfDeletionAutopromotes = do
  (alice, tid, [charlie]) <- createTeam OwnDomain 2

  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature OwnDomain tid "preventAdminlessGroups" (object ["status" .= "enabled"]) >>= assertSuccess

  [alice1, charlie1] <- traverse (createMLSClient def) [alice, charlie]
  traverse_ (uploadNewKeyPackage def) [alice1, charlie1]

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

  void $ deleteUser charlie >>= getBody 200

  eventually $ do
    bindResponse (getConversation alice conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "members.self.conversation_role" `shouldMatch` "wire_admin"
      members <- resp.json %. "members.others" & asList
      shouldBeEmpty members
