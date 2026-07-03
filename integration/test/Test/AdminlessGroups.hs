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
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import MLS.Util
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

  -- a remote user is not eligible
  remoteUser <- randomUser OtherDomain def
  connectTwoUsers alice remoteUser

  -- app is not eligible
  let newApp :: NewApp
      newApp = def {name = "some-app", description = "non-eligible app member"}
  app <- bindResponse (createApp alice tid newApp) $ \resp -> do
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
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "deletionTimeout" .= (1 :: Int),
                "reminderTimeouts" .= ([0] :: [Int]),
                "promotionStrategy" .= "random"
              ]
        ]
    )
    >>= assertSuccess

  alice1 <- createMLSClient def alice
  void $ uploadNewKeyPackage def alice1

  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  convIdText <- conv %. "qualified_id.id" & asString
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [] >>= sendAndConsumeCommitBundle

  now <- liftIO getCurrentTime

  -- alice leaves the conversation, no error, group will be marked for deletion
  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

  deletionJobs <- listArbiterJobs alice "adminless_deletion_jobs" >>= getJSON 200
  deletionJobs %. "jobs"
    & asList >>= \jobs -> do
      jobInfo <- forM jobs $ \job -> do
        jobTeamId <- job %. "payload.team_id" & asString
        jobConversationId <- job %. "payload.conversation_id" & asString
        notVisibleUntil <- job %. "notVisibleUntil" & asString
        pure (job, jobTeamId, jobConversationId, notVisibleUntil)
      let matchingJobs = [job | (job, jobTeamId, jobConversationId, _) <- jobInfo, jobTeamId == tid && jobConversationId == convIdText]
      assertBool
        ( "expected one deletion job for team "
            <> show tid
            <> " and conversation "
            <> convIdText
            <> ", but saw: "
            <> show [(team, conversation, visibleUntil) | (_, team, conversation, visibleUntil) <- jobInfo]
        )
        (length matchingJobs == 1)
      [job] <- pure matchingJobs
      assertJobTimeout now (1 :: Int) job

  reminderJobs <- listArbiterJobs alice "adminless_reminder_jobs" >>= getJSON 200
  reminderJobs %. "jobs"
    & asList >>= \jobs -> do
      jobInfo <- forM jobs $ \job -> do
        jobTeamId <- job %. "payload.team_id" & asString
        jobConversationId <- job %. "payload.conversation_id" & asString
        notVisibleUntil <- job %. "notVisibleUntil" & asString
        pure (job, jobTeamId, jobConversationId, notVisibleUntil)
      let matchingJobs = [job | (job, jobTeamId, jobConversationId, _) <- jobInfo, jobTeamId == tid && jobConversationId == convIdText]
      assertBool
        ( "expected one reminder job for team "
            <> show tid
            <> " and conversation "
            <> convIdText
            <> ", but saw: "
            <> show [(team, conversation, visibleUntil) | (_, team, conversation, visibleUntil) <- jobInfo]
        )
        (length matchingJobs == 1)
      [job] <- pure matchingJobs
      assertJobTimeout now (1 :: Int) job
  where
    assertJobTimeout scheduledAt expectedDays job = do
      notVisibleUntilStr <- job %. "notVisibleUntil" & asString
      notVisibleUntil <- assertJust ("expected ISO 8601 timestamp, got: " <> notVisibleUntilStr) $ iso8601ParseM @Maybe @UTCTime notVisibleUntilStr
      let expected = addUTCTime (fromIntegral expectedDays * nominalDay) scheduledAt
          tolerance = 120 :: NominalDiffTime
      assertBool
        ("notVisibleUntil " <> show notVisibleUntil <> " is not within tolerance of expected " <> show expected)
        (notVisibleUntil >= addUTCTime (negate tolerance) expected && notVisibleUntil <= addUTCTime tolerance expected)

    listArbiterJobs user table = do
      req <- baseRequest user Brig Unversioned $ joinHttpPath ["i", "jobs", "api", "v1", table, "jobs"]
      submit "GET" $ req & addQueryParams [("limit", "100"), ("offset", "0")]

testAdminlessJobsExecuteViaArbiterApi :: (HasCallStack) => App ()
testAdminlessJobsExecuteViaArbiterApi = do
  (alice, tid, _) <- createTeam OwnDomain 1

  -- we disable the feature and create an adminless group
  setTeamFeatureLockStatus alice tid "preventAdminlessGroups" "unlocked"
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    (object ["status" .= "disabled"])
    >>= assertSuccess

  alice1 <- createMLSClient def alice
  void $ uploadNewKeyPackage def alice1

  conv <- postConversation alice defMLS {team = Just tid} >>= getJSON 201
  convId <- objConvId conv
  convIdText <- conv %. "qualified_id.id" & asString
  aliceIdText <- alice %. "qualified_id.id" & asString
  createGroup def alice1 convId
  void $ createAddCommit alice1 convId [] >>= sendAndConsumeCommitBundle

  bindResponse (removeMember alice conv alice) $ \resp -> do
    resp.status `shouldMatchInt` 200

  bindResponse (GalleyI.getConversation conv) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- now we enabled the feature so that the job will get executed
  patchTeamFeature
    OwnDomain
    tid
    "preventAdminlessGroups"
    ( object
        [ "status" .= "enabled",
          "config"
            .= object
              [ "deletionTimeout" .= (7 :: Int),
                "reminderTimeouts" .= ([2] :: [Int]),
                "promotionStrategy" .= "random"
              ]
        ]
    )
    >>= assertSuccess

  now <- liftIO getCurrentTime
  let deletionAt = addUTCTime (-5) now

  insertedJob <-
    postArbiterJob
      alice
      "adminless_deletion_jobs"
      [ "payload"
          .= object
            [ "team_id" .= tid,
              "conversation_id" .= convIdText,
              "orig_user_id" .= aliceIdText
            ],
        "groupKey" .= convIdText,
        "priority" .= (0 :: Int),
        "notVisibleUntil" .= deletionAt,
        "dedupKey" .= Null,
        "maxAttempts" .= (3 :: Int)
      ]
      >>= getJSON 200
      >>= (%. "job")
  assertArbiterJobMatches insertedJob tid convIdText deletionAt

  retryT $ do
    bindResponse (GalleyI.getConversation conv) $ \resp -> do
      resp.status `shouldMatchInt` 404
  where
    assertArbiterJobMatches job expectedTeamId expectedConvId expectedNotVisibleUntil = do
      jobTeamId <- job %. "payload.team_id" & asString
      jobConversationId <- job %. "payload.conversation_id" & asString
      jobTeamId `shouldMatch` expectedTeamId
      jobConversationId `shouldMatch` expectedConvId
      assertJobVisibleAt expectedNotVisibleUntil job

    assertJobVisibleAt expected job = do
      notVisibleUntilStr <- job %. "notVisibleUntil" & asString
      notVisibleUntil <- assertJust ("expected ISO 8601 timestamp, got: " <> notVisibleUntilStr) $ iso8601ParseM @Maybe @UTCTime notVisibleUntilStr
      let tolerance = 5 :: NominalDiffTime
      assertBool
        ("notVisibleUntil " <> show notVisibleUntil <> " is not within tolerance of expected " <> show expected)
        (notVisibleUntil >= addUTCTime (negate tolerance) expected && notVisibleUntil <= addUTCTime tolerance expected)

    postArbiterJob user table body = do
      req <- baseRequest user Brig Unversioned $ joinHttpPath ["i", "jobs", "api", "v1", table, "jobs"]
      submit "POST" $ addJSONObject body req

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
