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

module API.Teams
  ( tests,
  )
where

import API.SQS
import API.Util
import qualified API.Util as Util
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types.Team.LegalHold (LegalHoldStatus (..), LegalHoldTeamConfig (..))
import Control.Lens hiding ((#), (.=))
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import qualified Data.Currency as Currency
import Data.Id
import Data.List1
import qualified Data.List1 as List1
import Data.Misc (PlainTextPassword (..))
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Galley.Options (optSettings, setFeatureFlags)
import Galley.Types hiding (EventData (..), EventType (..), MemberUpdate (..))
import qualified Galley.Types as Conv
import Galley.Types.Conversations.Roles
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import Galley.Types.Teams.SSO
import Gundeck.Types.Notification
import Imports
import Network.HTTP.Types.Status (status403)
import qualified Network.Wai.Utilities.Error as Error
import qualified Network.Wai.Utilities.Error as Wai
import Test.Tasty
import Test.Tasty.Cannon ((#), TimeoutUnit (..))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers (test)
import TestSetup (TestM, TestSetup, tsCannon, tsGConf, tsGalley)
import UnliftIO (mapConcurrently, mapConcurrently_)

tests :: IO TestSetup -> TestTree
tests s =
  testGroup "Teams API" $
    [ test s "create team" testCreateTeam,
      test s "create multiple binding teams fail" testCreateMulitpleBindingTeams,
      test s "create binding team with currency" testCreateBindingTeamWithCurrency,
      test s "create team with members" testCreateTeamWithMembers,
      testGroup "List Team Members" $
        [ test s "a member should be able to list their team" testListTeamMembersDefaultLimit,
          test s "the list should be limited to the number requested (hard truncation is not tested here)" testListTeamMembersTruncated
        ],
      testGroup "List Team members unchecked" $
        [test s "the list should be truncated" testUncheckedListTeamMembers],
      test s "enable/disable SSO" testEnableSSOPerTeam,
      test s "create 1-1 conversation between non-binding team members (fail)" testCreateOne2OneFailNonBindingTeamMembers,
      test s "create 1-1 conversation between binding team members" (testCreateOne2OneWithMembers RoleMember),
      test s "create 1-1 conversation between binding team members as partner" (testCreateOne2OneWithMembers RoleExternalPartner),
      test s "add new team member" testAddTeamMember,
      test s "add new team member binding teams" testAddTeamMemberCheckBound,
      test s "add new team member internal" testAddTeamMemberInternal,
      test s "remove team member" testRemoveTeamMember,
      test s "remove team member (binding, owner has passwd)" (testRemoveBindingTeamMember True),
      test s "remove team member (binding, owner has no passwd)" (testRemoveBindingTeamMember False),
      test s "add team conversation (no role as argument)" testAddTeamConvLegacy,
      test s "add team conversation with role" testAddTeamConvWithRole,
      test s "add team conversation as partner (fail)" testAddTeamConvAsExternalPartner,
      test s "add managed conversation through public endpoint (fail)" testAddManagedConv,
      test s "add managed team conversation ignores given users" testAddTeamConvWithUsers,
      -- Queue is emptied here to ensure that lingering events do not affect other tests
      test s "add team member to conversation without connection" (testAddTeamMemberToConv >> ensureQueueEmpty),
      test s "update conversation as member" (testUpdateTeamConv RoleMember roleNameWireAdmin),
      test s "update conversation as partner" (testUpdateTeamConv RoleExternalPartner roleNameWireMember),
      test s "delete non-binding team" testDeleteTeam,
      test s "delete binding team (owner has passwd)" (testDeleteBindingTeam True),
      test s "delete binding team (owner has no passwd)" (testDeleteBindingTeam False),
      test s "delete team conversation" testDeleteTeamConv,
      test s "update team data" testUpdateTeam,
      test s "update team member" testUpdateTeamMember,
      test s "update team status" testUpdateTeamStatus,
      test s "post crypto broadcast message json" postCryptoBroadcastMessageJson,
      test s "post crypto broadcast message protobuf" postCryptoBroadcastMessageProto,
      test s "post crypto broadcast message redundant/missing" postCryptoBroadcastMessageJson2,
      test s "post crypto broadcast message no-team" postCryptoBroadcastMessageNoTeam,
      test s "post crypto broadcast message 100 (or max conns)" postCryptoBroadcastMessage100OrMaxConns,
      test s "feature flags" testFeatureFlags,
      testGroup "get team size" $
        [ test s "it should return the team size when it is less than the limit" testTeamSize,
          test s "it should return that the team size is larger when it is more than the truncation limit" testTeamSizeTruncated
        ]
    ]

timeout :: WS.Timeout
timeout = 3 # Second

testCreateTeam :: TestM ()
testCreateTeam = do
  c <- view tsCannon
  owner <- Util.randomUser
  WS.bracketR c owner $ \wsOwner -> do
    tid <- Util.createNonBindingTeam "foo" owner []
    team <- Util.getTeam owner tid
    assertQueueEmpty
    liftIO $ do
      assertEqual "owner" owner (team ^. teamCreator)
      eventChecks <- WS.awaitMatch timeout wsOwner $ \notif -> do
        ntfTransient notif @?= False
        let e = List1.head (WS.unpackPayload notif)
        e ^. eventType @?= TeamCreate
        e ^. eventTeam @?= tid
        e ^. eventData @?= Just (EdTeamCreate team)
      void $ WS.assertSuccess eventChecks

testCreateMulitpleBindingTeams :: TestM ()
testCreateMulitpleBindingTeams = do
  g <- view tsGalley
  owner <- Util.randomUser
  _ <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  -- Cannot create more teams if bound (used internal API)
  let nt = NonBindingNewTeam $ newNewTeam (unsafeRange "owner") (unsafeRange "icon")
  post (g . path "/teams" . zUser owner . zConn "conn" . json nt)
    !!! const 403 === statusCode
  -- If never used the internal API, can create multiple teams
  owner' <- Util.randomUser
  void $ Util.createNonBindingTeam "foo" owner' []
  void $ Util.createNonBindingTeam "foo" owner' []

testCreateBindingTeamWithCurrency :: TestM ()
testCreateBindingTeamWithCurrency = do
  _owner <- Util.randomUser
  _ <- Util.createTeamInternal "foo" _owner
  -- Backwards compatible
  assertQueue "create team" (tActivateWithCurrency Nothing)
  -- Ensure currency is properly journaled
  _owner <- Util.randomUser
  _ <- Util.createTeamInternalWithCurrency "foo" _owner Currency.USD
  assertQueue "create team" (tActivateWithCurrency $ Just Currency.USD)

testCreateTeamWithMembers :: TestM ()
testCreateTeamWithMembers = do
  c <- view tsCannon
  owner <- Util.randomUser
  user1 <- Util.randomUser
  user2 <- Util.randomUser
  let pp = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  let m1 = newTeamMember' pp user1
  let m2 = newTeamMember' pp user2
  Util.connectUsers owner (list1 user1 [user2])
  WS.bracketR3 c owner user1 user2 $ \(wsOwner, wsUser1, wsUser2) -> do
    tid <- Util.createNonBindingTeam "foo" owner [m1, m2]
    team <- Util.getTeam owner tid
    mem <- Util.getTeamMembers owner tid
    liftIO $ do
      assertEqual
        "members"
        (Set.fromList [newTeamMember' fullPermissions owner, m1, m2])
        (Set.fromList (mem ^. teamMembers))
      void $ mapConcurrently (checkCreateEvent team) [wsOwner, wsUser1, wsUser2]
  where
    checkCreateEvent team w = WS.assertMatch_ timeout w $ \notif -> do
      ntfTransient notif @?= False
      let e = List1.head (WS.unpackPayload notif)
      e ^. eventType @?= TeamCreate
      e ^. eventTeam @?= (team ^. teamId)
      e ^. eventData @?= Just (EdTeamCreate team)

testListTeamMembersDefaultLimit :: TestM ()
testListTeamMembersDefaultLimit = do
  (owner, tid, [member1, member2]) <- Util.createBindingTeamWithNMembers 2
  listFromServer <- Util.getTeamMembers owner tid
  liftIO $
    assertEqual
      "list members"
      (Set.fromList [owner, member1, member2])
      (Set.fromList (map (^. userId) $ listFromServer ^. teamMembers))
  liftIO $
    assertBool
      "member list indicates that there are no more members"
      (not $ listFromServer ^. teamMemberListHasMore)

testListTeamMembersTruncated :: TestM ()
testListTeamMembersTruncated = do
  (owner, tid, _) <- Util.createBindingTeamWithNMembers 4
  listFromServer <- Util.getTeamMembersTruncated owner tid 2
  liftIO $
    assertEqual
      "member list is not limited to the requested number"
      2
      (length $ listFromServer ^. teamMembers)
  liftIO $
    assertBool
      "member list does not indicate that there are more members"
      (listFromServer ^. teamMemberListHasMore)

testUncheckedListTeamMembers :: TestM ()
testUncheckedListTeamMembers = do
  (_, tid, _) <- Util.createBindingTeamWithNMembers 4
  listFromServer <- Util.getTeamMembersInternalTruncated tid 2
  liftIO $
    assertEqual
      "member list is not limited to the requested number"
      2
      (length $ listFromServer ^. teamMembers)
  liftIO $
    assertBool
      "member list does not indicate that there are more members"
      (listFromServer ^. teamMemberListHasMore)

testTeamSize :: TestM ()
testTeamSize = do
  (_, tid, _) <- Util.createBindingTeamWithNMembers 4
  sizeFromServer <- getTruncatedTeamSize tid hardTruncationLimit
  liftIO $ assertEqual "team size" (mkTruncatedTeamSize hardTruncationLimit 5) sizeFromServer

testTeamSizeTruncated :: TestM ()
testTeamSizeTruncated = do
  (_, tid, _) <- Util.createBindingTeamWithNMembers 4
  sizeFromServer <- getTruncatedTeamSize tid 2
  liftIO $ assertEqual "team size is larger than limit" (mkLargeTeamSize 2) sizeFromServer

testEnableSSOPerTeam :: TestM ()
testEnableSSOPerTeam = do
  owner <- Util.randomUser
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  let check :: HasCallStack => String -> SSOStatus -> TestM ()
      check msg enabledness = do
        SSOTeamConfig status <- responseJsonUnsafe <$> (getSSOEnabledInternal tid <!! testResponse 200 Nothing)
        liftIO $ assertEqual msg enabledness status
  let putSSOEnabledInternalCheckNotImplemented :: HasCallStack => TestM ()
      putSSOEnabledInternalCheckNotImplemented = do
        g <- view tsGalley
        Wai.Error status label _ <-
          responseJsonUnsafe
            <$> put
              ( g
                  . paths ["i", "teams", toByteString' tid, "features", "sso"]
                  . json (SSOTeamConfig SSODisabled)
              )
        liftIO $ do
          assertEqual "bad status" status403 status
          assertEqual "bad label" "not-implemented" label
  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSOEnabledByDefault -> check "Teams should start with SSO enabled" SSOEnabled
    FeatureSSODisabledByDefault -> check "Teams should start with SSO disabled" SSODisabled
  putSSOEnabledInternal tid SSOEnabled
  check "Calling 'putEnabled True' should enable SSO" SSOEnabled
  putSSOEnabledInternalCheckNotImplemented

testCreateOne2OneFailNonBindingTeamMembers :: TestM ()
testCreateOne2OneFailNonBindingTeamMembers = do
  owner <- Util.randomUser
  let p1 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  let p2 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember, AddTeamMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  mem2 <- newTeamMember' p2 <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [mem2 ^. userId])
  tid <- Util.createNonBindingTeam "foo" owner [mem1, mem2]
  -- Cannot create a 1-1 conversation, not connected and in the same team but not binding
  Util.createOne2OneTeamConv (mem1 ^. userId) (mem2 ^. userId) Nothing tid !!! do
    const 404 === statusCode
    const "non-binding-team" === (Error.label . responseJsonUnsafeWithMsg "error label")
  -- Both have a binding team but not the same team
  owner1 <- Util.randomUser
  tid1 <- Util.createTeamInternal "foo" owner1
  assertQueue "create team" tActivate
  owner2 <- Util.randomUser
  void $ Util.createTeamInternal "foo" owner2
  assertQueue "create another team" tActivate
  Util.createOne2OneTeamConv owner1 owner2 Nothing tid1 !!! do
    const 403 === statusCode
    const "non-binding-team-members" === (Error.label . responseJsonUnsafeWithMsg "error label")

testCreateOne2OneWithMembers ::
  HasCallStack =>
  -- | Role of the user who creates the conversation
  Role ->
  TestM ()
testCreateOne2OneWithMembers (rolePermissions -> perms) = do
  c <- view tsCannon
  owner <- Util.randomUser
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  mem1 <- newTeamMember' perms <$> Util.randomUser
  WS.bracketR c (mem1 ^. userId) $ \wsMem1 -> do
    Util.addTeamMemberInternal tid mem1
    checkTeamMemberJoin tid (mem1 ^. userId) wsMem1
    assertQueue "team member join" $ tUpdate 2 [owner]
  void $ retryWhileN 10 repeatIf (Util.createOne2OneTeamConv owner (mem1 ^. userId) Nothing tid)
  -- Recreating a One2One is a no-op, returns a 200
  Util.createOne2OneTeamConv owner (mem1 ^. userId) Nothing tid !!! const 200 === statusCode
  where
    repeatIf :: ResponseLBS -> Bool
    repeatIf r = statusCode r /= 201

testAddTeamMember :: TestM ()
testAddTeamMember = do
  c <- view tsCannon
  g <- view tsGalley
  owner <- Util.randomUser
  let p1 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  let p2 = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember, AddTeamMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  mem2 <- newTeamMember' p2 <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [mem2 ^. userId])
  Util.connectUsers (mem1 ^. userId) (list1 (mem2 ^. userId) [])
  tid <- Util.createNonBindingTeam "foo" owner [mem1, mem2]
  mem3 <- newTeamMember' p1 <$> Util.randomUser
  let payload = json (newNewTeamMember mem3)
  Util.connectUsers (mem1 ^. userId) (list1 (mem3 ^. userId) [])
  Util.connectUsers (mem2 ^. userId) (list1 (mem3 ^. userId) [])
  -- `mem1` lacks permission to add new team members
  post (g . paths ["teams", toByteString' tid, "members"] . zUser (mem1 ^. userId) . payload)
    !!! const 403 === statusCode
  WS.bracketRN c [owner, (mem1 ^. userId), (mem2 ^. userId), (mem3 ^. userId)] $ \[wsOwner, wsMem1, wsMem2, wsMem3] -> do
    -- `mem2` has `AddTeamMember` permission
    Util.addTeamMember (mem2 ^. userId) tid mem3
    mapConcurrently_ (checkTeamMemberJoin tid (mem3 ^. userId)) [wsOwner, wsMem1, wsMem2, wsMem3]

testAddTeamMemberCheckBound :: TestM ()
testAddTeamMemberCheckBound = do
  g <- view tsGalley
  ownerBound <- Util.randomUser
  tidBound <- Util.createTeamInternal "foo" ownerBound
  assertQueue "create team" tActivate
  rndMem <- newTeamMember' (Util.symmPermissions []) <$> Util.randomUser
  -- Cannot add any users to bound teams
  post (g . paths ["teams", toByteString' tidBound, "members"] . zUser ownerBound . zConn "conn" . json (newNewTeamMember rndMem))
    !!! const 403 === statusCode
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  -- Cannot add bound users to any teams
  let boundMem = newTeamMember' (Util.symmPermissions []) ownerBound
  post (g . paths ["teams", toByteString' tid, "members"] . zUser owner . zConn "conn" . json (newNewTeamMember boundMem))
    !!! const 403 === statusCode

testAddTeamMemberInternal :: TestM ()
testAddTeamMemberInternal = do
  c <- view tsCannon
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  let p1 = Util.symmPermissions [GetBilling] -- permissions are irrelevant on internal endpoint
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  WS.bracketRN c [owner, mem1 ^. userId] $ \[wsOwner, wsMem1] -> do
    Util.addTeamMemberInternal tid mem1
    liftIO . void $ mapConcurrently (checkJoinEvent tid (mem1 ^. userId)) [wsOwner, wsMem1]
    assertQueue "tem member join" $ tUpdate 2 [owner]
  void $ Util.getTeamMemberInternal tid (mem1 ^. userId)
  where
    checkJoinEvent tid usr w = WS.assertMatch_ timeout w $ \notif -> do
      ntfTransient notif @?= False
      let e = List1.head (WS.unpackPayload notif)
      e ^. eventType @?= MemberJoin
      e ^. eventTeam @?= tid
      e ^. eventData @?= Just (EdMemberJoin usr)

testRemoveTeamMember :: TestM ()
testRemoveTeamMember = do
  c <- view tsCannon
  g <- view tsGalley
  owner <- Util.randomUser
  let p1 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  let p2 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember, RemoveTeamMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  mem2 <- newTeamMember' p2 <$> Util.randomUser
  mext1 <- Util.randomUser
  mext2 <- Util.randomUser
  mext3 <- Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [mem2 ^. userId, mext1, mext2, mext3])
  tid <- Util.createNonBindingTeam "foo" owner [mem1, mem2]
  -- Managed conversation:
  void $ Util.createManagedConv owner tid [] (Just "gossip") Nothing Nothing
  -- Regular conversation:
  cid2 <- Util.createTeamConv owner tid [mem1 ^. userId, mem2 ^. userId, mext1] (Just "blaa") Nothing Nothing
  -- Member external 2 is a guest and not a part of any conversation that mem1 is a part of
  void $ Util.createTeamConv owner tid [mem2 ^. userId, mext2] (Just "blaa") Nothing Nothing
  -- Member external 3 is a guest and part of a conversation that mem1 is a part of
  cid3 <- Util.createTeamConv owner tid [mem1 ^. userId, mext3] (Just "blaa") Nothing Nothing
  WS.bracketRN c [owner, mem1 ^. userId, mem2 ^. userId, mext1, mext2, mext3] $ \ws@[wsOwner, wsMem1, wsMem2, wsMext1, _wsMext2, wsMext3] -> do
    -- `mem1` lacks permission to remove team members
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem2 ^. userId)]
          . zUser (mem1 ^. userId)
          . zConn "conn"
      )
      !!! const 403
      === statusCode
    -- `mem2` has `RemoveTeamMember` permission
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
          . zUser (mem2 ^. userId)
          . zConn "conn"
      )
      !!! const 200
      === statusCode
    -- Ensure that `mem1` is still a user (tid is not a binding team)
    Util.ensureDeletedState False owner (mem1 ^. userId)
    mapConcurrently_ (checkTeamMemberLeave tid (mem1 ^. userId)) [wsOwner, wsMem1, wsMem2]
    checkConvMemberLeaveEvent cid2 (mem1 ^. userId) wsMext1
    checkConvMemberLeaveEvent cid3 (mem1 ^. userId) wsMext3
    WS.assertNoEvent timeout ws

testRemoveBindingTeamMember :: Bool -> TestM ()
testRemoveBindingTeamMember ownerHasPassword = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser' ownerHasPassword
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  mext <- Util.randomUser
  let p1 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  Util.addTeamMemberInternal tid mem1
  assertQueue "team member join" $ tUpdate 2 [owner]
  Util.connectUsers owner (singleton mext)
  cid1 <- Util.createTeamConv owner tid [(mem1 ^. userId), mext] (Just "blaa") Nothing Nothing
  when ownerHasPassword $ do
    -- Deleting from a binding team with empty body is invalid
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
          . zUser owner
          . zConn "conn"
      )
      !!! const 400
      === statusCode
    -- Deleting from a binding team without a password is forbidden
    delete
      ( g
          . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
          . zUser owner
          . zConn "conn"
          . json (newTeamMemberDeleteData Nothing)
      )
      !!! const 403
      === statusCode
  -- Deleting from a binding team with wrong password
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
        . zUser owner
        . zConn "conn"
        . json (newTeamMemberDeleteData (Just $ PlainTextPassword "wrong passwd"))
    )
    !!! do
      const 403 === statusCode
      const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  -- Mem1 is still part of Wire
  Util.ensureDeletedState False owner (mem1 ^. userId)
  WS.bracketR2 c owner mext $ \(wsOwner, wsMext) -> do
    if ownerHasPassword
      then do
        delete
          ( g
              . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
              . zUser owner
              . zConn "conn"
              . json (newTeamMemberDeleteData (Just $ Util.defPassword))
          )
          !!! const 202
          === statusCode
      else do
        -- Deleting from a binding team without a password is fine if the owner is
        -- authenticated, but has none.
        delete
          ( g
              . paths ["teams", toByteString' tid, "members", toByteString' (mem1 ^. userId)]
              . zUser owner
              . zConn "conn"
              . json (newTeamMemberDeleteData Nothing)
          )
          !!! const 202
          === statusCode
    checkTeamMemberLeave tid (mem1 ^. userId) wsOwner
    checkConvMemberLeaveEvent cid1 (mem1 ^. userId) wsMext
    assertQueue "team member leave" $ tUpdate 1 [owner]
    WS.assertNoEvent timeout [wsMext]
    -- Mem1 is now gone from Wire
    Util.ensureDeletedState True owner (mem1 ^. userId)

testAddTeamConvLegacy :: TestM ()
testAddTeamConvLegacy = do
  c <- view tsCannon
  owner <- Util.randomUser
  extern <- Util.randomUser
  let p = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  mem1 <- newTeamMember' p <$> Util.randomUser
  mem2 <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [extern, mem2 ^. userId])
  tid <- Util.createNonBindingTeam "foo" owner [mem2]
  let allUserIds = [owner, extern, mem1 ^. userId, mem2 ^. userId]
  WS.bracketRN c allUserIds $ \wss -> do
    cid <- Util.createTeamConvLegacy owner tid allUserIds (Just "blaa")
    mapM_ (checkConvCreateEvent cid) wss
    -- All members become admin by default
    mapM_ (assertConvMemberWithRole roleNameWireAdmin cid) allUserIds

testAddTeamConvWithRole :: TestM ()
testAddTeamConvWithRole = do
  c <- view tsCannon
  owner <- Util.randomUser
  extern <- Util.randomUser
  let p = Util.symmPermissions [CreateConversation, DoNotUseDeprecatedAddRemoveConvMember]
  mem1 <- newTeamMember' p <$> Util.randomUser
  mem2 <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers owner (list1 (mem1 ^. userId) [extern, mem2 ^. userId])
  tid <- Util.createNonBindingTeam "foo" owner [mem2]
  WS.bracketRN c [owner, extern, mem1 ^. userId, mem2 ^. userId] $ \ws@[wsOwner, wsExtern, wsMem1, wsMem2] -> do
    -- Managed conversation:
    cid1 <- Util.createManagedConv owner tid [] (Just "gossip") Nothing Nothing
    checkConvCreateEvent cid1 wsOwner
    checkConvCreateEvent cid1 wsMem2
    -- Regular conversation:
    cid2 <- Util.createTeamConvWithRole owner tid [extern] (Just "blaa") Nothing Nothing roleNameWireAdmin
    checkConvCreateEvent cid2 wsOwner
    checkConvCreateEvent cid2 wsExtern
    mapM_ (assertConvMemberWithRole roleNameWireAdmin cid2) [owner, extern]
    -- Regular conversation (using member role for participants):
    cid3 <- Util.createTeamConvWithRole owner tid [extern] (Just "blaa") Nothing Nothing roleNameWireMember
    checkConvCreateEvent cid3 wsOwner
    checkConvCreateEvent cid3 wsExtern
    assertConvMemberWithRole roleNameWireAdmin cid3 owner
    assertConvMemberWithRole roleNameWireMember cid3 extern
    -- mem2 is not a conversation member and no longer receives
    -- an event that a new team conversation has been created

    Util.addTeamMember owner tid mem1
    checkTeamMemberJoin tid (mem1 ^. userId) wsOwner
    checkTeamMemberJoin tid (mem1 ^. userId) wsMem1
    checkTeamMemberJoin tid (mem1 ^. userId) wsMem2
    -- New team members are added automatically to managed conversations ...
    Util.assertConvMember (mem1 ^. userId) cid1
    -- ... but not to regular ones.
    Util.assertNotConvMember (mem1 ^. userId) cid2
    -- Managed team conversations get all team members added implicitly.
    cid4 <- Util.createManagedConv owner tid [] (Just "blup") Nothing Nothing
    for_ [owner, mem1 ^. userId, mem2 ^. userId] $ \u ->
      Util.assertConvMember u cid4
    checkConvCreateEvent cid4 wsOwner
    checkConvCreateEvent cid4 wsMem1
    checkConvCreateEvent cid4 wsMem2
    -- Non team members are never added implicitly.
    for_ [cid1, cid4] $
      Util.assertNotConvMember extern
    WS.assertNoEvent timeout ws

testAddTeamConvAsExternalPartner :: TestM ()
testAddTeamConvAsExternalPartner = do
  owner <- Util.randomUser
  memMember1 <- newTeamMember' (rolePermissions RoleMember) <$> Util.randomUser
  memMember2 <- newTeamMember' (rolePermissions RoleMember) <$> Util.randomUser
  memExternalPartner <- newTeamMember' (rolePermissions RoleExternalPartner) <$> Util.randomUser
  Util.connectUsers
    owner
    (list1 (memMember1 ^. userId) [memExternalPartner ^. userId, memMember2 ^. userId])
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  forM_ [(2, memMember1), (3, memMember2), (4, memExternalPartner)] $ \(i, mem) -> do
    Util.addTeamMemberInternal tid mem
    assertQueue ("team member join #" ++ show i) $ tUpdate i [owner]
  let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
  Util.createTeamConvAccessRaw
    (memExternalPartner ^. userId)
    tid
    [memMember1 ^. userId, memMember2 ^. userId]
    (Just "blaa")
    acc
    (Just TeamAccessRole)
    Nothing
    Nothing
    !!! do
      const 403 === statusCode
      const "operation-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")

testAddManagedConv :: TestM ()
testAddManagedConv = do
  g <- view tsGalley
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  let tinfo = ConvTeamInfo tid True
  let conv =
        NewConvManaged $
          NewConv [makeIdOpaque owner] (Just "blah") (Set.fromList []) Nothing (Just tinfo) Nothing Nothing roleNameWireAdmin
  post
    ( g
        . path "/conversations"
        . zUser owner
        . zConn "conn"
        . zType "access"
        . json conv
    )
    !!! const 400 === statusCode

testAddTeamConvWithUsers :: TestM ()
testAddTeamConvWithUsers = do
  owner <- Util.randomUser
  extern <- Util.randomUser
  Util.connectUsers owner (list1 extern [])
  tid <- Util.createNonBindingTeam "foo" owner []
  -- Create managed team conversation and erroneously specify external users.
  cid <- Util.createManagedConv owner tid [extern] (Just "gossip") Nothing Nothing
  -- External users have been ignored.
  Util.assertNotConvMember extern cid
  -- Team members are present.
  Util.assertConvMember owner cid

testAddTeamMemberToConv :: TestM ()
testAddTeamMemberToConv = do
  personalUser <- Util.randomUser
  ownerT1 <- Util.randomUser
  let p = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem1T1 <- newTeamMember' p <$> Util.randomUser
  mem2T1 <- newTeamMember' p <$> Util.randomUser
  mem3T1 <- newTeamMember' (Util.symmPermissions []) <$> Util.randomUser
  mem4T1 <- newTeamMember' (Util.symmPermissions []) <$> Util.randomUser
  ownerT2 <- Util.randomUser
  mem1T2 <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers ownerT1 (list1 (mem1T1 ^. userId) [mem2T1 ^. userId, mem3T1 ^. userId, ownerT2, personalUser])
  tidT1 <- Util.createNonBindingTeam "foo" ownerT1 [mem1T1, mem2T1, mem3T1]
  tidT2 <- Util.createTeamInternal "foo" ownerT2
  _ <- Util.addTeamMemberInternal tidT2 mem1T2
  -- Team owners create new regular team conversation:
  cidT1 <- Util.createTeamConv ownerT1 tidT1 [] (Just "blaa") Nothing Nothing
  cidT2 <- Util.createTeamConv ownerT2 tidT2 [] (Just "blaa") Nothing Nothing
  cidPersonal <- decodeConvId <$> Util.postConv personalUser [] (Just "blaa") [] Nothing Nothing
  -- NOTE: This functionality was _changed_ as there was no need for it...
  -- mem1T1 (who is *not* a member of the new conversation) can *not* add other team members
  -- despite being a team member and having the permission `DoNotUseDeprecatedAddRemoveConvMember`.
  Util.assertNotConvMember (mem1T1 ^. userId) cidT1
  Util.postMembers (mem1T1 ^. userId) (list1 (mem2T1 ^. userId) []) cidT1 !!! const 404 === statusCode
  Util.assertNotConvMember (mem2T1 ^. userId) cidT1
  -- OTOH, mem3T1 _can_ add another team member despite lacking the required team permission
  -- since conversation roles trump any team roles. Note that all users are admins by default
  Util.assertConvMember ownerT1 cidT1
  Util.postMembers ownerT1 (list1 (mem2T1 ^. userId) []) cidT1 !!! const 200 === statusCode
  Util.assertConvMember (mem2T1 ^. userId) cidT1
  -- The following tests check the logic: users can add other users to a conversation
  -- iff:
  --    - *the adding user is connected to the users being added*
  --    OR
  --    - *the adding user is part of the team of the users being added*

  -- Now we add someone from T2 that we are connected to
  Util.postMembers ownerT1 (list1 ownerT2 []) cidT1 !!! const 200 === statusCode
  Util.assertConvMember ownerT2 cidT1
  -- And they can add their own team members
  Util.postMembers ownerT2 (list1 (mem1T2 ^. userId) []) cidT1 !!! const 200 === statusCode
  Util.assertConvMember (mem1T2 ^. userId) cidT1
  -- Still, they cannot add random members without a connection from T1, despite the conversation being "hosted" there
  Util.postMembers ownerT2 (list1 (mem4T1 ^. userId) []) cidT1 !!! const 403 === statusCode
  Util.assertNotConvMember (mem4T1 ^. userId) cidT1
  -- Now let's look at convs hosted on team2
  -- ownerT2 *is* connected to ownerT1
  Util.postMembers ownerT2 (list1 ownerT1 []) cidT2 !!! const 200 === statusCode
  Util.assertConvMember ownerT1 cidT2
  -- and mem1T2 is on the same team, but mem1T1 is *not*
  Util.postMembers ownerT2 (list1 (mem1T2 ^. userId) [mem1T1 ^. userId]) cidT2 !!! const 403 === statusCode
  Util.assertNotConvMember (mem1T1 ^. userId) cidT2
  Util.assertNotConvMember (mem1T2 ^. userId) cidT2
  -- mem1T2 is on the same team, so that is fine too
  Util.postMembers ownerT2 (list1 (mem1T2 ^. userId) []) cidT2 !!! const 200 === statusCode
  Util.assertConvMember (mem1T2 ^. userId) cidT2
  -- ownerT2 is *NOT* connected to mem3T1 and not on the same team, so should not be allowed to add
  Util.postMembers ownerT2 (list1 (mem3T1 ^. userId) []) cidT2 !!! const 403 === statusCode
  Util.assertNotConvMember (mem3T1 ^. userId) cidT2
  -- For personal conversations, same logic applies

  -- Can add connected users
  Util.postMembers personalUser (list1 ownerT1 []) cidPersonal !!! const 200 === statusCode
  Util.assertConvMember ownerT1 cidPersonal
  -- Can *not* add users that are *not* connected
  Util.postMembers personalUser (list1 ownerT2 []) cidPersonal !!! const 403 === statusCode
  Util.assertNotConvMember ownerT2 cidPersonal
  -- Users of the same team can add one another
  Util.postMembers ownerT1 (list1 (mem1T1 ^. userId) []) cidPersonal !!! const 200 === statusCode
  Util.assertConvMember (mem1T1 ^. userId) cidPersonal
  -- Users can not add across teams if *not* connected
  Util.postMembers (mem1T1 ^. userId) (list1 ownerT2 []) cidPersonal !!! const 403 === statusCode
  Util.assertNotConvMember ownerT2 cidPersonal
  -- Users *can* add across teams if *connected*
  Util.postMembers ownerT1 (list1 ownerT2 []) cidPersonal !!! const 200 === statusCode
  Util.assertConvMember ownerT2 cidPersonal

testUpdateTeamConv ::
  -- | Team role of the user who creates the conversation
  Role ->
  -- | Conversation role of the user who creates the conversation
  RoleName ->
  TestM ()
testUpdateTeamConv (rolePermissions -> perms) convRole = do
  owner <- Util.randomUser
  member <- Util.randomUser
  Util.connectUsers owner (list1 member [])
  tid <- Util.createNonBindingTeam "foo" owner [newTeamMember member perms Nothing]
  cid <- Util.createTeamConvWithRole owner tid [member] (Just "gossip") Nothing Nothing convRole
  resp <- updateTeamConv member cid (ConversationRename "not gossip")
  -- FUTUREWORK: Ensure that the team role _really_ does not matter
  liftIO $ assertEqual "status conv" convRoleCheck (statusCode resp)
  where
    convRoleCheck = if isActionAllowed ModifyConversationName convRole == Just True then 200 else 403

testDeleteTeam :: TestM ()
testDeleteTeam = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser
  let p = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  member <- newTeamMember' p <$> Util.randomUser
  extern <- Util.randomUser
  Util.connectUsers owner (list1 (member ^. userId) [extern])
  tid <- Util.createNonBindingTeam "foo" owner [member]
  cid1 <- Util.createTeamConv owner tid [] (Just "blaa") Nothing Nothing
  cid2 <- Util.createManagedConv owner tid [] (Just "blup") Nothing Nothing
  Util.assertConvMember owner cid2
  Util.assertConvMember (member ^. userId) cid2
  Util.assertNotConvMember extern cid2
  Util.postMembers owner (list1 extern []) cid1 !!! const 200 === statusCode
  Util.assertConvMember owner cid1
  Util.assertConvMember extern cid1
  Util.assertNotConvMember (member ^. userId) cid1
  void $ WS.bracketR3 c owner extern (member ^. userId) $ \(wsOwner, wsExtern, wsMember) -> do
    delete (g . paths ["teams", toByteString' tid] . zUser owner . zConn "conn")
      !!! const 202 === statusCode
    checkTeamDeleteEvent tid wsOwner
    checkTeamDeleteEvent tid wsMember
    -- team members should not receive conversation delete events
    checkConvDeleteEvent cid1 wsExtern
    WS.assertNoEvent timeout [wsOwner, wsExtern, wsMember]
  get (g . paths ["teams", toByteString' tid] . zUser owner)
    !!! const 404 === statusCode
  get (g . paths ["teams", toByteString' tid, "members"] . zUser owner)
    !!! const 403 === statusCode
  get (g . paths ["teams", toByteString' tid, "conversations"] . zUser owner)
    !!! const 403 === statusCode
  for_ [owner, extern, member ^. userId] $ \u -> do
    -- Ensure no user got deleted
    Util.ensureDeletedState False owner u
    for_ [cid1, cid2] $ \x -> do
      Util.getConv u x !!! const 404 === statusCode
      Util.getSelfMember u x !!! do
        const 200 === statusCode
        const (Just Null) === responseJsonMaybe
  assertQueueEmpty

testDeleteBindingTeam :: Bool -> TestM ()
testDeleteBindingTeam ownerHasPassword = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser' ownerHasPassword
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  let p1 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem1 <- newTeamMember' p1 <$> Util.randomUser
  let p2 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem2 <- newTeamMember' p2 <$> Util.randomUser
  let p3 = Util.symmPermissions [DoNotUseDeprecatedAddRemoveConvMember]
  mem3 <- newTeamMember' p3 <$> Util.randomUser
  Util.addTeamMemberInternal tid mem1
  assertQueue "team member join 2" $ tUpdate 2 [owner]
  Util.addTeamMemberInternal tid mem2
  assertQueue "team member join 3" $ tUpdate 3 [owner]
  Util.addTeamMemberInternal tid mem3
  assertQueue "team member join 4" $ tUpdate 4 [owner]
  extern <- Util.randomUser
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json (newTeamDeleteData (Just $ PlainTextPassword "wrong passwd"))
    )
    !!! do
      const 403 === statusCode
      const "access-denied" === (Error.label . responseJsonUnsafeWithMsg "error label")
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' (mem3 ^. userId)]
        . zUser owner
        . zConn "conn"
        . json
          ( newTeamMemberDeleteData
              ( if ownerHasPassword
                  then Just Util.defPassword
                  else Nothing
              )
          )
    )
    !!! const 202
    === statusCode
  assertQueue "team member leave 1" $ tUpdate 3 [owner]
  void $ WS.bracketRN c [owner, (mem1 ^. userId), (mem2 ^. userId), extern] $ \[wsOwner, wsMember1, wsMember2, wsExtern] -> do
    delete
      ( g
          . paths ["teams", toByteString' tid]
          . zUser owner
          . zConn "conn"
          . json
            ( newTeamDeleteData
                ( if ownerHasPassword
                    then Just Util.defPassword
                    else Nothing
                )
            )
      )
      !!! const 202
      === statusCode
    checkUserDeleteEvent owner wsOwner
    checkUserDeleteEvent (mem1 ^. userId) wsMember1
    checkUserDeleteEvent (mem2 ^. userId) wsMember2
    checkTeamDeleteEvent tid wsOwner
    checkTeamDeleteEvent tid wsMember1
    checkTeamDeleteEvent tid wsMember2
    WS.assertNoEvent (1 # Second) [wsExtern]
    -- Note that given the async nature of team deletion, we may
    -- have other events in the queue (such as TEAM_UPDATE)
    tryAssertQueue 10 "team delete, should be there" tDelete
  forM_ [owner, (mem1 ^. userId), (mem2 ^. userId)] $
    -- Ensure users are marked as deleted; since we already
    -- received the event, should _really_ be deleted
    Util.ensureDeletedState True extern
  -- Let's clean it up, just in case
  ensureQueueEmpty

testDeleteTeamConv :: TestM ()
testDeleteTeamConv = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser
  let p = Util.symmPermissions [DoNotUseDeprecatedDeleteConversation]
  member <- newTeamMember' p <$> Util.randomUser
  extern <- Util.randomUser
  Util.connectUsers owner (list1 (member ^. userId) [extern])
  tid <- Util.createNonBindingTeam "foo" owner [member]
  cid1 <- Util.createTeamConv owner tid [] (Just "blaa") Nothing Nothing
  let access = ConversationAccessUpdate [InviteAccess, CodeAccess] ActivatedAccessRole
  putAccessUpdate owner cid1 access !!! const 200 === statusCode
  code <- decodeConvCodeEvent <$> (postConvCode owner cid1 <!! const 201 === statusCode)
  cid2 <- Util.createManagedConv owner tid [] (Just "blup") Nothing Nothing
  Util.postMembers owner (list1 extern [member ^. userId]) cid1 !!! const 200 === statusCode
  for_ [owner, member ^. userId, extern] $ \u -> Util.assertConvMember u cid1
  for_ [owner, member ^. userId] $ \u -> Util.assertConvMember u cid2
  WS.bracketR3 c owner extern (member ^. userId) $ \(wsOwner, wsExtern, wsMember) -> do
    delete
      ( g
          . paths ["teams", toByteString' tid, "conversations", toByteString' cid2]
          . zUser (member ^. userId)
          . zConn "conn"
      )
      !!! const 200
      === statusCode
    -- We no longer send duplicate conv deletion events
    -- i.e., as both a regular "conversation.delete" to all
    -- conversation members and as "team.conversation-delete"
    -- to all team members not part of the conversation
    checkConvDeleteEvent cid2 wsOwner
    checkConvDeleteEvent cid2 wsMember
    WS.assertNoEvent timeout [wsOwner, wsMember]
    delete
      ( g
          . paths ["teams", toByteString' tid, "conversations", toByteString' cid1]
          . zUser (member ^. userId)
          . zConn "conn"
      )
      !!! const 200
      === statusCode
    -- We no longer send duplicate conv deletion events
    -- i.e., as both a regular "conversation.delete" to all
    -- conversation members and as "team.conversation-delete"
    -- to all team members not part of the conversation
    checkConvDeleteEvent cid1 wsOwner
    checkConvDeleteEvent cid1 wsMember
    checkConvDeleteEvent cid1 wsExtern
    WS.assertNoEvent timeout [wsOwner, wsMember, wsExtern]
  for_ [cid1, cid2] $ \x ->
    for_ [owner, member ^. userId, extern] $ \u -> do
      Util.getConv u x !!! const 404 === statusCode
      Util.assertNotConvMember u x
  postConvCodeCheck code !!! const 404 === statusCode

testUpdateTeam :: TestM ()
testUpdateTeam = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser
  let p = Util.symmPermissions [DoNotUseDeprecatedDeleteConversation]
  member <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers owner (list1 (member ^. userId) [])
  tid <- Util.createNonBindingTeam "foo" owner [member]
  let bad = object ["name" .= T.replicate 100 "too large"]
  put
    ( g
        . paths ["teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json bad
    )
    !!! const 400
    === statusCode
  let u =
        newTeamUpdateData
          & nameUpdate .~ (Just $ unsafeRange "bar")
          & iconUpdate .~ (Just $ unsafeRange "xxx")
          & iconKeyUpdate .~ (Just $ unsafeRange "yyy")
  WS.bracketR2 c owner (member ^. userId) $ \(wsOwner, wsMember) -> do
    put
      ( g
          . paths ["teams", toByteString' tid]
          . zUser owner
          . zConn "conn"
          . json u
      )
      !!! const 200
      === statusCode
    checkTeamUpdateEvent tid u wsOwner
    checkTeamUpdateEvent tid u wsMember
    WS.assertNoEvent timeout [wsOwner, wsMember]
  where
    checkTeamUpdateEvent tid upd w = WS.assertMatch_ timeout w $ \notif -> do
      ntfTransient notif @?= False
      let e = List1.head (WS.unpackPayload notif)
      e ^. eventType @?= TeamUpdate
      e ^. eventTeam @?= tid
      e ^. eventData @?= Just (EdTeamUpdate upd)

testUpdateTeamMember :: TestM ()
testUpdateTeamMember = do
  g <- view tsGalley
  c <- view tsCannon
  owner <- Util.randomUser
  let p = Util.symmPermissions [SetMemberPermissions]
  member <- newTeamMember' p <$> Util.randomUser
  Util.connectUsers owner (list1 (member ^. userId) [])
  tid <- Util.createNonBindingTeam "foo" owner [member]
  -- Must have at least 1 member with full permissions
  let changeOwner = newNewTeamMember (newTeamMember' p owner)
  put
    ( g
        . paths ["teams", toByteString' tid, "members"]
        . zUser (member ^. userId)
        . zConn "conn"
        . json changeOwner
    )
    !!! do
      const 403 === statusCode
      const "no-other-owner" === (Error.label . responseJsonUnsafeWithMsg "error label")
  let changeMember = newNewTeamMember (member & permissions .~ fullPermissions)
  WS.bracketR2 c owner (member ^. userId) $ \(wsOwner, wsMember) -> do
    put
      ( g
          . paths ["teams", toByteString' tid, "members"]
          . zUser owner
          . zConn "conn"
          . json changeMember
      )
      !!! const 200
      === statusCode
    member' <- Util.getTeamMember owner tid (member ^. userId)
    liftIO $ assertEqual "permissions" (member' ^. permissions) (changeMember ^. ntmNewTeamMember . permissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsOwner (pure fullPermissions)
    checkTeamMemberUpdateEvent tid (member ^. userId) wsMember (pure fullPermissions)
    WS.assertNoEvent timeout [wsOwner, wsMember]
  -- Now that the other member has full permissions, it can demote the owner
  WS.bracketR2 c (member ^. userId) owner $ \(wsMember, wsOwner) -> do
    put
      ( g
          . paths ["teams", toByteString' tid, "members"]
          . zUser (member ^. userId)
          . zConn "conn"
          . json changeOwner
      )
      !!! const 200
      === statusCode
    owner' <- Util.getTeamMember (member ^. userId) tid owner
    liftIO $ assertEqual "permissions" (owner' ^. permissions) (changeOwner ^. ntmNewTeamMember . permissions)
    -- owner no longer has GetPermissions, but she can still see the update because it's about her!
    checkTeamMemberUpdateEvent tid owner wsOwner (pure p)
    checkTeamMemberUpdateEvent tid owner wsMember (pure p)
    WS.assertNoEvent timeout [wsOwner, wsMember]
  assertQueueEmpty
  where
    checkTeamMemberUpdateEvent tid uid w mPerm = WS.assertMatch_ timeout w $ \notif -> do
      ntfTransient notif @?= False
      let e = List1.head (WS.unpackPayload notif)
      e ^. eventType @?= MemberUpdate
      e ^. eventTeam @?= tid
      e ^. eventData @?= Just (EdMemberUpdate uid mPerm)

testUpdateTeamStatus :: TestM ()
testUpdateTeamStatus = do
  g <- view tsGalley
  owner <- Util.randomUser
  tid <- Util.createTeamInternal "foo" owner
  assertQueue "create team" tActivate
  -- Check for idempotency
  Util.changeTeamStatus tid Active
  assertQueueEmpty
  Util.changeTeamStatus tid Suspended
  assertQueue "suspend first time" tSuspend
  Util.changeTeamStatus tid Suspended
  assertQueueEmpty
  Util.changeTeamStatus tid Suspended
  assertQueueEmpty
  Util.changeTeamStatus tid Active
  assertQueue "activate again" tActivate
  void $
    put
      ( g
          . paths ["i", "teams", toByteString' tid, "status"]
          . json (TeamStatusUpdate Deleted Nothing)
      )
      !!! do
        const 403 === statusCode
        const "invalid-team-status-update" === (Error.label . responseJsonUnsafeWithMsg "error label")

checkUserDeleteEvent :: HasCallStack => UserId -> WS.WebSocket -> TestM ()
checkUserDeleteEvent uid w = WS.assertMatch_ timeout w $ \notif -> do
  let j = Object $ List1.head (ntfPayload notif)
  let etype = j ^? key "type" . _String
  let euser = j ^? key "id" . _String
  etype @?= Just "user.delete"
  euser @?= Just (UUID.toText (toUUID uid))

checkTeamMemberJoin :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberJoin tid uid w = WS.awaitMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventType @?= MemberJoin
  e ^. eventTeam @?= tid
  e ^. eventData @?= Just (EdMemberJoin uid)

checkTeamMemberLeave :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberLeave tid usr w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventType @?= MemberLeave
  e ^. eventTeam @?= tid
  e ^. eventData @?= Just (EdMemberLeave usr)

checkConvCreateEvent :: HasCallStack => ConvId -> WS.WebSocket -> TestM ()
checkConvCreateEvent cid w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtType e @?= Conv.ConvCreate
  case evtData e of
    Just (Conv.EdConversation x) -> cnvId x @?= cid
    other -> assertFailure $ "Unexpected event data: " <> show other

checkTeamDeleteEvent :: HasCallStack => TeamId -> WS.WebSocket -> TestM ()
checkTeamDeleteEvent tid w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventType @?= TeamDelete
  e ^. eventTeam @?= tid
  e ^. eventData @?= Nothing

checkConvDeleteEvent :: HasCallStack => ConvId -> WS.WebSocket -> TestM ()
checkConvDeleteEvent cid w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtType e @?= Conv.ConvDelete
  evtConv e @?= cid
  evtData e @?= Nothing

checkConvMemberLeaveEvent :: HasCallStack => ConvId -> UserId -> WS.WebSocket -> TestM ()
checkConvMemberLeaveEvent cid usr w = WS.assertMatch_ timeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtConv e @?= cid
  evtType e @?= Conv.MemberLeave
  case evtData e of
    Just (Conv.EdMembersLeave mm) -> mm @?= Conv.UserIdList [usr]
    other -> assertFailure $ "Unexpected event data: " <> show other

postCryptoBroadcastMessageJson :: TestM ()
postCryptoBroadcastMessageJson = do
  c <- view tsCannon
  -- Team1: Alice, Bob. Team2: Charlie. Regular user: Dan. Connect Alice,Charlie,Dan
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (charlie, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  (dan, dc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 charlie [dan])
  tid1 <- createTeamInternal "foo" alice
  assertQueue "" tActivate
  addTeamMemberInternal tid1 $ newTeamMember' (symmPermissions []) bob
  assertQueue "" $ tUpdate 2 [alice]
  _ <- createTeamInternal "foo" charlie
  assertQueue "" tActivate
  -- A second client for Alice
  ac2 <- randomClient alice (someLastPrekeys !! 4)
  -- Complete: Alice broadcasts a message to Bob,Charlie,Dan and herself
  let t = 1 # Second -- WS receive timeout
  let msg = [(alice, ac2, "ciphertext0"), (bob, bc, "ciphertext1"), (charlie, cc, "ciphertext2"), (dan, dc, "ciphertext3")]
  WS.bracketRN c [bob, charlie, dan] $ \[wsB, wsC, wsD] ->
    -- Alice's clients 1 and 2 listen to their own messages only
    WS.bracketR (c . queryItem "client" (toByteString' ac2)) alice $ \wsA2 ->
      WS.bracketR (c . queryItem "client" (toByteString' ac)) alice $ \wsA1 -> do
        Util.postOtrBroadcastMessage id alice ac msg !!! do
          const 201 === statusCode
          assertTrue_ (eqMismatch [] [] [] . responseJsonUnsafe)
        -- Bob should get the broadcast (team member of alice)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext1")
        -- Charlie should get the broadcast (contact of alice and user of teams feature)
        void . liftIO $ WS.assertMatch t wsC (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext2")
        -- Dan should get the broadcast (contact of alice and not user of teams feature)
        void . liftIO $ WS.assertMatch t wsD (wsAssertOtr (selfConv dan) alice ac dc "ciphertext3")
        -- Alice's first client should not get the broadcast
        assertNoMsg wsA1 (wsAssertOtr (selfConv alice) alice ac ac "ciphertext0")
        -- Alice's second client should get the broadcast
        void . liftIO $ WS.assertMatch t wsA2 (wsAssertOtr (selfConv alice) alice ac ac2 "ciphertext0")

postCryptoBroadcastMessageJson2 :: TestM ()
postCryptoBroadcastMessageJson2 = do
  c <- view tsCannon
  -- Team1: Alice, Bob. Team2: Charlie. Connect Alice,Charlie
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (charlie, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 charlie [])
  tid1 <- createTeamInternal "foo" alice
  assertQueue "" tActivate
  addTeamMemberInternal tid1 $ newTeamMember' (symmPermissions []) bob
  assertQueue "" $ tUpdate 2 [alice]
  let t = 3 # Second -- WS receive timeout
      -- Missing charlie
  let m1 = [(bob, bc, "ciphertext1")]
  Util.postOtrBroadcastMessage id alice ac m1 !!! do
    const 412 === statusCode
    assertTrue "1: Only Charlie and his device" (eqMismatch [(charlie, Set.singleton cc)] [] [] . responseJsonUnsafe)
  -- Complete
  WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, "ciphertext2"), (charlie, cc, "ciphertext2")]
    Util.postOtrBroadcastMessage id alice ac m2 !!! do
      const 201 === statusCode
      assertTrue "No devices expected" (eqMismatch [] [] [] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext2")
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext2")
  -- Redundant self
  WS.bracketR3 c alice bob charlie $ \(wsA, wsB, wsE) -> do
    let m3 = [(alice, ac, "ciphertext3"), (bob, bc, "ciphertext3"), (charlie, cc, "ciphertext3")]
    Util.postOtrBroadcastMessage id alice ac m3 !!! do
      const 201 === statusCode
      assertTrue "2: Only Alice and her device" (eqMismatch [] [(alice, Set.singleton ac)] [] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext3")
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext3")
    -- Alice should not get it
    assertNoMsg wsA (wsAssertOtr (selfConv alice) alice ac ac "ciphertext3")
  -- Deleted charlie
  WS.bracketR2 c bob charlie $ \(wsB, wsE) -> do
    deleteClient charlie cc (Just defPassword) !!! const 200 === statusCode
    let m4 = [(bob, bc, "ciphertext4"), (charlie, cc, "ciphertext4")]
    Util.postOtrBroadcastMessage id alice ac m4 !!! do
      const 201 === statusCode
      assertTrue "3: Only Charlie and his device" (eqMismatch [] [] [(charlie, Set.singleton cc)] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr (selfConv bob) alice ac bc "ciphertext4")
    -- charlie should not get it
    assertNoMsg wsE (wsAssertOtr (selfConv charlie) alice ac cc "ciphertext4")

postCryptoBroadcastMessageProto :: TestM ()
postCryptoBroadcastMessageProto = do
  -- similar to postCryptoBroadcastMessageJson except uses protobuf

  -- Team1: Alice, Bob. Team2: Charlie. Regular user: Dan. Connect Alice,Charlie,Dan
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (charlie, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  (dan, dc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 charlie [dan])
  tid1 <- createTeamInternal "foo" alice
  assertQueue "" tActivate
  addTeamMemberInternal tid1 $ newTeamMember' (symmPermissions []) bob
  assertQueue "" $ tUpdate 2 [alice]
  _ <- createTeamInternal "foo" charlie
  assertQueue "" tActivate
  -- Complete: Alice broadcasts a message to Bob,Charlie,Dan
  let t = 1 # Second -- WS receive timeout
  let ciphertext = encodeCiphertext "hello bob"
  WS.bracketRN c [alice, bob, charlie, dan] $ \ws@[_, wsB, wsC, wsD] -> do
    let msg = otrRecipients [(bob, [(bc, ciphertext)]), (charlie, [(cc, ciphertext)]), (dan, [(dc, ciphertext)])]
    Util.postProtoOtrBroadcast alice ac msg !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [] [] . responseJsonUnsafe)
    -- Bob should get the broadcast (team member of alice)
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr' (encodeCiphertext "data") (selfConv bob) alice ac bc ciphertext)
    -- Charlie should get the broadcast (contact of alice and user of teams feature)
    void . liftIO $ WS.assertMatch t wsC (wsAssertOtr' (encodeCiphertext "data") (selfConv charlie) alice ac cc ciphertext)
    -- Dan should get the broadcast (contact of alice and not user of teams feature)
    void . liftIO $ WS.assertMatch t wsD (wsAssertOtr' (encodeCiphertext "data") (selfConv dan) alice ac dc ciphertext)
    -- Alice should not get her own broadcast
    WS.assertNoEvent timeout ws

postCryptoBroadcastMessageNoTeam :: TestM ()
postCryptoBroadcastMessageNoTeam = do
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  connectUsers alice (list1 bob [])
  let msg = [(bob, bc, "ciphertext1")]
  Util.postOtrBroadcastMessage id alice ac msg !!! const 404 === statusCode

postCryptoBroadcastMessage100OrMaxConns :: TestM ()
postCryptoBroadcastMessage100OrMaxConns = do
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (someLastPrekeys !! 0)
  _ <- createTeamInternal "foo" alice
  assertQueue "" tActivate
  ((bob, bc), others) <- createAndConnectUserWhileLimitNotReached alice (100 :: Int) [] (someLastPrekeys !! 1)
  connectUsers alice (list1 bob (fst <$> others))
  let t = 3 # Second -- WS receive timeout
  WS.bracketRN c (bob : (fst <$> others)) $ \ws -> do
    let f (u, clt) = (u, clt, "ciphertext")
    let msg = (bob, bc, "ciphertext") : (f <$> others)
    Util.postOtrBroadcastMessage id alice ac msg !!! do
      const 201 === statusCode
      assertTrue_ (eqMismatch [] [] [] . responseJsonUnsafe)
    void . liftIO $ WS.assertMatch t (Imports.head ws) (wsAssertOtr (selfConv bob) alice ac bc "ciphertext")
    for_ (zip (tail ws) others) $ \(wsU, (u, clt)) ->
      liftIO $ WS.assertMatch t wsU (wsAssertOtr (selfConv u) alice ac clt "ciphertext")
  where
    createAndConnectUserWhileLimitNotReached alice remaining acc pk = do
      (uid, cid) <- randomUserWithClient pk
      (r1, r2) <- List1.head <$> connectUsersUnchecked alice (singleton uid)
      case (statusCode r1, statusCode r2, remaining, acc) of
        (201, 200, 0, []) -> error "Need to connect with at least 1 user"
        (201, 200, 0, (x : xs)) -> return (x, xs)
        (201, 200, _, _) -> createAndConnectUserWhileLimitNotReached alice (remaining -1) ((uid, cid) : acc) pk
        (403, 403, _, []) -> error "Need to connect with at least 1 user"
        (403, 403, _, (x : xs)) -> return (x, xs)
        (xxx, yyy, _, _) -> error ("Unexpected while connecting users: " ++ show xxx ++ " and " ++ show yyy)

newTeamMember' :: Permissions -> UserId -> TeamMember
newTeamMember' perms uid = newTeamMember uid perms Nothing

getSSOEnabled :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getSSOEnabled uid tid = do
  g <- view tsGalley
  get $
    g
      . paths ["teams", toByteString' tid, "features", "sso"]
      . zUser uid

getSSOEnabledInternal :: HasCallStack => TeamId -> TestM ResponseLBS
getSSOEnabledInternal tid = do
  g <- view tsGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", "sso"]

putSSOEnabledInternal :: HasCallStack => TeamId -> SSOStatus -> TestM ()
putSSOEnabledInternal tid enabled = do
  g <- view tsGalley
  void . put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "sso"]
      . json (SSOTeamConfig enabled)
      . expect2xx

getLegalHoldEnabled :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getLegalHoldEnabled uid tid = do
  g <- view tsGalley
  get $
    g
      . paths ["teams", toByteString' tid, "features", "legalhold"]
      . zUser uid

getLegalHoldEnabledInternal :: HasCallStack => TeamId -> TestM ResponseLBS
getLegalHoldEnabledInternal tid = do
  g <- view tsGalley
  get $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]

putLegalHoldEnabledInternal :: HasCallStack => TeamId -> LegalHoldStatus -> TestM ()
putLegalHoldEnabledInternal = putLegalHoldEnabledInternal' expect2xx

putLegalHoldEnabledInternal' :: HasCallStack => (Request -> Request) -> TeamId -> LegalHoldStatus -> TestM ()
putLegalHoldEnabledInternal' reqmod tid enabled = do
  g <- view tsGalley
  void . put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . json (LegalHoldTeamConfig enabled)
      . reqmod

testFeatureFlags :: TestM ()
testFeatureFlags = do
  owner <- Util.randomUser
  tid <- Util.createNonBindingTeam "foo" owner []
  -- sso

  let getSSO :: HasCallStack => SSOStatus -> TestM ()
      getSSO expected = getSSOEnabled owner tid !!! do
        statusCode === const 200
        responseJsonEither === const (Right (SSOTeamConfig expected))
      getSSOInternal :: HasCallStack => SSOStatus -> TestM ()
      getSSOInternal expected = getSSOEnabledInternal tid !!! do
        statusCode === const 200
        responseJsonEither === const (Right (SSOTeamConfig expected))
      setSSOInternal :: HasCallStack => SSOStatus -> TestM ()
      setSSOInternal = putSSOEnabledInternal tid
  featureSSO <- view (tsGConf . optSettings . setFeatureFlags . flagSSO)
  case featureSSO of
    FeatureSSODisabledByDefault -> do
      getSSO SSODisabled
      getSSOInternal SSODisabled
      setSSOInternal SSOEnabled
      getSSO SSOEnabled
      getSSOInternal SSOEnabled
    FeatureSSOEnabledByDefault -> do
      -- since we don't allow to disable (see 'disableSsoNotImplemented'), we can't test
      -- much here.  (disable failure is covered in "enable/disable SSO" above.)
      getSSO SSOEnabled
      getSSOInternal SSOEnabled
  -- legalhold

  let getLegalHold :: HasCallStack => LegalHoldStatus -> TestM ()
      getLegalHold expected = getLegalHoldEnabled owner tid !!! do
        statusCode === const 200
        responseJsonEither === const (Right (LegalHoldTeamConfig expected))
      getLegalHoldInternal :: HasCallStack => LegalHoldStatus -> TestM ()
      getLegalHoldInternal expected = getLegalHoldEnabledInternal tid !!! do
        statusCode === const 200
        responseJsonEither === const (Right (LegalHoldTeamConfig expected))
      setLegalHoldInternal :: HasCallStack => LegalHoldStatus -> TestM ()
      setLegalHoldInternal = putLegalHoldEnabledInternal tid
  getLegalHold LegalHoldDisabled
  getLegalHoldInternal LegalHoldDisabled
  featureLegalHold <- view (tsGConf . optSettings . setFeatureFlags . flagLegalHold)
  case featureLegalHold of
    FeatureLegalHoldDisabledByDefault -> do
      setLegalHoldInternal LegalHoldEnabled
      getLegalHold LegalHoldEnabled
      getLegalHoldInternal LegalHoldEnabled
    FeatureLegalHoldDisabledPermanently -> do
      putLegalHoldEnabledInternal' expect4xx tid LegalHoldEnabled
