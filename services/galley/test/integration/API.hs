{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API
  ( tests,
  )
where

import qualified API.CustomBackend as CustomBackend
import qualified API.Federation as Federation
import qualified API.MLS
import qualified API.MessageTimer as MessageTimer
import qualified API.Roles as Roles
import API.SQS
import qualified API.Teams as Teams
import qualified API.Teams.Feature as TeamFeature
import qualified API.Teams.LegalHold as Teams.LegalHold
import qualified API.Teams.LegalHold.DisabledByDefault
import API.Util
import qualified API.Util as Util
import API.Util.TeamFeature as TeamFeatures
import qualified API.Util.TeamFeature as Util
import Bilge hiding (head, timeout)
import qualified Bilge
import Bilge.Assert
import Control.Exception (throw)
import Control.Lens (at, ix, preview, view, (.~), (?~))
import Control.Monad.Trans.Maybe
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.Code as Code
import Data.Domain
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import qualified Data.List1 as List1
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Singletons
import qualified Data.Text.Ascii as Ascii
import Data.Time.Clock (getCurrentTime)
import Federator.Discovery (DiscoveryFailure (..))
import Federator.MockServer
import Galley.API.Mapping
import Galley.Options (optFederator)
import Galley.Types.Conversations.Intra
import Galley.Types.Conversations.Members
import Imports
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Util.Options (Endpoint (Endpoint))
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Internal.Notification
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import qualified Wire.API.Team.Feature as Public
import qualified Wire.API.Team.Member as Teams
import Wire.API.User

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Galley integration tests"
    [ Teams.LegalHold.tests s,
      API.Teams.LegalHold.DisabledByDefault.tests s,
      mainTests,
      Teams.tests s,
      MessageTimer.tests s,
      Roles.tests s,
      CustomBackend.tests s,
      TeamFeature.tests s,
      Federation.tests s,
      API.MLS.tests s
    ]
  where
    mainTests =
      testGroup
        "Main Conversations API"
        [ test s "status" status,
          test s "metrics" metrics,
          test s "fetch conversation by qualified ID (v2)" testGetConvQualifiedV2,
          test s "list conversation ids" testListConvIds,
          test s "paginate through conversation ids with v2 API" paginateConvIds,
          test s "paginate through /conversations/list-ids" paginateConvListIds,
          test s "paginate through /conversations/list-ids - page ending at locals and remote domain" paginateConvListIdsPageEndingAtLocalsAndDomain,
          test s "page through conversations" getConvsPagingOk,
          test s "upgrade connect/invite conversation" putConvAcceptOk,
          test s "upgrade conversation retries" putConvAcceptRetry,
          test s "repeat / cancel connect requests" postRepeatConnectConvCancel,
          test s "block/unblock a connect/1-1 conversation" putBlockConvOk,
          test s "conversation meta access" accessConvMeta,
          test s "add members" postMembersOk,
          test s "add existing members" postMembersOk2,
          test s "add past members" postMembersOk3,
          test s "add guest forbidden when no guest access role" postMembersFailNoGuestAccess,
          test s "generate guest link forbidden when no guest or non-team-member access role" generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess,
          test s "fail to add members when not connected" postMembersFail,
          test s "fail to add too many members" postTooManyMembersFail,
          test s "add remote members" testAddRemoteMember,
          test s "post conversations/list/v2" testBulkGetQualifiedConvs,
          test s "add remote members on invalid domain" testAddRemoteMemberInvalidDomain,
          test s "add remote members when federation isn't enabled" testAddRemoteMemberFederationDisabled,
          test s "add remote members when federator is unavailable" testAddRemoteMemberFederationUnavailable,
          test s "other member update role" putOtherMemberOk,
          test s "qualified other member update role" putQualifiedOtherMemberOk,
          test s "member update (otr mute)" putMemberOtrMuteOk,
          test s "member update (otr archive)" putMemberOtrArchiveOk,
          test s "member update (hidden)" putMemberHiddenOk,
          test s "member update (everything b)" putMemberAllOk,
          test s "remote conversation member update (otr mute)" putRemoteConvMemberOtrMuteOk,
          test s "remote conversation member update (otr archive)" putRemoteConvMemberOtrArchiveOk,
          test s "remote conversation member update (otr hidden)" putRemoteConvMemberHiddenOk,
          test s "remote conversation member update (everything)" putRemoteConvMemberAllOk,
          test s "conversation receipt mode update" putReceiptModeOk,
          test s "conversation receipt mode update with remote members" putReceiptModeWithRemotesOk,
          test s "remote conversation receipt mode update" putRemoteReceiptModeOk,
          test s "leave connect conversation" leaveConnectConversation,
          test s "join conversation" postJoinConvOk,
          test s "get code-access conversation information" testJoinCodeConv,
          test s "join code-access conversation" postJoinCodeConvOk,
          test s "convert invite to code-access conversation" postConvertCodeConv,
          test s "convert code to team-access conversation" postConvertTeamConv,
          test s "local and remote guests are removed when access changes" testAccessUpdateGuestRemoved,
          test s "team member can't join via guest link if access role removed" testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved,
          test s "cannot join private conversation" postJoinConvFail,
          test s "revoke guest links for team conversation" testJoinTeamConvGuestLinksDisabled,
          test s "revoke guest links for non-team conversation" testJoinNonTeamConvGuestLinksDisabled,
          test s "get code rejected if guest links disabled" testGetCodeRejectedIfGuestLinksDisabled,
          test s "post code rejected if guest links disabled" testPostCodeRejectedIfGuestLinksDisabled,
          test s "remove user with only local convs" removeUserNoFederation,
          test s "remove user with local and remote convs" removeUser,
          test s "iUpsertOne2OneConversation" testAllOne2OneConversationRequests,
          test s "get guest links status from foreign team conversation" getGuestLinksStatusFromForeignTeamConv
        ]

-------------------------------------------------------------------------------
-- API Tests

status :: TestM ()
status = do
  g <- viewGalley
  get (g . path "/i/status")
    !!! const 200 === statusCode
  Bilge.head (g . path "/i/status")
    !!! const 200 === statusCode

metrics :: TestM ()
metrics = do
  g <- viewGalley
  get (g . path "/i/metrics") !!! do
    const 200 === statusCode
    -- Should contain the request duration metric in its output
    const (Just "TYPE http_request_duration_seconds histogram") =~= responseBody

testGetConvQualifiedV2 :: TestM ()
testGetConvQualifiedV2 = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (list1 bob [])
  conv <-
    responseJsonError
      =<< postConvQualified
        alice
        defNewProteusConv
          { newConvUsers = [bob]
          }
        <!! const 201 === statusCode
  let qcnv = cnvQualifiedId conv
  conv' <-
    fmap (unVersioned @'V2) . responseJsonError
      =<< getConvQualifiedV2 alice qcnv
        <!! const 200 === statusCode
  liftIO $ conv @=? conv'

postJoinConvOk :: TestM ()
postJoinConvOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess, LinkAccess] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    postJoinConv bob conv !!! const 200 === statusCode
    postJoinConv bob conv !!! const 204 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMemberJoinWithRole qconv qbob [qbob] roleNameWireMember

testJoinCodeConv :: TestM ()
testJoinCodeConv = do
  let convName = "gossip"
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  alice <- randomUser
  convId <- decodeConvId <$> postConv alice [] (Just convName) [CodeAccess] (Just noGuestsAccess) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice convId

  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  getJoinCodeConv bob (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither

  -- A user that would not be able to join conversation cannot view it either.
  eve <- ephemeralUser
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const 403 === statusCode

testGetCodeRejectedIfGuestLinksDisabled :: TestM ()
testGetCodeRejectedIfGuestLinksDisabled = do
  galley <- viewGalley
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, GuestAccessRole] []
  let createConvWithGuestLink = do
        convId <- decodeConvId <$> postTeamConv teamId owner [] (Just "testConversation") [CodeAccess] (Just accessRoles) Nothing
        void $ decodeConvCodeEvent <$> postConvCode owner convId
        pure convId
  convId <- createConvWithGuestLink
  let checkGetCode expectedStatus = getConvCode owner convId !!! const expectedStatus === statusCode
  let setStatus tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode

  checkGetCode 200
  setStatus Public.FeatureStatusDisabled
  checkGetCode 409
  setStatus Public.FeatureStatusEnabled
  checkGetCode 200

testPostCodeRejectedIfGuestLinksDisabled :: TestM ()
testPostCodeRejectedIfGuestLinksDisabled = do
  galley <- viewGalley
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  convId <- decodeConvId <$> postTeamConv teamId owner [] (Just "testConversation") [CodeAccess] (Just noGuestsAccess) Nothing
  let checkPostCode expectedStatus = postConvCode owner convId !!! statusCode === const expectedStatus
  let setStatus tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode

  checkPostCode 201
  setStatus Public.FeatureStatusDisabled
  checkPostCode 409
  setStatus Public.FeatureStatusEnabled
  checkPostCode 200

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Check if guests cannot join anymore if guest invite feature was disabled on team level
testJoinTeamConvGuestLinksDisabled :: TestM ()
testJoinTeamConvGuestLinksDisabled = do
  galley <- viewGalley
  let convName = "testConversation"
  (owner, teamId, [alice]) <- Util.createBindingTeamWithNMembers 1
  eve <- ephemeralUser
  bob <- randomUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
  convId <- decodeConvId <$> postTeamConv teamId owner [] (Just convName) [CodeAccess, LinkAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode owner convId

  let checkFeatureStatus fstatus =
        Util.getTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId !!! do
          const 200 === statusCode
          const (Right (Public.withStatus fstatus Public.LockStatusUnlocked Public.GuestLinksConfig Public.FeatureTTLUnlimited)) === responseJsonEither

  -- guest can join if guest link feature is enabled
  checkFeatureStatus Public.FeatureStatusEnabled
  getJoinCodeConv eve (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode
  postConvCodeCheck cCode !!! const 200 === statusCode
  postJoinCodeConv eve cCode !!! const 200 === statusCode
  -- non-team-members can join as well
  postJoinCodeConv bob cCode !!! const 200 === statusCode

  -- disabled guest links feature
  let disabled = Public.WithStatusNoLock Public.FeatureStatusDisabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId disabled !!! do
    const 200 === statusCode

  -- guest can't join if guest link feature is disabled
  eve' <- ephemeralUser
  bob' <- randomUser
  getJoinCodeConv eve' (conversationKey cCode) (conversationCode cCode) !!! do
    const 409 === statusCode
  postConvCodeCheck cCode !!! const 404 === statusCode
  postJoinCodeConv eve' cCode !!! const 409 === statusCode
  -- non-team-members can't join either
  postJoinCodeConv bob' cCode !!! const 409 === statusCode
  -- team members can't join either
  postJoinCodeConv alice cCode !!! const 409 === statusCode
  -- check feature status is still disabled
  checkFeatureStatus Public.FeatureStatusDisabled

  -- after re-enabling, the old link is still valid
  let enabled = Public.WithStatusNoLock Public.FeatureStatusEnabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId enabled !!! do
    const 200 === statusCode
  getJoinCodeConv eve' (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode
  postConvCodeCheck cCode !!! const 200 === statusCode
  postJoinCodeConv eve' cCode !!! const 200 === statusCode
  postJoinCodeConv bob' cCode !!! const 200 === statusCode
  checkFeatureStatus Public.FeatureStatusEnabled

-- @END

testJoinNonTeamConvGuestLinksDisabled :: TestM ()
testJoinNonTeamConvGuestLinksDisabled = do
  galley <- viewGalley
  let convName = "testConversation"
  (owner, teamId, []) <- Util.createBindingTeamWithNMembers 0
  userNotInTeam <- randomUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [NonTeamMemberAccessRole] [GuestAccessRole]
  convId <- decodeConvId <$> postConv owner [] (Just convName) [CodeAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode owner convId

  -- works by default
  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode

  -- for non-team conversations it still works if status is disabled for the team but not server wide
  let tfStatus = Public.WithStatusNoLock Public.FeatureStatusDisabled Public.GuestLinksConfig Public.FeatureTTLUnlimited
  TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley owner teamId tfStatus !!! do
    const 200 === statusCode

  getJoinCodeConv userNotInTeam (conversationKey cCode) (conversationCode cCode) !!! do
    const (Right (ConversationCoverView convId (Just convName))) === responseJsonEither
    const 200 === statusCode

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test case covers a negative check that if access code of a guest link is revoked no further
-- people can join the group conversation. Additionally it covers:
-- Random users can use invite link
-- Reusing previously used link yields same conv (idempotency)
-- Guest can use invite link
-- Guest cannot create invite link
-- Non-admin cannot create invite link
postJoinCodeConvOk :: TestM ()
postJoinCodeConvOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  eve <- ephemeralUser
  dave <- ephemeralUser
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole] [GuestAccessRole]
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [CodeAccess] (Just accessRoles) Nothing
  let qconv = Qualified conv (qDomain qbob)
  cCode <- decodeConvCodeEvent <$> postConvCode alice conv
  -- currently ConversationCode is used both as return type for POST ../code and as body for ../join
  -- POST /code gives code,key,uri
  -- POST /join expects code,key
  -- TODO: Should there be two different types?
  let payload = cCode {conversationUri = Nothing} -- unnecessary step, cCode can be posted as-is also.
      incorrectCode = cCode {conversationCode = Code.Value (unsafeRange (Ascii.encodeBase64Url "incorrect-code"))}
  -- with ActivatedAccess, bob can join, but not eve
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- incorrect code/key does not work
    postJoinCodeConv bob incorrectCode !!! const 404 === statusCode
    -- correct code works
    postJoinCodeConv bob payload !!! const 200 === statusCode
    -- non-admin cannot create invite link
    postConvCode bob conv !!! const 403 === statusCode
    -- test no-op
    postJoinCodeConv bob payload !!! const 204 === statusCode
    -- eve cannot join
    postJoinCodeConv eve payload !!! const 403 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMemberJoinWithRole qconv qbob [qbob] roleNameWireMember
    -- changing access to non-activated should give eve access
    Right accessRolesWithGuests <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
    let nonActivatedAccess = ConversationAccessData (Set.singleton CodeAccess) accessRolesWithGuests
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 200 === statusCode
    postJoinCodeConv eve payload !!! const 200 === statusCode
    -- guest cannot create invite link
    postConvCode eve conv !!! const 403 === statusCode
    -- after removing CodeAccess, no further people can join
    let noCodeAccess = ConversationAccessData (Set.singleton InviteAccess) accessRoles
    putQualifiedAccessUpdate alice qconv noCodeAccess !!! const 200 === statusCode
    postJoinCodeConv dave payload !!! const 404 === statusCode

-- @END

postConvertCodeConv :: TestM ()
postConvertCodeConv = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [InviteAccess] Nothing Nothing
  let qconv = Qualified conv (qDomain qalice)
  -- Cannot do code operations if conversation not in code access
  postConvCode alice conv !!! const 403 === statusCode
  deleteConvCode alice conv !!! const 403 === statusCode
  getConvCode alice conv !!! const 403 === statusCode
  -- cannot change to (Set.fromList [TeamMemberAccessRole]) as not a team conversation
  let teamAccess = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole])
  putQualifiedAccessUpdate alice qconv teamAccess !!! const 403 === statusCode
  -- change access
  WS.bracketR c alice $ \wsA -> do
    let nonActivatedAccess =
          ConversationAccessData
            (Set.fromList [InviteAccess, CodeAccess])
            (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 200 === statusCode
    -- test no-op
    putQualifiedAccessUpdate alice qconv nonActivatedAccess !!! const 204 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA] $
        wsAssertConvAccessUpdate qconv qalice nonActivatedAccess
  -- Create/get/update/delete codes
  getConvCode alice conv !!! const 404 === statusCode
  c1 <- decodeConvCodeEvent <$> (postConvCode alice conv <!! const 201 === statusCode)
  postConvCodeCheck c1 !!! const 200 === statusCode
  c1' <- decodeConvCode <$> (getConvCode alice conv <!! const 200 === statusCode)
  liftIO $ assertEqual "c1 c1' codes should match" c1 c1'
  postConvCode alice conv !!! const 200 === statusCode
  c2 <- decodeConvCode <$> (postConvCode alice conv <!! const 200 === statusCode)
  liftIO $ assertEqual "c1 c2 codes should match" c1 c2
  deleteConvCode alice conv !!! const 200 === statusCode
  getConvCode alice conv !!! const 404 === statusCode
  -- create a new code; then revoking CodeAccess should make existing codes invalid
  void $ postConvCode alice conv
  let noCodeAccess =
        ConversationAccessData
          (Set.singleton InviteAccess)
          (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate alice qconv noCodeAccess !!! const 200 === statusCode
  getConvCode alice conv !!! const 403 === statusCode

postConvertTeamConv :: TestM ()
postConvertTeamConv = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  -- Create a team conversation with team-alice, team-bob, activated-eve
  -- Non-activated mallory can join
  (alice, tid) <- createBindingTeam
  let qalice = Qualified alice localDomain
  bob <- view Teams.userId <$> addUserToTeam alice tid
  assertTeamUpdate "team member (bob) join" tid 2 [alice]
  refreshIndex
  dave <- view Teams.userId <$> addUserToTeam alice tid
  assertTeamUpdate "team member (dave) join" tid 3 [alice]
  refreshIndex
  (eve, qeve) <- randomUserTuple
  connectUsers alice (singleton eve)
  let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
  -- creating a team-only conversation containing eve should fail
  createTeamConvAccessRaw alice tid [bob, eve] (Just "blaa") acc (Just (Set.fromList [TeamMemberAccessRole])) Nothing Nothing
    !!! const 403 === statusCode
  Right accessRoles <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole] []
  -- create conversation allowing any type of guest
  conv <- createTeamConvAccess alice tid [bob, eve] (Just "blaa") acc (Just accessRoles) Nothing Nothing
  -- mallory joins by herself
  mallory <- ephemeralUser
  let qmallory = Qualified mallory localDomain
      qconv = Qualified conv localDomain
  j <- decodeConvCodeEvent <$> postConvCode alice conv
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    postJoinCodeConv mallory j !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE] $
        wsAssertMemberJoinWithRole qconv qmallory [qmallory] roleNameWireMember
  WS.bracketRN c [alice, bob, eve, mallory] $ \[wsA, wsB, wsE, wsM] -> do
    let teamAccess =
          ConversationAccessData
            (Set.fromList [InviteAccess, CodeAccess])
            (Set.fromList [TeamMemberAccessRole])
    putQualifiedAccessUpdate alice qconv teamAccess !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertConvAccessUpdate qconv qalice teamAccess
    -- non-team members get kicked out
    liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice (pure qeve)
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsE, wsM] $
        wsAssertMemberLeave qconv qalice (pure qmallory)
    -- joining (for mallory) is no longer possible
    postJoinCodeConv mallory j !!! const 403 === statusCode
    -- team members (dave) can still join
    postJoinCodeConv dave j !!! const 200 === statusCode

-- @SF.Federation @SF.Separation @TSFI.RESTfulAPI @S2
--
-- The test asserts that, among others, remote users are removed from a
-- conversation when an access update occurs that disallows guests from
-- accessing.
testAccessUpdateGuestRemoved :: TestM ()
testAccessUpdateGuestRemoved = do
  -- alice, bob are in a team
  (tid, alice, [bob]) <- createBindingTeamWithQualifiedMembers 2

  -- charlie is a local guest
  charlie <- randomQualifiedUser
  connectUsers (qUnqualified alice) (pure (qUnqualified charlie))

  -- dee is a remote guest
  let remoteDomain = Domain "far-away.example.com"
  dee <- Qualified <$> randomId <*> pure remoteDomain

  connectWithRemoteUser (qUnqualified alice) dee

  -- they are all in a local conversation
  conv <-
    responseJsonError
      =<< postConvWithRemoteUsers
        (qUnqualified alice)
        defNewProteusConv
          { newConvQualifiedUsers = [bob, charlie, dee],
            newConvTeam = Just (ConvTeamInfo tid)
          }
        <!! const 201 === statusCode

  c <- view tsCannon
  WS.bracketRN c (map qUnqualified [alice, bob, charlie]) $ \[wsA, wsB, wsC] -> do
    -- conversation access role changes to team only
    (_, reqs) <- withTempMockFederator' (mockReply ()) $ do
      putQualifiedAccessUpdate
        (qUnqualified alice)
        (cnvQualifiedId conv)
        (ConversationAccessData mempty (Set.fromList [TeamMemberAccessRole]))
        !!! const 200 === statusCode

      -- charlie and dee are kicked out
      --
      -- note that removing users happens asynchronously, so this check should
      -- happen while the mock federator is still available
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave (cnvQualifiedId conv) alice [charlie]
      WS.assertMatchN_ (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave (cnvQualifiedId conv) alice [dee]

    -- dee's remote receives a notification
    let compareLists [] ys = [] @?= ys
        compareLists (x : xs) ys = case break (== x) ys of
          (ys1, _ : ys2) -> compareLists xs (ys1 <> ys2)
          _ -> assertFailure $ "Could not find " <> show x <> " in " <> show ys
    liftIO $
      compareLists
        ( map
            ( \fr -> do
                cu <- eitherDecode (frBody fr)
                pure (F.cuOrigUserId cu, F.cuAction cu)
            )
            ( filter
                ( \fr ->
                    frComponent fr == Galley
                      && frRPC fr == "on-conversation-updated"
                )
                reqs
            )
        )
        [ Right (alice, SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure charlie)),
          Right (alice, SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure dee)),
          Right
            ( alice,
              SomeConversationAction
                (sing @'ConversationAccessDataTag)
                ConversationAccessData
                  { cupAccess = mempty,
                    cupAccessRoles = Set.fromList [TeamMemberAccessRole]
                  }
            )
        ]

  -- only alice and bob remain
  conv2 <-
    responseJsonError
      =<< getConvQualified (qUnqualified alice) (cnvQualifiedId conv)
        <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv2)) @?= [bob]

-- @END

testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved :: TestM ()
testTeamMemberCantJoinViaGuestLinkIfAccessRoleRemoved = do
  -- given alice, bob, charlie and dee are in a team
  (alice, tid, [bob, charlie, dee]) <- createBindingTeamWithNMembers 3

  -- and given alice and bob are in a team conversation and alice created a guest link
  let accessRoles = Set.fromList [TeamMemberAccessRole, GuestAccessRole, ServiceAccessRole]
  qconvId <- decodeQualifiedConvId <$> postTeamConv tid alice [bob] (Just "chit chat") [CodeAccess] (Just accessRoles) Nothing
  cCode <- decodeConvCodeEvent <$> postConvCode alice (qUnqualified qconvId)

  -- then charlie can join via the guest link
  postJoinCodeConv charlie cCode !!! const 200 === statusCode

  -- when the guests are disabled for the conversation
  let accessData = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate alice qconvId accessData !!! const 200 === statusCode

  -- then dee cannot join via guest link
  postJoinCodeConv dee cCode !!! const 404 === statusCode

getGuestLinksStatusFromForeignTeamConv :: TestM ()
getGuestLinksStatusFromForeignTeamConv = do
  localDomain <- viewFederationDomain
  galley <- viewGalley
  let setTeamStatus u tid tfStatus =
        TeamFeatures.putTeamFeatureFlagWithGalley @Public.GuestLinksConfig galley u tid (Public.WithStatusNoLock tfStatus Public.GuestLinksConfig Public.FeatureTTLUnlimited) !!! do
          const 200 === statusCode
  let checkGuestLinksStatus u c s =
        getGuestLinkStatus galley u c !!! do
          const 200 === statusCode
          const s === (Public.wsStatus . (responseJsonUnsafe @(Public.WithStatus Public.GuestLinksConfig)))
  let checkGetGuestLinksStatus s u c =
        getGuestLinkStatus galley u c !!! do
          const s === statusCode

  -- given alice is in team A with guest links allowed
  (alice, teamA, [alex]) <- createBindingTeamWithNMembers 1
  let qalice = Qualified alice localDomain
  setTeamStatus alice teamA Public.FeatureStatusEnabled

  -- and given bob is in team B with guest links disallowed
  (bob, teamB, [bert]) <- createBindingTeamWithNMembers 1
  let qbert = Qualified bert localDomain
  setTeamStatus bob teamB Public.FeatureStatusDisabled

  -- and given alice and bob are connected
  connectUsers alice (singleton bob)

  -- and given bob creates a conversation, invites alice, and makes her group admin
  let accessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole]
  conv <- decodeConvId <$> postTeamConv teamB bob [] (Just "teams b's conversation") [InviteAccess] (Just accessRoles) Nothing
  let qconv = Qualified conv localDomain
  postMembersWithRole bob (pure qalice) qconv roleNameWireAdmin !!! const 200 === statusCode

  -- when alice gets the guest link status for the conversation
  -- then the status should be disabled
  checkGuestLinksStatus alice conv Public.FeatureStatusDisabled

  -- when bob gets the guest link status for the conversation
  -- then the status should be disabled
  checkGuestLinksStatus bob conv Public.FeatureStatusDisabled

  -- when bob enables guest links for his team and gets the guest link status for the conversation
  setTeamStatus bob teamB Public.FeatureStatusEnabled

  -- then the status should be enabled
  checkGuestLinksStatus bob conv Public.FeatureStatusEnabled

  -- when alice gets the guest link status for the conversation
  -- then the status should be enabled
  checkGuestLinksStatus alice conv Public.FeatureStatusEnabled

  -- when alice disables guest links for her team and gets the guest link status for the conversation
  setTeamStatus alice teamA Public.FeatureStatusDisabled

  -- then the guest link status for the conversation should still be enabled (note that in the UI she can't create guest links because her own team settings do not allow this)
  checkGuestLinksStatus alice conv Public.FeatureStatusEnabled

  -- when bob gets the guest link status for the conversation
  -- then the status should be enabled
  checkGuestLinksStatus bob conv Public.FeatureStatusEnabled

  -- when a user that is not in the conversation tries to get the guest link status
  -- then the result should be not found
  checkGetGuestLinksStatus 404 alex conv
  checkGetGuestLinksStatus 404 bert conv

  -- when a conversation member that is not an admin tries to get the guest link status
  -- then the result should be forbidden
  postMembersWithRole bob (pure qbert) qconv roleNameWireMember !!! const 200 === statusCode
  checkGetGuestLinksStatus 403 bert conv

postJoinConvFail :: TestM ()
postJoinConvFail = do
  alice <- randomUser
  bob <- randomUser
  conv <- decodeConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  void $ postJoinConv bob conv !!! const 403 === statusCode

paginateConvIds :: TestM ()
paginateConvIds = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (singleton bob)
  connectUsers alice (singleton eve)
  replicateM_ 253 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode
  -- 1 self conv, 2 convs with bob and eve, 253 gossips = 256 convs
  foldM_ (getChunk 16 alice) Nothing [15, 14 .. 0 :: Int]
  where
    getChunk size alice start n = do
      resp <- getConvIdsV2 alice start (Just size) <!! const 200 === statusCode
      let c = fromMaybe (ConversationList [] False) (responseJsonUnsafe resp)
      liftIO $ do
        -- This is because of the way this test is setup, we always get 16
        -- convs, even on the last one
        assertEqual
          ("Number of convs should match the requested size, " <> show n <> " more gets to go")
          (fromIntegral size)
          (length (convList c))

        if n > 0
          then assertEqual "hasMore should be True" True (convHasMore c)
          else assertEqual ("hasMore should be False, " <> show n <> " more chunks to go") False (convHasMore c)

      pure (Just (Right (last (convList c))))

testListConvIds :: TestM ()
testListConvIds = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  -- Each user has a Proteus self-conversation and the one-to-one coversation.
  for_ [alice, bob] $ \u -> do
    r :: ConversationList ConvId <-
      responseJsonError
        =<< getConvIdsV2 u Nothing (Just 5)
          <!! const 200 === statusCode
    liftIO $ do
      length (convList r) @?= 2
      convHasMore r @?= False

paginateConvListIds :: TestM ()
paginateConvListIds = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (list1 bob [eve])
  localDomain <- viewFederationDomain
  let qAlice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  replicateM_ 196 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  connectWithRemoteUser alice qChad
  replicateM_ 25 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qChad,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee
  replicateM_ 31 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qDee,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

  -- 1 Proteus self conv + 1 MLS self conv + 2 convs with bob and eve + 196
  -- local convs + 25 convs on chad.example.com + 31 on dee.example = 256 convs.
  -- Getting them 16 at a time should get all them in 16 times.
  foldM_ (getChunkedConvs 16 0 alice) Nothing [16, 15 .. 0 :: Int]

-- This test ensures to setup conversations so that a page would end exactly
-- when local convs are exhausted and then exactly when another remote domain's
-- convs are exhausted. As the local convs and remote convs are stored in two
-- different tables, this is an important edge case to test.
paginateConvListIdsPageEndingAtLocalsAndDomain :: TestM ()
paginateConvListIdsPageEndingAtLocalsAndDomain = do
  [alice, bob, eve] <- randomUsers 3
  connectUsers alice (list1 bob [eve])
  localDomain <- viewFederationDomain
  let qAlice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  -- With page size 16, 28 group convs + 2 one-to-one convs + 1 Proteus self
  -- conv + 1 MLS self conv, we get 32 convs. The 2nd page should end here.
  replicateM_ 28 $
    postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
      !!! const 201 === statusCode

  -- We should be able to page through current state in 2 pages exactly
  foldM_ (getChunkedConvs 16 0 alice) Nothing [2, 1, 0 :: Int]

  remoteChad <- randomId
  let chadDomain = Domain "chad.example.com"
      qChad = Qualified remoteChad chadDomain
  connectWithRemoteUser alice qChad

  -- The 3rd page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qChad,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient chadDomain cu

  remoteDee <- randomId
  let deeDomain = Domain "dee.example.com"
      qDee = Qualified remoteDee deeDomain
  connectWithRemoteUser alice qDee

  -- The 4th and last page will end with this domain
  replicateM_ 16 $ do
    conv <- randomId
    let cu =
          F.ConversationUpdate
            { F.cuTime = now,
              F.cuOrigUserId = qDee,
              F.cuConvId = conv,
              F.cuAlreadyPresentUsers = [],
              F.cuAction = SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
            }
    runFedClient @"on-conversation-updated" fedGalleyClient deeDomain cu

  foldM_ (getChunkedConvs 16 0 alice) Nothing [4, 3, 2, 1, 0 :: Int]

-- | Gets chunked conversation ids given size of each chunk, size of the last
-- chunk, requesting user and @n@ which represents how many chunks are remaining
-- to go, when this is 0, it is assumed that this chunk is last and the response
-- must set @has_more@ to 'False' and the number of conv ids returned should
-- match @lastSize@.
getChunkedConvs :: HasCallStack => Int32 -> Int -> UserId -> Maybe ConversationPagingState -> Int -> TestM (Maybe ConversationPagingState)
getChunkedConvs size lastSize alice pagingState n = do
  resp <- getConvPage alice pagingState (Just size) <!! const 200 === statusCode
  let c = responseJsonUnsafeWithMsg @ConvIdsPage "failed to parse ConvIdsPage" resp
  liftIO $ do
    if n > 0
      then assertEqual ("Number of convs should match the requested size, " <> show n <> " more chunks to go") (fromIntegral size) (length (mtpResults c))
      else assertEqual "Number of convs should match the last size, no more chunks to go" lastSize (length (mtpResults c))

    if n > 0
      then assertEqual ("hasMore should be True, " <> show n <> " more chunk(s) to go") True (mtpHasMore c)
      else assertEqual "hasMore should be False, no more chunks to go" False (mtpHasMore c)

  pure . Just $ mtpPagingState c

getConvsPagingOk :: TestM ()
getConvsPagingOk = do
  [ally, bill, carl] <- randomUsers 3
  connectUsers ally (list1 bill [carl])
  replicateM_ 10 $ postConv ally [bill, carl] (Just "gossip") [] Nothing Nothing

  walk ally [3, 3, 3, 3, 2] -- 10 (group) + 2 (1:1) + 2 (self)
  walk bill [3, 3, 3, 3, 1] -- 10 (group) + 1 (1:1) + 2 (self)
  walk carl [3, 3, 3, 3, 1] -- 10 (group) + 1 (1:1) + 2 (self)
  where
    walk :: Foldable t => UserId -> t Int -> TestM ()
    walk u = foldM_ (next u 3) Nothing

    next ::
      UserId ->
      Int32 ->
      Maybe ConversationPagingState ->
      Int ->
      TestM (Maybe ConversationPagingState)
    next u step state n = do
      (ids1, state') <- do
        r :: ConvIdsPage <-
          responseJsonError
            =<< getConvPage u state (Just step)
              <!! const 200 === statusCode
        pure (mtpResults r, mtpPagingState r)
      liftIO $ assertEqual "unexpected length (getConvIds)" n (length ids1)

      ids2 <- do
        r <-
          responseJsonError
            =<< getConvs u ids1 <!! const 200 === statusCode
        pure $ map cnvQualifiedId (crFound r)
      liftIO $ assertEqual "unexpected length (getConvs)" n (length ids2)
      liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids2)

      pure (Just state')

putConvAcceptOk :: TestM ()
putConvAcceptOk = do
  alice <- randomUser
  bob <- randomUser
  qcnv <- decodeQualifiedConvId <$> postConnectConv alice bob "Alice" "come to zeta!" Nothing
  putConvAccept bob (qUnqualified qcnv) !!! const 200 === statusCode
  getConvQualified alice qcnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap cnvType . responseJsonUnsafe
  getConvQualified bob qcnv !!! do
    const 200 === statusCode
    const (Just One2OneConv) === fmap cnvType . responseJsonUnsafe

putConvAcceptRetry :: TestM ()
putConvAcceptRetry = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  cnv <- decodeConvId <$> postO2OConv alice bob (Just "chat")
  -- If the conversation type is already One2One, everything is 200 OK
  putConvAccept bob cnv !!! const 200 === statusCode

postRepeatConnectConvCancel :: TestM ()
postRepeatConnectConvCancel = do
  alice <- randomUser
  bob <- randomUser
  -- Alice wants to connect
  rsp1 <- postConnectConv alice bob "A" "a" Nothing <!! const 201 === statusCode
  let cnv = responseJsonUnsafeWithMsg "conversation" rsp1
  liftIO $ do
    ConnectConv @=? cnvType cnv
    Just "A" @=? cnvName cnv
    [] @=? cmOthers (cnvMembers cnv)
    privateAccess @=? cnvAccess cnv
  -- Alice blocks / cancels
  cancel alice cnv
  -- Alice makes another connect attempt
  rsp2 <- postConnectConv alice bob "A2" "a2" Nothing <!! const 200 === statusCode
  let cnv2 = responseJsonUnsafeWithMsg "conversation" rsp2
  liftIO $ do
    ConnectConv @=? cnvType cnv2
    Just "A2" @=? cnvName cnv2
    [] @=? cmOthers (cnvMembers cnv2)
    privateAccess @=? cnvAccess cnv2
  -- Alice blocks / cancels again
  cancel alice cnv
  -- Now Bob attempts to connect
  rsp3 <- postConnectConv bob alice "B" "b" Nothing <!! const 200 === statusCode
  let cnv3 = responseJsonUnsafeWithMsg "conversation" rsp3
  liftIO $ do
    ConnectConv @=? cnvType cnv3
    Just "B" @=? cnvName cnv3
    privateAccess @=? cnvAccess cnv3
  -- Bob accepting is a no-op, since he is already a member
  let qconvId = cnvQualifiedId cnv
  let convId = qUnqualified qconvId
  putConvAccept bob convId !!! const 200 === statusCode
  cnvX <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified bob qconvId
  liftIO $ do
    ConnectConv @=? cnvType cnvX
    Just "B" @=? cnvName cnvX
    privateAccess @=? cnvAccess cnvX
  -- Alice accepts, finally turning it into a 1-1
  putConvAccept alice convId !!! const 200 === statusCode
  cnv4 <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified alice qconvId
  liftIO $ do
    One2OneConv @=? cnvType cnv4
    Just "B" @=? cnvName cnv4
    privateAccess @=? cnvAccess cnv4
  where
    cancel u c = do
      g <- viewGalley
      let cnvId = qUnqualified . cnvQualifiedId
      put (g . paths ["/i/conversations", toByteString' (cnvId c), "block"] . zUser u)
        !!! const 200 === statusCode
      getConv u (cnvId c) !!! const 403 === statusCode

putBlockConvOk :: TestM ()
putBlockConvOk = do
  g <- viewGalley
  alice <- randomUser
  bob <- randomUser
  conv <- responseJsonUnsafeWithMsg "conversation" <$> postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")
  let qconvId = cnvQualifiedId conv
  let convId = qUnqualified qconvId
  getConvQualified alice qconvId !!! const 200 === statusCode
  getConvQualified bob qconvId !!! const 403 === statusCode
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- A is still the only member of the 1-1
  getConvQualified alice qconvId !!! do
    const 200 === statusCode
    const (cnvMembers conv) === cnvMembers . responseJsonUnsafeWithMsg "conversation"
  -- B accepts the conversation by unblocking
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  getConvQualified bob qconvId !!! const 200 === statusCode
  -- B blocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "block"] . zUser bob)
    !!! const 200 === statusCode
  -- B no longer sees the 1-1
  getConvQualified bob qconvId !!! const 403 === statusCode
  -- B unblocks A in the 1-1
  put (g . paths ["/i/conversations", toByteString' convId, "unblock"] . zUser bob)
    !!! const 200 === statusCode
  -- B sees the blocked 1-1 again
  getConvQualified bob qconvId !!! do
    const 200 === statusCode

accessConvMeta :: TestM ()
accessConvMeta = do
  g <- viewGalley
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  let meta =
        ConversationMetadata
          RegularConv
          alice
          [InviteAccess]
          (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole])
          (Just "gossip")
          Nothing
          Nothing
          Nothing
  get (g . paths ["i/conversations", toByteString' conv, "meta"] . zUser alice) !!! do
    const 200 === statusCode
    const (Just meta) === (decode <=< responseBody)

leaveConnectConversation :: TestM ()
leaveConnectConversation = do
  (alice, qalice) <- randomUserTuple
  bob <- randomUser
  bdy <- postConnectConv alice bob "alice" "ni" Nothing <!! const 201 === statusCode
  let c = maybe (error "invalid connect conversation") (qUnqualified . cnvQualifiedId) (responseJsonUnsafe bdy)
  qc <- Qualified c <$> viewFederationDomain
  deleteMemberQualified alice qalice qc !!! const 403 === statusCode

testAddRemoteMember :: TestM ()
testAddRemoteMember = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  let localDomain = qDomain qalice
  bobId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let qconvId = Qualified convId localDomain

  postQualifiedMembers alice (remoteBob :| []) convId !!! do
    const 403 === statusCode
    const (Right (Just "not-connected")) === fmap (view (at "label")) . responseJsonEither @Object

  connectWithRemoteUser alice remoteBob

  (resp, reqs) <-
    withTempMockFederator' (respond remoteBob) $
      postQualifiedMembers alice (remoteBob :| []) convId
        <!! const 200 === statusCode
  liftIO $ do
    map frTargetDomain reqs @?= [remoteDomain, remoteDomain]
    map frRPC reqs @?= ["on-new-remote-conversation", "on-conversation-updated"]

  let e = responseJsonUnsafe resp
  let bobMember = SimpleMember remoteBob roleNameWireAdmin
  liftIO $ do
    evtConv e @?= qconvId
    evtType e @?= MemberJoin
    evtData e @?= EdMembersJoin (SimpleMembers [bobMember])
    evtFrom e @?= qalice
  conv <- responseJsonUnsafeWithMsg "conversation" <$> getConvQualified alice qconvId
  liftIO $ do
    let actual = cmOthers $ cnvMembers conv
    let expected = [OtherMember remoteBob Nothing roleNameWireAdmin]
    assertEqual "other members should include remoteBob" expected actual
  where
    respond :: Qualified UserId -> Mock LByteString
    respond bob =
      asum
        [ guardComponent Brig *> mockReply [mkProfile bob (Name "bob")],
          "on-new-remote-conversation" ~> EmptyResponse,
          "on-conversation-updated" ~> ()
        ]

-- | Tests getting many converations given their ids.
--
-- In this test, Alice is a local user, who will be asking for metadata of these
-- conversations:
--
-- - A local conversation which she is part of
--
-- - A remote conv on a.far-away.example.com (with Bob)
--
-- - A remote conv on b.far-away.example.com (with Carl)
--
-- - A remote conv on a.far-away.example.com, which is not found in the local DB
--
-- - A remote conv on b.far-away.example.com, it is found in the local DB but
--   the remote does not return it
--
-- - A remote conv on c.far-away.example.com (with Dee), for which the federated call fails
--
-- - A local conversation which doesn't exist
--
-- - A local conversation which they're not part of
testBulkGetQualifiedConvs :: TestM ()
testBulkGetQualifiedConvs = do
  localDomain <- viewFederationDomain
  aliceQ <- randomQualifiedUser
  let alice = qUnqualified aliceQ
      lAlice = toLocalUnsafe localDomain alice
  bobId <- randomId
  carlId <- randomId
  deeId <- randomId
  let remoteDomainA = Domain "a.far-away.example.com"
      remoteDomainB = Domain "b.far-away.example.com"
      remoteDomainC = Domain "c.far-away.example.com"
      bobQ = Qualified bobId remoteDomainA
      carlQ = Qualified carlId remoteDomainB
      deeQ = Qualified deeId remoteDomainC

  connectWithRemoteUser alice bobQ
  connectWithRemoteUser alice carlQ
  connectWithRemoteUser alice deeQ

  localConv <- responseJsonUnsafe <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  let localConvId = cnvQualifiedId localConv

  remoteConvIdA <- randomQualifiedId remoteDomainA
  remoteConvIdB <- randomQualifiedId remoteDomainB
  remoteConvIdALocallyNotFound <- randomQualifiedId remoteDomainA
  remoteConvIdBNotFoundOnRemote <- randomQualifiedId remoteDomainB
  localConvIdNotFound <- randomQualifiedId localDomain
  remoteConvIdCFailure <- randomQualifiedId remoteDomainC

  eve <- randomQualifiedUser
  localConvIdNotParticipating <- decodeQualifiedConvId <$> postConv (qUnqualified eve) [] (Just "gossip about alice!") [] Nothing Nothing

  let aliceAsOtherMember = OtherMember aliceQ Nothing roleNameWireAdmin
  registerRemoteConv remoteConvIdA bobId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdB carlId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdBNotFoundOnRemote carlId Nothing (Set.fromList [aliceAsOtherMember])
  registerRemoteConv remoteConvIdCFailure deeId Nothing (Set.fromList [aliceAsOtherMember])

  let bobAsOtherMember = OtherMember bobQ Nothing roleNameWireAdmin
      carlAsOtherMember = OtherMember carlQ Nothing roleNameWireAdmin
      mockConversationA = mkProteusConv (qUnqualified remoteConvIdA) bobId roleNameWireAdmin [bobAsOtherMember]
      mockConversationB = mkProteusConv (qUnqualified remoteConvIdB) carlId roleNameWireAdmin [carlAsOtherMember]
      req =
        ListConversations . unsafeRange $
          [ localConvId,
            remoteConvIdA,
            remoteConvIdB,
            remoteConvIdALocallyNotFound,
            localConvIdNotFound,
            localConvIdNotParticipating,
            remoteConvIdBNotFoundOnRemote,
            remoteConvIdCFailure
          ]
  (respAll, receivedRequests) <- do
    let mock = do
          d <- frTargetDomain <$> getRequest
          asum
            [ guard (d == remoteDomainA) *> mockReply (GetConversationsResponse [mockConversationA]),
              guard (d == remoteDomainB) *> mockReply (GetConversationsResponse [mockConversationB]),
              guard (d == remoteDomainC) *> liftIO (throw (DiscoveryFailureSrvNotAvailable "domainC")),
              do
                r <- getRequest
                liftIO . assertFailure $ "Unrecognized domain: " <> show r
            ]
    withTempMockFederator' mock (listConvs alice req)
  convs <- responseJsonUnsafe <$> (pure respAll <!! const 200 === statusCode)

  liftIO $ do
    let expectedFound =
          sortOn
            cnvQualifiedId
            $ pure (remoteConversationView lAlice defMemberStatus (toRemoteUnsafe remoteDomainA mockConversationA))
              <> pure (remoteConversationView lAlice defMemberStatus (toRemoteUnsafe remoteDomainB mockConversationB))
              <> [localConv]
        actualFound = sortOn cnvQualifiedId $ crFound convs
    assertEqual "found conversations" expectedFound actualFound

    -- Assumes only one request is made
    let requestedConvIdsA =
          fmap F.gcrConvIds
            . (decode . frBody =<<)
            $ find ((== remoteDomainA) . frTargetDomain) receivedRequests
    assertEqual "only locally found conversations should be queried" (Just [qUnqualified remoteConvIdA]) requestedConvIdsA

    let expectedNotFound = sort [localConvIdNotFound, localConvIdNotParticipating, remoteConvIdALocallyNotFound, remoteConvIdBNotFoundOnRemote]
        actualNotFound = sort $ crNotFound convs
    assertEqual "not founds" expectedNotFound actualNotFound
    assertEqual "failures" [remoteConvIdCFailure] (crFailed convs)

testAddRemoteMemberInvalidDomain :: TestM ()
testAddRemoteMemberInvalidDomain = do
  alice <- randomUser
  bobId <- randomId
  let remoteBob = Qualified bobId (Domain "invalid.example.com")
  convId <- decodeConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing

  connectWithRemoteUser alice remoteBob

  postQualifiedMembers alice (remoteBob :| []) convId
    !!! do
      const 422 === statusCode
      const (Just "/federation/api-version")
        === preview (ix "data" . ix "path") . responseJsonUnsafe @Value
      const (Just "invalid.example.com")
        === preview (ix "data" . ix "domain") . responseJsonUnsafe @Value

-- This test is a safeguard to ensure adding remote members will fail
-- on environments where federation isn't configured (such as our production as of May 2021)
testAddRemoteMemberFederationDisabled :: TestM ()
testAddRemoteMemberFederationDisabled = do
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  qconvId <- decodeQualifiedConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let convId = qUnqualified qconvId
  connectWithRemoteUser alice remoteBob

  -- federator endpoint not configured is equivalent to federation being disabled
  -- This is the case on staging/production in May 2021.
  let federatorNotConfigured = optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $
    postQualifiedMembers alice (remoteBob :| []) convId !!! do
      const 400 === statusCode
      const (Right "federation-not-enabled") === fmap label . responseJsonEither

  -- the member is not actually added to the conversation
  conv <- responseJsonError =<< getConvQualified alice qconvId <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv)) @?= []

testAddRemoteMemberFederationUnavailable :: TestM ()
testAddRemoteMemberFederationUnavailable = do
  alice <- randomUser
  remoteBob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  qconvId <- decodeQualifiedConvId <$> postConv alice [] (Just "remote gossip") [] Nothing Nothing
  let convId = qUnqualified qconvId
  connectWithRemoteUser alice remoteBob

  -- federator endpoint being configured in brig and/or galley, but not being
  -- available (i.e. no service listing on that IP/port) can happen due to a
  -- misconfiguration of federator. That should give a 500.
  -- Port 1 should always be wrong hopefully.
  let federatorUnavailable = optFederator ?~ Endpoint "127.0.0.1" 1
  withSettingsOverrides federatorUnavailable $
    postQualifiedMembers alice (remoteBob :| []) convId !!! do
      const 500 === statusCode
      const (Right "federation-not-available") === fmap label . responseJsonEither

  -- in this case, we discover that federation is unavailable too late, and the
  -- member has already been added to the conversation
  conv <- responseJsonError =<< getConvQualified alice qconvId <!! const 200 === statusCode
  liftIO $ map omQualifiedId (cmOthers (cnvMembers conv)) @?= [remoteBob]

postMembersOk :: TestM ()
postMembersOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  chuck <- randomUser
  qeve <- randomQualifiedUser
  let eve = qUnqualified qeve
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qalice)
  e <- responseJsonError =<< postMembers alice (pure qeve) qconv <!! const 200 === statusCode
  liftIO $ do
    evtConv e @?= qconv
    evtType e @?= MemberJoin
    evtData e @?= EdMembersJoin (SimpleMembers [SimpleMember qeve roleNameWireAdmin])
    evtFrom e @?= qalice
  -- Check that last_event markers are set for all members
  forM_ [alice, bob, chuck, eve] $ \u -> do
    _ <- getSelfMember u conv <!! const 200 === statusCode
    pure ()

postMembersOk2 :: TestM ()
postMembersOk2 = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomQualifiedUser
  connectUsers alice (list1 bob [qUnqualified chuck])
  connectUsers bob (singleton . qUnqualified $ chuck)
  conv <- decodeConvId <$> postConv alice [bob, qUnqualified chuck] Nothing [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers bob (pure chuck) qconv !!! do
    const 204 === statusCode
    const Nothing === responseBody
  chuck' <- responseJsonUnsafe <$> (getSelfMember (qUnqualified chuck) conv <!! const 200 === statusCode)
  liftIO $
    assertEqual "wrong self member" (Just chuck) (memId <$> chuck')

postMembersOk3 :: TestM ()
postMembersOk3 = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  eve <- randomUser
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  -- Bob leaves
  deleteMemberQualified bob qbob qconv !!! const 200 === statusCode
  -- Fetch bob
  getSelfMember bob conv !!! const 200 === statusCode
  -- Alice re-adds Bob to the conversation
  postMembers alice (pure qbob) qconv !!! const 200 === statusCode
  -- Fetch bob again
  getSelfMember bob conv !!! const 200 === statusCode

postMembersFailNoGuestAccess :: TestM ()
postMembersFailNoGuestAccess = do
  alice <- randomUser
  bob <- randomUser
  peter <- randomUser
  eve <- ephemeralUser
  qeve <- Qualified eve <$> viewFederationDomain
  connectUsers alice (list1 bob [peter])
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole, NonTeamMemberAccessRole] [GuestAccessRole]
  conv <- decodeConvId <$> postConv alice [bob, peter] (Just "gossip") [] (Just noGuestsAccess) Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers alice (pure qeve) qconv !!! const 403 === statusCode

generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess :: TestM ()
generateGuestLinkFailIfNoNonTeamMemberOrNoGuestAccess = do
  alice <- randomUser
  bob <- randomUser
  connectUsers alice (singleton bob)
  Right noGuestsAccess <- liftIO $ genAccessRolesV2 [TeamMemberAccessRole] [GuestAccessRole, NonTeamMemberAccessRole]
  convId <- decodeConvId <$> postConv alice [bob] (Just "gossip") [CodeAccess] (Just noGuestsAccess) Nothing
  postConvCode alice convId !!! const 403 === statusCode

postMembersFail :: TestM ()
postMembersFail = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  chuck <- randomUser
  (dave, qdave) <- randomUserTuple
  (eve, qeve) <- randomUserTuple
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers eve (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  postMembers eve (pure qbob) qconv !!! const 404 === statusCode
  postMembers alice (pure qeve) qconv !!! const 200 === statusCode
  -- Not connected but already there
  postMembers chuck (pure qeve) qconv !!! const 204 === statusCode
  postMembers chuck (pure qdave) qconv !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe
  void $ connectUsers chuck (singleton dave)
  postMembers chuck (pure qdave) qconv !!! const 200 === statusCode
  postMembers chuck (pure qdave) qconv !!! const 204 === statusCode

postTooManyMembersFail :: TestM ()
postTooManyMembersFail = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  qconv <- Qualified conv <$> viewFederationDomain
  x : xs <- replicateM (n - 2) randomQualifiedUser
  postMembers chuck (x :| xs) qconv !!! do
    const 403 === statusCode
    const (Just "too-many-members") === fmap label . responseJsonUnsafe

putQualifiedOtherMemberOk :: TestM ()
putQualifiedOtherMemberOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
      alice = qUnqualified qalice
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
      expectedMemberUpdateData =
        MemberUpdateData
          { misTarget = qalice,
            misOtrMutedStatus = Nothing,
            misOtrMutedRef = Nothing,
            misOtrArchived = Nothing,
            misOtrArchivedRef = Nothing,
            misHidden = Nothing,
            misHiddenRef = Nothing,
            misConvRoleName = Just roleNameWireMember
          }

  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- demote qalice
    putOtherMemberQualified bob qalice (OtherMemberUpdate (Just roleNameWireMember)) qconv
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate expectedMemberUpdateData

putOtherMemberOk :: TestM ()
putOtherMemberOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  qbob <- randomQualifiedUser
  let alice = qUnqualified qalice
      bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  let qconv = Qualified conv (qDomain qbob)
      expectedMemberUpdateData =
        MemberUpdateData
          { misTarget = qalice,
            misOtrMutedStatus = Nothing,
            misOtrMutedRef = Nothing,
            misOtrArchived = Nothing,
            misOtrArchivedRef = Nothing,
            misHidden = Nothing,
            misHiddenRef = Nothing,
            misConvRoleName = Just roleNameWireMember
          }

  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- demote alice
    putOtherMember bob alice (OtherMemberUpdate (Just roleNameWireMember)) conv
      !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate expectedMemberUpdateData

putMemberOtrMuteOk :: TestM ()
putMemberOtrMuteOk = do
  putMemberOk (memberUpdate {mupOtrMuteStatus = Just 1, mupOtrMuteRef = Just "ref"})
  putMemberOk (memberUpdate {mupOtrMuteStatus = Just 0})

putMemberOtrArchiveOk :: TestM ()
putMemberOtrArchiveOk = do
  putMemberOk (memberUpdate {mupOtrArchive = Just True, mupOtrArchiveRef = Just "ref"})
  putMemberOk (memberUpdate {mupOtrArchive = Just False})

putMemberHiddenOk :: TestM ()
putMemberHiddenOk = do
  putMemberOk (memberUpdate {mupHidden = Just True, mupHiddenRef = Just "ref"})
  putMemberOk (memberUpdate {mupHidden = Just False})

putMemberAllOk :: TestM ()
putMemberAllOk =
  putMemberOk
    ( memberUpdate
        { mupOtrMuteStatus = Just 0,
          mupOtrMuteRef = Just "mref",
          mupOtrArchive = Just True,
          mupOtrArchiveRef = Just "aref",
          mupHidden = Just True,
          mupHiddenRef = Just "href"
        }
    )

putRemoteConvMemberOtrMuteOk :: TestM ()
putRemoteConvMemberOtrMuteOk = do
  putRemoteConvMemberOk (memberUpdate {mupOtrMuteStatus = Just 1, mupOtrMuteRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupOtrMuteStatus = Just 0})

putRemoteConvMemberOtrArchiveOk :: TestM ()
putRemoteConvMemberOtrArchiveOk = do
  putRemoteConvMemberOk (memberUpdate {mupOtrArchive = Just True, mupOtrArchiveRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupOtrArchive = Just False})

putRemoteConvMemberHiddenOk :: TestM ()
putRemoteConvMemberHiddenOk = do
  putRemoteConvMemberOk (memberUpdate {mupHidden = Just True, mupHiddenRef = Just "ref"})
  putRemoteConvMemberOk (memberUpdate {mupHidden = Just False})

putRemoteConvMemberAllOk :: TestM ()
putRemoteConvMemberAllOk =
  putRemoteConvMemberOk
    ( memberUpdate
        { mupOtrMuteStatus = Just 0,
          mupOtrMuteRef = Just "mref",
          mupOtrArchive = Just True,
          mupOtrArchiveRef = Just "aref",
          mupHidden = Just True,
          mupHiddenRef = Just "href"
        }
    )

putMemberOk :: MemberUpdate -> TestM ()
putMemberOk update = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  getConv alice conv !!! const 200 === statusCode
  -- Expected member state
  let memberBob =
        Member
          { memId = qbob,
            memService = Nothing,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = roleNameWireAdmin
          }
  -- Update member state & verify push notification
  WS.bracketR c bob $ \ws -> do
    putMember bob update qconv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      case evtData e of
        EdMemberUpdate mis -> do
          assertEqual "otr_muted_status" (mupOtrMuteStatus update) (misOtrMutedStatus mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x
  -- Verify new member state
  rs <- getConvQualified bob qconv <!! const 200 === statusCode
  let bob' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust bob')
    let newBob = fromJust bob'
    assertEqual "id" (memId memberBob) (memId newBob)
    assertEqual "otr_muted_status" (memOtrMutedStatus memberBob) (memOtrMutedStatus newBob)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberBob) (memOtrMutedRef newBob)
    assertEqual "otr_archived" (memOtrArchived memberBob) (memOtrArchived newBob)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberBob) (memOtrArchivedRef newBob)
    assertEqual "hidden" (memHidden memberBob) (memHidden newBob)
    assertEqual "hidden_ref" (memHiddenRef memberBob) (memHiddenRef newBob)

putRemoteConvMemberOk :: MemberUpdate -> TestM ()
putRemoteConvMemberOk update = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation with alice
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu

  -- Expected member state
  let memberAlice =
        Member
          { memId = qalice,
            memService = Nothing,
            memOtrMutedStatus = mupOtrMuteStatus update,
            memOtrMutedRef = mupOtrMuteRef update,
            memOtrArchived = Just True == mupOtrArchive update,
            memOtrArchivedRef = mupOtrArchiveRef update,
            memHidden = Just True == mupHidden update,
            memHiddenRef = mupHiddenRef update,
            memConvRoleName = roleNameWireMember
          }
  -- Update member state & verify push notification
  WS.bracketR c alice $ \ws -> do
    putMember alice update qconv !!! const 200 === statusCode
    void . liftIO . WS.assertMatch (5 # Second) ws $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qalice
      case evtData e of
        EdMemberUpdate mis -> do
          assertEqual "otr_muted_status" (mupOtrMuteStatus update) (misOtrMutedStatus mis)
          assertEqual "otr_muted_ref" (mupOtrMuteRef update) (misOtrMutedRef mis)
          assertEqual "otr_archived" (mupOtrArchive update) (misOtrArchived mis)
          assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
          assertEqual "hidden" (mupHidden update) (misHidden mis)
          assertEqual "hidden_ref" (mupHiddenRef update) (misHiddenRef mis)
        x -> assertFailure $ "Unexpected event data: " ++ show x

  -- Fetch remote conversation
  let bobAsLocal =
        LocalMember
          (qUnqualified qbob)
          defMemberStatus
          Nothing
          roleNameWireAdmin
  let mockConversation =
        mkProteusConv
          (qUnqualified qconv)
          (qUnqualified qbob)
          roleNameWireMember
          [localMemberToOther remoteDomain bobAsLocal]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  (rs, _) <-
    withTempMockFederator'
      (mockReply remoteConversationResponse)
      $ getConvQualified alice qconv
        <!! const 200 === statusCode

  -- Verify new member state
  let alice' = cmSelf . cnvMembers <$> responseJsonUnsafe rs
  liftIO $ do
    assertBool "user" (isJust alice')
    let newAlice = fromJust alice'
    assertEqual "id" (memId memberAlice) (memId newAlice)
    assertEqual "otr_muted_status" (memOtrMutedStatus memberAlice) (memOtrMutedStatus newAlice)
    assertEqual "otr_muted_ref" (memOtrMutedRef memberAlice) (memOtrMutedRef newAlice)
    assertEqual "otr_archived" (memOtrArchived memberAlice) (memOtrArchived newAlice)
    assertEqual "otr_archived_ref" (memOtrArchivedRef memberAlice) (memOtrArchivedRef newAlice)
    assertEqual "hidden" (memHidden memberAlice) (memHidden newAlice)
    assertEqual "hidden_ref" (memHiddenRef memberAlice) (memHiddenRef newAlice)

putReceiptModeOk :: TestM ()
putReceiptModeOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  cnv <- decodeConvId <$> postConv alice [bob, jane] (Just "gossip") [] Nothing Nothing
  let qcnv = Qualified cnv (qDomain qalice)
  WS.bracketR3 c alice bob jane $ \(_wsA, wsB, _wsJ) -> do
    -- By default, nothing is set
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just Nothing) === fmap cnvReceiptMode . responseJsonUnsafe
    -- Set receipt mode
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 200 === statusCode
    -- Ensure the field is properly set
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
    void . liftIO $ checkWs qalice (qcnv, wsB)
    -- No changes
    putReceiptMode alice cnv (ReceiptMode 0) !!! const 204 === statusCode
    -- No event should have been generated
    WS.assertNoEvent (1 # Second) [wsB]
    -- Ensure that the new field remains unchanged
    getConvQualified alice qcnv !!! do
      const 200 === statusCode
      const (Just $ Just (ReceiptMode 0)) === fmap cnvReceiptMode . responseJsonUnsafe
  qcnv' <- decodeQualifiedConvId <$> postConvWithReceipt alice [bob, jane] (Just "gossip") [] Nothing Nothing (ReceiptMode 0)
  getConvQualified alice qcnv' !!! do
    const 200 === statusCode
    const (Just (Just (ReceiptMode 0))) === fmap cnvReceiptMode . responseJsonUnsafe
  where
    checkWs qalice (qcnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= qcnv
      evtType e @?= ConvReceiptModeUpdate
      evtFrom e @?= qalice
      case evtData e of
        EdConvReceiptModeUpdate (ConversationReceiptModeUpdate (ReceiptMode mode)) ->
          assertEqual "modes should match" mode 0
        _ -> assertFailure "Unexpected event data"

-- | Test setup
-- A (local)  - alice: admin on remote conversation, adam: regular member of remote conversation
-- B (mocked) - owns the conversation
--
-- The federator on A is also mocked.
--
-- alice changes receipt remote via client api
-- assertion: A's federator is called correctly
-- assertion: backend A generates events for adam
-- and federator's response
putRemoteReceiptModeOk :: TestM ()
putRemoteReceiptModeOk = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation at bob with alice as admin
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cuAddAlice =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireAdmin)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAlice

  -- add another user adam as member
  qadam <- randomQualifiedUser
  let adam = qUnqualified qadam
  connectWithRemoteUser adam qbob
  let cuAddAdam =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qadam) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAddAdam

  let newReceiptMode = ReceiptMode 42
  let action = ConversationReceiptModeUpdate newReceiptMode
  let responseConvUpdate =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qalice,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [adam],
            cuAction =
              SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) action
          }
  let mockResponse = mockReply (ConversationUpdateResponseUpdate responseConvUpdate)

  WS.bracketR c adam $ \wsAdam -> do
    (res, federatedRequests) <- withTempMockFederator' mockResponse $ do
      putQualifiedReceiptMode alice qconv newReceiptMode
        <!! const 200 === statusCode

    let event :: Event = responseJsonUnsafe res
    let (EdConvReceiptModeUpdate (ConversationReceiptModeUpdate receiptModeEvent)) = evtData event

    liftIO $ assertEqual "Unexcepected receipt mode in event" newReceiptMode receiptModeEvent

    cFedReq <- assertOne $ filter (\r -> frTargetDomain r == remoteDomain && frRPC r == "update-conversation") federatedRequests
    cFedReqBody <- assertRight $ parseFedRequest cFedReq
    liftIO $ do
      curUser cFedReqBody @?= alice
      curConvId cFedReqBody @?= qUnqualified qconv
      curAction cFedReqBody @?= SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) action

    WS.assertMatch_ (5 # Second) wsAdam $ \n -> do
      liftIO $ wsAssertConvReceiptModeUpdate qconv qalice newReceiptMode n

putReceiptModeWithRemotesOk :: TestM ()
putReceiptModeWithRemotesOk = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob

  connectWithRemoteUser bob qalice

  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice]}
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putQualifiedReceiptMode bob qconv (ReceiptMode 43) !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu
        @?= SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) (ConversationReceiptModeUpdate (ReceiptMode 43))

    void . liftIO . WS.assertMatch (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvReceiptModeUpdate
      evtFrom e @?= qbob
      evtData e
        @?= EdConvReceiptModeUpdate
          (ConversationReceiptModeUpdate (ReceiptMode 43))

removeUserNoFederation :: TestM ()
removeUserNoFederation = do
  c <- view tsCannon
  [alice, bob, carl] <- replicateM 3 randomQualifiedUser
  let [alice', bob', carl'] = qUnqualified <$> [alice, bob, carl]

  connectUsers alice' (list1 bob' [carl'])

  qconv1 <- decodeQualifiedConvId <$> postConv alice' [bob'] (Just "gossip") [] Nothing Nothing
  qconv2 <- decodeQualifiedConvId <$> postConv alice' [bob', carl'] (Just "gossip2") [] Nothing Nothing
  qconv3 <- decodeQualifiedConvId <$> postConv alice' [carl'] (Just "gossip3") [] Nothing Nothing

  WS.bracketR3 c alice' bob' carl' $ \(wsA, wsB, wsC) -> do
    deleteUser bob' !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB] $
        wsAssertMembersLeave qconv1 bob [bob]
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        wsAssertMembersLeave qconv2 bob [bob]
  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv1
  mems2 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv2
  mems3 <- fmap cnvMembers . responseJsonUnsafe <$> getConvQualified alice' qconv3
  let other u = find ((== u) . omQualifiedId) . cmOthers
  liftIO $ do
    (mems1 >>= other bob) @?= Nothing
    (mems2 >>= other bob) @?= Nothing
    (mems2 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)
    (mems3 >>= other bob) @?= Nothing
    (mems3 >>= other carl) @?= Just (OtherMember carl Nothing roleNameWireAdmin)

removeUser :: TestM ()
removeUser = do
  c <- view tsCannon
  [alice, alexDel, amy] <- replicateM 3 randomQualifiedUser
  let [alice', alexDel', amy'] = qUnqualified <$> [alice, alexDel, amy]

  let bDomain = Domain "b.example.com"
  bart <- randomQualifiedId bDomain
  berta <- randomQualifiedId bDomain

  let cDomain = Domain "c.example.com"
  carl <- randomQualifiedId cDomain

  let dDomain = Domain "d.example.com"
  dwight <- randomQualifiedId dDomain
  dory <- randomQualifiedId dDomain

  connectUsers alice' (list1 alexDel' [amy'])
  connectWithRemoteUser alice' bart
  connectWithRemoteUser alice' berta
  connectWithRemoteUser alexDel' bart
  connectWithRemoteUser alice' carl
  connectWithRemoteUser alexDel' carl
  connectWithRemoteUser alice' dwight
  connectWithRemoteUser alexDel' dory

  qconvA1 <- decodeQualifiedConvId <$> postConv alice' [alexDel'] (Just "gossip") [] Nothing Nothing
  qconvA2 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' defNewProteusConv {newConvQualifiedUsers = [alexDel, amy, berta, dwight]}
  qconvA3 <- decodeQualifiedConvId <$> postConv alice' [amy'] (Just "gossip3") [] Nothing Nothing
  qconvA4 <- decodeQualifiedConvId <$> postConvWithRemoteUsers alice' defNewProteusConv {newConvQualifiedUsers = [alexDel, bart, carl]}
  convB1 <- randomId -- a remote conversation at 'bDomain' that Alice, AlexDel and Bart will be in
  convB2 <- randomId -- a remote conversation at 'bDomain' that AlexDel and Bart will be in
  convC1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Carl will be in
  convD1 <- randomId -- a remote conversation at 'cDomain' that AlexDel and Dory will be in
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient
  let nc cid creator quids =
        F.ConversationCreated
          { F.ccTime = now,
            F.ccOrigUserId = qUnqualified creator,
            F.ccCnvId = cid,
            F.ccCnvType = RegularConv,
            F.ccCnvAccess = [],
            F.ccCnvAccessRoles = Set.fromList [],
            F.ccCnvName = Just "gossip4",
            F.ccNonCreatorMembers = Set.fromList $ createOtherMember <$> quids,
            F.ccMessageTimer = Nothing,
            F.ccReceiptMode = Nothing,
            F.ccProtocol = ProtocolProteus
          }
  runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB1 bart [alice, alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient bDomain $ nc convB2 bart [alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient cDomain $ nc convC1 carl [alexDel]
  runFedClient @"on-conversation-created" fedGalleyClient dDomain $ nc convD1 dory [alexDel]

  WS.bracketR3 c alice' alexDel' amy' $ \(wsAlice, wsAlexDel, wsAmy) -> do
    let handler = do
          d <- frTargetDomain <$> getRequest
          asum
            [ do
                guard (d == dDomain)
                throw (DiscoveryFailureSrvNotAvailable "dDomain"),
              do
                guard (d `elem` [bDomain, cDomain])
                asum
                  [ "leave-conversation" ~> F.LeaveConversationResponse (Right ()),
                    "on-conversation-updated" ~> ()
                  ]
            ]
    (_, fedRequests) <-
      withTempMockFederator' handler $
        deleteUser alexDel' !!! const 200 === statusCode

    liftIO $ do
      assertEqual ("expect exactly 7 federated requests in : " <> show fedRequests) 7 (length fedRequests)

    liftIO $ do
      bReq <- assertOne $ filter (matchFedRequest bDomain "on-user-deleted-conversations") fedRequests
      frComponent bReq @?= Galley
      frRPC bReq @?= "on-user-deleted-conversations"
      Right udcnB <- pure . eitherDecode . frBody $ bReq
      sort (fromRange (F.udcvConversations udcnB)) @?= sort [convB1, convB2]
      F.udcvUser udcnB @?= qUnqualified alexDel

    liftIO $ do
      cReq <- assertOne $ filter (matchFedRequest cDomain "on-user-deleted-conversations") fedRequests
      frComponent cReq @?= Galley
      frRPC cReq @?= "on-user-deleted-conversations"
      Right udcnC <- pure . eitherDecode . frBody $ cReq
      sort (fromRange (F.udcvConversations udcnC)) @?= sort [convC1]
      F.udcvUser udcnC @?= qUnqualified alexDel

    liftIO $ do
      dReq <- assertOne $ filter (matchFedRequest dDomain "on-user-deleted-conversations") fedRequests
      frComponent dReq @?= Galley
      frRPC dReq @?= "on-user-deleted-conversations"
      Right udcnD <- pure . eitherDecode . frBody $ dReq
      sort (fromRange (F.udcvConversations udcnD)) @?= sort [convD1]
      F.udcvUser udcnD @?= qUnqualified alexDel

    liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsAlice, wsAlexDel] $
        wsAssertMembersLeave qconvA1 alexDel [alexDel]
      WS.assertMatchN_ (5 # Second) [wsAlice, wsAlexDel, wsAmy] $
        wsAssertMembersLeave qconvA2 alexDel [alexDel]

    liftIO $ do
      let bConvUpdateRPCs = filter (matchFedRequest bDomain "on-conversation-updated") fedRequests
      bConvUpdates <- mapM (assertRight . eitherDecode . frBody) bConvUpdateRPCs

      bConvUpdatesA2 <- assertOne $ filter (\cu -> cuConvId cu == qUnqualified qconvA2) bConvUpdates
      cuOrigUserId bConvUpdatesA2 @?= alexDel
      cuAction bConvUpdatesA2 @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers bConvUpdatesA2 @?= [qUnqualified berta]

      bConvUpdatesA4 <- assertOne $ filter (\cu -> cuConvId cu == qUnqualified qconvA4) bConvUpdates
      cuOrigUserId bConvUpdatesA4 @?= alexDel
      cuAction bConvUpdatesA4 @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers bConvUpdatesA4 @?= [qUnqualified bart]

    liftIO $ do
      cConvUpdateRPC <- assertOne $ filter (matchFedRequest cDomain "on-conversation-updated") fedRequests
      Right convUpdate <- pure . eitherDecode . frBody $ cConvUpdateRPC
      cuConvId convUpdate @?= qUnqualified qconvA4
      cuOrigUserId convUpdate @?= alexDel
      cuAction convUpdate @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers convUpdate @?= [qUnqualified carl]

    liftIO $ do
      dConvUpdateRPC <- assertOne $ filter (matchFedRequest dDomain "on-conversation-updated") fedRequests
      Right convUpdate <- pure . eitherDecode . frBody $ dConvUpdateRPC
      cuConvId convUpdate @?= qUnqualified qconvA2
      cuOrigUserId convUpdate @?= alexDel
      cuAction convUpdate @?= SomeConversationAction (sing @'ConversationLeaveTag) ()
      cuAlreadyPresentUsers convUpdate @?= [qUnqualified dwight]

  -- Check memberships
  mems1 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA1
  mems2 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA2
  mems3 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA3
  mems4 <- fmap cnvMembers . responseJsonError =<< getConvQualified alice' qconvA4
  let findOther u = find ((== u) . omQualifiedId) . cmOthers
  liftIO $ do
    findOther alexDel mems1 @?= Nothing
    findOther alexDel mems2 @?= Nothing
    findOther amy mems2 @?= Just (OtherMember amy Nothing roleNameWireAdmin)
    findOther alexDel mems3 @?= Nothing
    findOther amy mems3 @?= Just (OtherMember amy Nothing roleNameWireAdmin)
    findOther alexDel mems4 @?= Nothing
  where
    createOtherMember :: Qualified UserId -> OtherMember
    createOtherMember quid =
      OtherMember
        { omQualifiedId = quid,
          omService = Nothing,
          omConvRoleName = roleNameWireAdmin
        }

testAllOne2OneConversationRequests :: TestM ()
testAllOne2OneConversationRequests = do
  for_ [LocalActor, RemoteActor] $ \actor ->
    for_ [Included, Excluded] $ \desired ->
      for_ [True, False] $ \shouldBeLocal ->
        testOne2OneConversationRequest shouldBeLocal actor desired

testOne2OneConversationRequest :: Bool -> Actor -> DesiredMembership -> TestM ()
testOne2OneConversationRequest shouldBeLocal actor desired = do
  alice <- qTagUnsafe <$> randomQualifiedUser
  (bob, expectedConvId) <- generateRemoteAndConvId shouldBeLocal alice

  convId <- do
    let req = UpsertOne2OneConversationRequest alice bob actor desired Nothing
    res <-
      iUpsertOne2OneConversation req
        <!! statusCode === const 200
    uuorConvId <$> responseJsonError res

  liftIO $ convId @?= expectedConvId

  if shouldBeLocal
    then
      ( do
          members <- case actor of
            LocalActor -> runMaybeT $ do
              resp <- lift $ getConvQualified (tUnqualified alice) convId
              guard $ statusCode resp == 200
              conv <- lift $ responseJsonError resp
              pure . map omQualifiedId . cmOthers . cnvMembers $ conv
            RemoteActor -> do
              fedGalleyClient <- view tsFedGalleyClient
              GetConversationsResponse convs <-
                runFedClient @"get-conversations" fedGalleyClient (tDomain bob) $
                  F.GetConversationsRequest
                    { F.gcrUserId = tUnqualified bob,
                      F.gcrConvIds = [qUnqualified convId]
                    }
              pure
                . fmap (map omQualifiedId . rcmOthers . rcnvMembers)
                . listToMaybe
                $ convs
          liftIO $ case desired of
            Included -> members @?= Just []
            Excluded -> members @?= Nothing
      )
    else
      ( do
          found <- do
            let rconv = mkProteusConv (qUnqualified convId) (tUnqualified bob) roleNameWireAdmin []
            (resp, _) <-
              withTempMockFederator' (mockReply (F.GetConversationsResponse [rconv])) $
                getConvQualified (tUnqualified alice) convId
            pure $ statusCode resp == 200
          liftIO $ found @?= ((actor, desired) == (LocalActor, Included))
      )
