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

module API.Roles where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Control.Lens (view)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion (toByteString')
import Data.Domain
import Data.Id
import Data.List1
import qualified Data.List1 as List1
import Data.Qualified
import qualified Data.Set as Set
import Data.Singletons
import Federator.MockServer
import Imports
import Network.Wai.Utilities.Error
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Federation.Component
import Wire.API.Internal.Notification (Notification (..))

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Conversation roles"
    [ test s "conversation roles admin (and downgrade)" handleConversationRoleAdmin,
      test s "conversation roles member (and upgrade)" handleConversationRoleMember,
      test s "conversation role update with remote users present" roleUpdateWithRemotes,
      test s "conversation access update with remote users present" accessUpdateWithRemotes,
      test s "conversation role update of remote member" roleUpdateRemoteMember,
      test s "get all conversation roles" testAllConversationRoles,
      test s "access role update with v2" testAccessRoleUpdateV2,
      test s "test access roles of new conversations" testConversationAccessRole
    ]

testAllConversationRoles :: TestM ()
testAllConversationRoles = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  let role = roleNameWireAdmin
  c <- decodeConvId <$> postConvWithRole alice [bob] (Just "gossip") [] Nothing Nothing role
  g <- viewGalley
  get
    ( g
        . paths ["conversations", toByteString' c, "roles"]
        . zUser alice
    )
    !!! do
      const 200 === statusCode
      const (Right (ConversationRolesList [convRoleWireAdmin, convRoleWireMember])) === responseJsonEither

handleConversationRoleAdmin :: TestM ()
handleConversationRoleAdmin = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  (chuck, qchuck) <- randomUserTuple
  (eve, qeve) <- randomUserTuple
  (jack, qjack) <- randomUserTuple
  connectUsers alice (list1 bob [chuck, eve, jack])
  connectUsers eve (singleton bob)
  connectUsers bob (singleton jack)
  let role = roleNameWireAdmin
  cid <- WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    rsp <- postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role
    void $ assertConvWithRole rsp RegularConv alice qalice [qbob, qchuck] (Just "gossip") Nothing role
    let cid = decodeConvId rsp
        qcid = Qualified cid localDomain
    -- Make sure everyone gets the correct event
    postMembersWithRole alice (pure qeve) qcid role !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        wsAssertMemberJoinWithRole qcid qalice [qeve] role
    -- Add a member to help out with testing
    postMembersWithRole alice (pure qjack) qcid roleNameWireMember !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        wsAssertMemberJoinWithRole qcid qalice [qjack] roleNameWireMember
    pure cid
  -- Added bob as a wire_admin and do the checks
  wireAdminChecks cid alice bob jack
  -- Demote bob and run the member checks
  WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    let updateDown = OtherMemberUpdate (Just roleNameWireMember)
        qcid = Qualified cid localDomain
    putOtherMember alice bob updateDown cid !!! assertActionSucceeded
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $ do
      wsAssertMemberUpdateWithRole qcid qalice bob roleNameWireMember
  wireMemberChecks cid bob alice jack

handleConversationRoleMember :: TestM ()
handleConversationRoleMember = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  (chuck, qchuck) <- randomUserTuple
  eve <- randomUser
  let qeve = Qualified eve localDomain
  jack <- randomUser
  connectUsers alice (list1 bob [chuck, eve])
  connectUsers bob (singleton chuck)
  connectUsers eve (list1 bob [jack])
  let role = roleNameWireMember
  cid <- WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    rsp <- postConvWithRole alice [bob, chuck] (Just "gossip") [] Nothing Nothing role
    void $ assertConvWithRole rsp RegularConv alice qalice [qbob, qchuck] (Just "gossip") Nothing role
    let cid = decodeConvId rsp
        qcid = Qualified cid localDomain
    -- Make sure everyone gets the correct event
    postMembersWithRole alice (pure qeve) qcid role !!! const 200 === statusCode
    void . liftIO $
      WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
        wsAssertMemberJoinWithRole qcid qalice [qeve] role
    pure cid
  -- Added bob as a wire_member and do the checks
  wireMemberChecks cid bob alice chuck
  -- Let's promote bob
  WS.bracketR3 c alice bob chuck $ \(wsA, wsB, wsC) -> do
    let qcid = Qualified cid localDomain
    let updateUp = OtherMemberUpdate (Just roleNameWireAdmin)
    -- Chuck cannot update, member only
    putOtherMember chuck bob updateUp cid !!! assertActionDenied
    putOtherMember alice bob updateUp cid !!! assertActionSucceeded
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $ do
      wsAssertMemberUpdateWithRole qcid qalice bob roleNameWireAdmin
  wireAdminChecks cid bob alice chuck

roleUpdateRemoteMember :: TestM ()
roleUpdateRemoteMember = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  qcharlie <- Qualified <$> randomId <*> pure remoteDomain
  let bob = qUnqualified qbob

  traverse_ (connectWithRemoteUser bob) [qalice, qcharlie]
  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice, qcharlie]}
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putOtherMemberQualified
          bob
          qcharlie
          (OtherMemberUpdate (Just roleNameWireMember))
          qconv
          !!! const 200 === statusCode

    req <- assertOne requests
    let mu =
          MemberUpdateData
            { misTarget = qcharlie,
              misOtrMutedStatus = Nothing,
              misOtrMutedRef = Nothing,
              misOtrArchived = Nothing,
              misOtrArchivedRef = Nothing,
              misHidden = Nothing,
              misHiddenRef = Nothing,
              misConvRoleName = Just roleNameWireMember
            }
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu
        @?= SomeConversationAction (sing @'ConversationMemberUpdateTag) (ConversationMemberUpdate qcharlie (OtherMemberUpdate (Just roleNameWireMember)))
      sort (F.cuAlreadyPresentUsers cu) @?= sort [qUnqualified qalice, qUnqualified qcharlie]

    liftIO . WS.assertMatch_ (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate mu

  conv <- responseJsonError =<< getConvQualified bob qconv <!! const 200 === statusCode
  let charlieAsMember = find (\m -> omQualifiedId m == qcharlie) (cmOthers (cnvMembers conv))
  liftIO $
    charlieAsMember
      @=? Just
        OtherMember
          { omQualifiedId = qcharlie,
            omService = Nothing,
            omConvRoleName = roleNameWireMember
          }

roleUpdateWithRemotes :: TestM ()
roleUpdateWithRemotes = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  qcharlie <- randomQualifiedUser
  let bob = qUnqualified qbob
      charlie = qUnqualified qcharlie

  connectUsers bob (singleton charlie)
  connectWithRemoteUser bob qalice
  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice, qcharlie]}
  let qconv = decodeQualifiedConvId resp

  WS.bracketR2 c bob charlie $ \(wsB, wsC) -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putOtherMemberQualified
          bob
          qcharlie
          (OtherMemberUpdate (Just roleNameWireAdmin))
          qconv
          !!! const 200 === statusCode

    req <- assertOne requests
    let mu =
          MemberUpdateData
            { misTarget = qcharlie,
              misOtrMutedStatus = Nothing,
              misOtrMutedRef = Nothing,
              misOtrArchived = Nothing,
              misOtrArchivedRef = Nothing,
              misHidden = Nothing,
              misHiddenRef = Nothing,
              misConvRoleName = Just roleNameWireAdmin
            }
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu
        @?= SomeConversationAction (sing @'ConversationMemberUpdateTag) (ConversationMemberUpdate qcharlie (OtherMemberUpdate (Just roleNameWireAdmin)))
      F.cuAlreadyPresentUsers cu @?= [qUnqualified qalice]

    liftIO . WS.assertMatchN_ (5 # Second) [wsB, wsC] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= MemberStateUpdate
      evtFrom e @?= qbob
      evtData e @?= EdMemberUpdate mu

accessUpdateWithRemotes :: TestM ()
accessUpdateWithRemotes = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  qcharlie <- randomQualifiedUser
  let bob = qUnqualified qbob
      charlie = qUnqualified qcharlie

  connectUsers bob (singleton charlie)
  connectWithRemoteUser bob qalice
  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice, qcharlie]}
  let qconv = decodeQualifiedConvId resp

  let access = ConversationAccessData (Set.singleton CodeAccess) (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  WS.bracketR2 c bob charlie $ \(wsB, wsC) -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putQualifiedAccessUpdate bob qconv access
          !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu @?= SomeConversationAction (sing @'ConversationAccessDataTag) access
      F.cuAlreadyPresentUsers cu @?= [qUnqualified qalice]

    liftIO . WS.assertMatchN_ (5 # Second) [wsB, wsC] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvAccessUpdate
      evtFrom e @?= qbob
      evtData e @?= EdConvAccessUpdate access

-- | Given an admin, another admin and a member run all
--   the necessary checks targeting the admin
wireAdminChecks ::
  ConvId ->
  UserId ->
  UserId ->
  UserId ->
  TestM ()
wireAdminChecks cid admin otherAdmin mem = do
  localDomain <- viewFederationDomain
  let role = roleNameWireAdmin
  let qcid = Qualified cid localDomain
      qadmin = Qualified admin localDomain
      qotherAdmin = Qualified otherAdmin localDomain
      qmem = Qualified mem localDomain
  (other, qother) <- randomUserTuple
  connectUsers admin (singleton other)
  -- Admins can perform all operations on the conversation; creator is not relevant

  -- Add members
  postMembers admin (pure qother) qcid !!! assertActionSucceeded
  -- Remove members, regardless of who they are
  forM_ [qotherAdmin, qmem] $ \victim -> do
    deleteMemberQualified admin victim qcid !!! assertActionSucceeded
    postMembersWithRole admin (pure victim) qcid role !!! assertActionSucceeded
  -- Modify the conversation name
  void $ putConversationName admin cid "gossip++" !!! assertActionSucceeded
  -- Modify other members roles
  forM_ [otherAdmin, mem] $ \victim -> do
    let updateDown = OtherMemberUpdate (Just roleNameWireMember)
    putOtherMember admin victim updateDown cid !!! assertActionSucceeded
    let updateUp = OtherMemberUpdate (Just roleNameWireAdmin)
    putOtherMember admin victim updateUp cid !!! assertActionSucceeded
  -- Updates for message timer, receipt mode or access
  putMessageTimerUpdate admin cid (ConversationMessageTimerUpdate $ Just 1000) !!! assertActionSucceeded
  putMessageTimerUpdate admin cid (ConversationMessageTimerUpdate $ Just 2000) !!! assertActionSucceeded
  putReceiptMode admin cid (ReceiptMode 0) !!! assertActionSucceeded
  putReceiptMode admin cid (ReceiptMode 1) !!! assertActionSucceeded
  let nonActivatedAccess = ConversationAccessData (Set.singleton CodeAccess) (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate admin qcid nonActivatedAccess !!! assertActionSucceeded
  let activatedAccess = ConversationAccessData (Set.singleton InviteAccess) (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate admin qcid activatedAccess !!! assertActionSucceeded
  -- Update your own member state
  let memUpdate = memberUpdate {mupOtrArchive = Just True}
  putMember admin memUpdate qcid !!! assertActionSucceeded
  -- You can also leave a conversation
  deleteMemberQualified admin qadmin qcid !!! assertActionSucceeded
  -- Readding the user
  postMembersWithRole otherAdmin (pure qadmin) qcid role !!! const 200 === statusCode

-- | Given a member, admin and otherMem, run all the necessary checks
--   targeting mem
wireMemberChecks ::
  ConvId ->
  UserId ->
  UserId ->
  UserId ->
  TestM ()
wireMemberChecks cid mem admin otherMem = do
  localDomain <- viewFederationDomain
  let role = roleNameWireMember
      qcid = Qualified cid localDomain
  (other, qother) <- randomUserTuple
  let qmem = Qualified mem localDomain
  connectUsers mem (singleton other)
  -- Members cannot perform pretty much any action on the conversation

  -- Cannot add members, regardless of their role
  postMembers mem (pure qother) qcid !!! assertActionDenied
  -- Cannot remove members, regardless of who they are
  forM_ ((`Qualified` localDomain) <$> [admin, otherMem]) $ \victim ->
    deleteMemberQualified mem victim qcid !!! assertActionDenied
  -- Cannot modify the conversation name
  void $ putConversationName mem cid "gossip++" !!! assertActionDenied
  -- Cannot modify other members roles
  forM_ [admin, otherMem] $ \victim -> do
    let update = OtherMemberUpdate (Just roleNameWireMember)
    putOtherMember mem victim update cid !!! assertActionDenied
  -- Make sure you cannot elevate your own role
  let sneakyOtherMemberUpdate = OtherMemberUpdate (Just roleNameWireAdmin)
  putOtherMember mem mem sneakyOtherMemberUpdate cid !!! do
    const 403 === statusCode
    const (Just "invalid-op") === fmap label . responseJsonUnsafe
  -- No updates for message timer, receipt mode or access
  putMessageTimerUpdate mem cid (ConversationMessageTimerUpdate Nothing) !!! assertActionDenied
  putReceiptMode mem cid (ReceiptMode 0) !!! assertActionDenied
  let nonActivatedAccess = ConversationAccessData (Set.singleton CodeAccess) (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, GuestAccessRole, ServiceAccessRole])
  putQualifiedAccessUpdate mem qcid nonActivatedAccess !!! assertActionDenied
  -- Finally, you can still do the following actions:

  -- Update your own member state
  let memUpdate = memberUpdate {mupOtrArchive = Just True}
  putMember mem memUpdate qcid !!! assertActionSucceeded
  -- Last option is to leave a conversation
  deleteMemberQualified mem qmem qcid !!! assertActionSucceeded
  -- Let's readd the user to make tests easier
  postMembersWithRole admin (pure qmem) qcid role !!! const 200 === statusCode

-- create a conversation and check that the access roles match
testConversationAccessRole :: TestM ()
testConversationAccessRole = do
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  let nc =
        defNewProteusConv
          { newConvQualifiedUsers = [bob],
            newConvAccessRoles = Just (Set.singleton TeamMemberAccessRole)
          }
  conv <-
    responseJsonError
      =<< postConvQualified (qUnqualified alice) nc
        <!! const 201 === statusCode
  liftIO $
    cnvAccessRoles conv @?= Set.singleton TeamMemberAccessRole

testAccessRoleUpdateV2 :: TestM ()
testAccessRoleUpdateV2 = do
  g <- view tsUnversionedGalley
  [alice, bob] <- createAndConnectUsers (replicate 2 Nothing)
  conv <-
    responseJsonError
      =<< postConvQualified
        (qUnqualified alice)
        defNewProteusConv
          { newConvQualifiedUsers = [bob]
          }
        <!! const 201 === statusCode
  let qcnv = cnvQualifiedId conv
  -- Using v2 qualified endpoint
  put
    ( g
        . paths
          [ "v2",
            "conversations",
            toByteString' (qDomain qcnv),
            toByteString' (qUnqualified qcnv),
            "access"
          ]
        . zUser (qUnqualified alice)
        . zConn "conn"
        . json
          ( object
              [ "access" .= ["invite" :: Text],
                "access_role_v2" .= ["guest" :: Text]
              ]
          )
    )
    !!! const 200 === statusCode
  -- Using v2 unqualified endpoint
  put
    ( g
        . paths
          [ "v2",
            "conversations",
            toByteString' (qUnqualified qcnv),
            "access"
          ]
        . zUser (qUnqualified alice)
        . zConn "conn"
        . json
          ( object
              [ "access" .= ["invite" :: Text],
                "access_role_v2" .= ["guest" :: Text]
              ]
          )
    )
    !!! const 204 === statusCode

--------------------------------------------------------------------------------
-- Utilities

assertActionSucceeded :: HasCallStack => Assertions ()
assertActionSucceeded = const 200 === statusCode

assertActionDenied :: HasCallStack => Assertions ()
assertActionDenied = do
  const 403 === statusCode
  const (Just "action-denied") === fmap label . responseJsonUnsafe
