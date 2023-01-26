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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module API.Federation where

import API.Util
import Bilge hiding (head)
import Bilge.Assert
import Control.Lens hiding ((#))
import qualified Data.Aeson as A
import Data.ByteString.Conversion (toByteString')
import Data.Domain
import Data.Id (ConvId, Id (..), UserId, newClientId, randomId)
import Data.Json.Util hiding ((#))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import qualified Data.List1 as List1
import qualified Data.Map as Map
import qualified Data.ProtoLens as Protolens
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Data.Singletons
import Data.String.Conversions
import Data.Time.Clock
import Data.Timeout (TimeoutUnit (..), (#))
import Data.UUID.V4 (nextRandom)
import Federator.MockServer
import Galley.Types.Conversations.Intra
import Imports
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as FedGalley
import Wire.API.Federation.Component
import Wire.API.Internal.Notification
import Wire.API.Message
import Wire.API.User.Client (PubClient (..))
import Wire.API.User.Profile

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "federation"
    [ test s "POST /federation/get-conversations : All Found" getConversationsAllFound,
      test s "POST /federation/get-conversations : Conversations user is not a part of are excluded from result" getConversationsNotPartOf,
      test s "POST /federation/on-conversation-created : Add local user to remote conversation" onConvCreated,
      test s "POST /federation/on-conversation-updated : Add local user to remote conversation" addLocalUser,
      test s "POST /federation/on-conversation-updated : Add only unconnected local users to remote conversation" addUnconnectedUsersOnly,
      test s "POST /federation/on-conversation-updated : Notify local user about other members joining" addRemoteUser,
      test s "POST /federation/on-conversation-updated : Remove a local user from a remote conversation" removeLocalUser,
      test s "POST /federation/on-conversation-updated : Remove a remote user from a remote conversation" removeRemoteUser,
      test s "POST /federation/on-conversation-updated : Notify local user about conversation rename" notifyConvRename,
      test s "POST /federation/on-conversation-updated : Notify local user about message timer update" notifyMessageTimer,
      test s "POST /federation/on-conversation-updated : Notify local user about member update" notifyMemberUpdate,
      test s "POST /federation/on-conversation-updated : Notify local user about receipt mode update" notifyReceiptMode,
      test s "POST /federation/on-conversation-updated : Notify local user about access update" notifyAccess,
      test s "POST /federation/on-conversation-updated : Notify local users about a deleted conversation" notifyDeletedConversation,
      test s "POST /federation/leave-conversation : Success" leaveConversationSuccess,
      test s "POST /federation/leave-conversation : Non-existent" leaveConversationNonExistent,
      test s "POST /federation/leave-conversation : Invalid type" leaveConversationInvalidType,
      test s "POST /federation/on-message-sent : Receive a message from another backend" onMessageSent,
      test s "POST /federation/send-message : Post a message sent from another backend" sendMessage,
      test s "POST /federation/on-user-deleted-conversations : Remove deleted remote user from local conversations" onUserDeleted,
      test s "POST /federation/update-conversation : Update local conversation by a remote admin " updateConversationByRemoteAdmin
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  bobQ <- randomQualifiedUser
  let bob = qUnqualified bobQ
      lBob = toLocalUnsafe (qDomain bobQ) (qUnqualified bobQ)
  (rAlice, cnv1Id) <- generateRemoteAndConvId True lBob
  let aliceQ = tUntagged rAlice
  carlQ <- randomQualifiedUser

  connectUsers bob (singleton (qUnqualified carlQ))
  connectWithRemoteUser bob aliceQ

  -- create & get group conv
  cnv2 <-
    responseJsonError
      =<< postConvWithRemoteUsers
        bob
        defNewProteusConv {newConvQualifiedUsers = [aliceQ, carlQ]}

  -- create a one-to-one conversation between bob and alice
  do
    let createO2O =
          UpsertOne2OneConversationRequest
            { uooLocalUser = lBob,
              uooRemoteUser = rAlice,
              uooActor = LocalActor,
              uooActorDesiredMembership = Included,
              uooConvId = Just cnv1Id
            }
    UpsertOne2OneConversationResponse cnv1IdReturned <-
      responseJsonError
        =<< iUpsertOne2OneConversation createO2O
    liftIO $ assertEqual "Mismatch in the generated conversation ID" cnv1IdReturned cnv1Id

  do
    convs <-
      responseJsonError
        =<< getConvs bob [cnv1Id, cnvQualifiedId cnv2] <!! do
          const 200 === statusCode
    liftIO $
      sort (map cnvQualifiedId (crFound convs))
        @?= sort [cnv1Id, cnvQualifiedId cnv2]

  -- get conversations

  fedGalleyClient <- view tsFedGalleyClient

  GetConversationsResponse convs <-
    runFedClient @"get-conversations" fedGalleyClient (qDomain aliceQ) $
      GetConversationsRequest
        (qUnqualified aliceQ)
        (map qUnqualified [cnv1Id, cnvQualifiedId cnv2])

  let c2 = find ((== qUnqualified (cnvQualifiedId cnv2)) . rcnvId) convs

  liftIO $ do
    assertEqual
      "name mismatch"
      (Just $ cnvName cnv2)
      (cnvmName . rcnvMetadata <$> c2)
    assertEqual
      "self member role mismatch"
      (Just . memConvRoleName . cmSelf $ cnvMembers cnv2)
      (rcmSelfRole . rcnvMembers <$> c2)
    assertEqual
      "other members mismatch"
      (Just (sort [bob, qUnqualified carlQ]))
      (fmap (sort . map (qUnqualified . omQualifiedId) . rcmOthers . rcnvMembers) c2)

-- @SF.Federation @TSFI.RESTfulAPI @S2
--
-- The test asserts that via a federation client a user cannot fetch
-- conversation details of a conversation they are not part of: they get an
-- empty list of details instead.
getConversationsNotPartOf :: TestM ()
getConversationsNotPartOf = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  localDomain <- viewFederationDomain
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  do
    convs <-
      responseJsonError
        =<< getConvs alice [cnvQualifiedId cnv1] <!! do
          const 200 === statusCode
    liftIO $
      map cnvQualifiedId (crFound convs)
        @?= [cnvQualifiedId cnv1]

  fedGalleyClient <- view tsFedGalleyClient
  rando <- Id <$> liftIO nextRandom
  GetConversationsResponse convs <-
    runFedClient @"get-conversations" fedGalleyClient localDomain $
      GetConversationsRequest rando [qUnqualified . cnvQualifiedId $ cnv1]
  liftIO $ assertEqual "conversation list not empty" [] convs

-- @END

onConvCreated :: TestM ()
onConvCreated = do
  c <- view tsCannon
  (alice, qAlice) <- randomUserTuple
  let remoteDomain = Domain "bobland.example.com"
  qBob <- Qualified <$> randomId <*> pure remoteDomain
  qDee <- Qualified <$> randomId <*> pure remoteDomain

  (charlie, qCharlie) <- randomUserTuple
  conv <- randomId
  let qconv = Qualified conv remoteDomain

  connectWithRemoteUser alice qBob
  -- Remote Bob creates a conversation with local Alice and Charlie;
  -- however Bob is not connected to Charlie but only to Alice.
  let requestMembers = Set.fromList (map asOtherMember [qAlice, qCharlie, qDee])

  WS.bracketR2 c alice charlie $ \(wsA, wsC) -> do
    registerRemoteConv qconv (qUnqualified qBob) (Just "gossip") requestMembers
    liftIO $ do
      let expectedSelf = qAlice
          expectedOthers = [(qBob, roleNameWireAdmin), (qDee, roleNameWireMember)]
          expectedFrom = qBob
      -- since Charlie is not connected to Bob; expect a conversation with Alice&Bob only
      WS.assertMatch_ (5 # Second) wsA $
        wsAssertConvCreateWithRole qconv expectedFrom expectedSelf expectedOthers
      WS.assertNoEvent (1 # Second) [wsC]
  convs <- listRemoteConvs remoteDomain alice
  liftIO $ convs @?= [Qualified conv remoteDomain]

addLocalUser :: TestM ()
addLocalUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  let qalice = Qualified alice localDomain
  let remoteDomain = Domain "bobland.example.com"
  bob <- randomId
  let qbob = Qualified bob remoteDomain
  charlie <- randomUser
  dee <- randomUser
  let qdee = Qualified dee localDomain
  conv <- randomId
  let qconv = Qualified conv remoteDomain

  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = now,
            FedGalley.cuOrigUserId = qbob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [charlie],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (qalice :| [qdee]) roleNameWireMember)
          }
  WS.bracketRN c [alice, charlie, dee] $ \[wsA, wsC, wsD] -> do
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu
    liftIO $ do
      WS.assertMatch_ (5 # Second) wsA $
        wsAssertMemberJoinWithRole qconv qbob [qalice] roleNameWireMember
      -- Since charlie is not really present in the conv, they don't get any
      -- notifications
      WS.assertNoEvent (1 # Second) [wsC]
      -- Since dee is not connected to bob, they don't get any notifications
      WS.assertNoEvent (1 # Second) [wsD]
  aliceConvs <- listRemoteConvs remoteDomain alice
  liftIO $ aliceConvs @?= [Qualified conv remoteDomain]
  deeConvs <- listRemoteConvs remoteDomain dee
  liftIO $ deeConvs @?= []

addUnconnectedUsersOnly :: TestM ()
addUnconnectedUsersOnly = do
  c <- view tsCannon
  (alice, qAlice) <- randomUserTuple
  (_charlie, qCharlie) <- randomUserTuple

  let remoteDomain = Domain "bobland.example.com"
  qBob <- Qualified <$> randomId <*> pure remoteDomain
  conv <- randomId
  let qconv = Qualified conv remoteDomain

  -- Bob is connected to Alice
  -- Bob is not connected to Charlie
  connectWithRemoteUser alice qBob
  let requestMembers = Set.fromList (map asOtherMember [qAlice])

  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  WS.bracketR c alice $ \wsA -> do
    -- Remote Bob creates a conversation with local Alice
    registerRemoteConv qconv (qUnqualified qBob) (Just "gossip") requestMembers
    liftIO $ do
      let expectedSelf = qAlice
          expectedOthers = [(qBob, roleNameWireAdmin)]
          expectedFrom = qBob
      WS.assertMatch_ (5 # Second) wsA $
        wsAssertConvCreateWithRole qconv expectedFrom expectedSelf expectedOthers

    -- Bob attempts to add unconnected Charlie (possible abuse)
    let cu =
          FedGalley.ConversationUpdate
            { FedGalley.cuTime = now,
              FedGalley.cuOrigUserId = qBob,
              FedGalley.cuConvId = conv,
              FedGalley.cuAlreadyPresentUsers = [alice],
              FedGalley.cuAction =
                SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (qCharlie :| []) roleNameWireMember)
            }
    -- Alice receives no notifications from this
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu
    WS.assertNoEvent (5 # Second) [wsA]

-- | This test invokes the federation endpoint:
--
--   'POST /federation/on-conversation-updated'
--
-- two times in a row: first adding a remote user to a local conversation, and
-- then removing them. The test asserts the expected list of conversations in
-- between the calls and after everything, and that a local conversation member
-- got notified of the removal.
removeLocalUser :: TestM ()
removeLocalUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  bob <- randomId
  let qAlice = Qualified alice localDomain
  let remoteDomain = Domain "bobland.example.com"
  let qBob = bob `Qualified` remoteDomain
  conv <- randomId
  let qconv = Qualified conv remoteDomain
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cuAdd =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = now,
            FedGalley.cuOrigUserId = qBob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qAlice) roleNameWireMember)
          }
      cuRemove =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = addUTCTime (secondsToNominalDiffTime 5) now,
            FedGalley.cuOrigUserId = qAlice,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [alice],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationLeaveTag) ()
          }

  connectWithRemoteUser alice qBob
  WS.bracketR c alice $ \ws -> do
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAdd
    afterAddition <- listRemoteConvs remoteDomain alice
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuRemove
    liftIO $ do
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMemberJoinWithRole qconv qBob [qAlice] roleNameWireMember
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMembersLeave qconv qAlice [qAlice]
    afterRemoval <- listRemoteConvs remoteDomain alice
    liftIO $ do
      afterAddition @?= [qconv]
      afterRemoval @?= []

-- characters:
--
-- alice: present local user
--
-- bob: present remote user
--
-- charlie: not present local user
--
-- dee: local user being removed
--
-- eve: remote user being removed
--
-- flo: not present local user being removed
removeRemoteUser :: TestM ()
removeRemoteUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  [bob, eve] <- replicateM 2 randomId
  charlie <- randomUser
  qDee <- randomQualifiedUser
  qFlo <- randomQualifiedUser
  let qAlice = Qualified alice localDomain
      remoteDomain = Domain "bobland.example.com"
      qBob = Qualified bob remoteDomain
      dee = qUnqualified qDee
      qEve = Qualified eve remoteDomain
      flo = qUnqualified qFlo
      aliceAsOtherMember = OtherMember qAlice Nothing roleNameWireMember
      deeAsOtherMember = OtherMember qDee Nothing roleNameWireMember
      eveAsOtherMember = OtherMember qEve Nothing roleNameWireMember
  conv <- randomId
  let qconv = Qualified conv remoteDomain
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime

  mapM_ (`connectWithRemoteUser` qBob) [alice, dee]
  registerRemoteConv qconv (qUnqualified qBob) (Just "gossip") (Set.fromList [aliceAsOtherMember, deeAsOtherMember, eveAsOtherMember])

  let cuRemove user =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = addUTCTime (secondsToNominalDiffTime 5) now,
            FedGalley.cuOrigUserId = qBob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [alice, charlie, dee],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure user)
          }

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qEve)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qEve]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qDee)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qDee]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qFlo)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA] $
        wsAssertMembersLeave qconv qBob [qFlo]
      WS.assertNoEvent (1 # Second) [wsC, wsF, wsD]

notifyUpdate :: [Qualified UserId] -> SomeConversationAction -> EventType -> EventData -> TestM ()
notifyUpdate extras action etype edata = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomId
  charlie <- randomUser
  conv <- randomId
  let bdom = Domain "bob.example.com"
      qbob = Qualified bob bdom
      qconv = Qualified conv bdom
      mkMember quid = OtherMember quid Nothing roleNameWireMember
  fedGalleyClient <- view tsFedGalleyClient

  mapM_ (`connectWithRemoteUser` qbob) [alice]
  registerRemoteConv
    qconv
    bob
    (Just "gossip")
    (Set.fromList (map mkMember (qalice : extras)))

  now <- liftIO getCurrentTime
  let cu =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = now,
            FedGalley.cuOrigUserId = qbob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [alice, charlie],
            FedGalley.cuAction = action
          }
  WS.bracketR2 c alice charlie $ \(wsA, wsC) -> do
    runFedClient @"on-conversation-updated" fedGalleyClient bdom cu
    liftIO $ do
      WS.assertMatch_ (5 # Second) wsA $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv e @?= qconv
        evtType e @?= etype
        evtFrom e @?= qbob
        evtData e @?= edata
      WS.assertNoEvent (1 # Second) [wsC]

notifyConvRename :: TestM ()
notifyConvRename = do
  let d = ConversationRename "gossip++"
  notifyUpdate [] (SomeConversationAction (sing @'ConversationRenameTag) d) ConvRename (EdConvRename d)

notifyMessageTimer :: TestM ()
notifyMessageTimer = do
  let d = ConversationMessageTimerUpdate (Just 5000)
  notifyUpdate
    []
    (SomeConversationAction (sing @'ConversationMessageTimerUpdateTag) d)
    ConvMessageTimerUpdate
    (EdConvMessageTimerUpdate d)

notifyReceiptMode :: TestM ()
notifyReceiptMode = do
  let d = ConversationReceiptModeUpdate (ReceiptMode 42)
  notifyUpdate
    []
    (SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) d)
    ConvReceiptModeUpdate
    (EdConvReceiptModeUpdate d)

notifyAccess :: TestM ()
notifyAccess = do
  let d = ConversationAccessData (Set.fromList [InviteAccess, LinkAccess]) (Set.fromList [TeamMemberAccessRole])
  notifyUpdate
    []
    (SomeConversationAction (sing @'ConversationAccessDataTag) d)
    ConvAccessUpdate
    (EdConvAccessUpdate d)

notifyMemberUpdate :: TestM ()
notifyMemberUpdate = do
  qdee <- randomQualifiedUser
  let d =
        MemberUpdateData
          { misTarget = qdee,
            misOtrMutedStatus = Nothing,
            misOtrMutedRef = Nothing,
            misOtrArchived = Nothing,
            misOtrArchivedRef = Nothing,
            misHidden = Nothing,
            misHiddenRef = Nothing,
            misConvRoleName = Just roleNameWireAdmin
          }
  notifyUpdate
    [qdee]
    (SomeConversationAction (sing @'ConversationMemberUpdateTag) (ConversationMemberUpdate qdee (OtherMemberUpdate (Just roleNameWireAdmin))))
    MemberStateUpdate
    (EdMemberUpdate d)

notifyDeletedConversation :: TestM ()
notifyDeletedConversation = do
  c <- view tsCannon

  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  bob <- randomId
  conv <- randomId
  let bobDomain = Domain "bob.example.com"
      qbob = Qualified bob bobDomain
      qconv = Qualified conv bobDomain
      mkMember quid = OtherMember quid Nothing roleNameWireMember

  mapM_ (`connectWithRemoteUser` qbob) [alice]
  registerRemoteConv
    qconv
    bob
    (Just "gossip")
    (Set.fromList (map mkMember [qalice]))

  fedGalleyClient <- view tsFedGalleyClient

  do
    aliceConvs <- listRemoteConvs bobDomain alice
    liftIO $ aliceConvs @?= [qconv]

  WS.bracketR c alice $ \wsAlice -> do
    now <- liftIO getCurrentTime
    let cu =
          FedGalley.ConversationUpdate
            { FedGalley.cuTime = now,
              FedGalley.cuOrigUserId = qbob,
              FedGalley.cuConvId = qUnqualified qconv,
              FedGalley.cuAlreadyPresentUsers = [alice],
              FedGalley.cuAction = SomeConversationAction (sing @'ConversationDeleteTag) ()
            }
    runFedClient @"on-conversation-updated" fedGalleyClient bobDomain cu

    liftIO $ do
      WS.assertMatch_ (5 # Second) wsAlice $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ConvDelete @=? evtType e

  do
    aliceConvs <- listRemoteConvs bobDomain alice
    liftIO $ aliceConvs @?= []

-- TODO: test adding non-existing users
-- TODO: test adding resulting in an empty notification

-- characters:
--
-- alice: present local user
--
-- bob: present remote user
--
-- charlie: not present local user
--
-- dee: present local user being added
--
-- eve: remote user being added
--
-- flo: not present local user being added
addRemoteUser :: TestM ()
addRemoteUser = do
  c <- view tsCannon
  let bdom = Domain "bob.example.com"
      edom = Domain "eve.example.com"
  qalice <- randomQualifiedUser
  qbob <- randomQualifiedId bdom
  qcharlie <- randomQualifiedUser
  qdee <- randomQualifiedUser
  qeve <- randomQualifiedId edom
  qflo <- randomQualifiedUser

  qconv <- randomQualifiedId bdom
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime

  mapM_ (flip connectWithRemoteUser qbob . qUnqualified) [qalice, qdee]

  registerRemoteConv qconv (qUnqualified qbob) (Just "gossip") (Set.fromList (map asOtherMember [qalice, qdee, qeve]))

  -- The conversation owning
  let cu =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = now,
            FedGalley.cuOrigUserId = qbob,
            FedGalley.cuConvId = qUnqualified qconv,
            FedGalley.cuAlreadyPresentUsers = map qUnqualified [qalice, qcharlie],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (qdee :| [qeve, qflo]) roleNameWireMember)
          }
  WS.bracketRN c (map qUnqualified [qalice, qcharlie, qdee, qflo]) $ \[wsA, wsC, wsD, wsF] -> do
    runFedClient @"on-conversation-updated" fedGalleyClient bdom cu
    void . liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsA, wsD] $
        wsAssertMemberJoinWithRole qconv qbob [qeve, qdee] roleNameWireMember
      WS.assertNoEvent (1 # Second) [wsC]
      WS.assertNoEvent (1 # Second) [wsF]

leaveConversationSuccess :: TestM ()
leaveConversationSuccess = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  [alice, bob] <- randomUsers 2
  let qBob = Qualified bob localDomain
      remoteDomain1 = Domain "far-away-1.example.com"
      remoteDomain2 = Domain "far-away-2.example.com"
  qChad <- (`Qualified` remoteDomain1) <$> randomId
  qDee <- (`Qualified` remoteDomain1) <$> randomId
  qEve <- (`Qualified` remoteDomain2) <$> randomId
  connectUsers alice (singleton bob)
  connectWithRemoteUser alice qChad
  connectWithRemoteUser alice qDee
  connectWithRemoteUser alice qEve

  let mock = do
        guardRPC "get-users-by-ids"
        d <- frTargetDomain <$> getRequest
        asum
          [ guard (d == remoteDomain1)
              *> mockReply [mkProfile qChad (Name "Chad"), mkProfile qDee (Name "Dee")],
            guard (d == remoteDomain2)
              *> mockReply [mkProfile qEve (Name "Eve")]
          ]

  (convId, _) <-
    withTempMockFederator' (mock <|> mockReply ()) $
      decodeConvId
        <$> postConvQualified
          alice
          defNewProteusConv
            { newConvQualifiedUsers = [qBob, qChad, qDee, qEve]
            }
  let qconvId = Qualified convId localDomain

  (_, federatedRequests) <-
    WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
      withTempMockFederator' (mock <|> mockReply ()) $ do
        g <- viewGalley
        let leaveRequest = FedGalley.LeaveConversationRequest convId (qUnqualified qChad)
        respBS <-
          post
            ( g
                . paths ["federation", "leave-conversation"]
                . content "application/json"
                . header "Wire-Origin-Domain" (toByteString' remoteDomain1)
                . json leaveRequest
            )
            <!! const 200 === statusCode
        parsedResp <- responseJsonError respBS
        liftIO $ do
          FedGalley.leaveResponse parsedResp @?= Right ()
          void . WS.assertMatch (3 # Second) wsAlice $
            wsAssertMembersLeave qconvId qChad [qChad]
          void . WS.assertMatch (3 # Second) wsBob $
            wsAssertMembersLeave qconvId qChad [qChad]

  liftIO $ fedRequestsForDomain remoteDomain1 Galley federatedRequests @?= []
  let [remote2GalleyFederatedRequest] = fedRequestsForDomain remoteDomain2 Galley federatedRequests
  assertLeaveUpdate remote2GalleyFederatedRequest qconvId qChad [qUnqualified qEve]

leaveConversationNonExistent :: TestM ()
leaveConversationNonExistent = do
  let remoteDomain = Domain "far-away.example.com"
  alice <- randomQualifiedId remoteDomain
  conv <- randomId

  g <- viewGalley
  let leaveRequest = FedGalley.LeaveConversationRequest conv (qUnqualified alice)
  resp <-
    fmap FedGalley.leaveResponse $
      responseJsonError
        =<< post
          ( g
              . paths ["federation", "leave-conversation"]
              . content "application/json"
              . header "Wire-Origin-Domain" (toByteString' remoteDomain)
              . json leaveRequest
          )
          <!! const 200 === statusCode
  liftIO $ resp @?= Left FedGalley.RemoveFromConversationErrorNotFound

leaveConversationInvalidType :: TestM ()
leaveConversationInvalidType = do
  let remoteDomain = Domain "far-away.example.com"
  alice <- qTagUnsafe <$> randomQualifiedUser

  (bob, conv) <- generateRemoteAndConvIdWithDomain remoteDomain True alice
  connectWithRemoteUser (tUnqualified alice) (tUntagged bob)
  createOne2OneConvWithRemote alice bob

  g <- viewGalley
  let leaveRequest = FedGalley.LeaveConversationRequest (qUnqualified conv) (tUnqualified bob)
  resp <-
    fmap FedGalley.leaveResponse $
      responseJsonError
        =<< post
          ( g
              . paths ["federation", "leave-conversation"]
              . content "application/json"
              . header "Wire-Origin-Domain" (toByteString' remoteDomain)
              . json leaveRequest
          )
          <!! const 200 === statusCode
  liftIO $ resp @?= Left FedGalley.RemoveFromConversationErrorRemovalNotAllowed

onMessageSent :: TestM ()
onMessageSent = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  eve <- randomUser
  bob <- randomId
  conv <- randomId
  let fromc = newClientId 0
      aliceC1 = newClientId 0
      aliceC2 = newClientId 1
      eveC = newClientId 0
      bdom = Domain "bob.example.com"
      qconv = Qualified conv bdom
      qbob = Qualified bob bdom
      qalice = Qualified alice localDomain
  now <- liftIO getCurrentTime
  fedGalleyClient <- view tsFedGalleyClient

  -- only add alice to the remote conversation
  connectWithRemoteUser alice qbob
  let cu =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = now,
            FedGalley.cuOrigUserId = qbob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [],
            FedGalley.cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient bdom cu

  let txt = "Hello from another backend"
      msg client = Map.fromList [(client, txt)]
      rcpts =
        UserClientMap $
          Map.fromListWith (<>) [(alice, msg aliceC1), (alice, msg aliceC2), (eve, msg eveC)]
      rm =
        FedGalley.RemoteMessage
          { FedGalley.rmTime = now,
            FedGalley.rmData = Nothing,
            FedGalley.rmSender = qbob,
            FedGalley.rmSenderClient = fromc,
            FedGalley.rmConversation = conv,
            FedGalley.rmPriority = Nothing,
            FedGalley.rmTransient = False,
            FedGalley.rmPush = False,
            FedGalley.rmRecipients = rcpts
          }

  -- send message to alice and check reception
  WS.bracketAsClientRN c [(alice, aliceC1), (alice, aliceC2), (eve, eveC)] $ \[wsA1, wsA2, wsE] -> do
    runFedClient @"on-message-sent" fedGalleyClient bdom rm
    liftIO $ do
      -- alice should receive the message on her first client
      WS.assertMatch_ (5 # Second) wsA1 $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv e @?= qconv
        evtType e @?= OtrMessageAdd
        evtFrom e @?= qbob
        evtData e @?= EdOtrMessage (OtrMessage fromc aliceC1 txt Nothing)

      -- alice should receive the message on her second client
      WS.assertMatch_ (5 # Second) wsA2 $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv e @?= qconv
        evtType e @?= OtrMessageAdd
        evtFrom e @?= qbob
        evtData e @?= EdOtrMessage (OtrMessage fromc aliceC2 txt Nothing)

      -- These should be the only events for each device of alice. This verifies
      -- that targetted delivery to the clients was used so that client 2 does
      -- not receive the message encrypted for client 1 and vice versa.
      WS.assertNoEvent (1 # Second) [wsA1]
      WS.assertNoEvent (1 # Second) [wsA2]

      -- eve should not receive the message
      WS.assertNoEvent (1 # Second) [wsE]

-- alice local, bob and chad remote in a local conversation
-- bob sends a message (using the RPC), we test that alice receives it and that
-- a call is made to the onMessageSent RPC to inform chad
sendMessage :: TestM ()
sendMessage = do
  cannon <- view tsCannon
  let remoteDomain = Domain "far-away.example.com"
  localDomain <- viewFederationDomain

  -- users and clients
  (alice, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  let aliceId = qUnqualified alice
  bobId <- randomId
  bobClient <- liftIO $ generate arbitrary
  let bob = Qualified bobId remoteDomain
      bobProfile = mkProfile bob (Name "Bob")
  chadId <- randomId
  chadClient <- liftIO $ generate arbitrary
  let chad = Qualified chadId remoteDomain
      chadProfile = mkProfile chad (Name "Chad")

  connectWithRemoteUser aliceId bob
  connectWithRemoteUser aliceId chad
  -- conversation
  let responses1 = guardComponent Brig *> mockReply [bobProfile, chadProfile]
  (convId, requests1) <-
    withTempMockFederator' (responses1 <|> mockReply ()) $
      fmap decodeConvId $
        postConvQualified
          aliceId
          defNewProteusConv
            { newConvQualifiedUsers = [bob, chad]
            }
          <!! const 201 === statusCode

  liftIO $ do
    [galleyReq] <- case requests1 of
      xs@[_] -> pure xs
      _ -> assertFailure "unexpected number of requests"
    frComponent galleyReq @?= Galley
    frRPC galleyReq @?= "on-conversation-created"
  let conv = Qualified convId localDomain

  -- we use bilge instead of the federation client to make a federated request
  -- here, because we need to make use of the mock federator, which at the moment
  -- supports only bilge requests
  let rcpts =
        [ (alice, aliceClient, "hi alice"),
          (chad, chadClient, "hi chad")
        ]
      msg = mkQualifiedOtrPayload bobClient rcpts "" MismatchReportAll
      msr =
        FedGalley.ProteusMessageSendRequest
          { FedGalley.pmsrConvId = convId,
            FedGalley.pmsrSender = bobId,
            FedGalley.pmsrRawMessage = Base64ByteString (Protolens.encodeMessage msg)
          }
  let mock = do
        guardComponent Brig
        mockReply $
          Map.fromList
            [ (chadId, Set.singleton (PubClient chadClient Nothing)),
              (bobId, Set.singleton (PubClient bobClient Nothing))
            ]
  (_, requests2) <- withTempMockFederator' (mock <|> mockReply ()) $ do
    WS.bracketR cannon aliceId $ \ws -> do
      g <- viewGalley
      msresp <-
        post
          ( g
              . paths ["federation", "send-message"]
              . content "application/json"
              . header "Wire-Origin-Domain" (toByteString' remoteDomain)
              . json msr
          )
          <!! do
            const 200 === statusCode
      (FedGalley.MessageSendResponse eithStatus) <- responseJsonError msresp
      liftIO $ case eithStatus of
        Left err -> assertFailure $ "Expected Right, got Left: " <> show err
        Right mss -> do
          assertEqual "missing clients should be empty" mempty (mssMissingClients mss)
          assertEqual "redundant clients should be empty" mempty (mssRedundantClients mss)
          assertEqual "deleted clients should be empty" mempty (mssDeletedClients mss)
          assertEqual "failed to send should be empty" mempty (mssFailedToSend mss)

      -- check that alice received the message
      WS.assertMatch_ (5 # Second) ws $
        wsAssertOtr' "" conv bob bobClient aliceClient (toBase64Text "hi alice")

  -- check that a request to propagate message to chad has been made
  liftIO $ do
    [_clientReq, receiveReq] <- case requests2 of
      xs@[_, _] -> pure xs
      _ -> assertFailure "unexpected number of requests"
    frComponent receiveReq @?= Galley
    frRPC receiveReq @?= "on-message-sent"
    rm <- case A.decode (frBody receiveReq) of
      Nothing -> assertFailure "invalid federated request body"
      Just x -> pure (x :: FedGalley.RemoteMessage ConvId)
    FedGalley.rmSender rm @?= bob
    Map.keysSet (userClientMap (FedGalley.rmRecipients rm))
      @?= Set.singleton chadId

-- | There are 3 backends in action here:
--
-- - Backend A (local) has Alice and Alex
-- - Backend B has Bob and Bart
-- - Backend C has Carl
--
-- Bob is in these convs:
-- - One2One Conv with Alice (ooConvId)
-- - Group conv with all users (groupConvId)
--
-- When bob gets deleted, backend A gets an RPC from bDomain stating that bob is
-- deleted and they would like bob to leave these converstaions:
-- - ooConvId -> Causes Alice to be notified
-- - groupConvId -> Causes Alice and Alex to be notified
-- - extraConvId -> Ignored
-- - noBobConvId -> Ignored
onUserDeleted :: TestM ()
onUserDeleted = do
  cannon <- view tsCannon
  let bDomain = Domain "b.far-away.example.com"
      cDomain = Domain "c.far-away.example.com"

  alice <- qTagUnsafe <$> randomQualifiedUser
  alex <- randomQualifiedUser
  (bob, ooConvId) <- generateRemoteAndConvIdWithDomain bDomain True alice
  bart <- randomQualifiedId bDomain
  carl <- randomQualifiedId cDomain

  connectWithRemoteUser (tUnqualified alice) (tUntagged bob)
  connectUsers (tUnqualified alice) (pure (qUnqualified alex))
  connectWithRemoteUser (tUnqualified alice) bart
  connectWithRemoteUser (tUnqualified alice) carl

  -- create 1-1 conversation between alice and bob
  createOne2OneConvWithRemote alice bob

  -- create group conversation with everybody
  groupConvId <-
    decodeQualifiedConvId
      <$> ( postConvWithRemoteUsers
              (tUnqualified alice)
              defNewProteusConv {newConvQualifiedUsers = [tUntagged bob, alex, bart, carl]}
              <!! const 201 === statusCode
          )

  -- extraneous conversation
  extraConvId <- randomId

  -- conversation without bob
  noBobConvId <-
    fmap decodeQualifiedConvId $
      postConvQualified (tUnqualified alice) defNewProteusConv {newConvQualifiedUsers = [alex]}
        <!! const 201 === statusCode

  WS.bracketR2 cannon (tUnqualified alice) (qUnqualified alex) $ \(wsAlice, wsAlex) -> do
    (resp, rpcCalls) <- withTempMockFederator' (mockReply ()) $ do
      let udcn =
            FedGalley.UserDeletedConversationsNotification
              { FedGalley.udcvUser = tUnqualified bob,
                FedGalley.udcvConversations =
                  unsafeRange
                    [ qUnqualified ooConvId,
                      qUnqualified groupConvId,
                      extraConvId,
                      qUnqualified noBobConvId
                    ]
              }
      g <- viewGalley
      responseJsonError
        =<< post
          ( g
              . paths ["federation", "on-user-deleted-conversations"]
              . content "application/json"
              . header "Wire-Origin-Domain" (toByteString' (tDomain bob))
              . json udcn
          )
          <!! const 200 === statusCode

    ooConvAfterDel <- responseJsonError =<< getConvQualified (tUnqualified alice) ooConvId <!! const 200 === statusCode
    groupConvAfterDel <- responseJsonError =<< getConvQualified (tUnqualified alice) groupConvId <!! const 200 === statusCode

    liftIO $ do
      resp @?= EmptyResponse

      -- Assert that bob gets removed from the conversation
      cmOthers (cnvMembers ooConvAfterDel) @?= []
      sort (map omQualifiedId (cmOthers (cnvMembers groupConvAfterDel))) @?= sort [alex, bart, carl]

      -- Assert that local user's get notifications only for the conversation
      -- bob was part of and it wasn't a One2OneConv
      void . WS.assertMatch (3 # Second) wsAlice $
        wsAssertMembersLeave groupConvId (tUntagged bob) [tUntagged bob]
      void . WS.assertMatch (3 # Second) wsAlex $
        wsAssertMembersLeave groupConvId (tUntagged bob) [tUntagged bob]
      -- Alice shouldn't get any other notifications because we don't notify
      -- on One2One convs.
      --
      -- Alex shouldn't get any other notifications because alex was
      -- not part of any other conversations with bob.
      WS.assertNoEvent (1 # Second) [wsAlice, wsAlex]

      -- There should be only 1 RPC call made to eve's domain for groupConvId.
      -- Bob's domain does not get a notification, because it's the one making
      -- the request.
      assertEqual ("Expected 1 RPC calls, got: " <> show rpcCalls) 1 (length rpcCalls)

      -- Assertions about RPC to 'cDomain'
      cDomainRPC <- assertOne $ filter (\c -> frTargetDomain c == cDomain) rpcCalls
      cDomainRPCReq <- assertRight $ parseFedRequest cDomainRPC
      FedGalley.cuOrigUserId cDomainRPCReq @?= tUntagged bob
      FedGalley.cuConvId cDomainRPCReq @?= qUnqualified groupConvId
      FedGalley.cuAlreadyPresentUsers cDomainRPCReq @?= [qUnqualified carl]
      FedGalley.cuAction cDomainRPCReq @?= SomeConversationAction (sing @'ConversationLeaveTag) ()

-- | We test only ReceiptMode update here
--
-- A : local domain, owns the conversation
-- B : bob is an admin of the converation
-- C : charlie is a regular member of the conversation
updateConversationByRemoteAdmin :: TestM ()
updateConversationByRemoteAdmin = do
  c <- view tsCannon
  (alice, qalice) <- randomUserTuple

  let bdomain = Domain "b.example.com"
      cdomain = Domain "c.example.com"
  qbob <- randomQualifiedId bdomain
  qcharlie <- randomQualifiedId cdomain
  mapM_ (connectWithRemoteUser alice) [qbob, qcharlie]

  let convName = "Test Conv"
  WS.bracketR c alice $ \wsAlice -> do
    (rsp, _federatedRequests) <-
      withTempMockFederator' (mockReply ()) $ do
        postConvQualified alice defNewProteusConv {newConvName = checked convName, newConvQualifiedUsers = [qbob, qcharlie]}
          <!! const 201 === statusCode

    cnv <- assertConv rsp RegularConv alice qalice [qbob, qcharlie] (Just convName) Nothing

    let newReceiptMode = ReceiptMode 41
    let action = SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) (ConversationReceiptModeUpdate newReceiptMode)

    (_, federatedRequests) <-
      withTempMockFederator' (mockReply ()) $ do
        -- promote chad to admin
        putOtherMemberQualified alice qbob (OtherMemberUpdate (Just roleNameWireAdmin)) cnv
          !!! const 200 === statusCode

        -- bob updates the conversation
        let cnvUpdateRequest =
              ConversationUpdateRequest
                { curUser = qUnqualified qbob,
                  curConvId = qUnqualified cnv,
                  curAction = action
                }
        resp <- do
          fedGalleyClient <- view tsFedGalleyClient
          runFedClient @"update-conversation" fedGalleyClient bdomain cnvUpdateRequest

        cnvUpdate' <- liftIO $ case resp of
          ConversationUpdateResponseError err -> assertFailure ("Expected ConversationUpdateResponseUpdate but got " <> show err)
          ConversationUpdateResponseNoChanges -> assertFailure "Expected ConversationUpdateResponseUpdate but got ConversationUpdateResponseNoChanges"
          ConversationUpdateResponseUpdate up -> pure up

        liftIO $ do
          cuOrigUserId cnvUpdate' @?= qbob
          cuAlreadyPresentUsers cnvUpdate' @?= [qUnqualified qbob]
          cuAction cnvUpdate' @?= action

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertConvReceiptModeUpdate cnv qalice newReceiptMode n

    -- backend B does *not* get notified of the conversation update ony of bob's promotion
    liftIO $ do
      [(_fr, cUpdate)] <- mapM parseConvUpdate $ filter (\r -> frTargetDomain r == bdomain) federatedRequests
      assertBool "Action is not a ConversationMemberUpdate" (isJust (getConvAction (sing @'ConversationMemberUpdateTag) (cuAction cUpdate)))

    -- conversation has been modified by action
    updatedConv :: Conversation <- fmap responseJsonUnsafe $ getConvQualified alice cnv <!! const 200 === statusCode
    liftIO $
      (cnvmReceiptMode . cnvMetadata) updatedConv @?= Just newReceiptMode

    -- backend C gets notified of the conversation update and bob's promotion
    liftIO $ do
      dUpdates <- mapM parseConvUpdate $ filter (\r -> frTargetDomain r == cdomain) federatedRequests

      (_fr1, _cu1, _up1) <- assertOne $ mapMaybe (\(fr, up) -> getConvAction (sing @'ConversationMemberUpdateTag) (cuAction up) <&> (fr,up,)) dUpdates

      (_fr2, convUpdate, receiptModeUpdate) <- assertOne $ mapMaybe (\(fr, up) -> getConvAction (sing @'ConversationReceiptModeUpdateTag) (cuAction up) <&> (fr,up,)) dUpdates

      cruReceiptMode receiptModeUpdate @?= newReceiptMode
      cuOrigUserId convUpdate @?= qbob
      cuConvId convUpdate @?= qUnqualified cnv
      cuAlreadyPresentUsers convUpdate @?= [qUnqualified qcharlie]

    WS.assertMatch_ (5 # Second) wsAlice $ \n -> do
      wsAssertConvReceiptModeUpdate cnv qbob newReceiptMode n
  where
    _toOtherMember qid = OtherMember qid Nothing roleNameWireAdmin
    _convView cnv usr = responseJsonUnsafeWithMsg "conversation" <$> getConv usr cnv

    parseConvUpdate :: FederatedRequest -> IO (FederatedRequest, ConversationUpdate)
    parseConvUpdate rpc = do
      frComponent rpc @?= Galley
      frRPC rpc @?= "on-conversation-updated"
      let convUpdate :: ConversationUpdate = fromRight (error $ "Could not parse ConversationUpdate from " <> show (frBody rpc)) $ A.eitherDecode (frBody rpc)
      pure (rpc, convUpdate)

getConvAction :: Sing tag -> SomeConversationAction -> Maybe (ConversationAction tag)
getConvAction tquery (SomeConversationAction tag action) =
  case (tag, tquery) of
    (SConversationJoinTag, SConversationJoinTag) -> Just action
    (SConversationJoinTag, _) -> Nothing
    (SConversationLeaveTag, SConversationLeaveTag) -> Just action
    (SConversationLeaveTag, _) -> Nothing
    (SConversationMemberUpdateTag, SConversationMemberUpdateTag) -> Just action
    (SConversationMemberUpdateTag, _) -> Nothing
    (SConversationDeleteTag, SConversationDeleteTag) -> Just action
    (SConversationDeleteTag, _) -> Nothing
    (SConversationRenameTag, SConversationRenameTag) -> Just action
    (SConversationRenameTag, _) -> Nothing
    (SConversationMessageTimerUpdateTag, SConversationMessageTimerUpdateTag) -> Just action
    (SConversationMessageTimerUpdateTag, _) -> Nothing
    (SConversationReceiptModeUpdateTag, SConversationReceiptModeUpdateTag) -> Just action
    (SConversationReceiptModeUpdateTag, _) -> Nothing
    (SConversationAccessDataTag, SConversationAccessDataTag) -> Just action
    (SConversationAccessDataTag, _) -> Nothing
    (SConversationRemoveMembersTag, SConversationRemoveMembersTag) -> Just action
    (SConversationRemoveMembersTag, _) -> Nothing
