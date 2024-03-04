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
import Control.Exception
import Control.Lens hiding ((#))
import Data.ByteString.Conversion (toByteString')
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import Data.List1 qualified as List1
import Data.Map qualified as Map
import Data.ProtoLens qualified as Protolens
import Data.Qualified
import Data.Set qualified as Set
import Data.Singletons
import Data.Time.Clock
import Data.Timeout (TimeoutUnit (..), (#))
import Data.UUID.V4 (nextRandom)
import Federator.MockServer
import Imports
import Network.HTTP.Types qualified as Http
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation qualified as Conv
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.Federation.API.Galley qualified as FedGalley
import Wire.API.Federation.Component
import Wire.API.Internal.Notification
import Wire.API.Message
import Wire.API.Routes.Internal.Galley.ConversationsIntra
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
      test s "POST /federation/leave-conversation : Non-existent" leaveConversationNonExistent,
      test s "POST /federation/leave-conversation : Invalid type" leaveConversationInvalidType,
      test s "POST /federation/on-message-sent : Receive a message from another backend" onMessageSent,
      test s "POST /federation/send-message : Post a message sent from another backend" sendMessage,
      test s "POST /federation/on-conversation-updated : Notify local user about conversation rename with an unavailable federator" notifyConvRenameUnavailable,
      test s "POST /federation/on-conversation-updated : Notify local user about message timer update with an unavailable federator" notifyMessageTimerUnavailable,
      test s "POST /federation/on-conversation-updated : Notify local user about receipt mode update with an unavailable federator" notifyReceiptModeUnavailable,
      test s "POST /federation/on-conversation-updated : Notify local user about access update with an unavailable federator" notifyAccessUnavailable
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
        Nothing
        defNewProteusConv {newConvQualifiedUsers = [aliceQ, carlQ]}

  -- create a one-to-one conversation between bob and alice
  do
    let createO2O =
          UpsertOne2OneConversationRequest
            { uooLocalUser = lBob,
              uooRemoteUser = rAlice,
              uooActor = LocalActor,
              uooActorDesiredMembership = Included,
              uooConvId = cnv1Id
            }
    iUpsertOne2OneConversation createO2O !!! const 200 === statusCode

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

  let c2 = find ((== qUnqualified (cnvQualifiedId cnv2)) . (.id)) convs

  liftIO $ do
    assertEqual
      "name mismatch"
      (Just $ Conv.cnvName cnv2)
      ((.metadata.cnvmName) <$> c2)
    assertEqual
      "self member role mismatch"
      (Just . memConvRoleName . cmSelf $ cnvMembers cnv2)
      ((.members.selfRole) <$> c2)
    assertEqual
      "other members mismatch"
      (Just (sort [bob, qUnqualified carlQ]))
      (fmap (sort . map (qUnqualified . omQualifiedId) . (.members.others)) c2)

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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu
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
  let requestMembers = Set.fromList ([asOtherMember qAlice])

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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu
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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuAdd
    afterAddition <- listRemoteConvs remoteDomain alice
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cuRemove
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
              SomeConversationAction
                (sing @'ConversationRemoveMembersTag)
                (ConversationRemoveMembers (pure user) EdReasonRemoved)
          }

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qEve)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qEve]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qDee)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qDee]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    void $ runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain (cuRemove qFlo)
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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient bdom cu
    liftIO $ do
      WS.assertMatch_ (5 # Second) wsA $ \n -> do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv e @?= qconv
        evtType e @?= etype
        evtFrom e @?= qbob
        evtData e @?= edata
      WS.assertNoEvent (1 # Second) [wsC]

notifyUpdateUnavailable :: [Qualified UserId] -> SomeConversationAction -> EventType -> EventData -> TestM ()
notifyUpdateUnavailable extras action etype edata = do
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

  connectWithRemoteUser alice qbob
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
    ((), _fedRequests) <-
      withTempMockFederator' (throw $ MockErrorResponse Http.status500 "Down for maintenance") $
        void $
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

notifyConvRenameUnavailable :: TestM ()
notifyConvRenameUnavailable = do
  let d = ConversationRename "gossip++"
  notifyUpdateUnavailable [] (SomeConversationAction (sing @'ConversationRenameTag) d) ConvRename (EdConvRename d)

notifyMessageTimerUnavailable :: TestM ()
notifyMessageTimerUnavailable = do
  let d = ConversationMessageTimerUpdate (Just 5000)
  notifyUpdateUnavailable
    []
    (SomeConversationAction (sing @'ConversationMessageTimerUpdateTag) d)
    ConvMessageTimerUpdate
    (EdConvMessageTimerUpdate d)

notifyReceiptModeUnavailable :: TestM ()
notifyReceiptModeUnavailable = do
  let d = ConversationReceiptModeUpdate (ReceiptMode 42)
  notifyUpdateUnavailable
    []
    (SomeConversationAction (sing @'ConversationReceiptModeUpdateTag) d)
    ConvReceiptModeUpdate
    (EdConvReceiptModeUpdate d)

notifyAccessUnavailable :: TestM ()
notifyAccessUnavailable = do
  let d = ConversationAccessData (Set.fromList [InviteAccess, LinkAccess]) (Set.fromList [TeamMemberAccessRole])
  notifyUpdateUnavailable
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
    (Set.fromList ([mkMember qalice]))

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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient bobDomain cu

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
    void $ runFedClient @"on-conversation-updated" fedGalleyClient bdom cu
    void . liftIO $ do
      WS.assertMatchN_ (5 # Second) [wsA, wsD] $
        wsAssertMemberJoinWithRole qconv qbob [qeve, qdee] roleNameWireMember
      WS.assertNoEvent (1 # Second) [wsC]
      WS.assertNoEvent (1 # Second) [wsF]

leaveConversationNonExistent :: TestM ()
leaveConversationNonExistent = do
  let remoteDomain = Domain "far-away.example.com"
  alice <- randomQualifiedId remoteDomain
  conv <- randomId

  g <- viewGalley
  let leaveRequest = FedGalley.LeaveConversationRequest conv (qUnqualified alice)
  resp <-
    fmap (.response) $
      responseJsonError @_ @LeaveConversationResponse
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
    fmap (.response) $
      responseJsonError @_ @LeaveConversationResponse
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
  let fromc = ClientId 0
      aliceC1 = ClientId 0
      aliceC2 = ClientId 1
      eveC = ClientId 0
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
  void $ runFedClient @"on-conversation-updated" fedGalleyClient bdom cu

  let txt = "Hello from another backend"
      msg client = Map.fromList [(client, txt)]
      rcpts =
        UserClientMap $
          Map.fromListWith (<>) [(alice, msg aliceC1), (alice, msg aliceC2), (eve, msg eveC)]
      rm =
        FedGalley.RemoteMessage
          { FedGalley.time = now,
            FedGalley._data = Nothing,
            FedGalley.sender = qbob,
            FedGalley.senderClient = fromc,
            FedGalley.conversation = conv,
            FedGalley.priority = Nothing,
            FedGalley.transient = False,
            FedGalley.push = False,
            FedGalley.recipients = rcpts
          }

  -- send message to alice and check reception
  WS.bracketAsClientRN c [(alice, aliceC1), (alice, aliceC2), (eve, eveC)] $ \[wsA1, wsA2, wsE] -> do
    void $ runFedClient @"on-message-sent" fedGalleyClient bdom rm
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
    withTempMockFederator' ("get-not-fully-connected-backends" ~> NonConnectedBackends mempty <|> responses1 <|> mockReply EmptyResponse) $
      fmap decodeConvId $
        postConvQualified
          aliceId
          Nothing
          defNewProteusConv
            { newConvQualifiedUsers = [bob, chad]
            }
          <!! const 201 === statusCode

  liftIO $ do
    galleyReq <- case requests1 of
      [_, r] -> pure r -- (the first request is to `"get-not-fully-connected-backends"`.)
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
          { FedGalley.convId = convId,
            FedGalley.sender = bobId,
            FedGalley.rawMessage = Base64ByteString (Protolens.encodeMessage msg)
          }
  let mock = do
        guardComponent Brig
        mockReply $
          Map.fromList
            [ (chadId, Set.singleton (PubClient chadClient Nothing)),
              (bobId, Set.singleton (PubClient bobClient Nothing))
            ]
  void $ withTempMockFederator' (mock <|> mockReply EmptyResponse) $ do
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
    (SConversationUpdateProtocolTag, SConversationUpdateProtocolTag) -> Just action
    (SConversationUpdateProtocolTag, _) -> Nothing
