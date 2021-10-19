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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module API.Federation where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens hiding ((#))
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.ByteString.Conversion (toByteString')
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id (ConvId, Id (..), UserId, newClientId, randomId)
import Data.Json.Util (Base64ByteString (..), toBase64Text)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1
import qualified Data.List1 as List1
import qualified Data.Map as Map
import qualified Data.ProtoLens as Protolens
import Data.Qualified (Qualified (..))
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Timeout (TimeoutUnit (..), (#))
import Data.UUID.V4 (nextRandom)
import Galley.Types
import Gundeck.Types.Notification
import Imports
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation.Action (ConversationAction (..))
import Wire.API.Conversation.Member (Member (..))
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley (GetConversationsRequest (..), GetConversationsResponse (..), RemoteConvMembers (..), RemoteConversation (..))
import qualified Wire.API.Federation.API.Galley as FedGalley
import qualified Wire.API.Federation.GRPC.Types as F
import Wire.API.Message (ClientMismatchStrategy (..), MessageSendingStatus (mssDeletedClients, mssFailedToSend, mssRedundantClients), mkQualifiedOtrPayload, mssMissingClients)
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
      test s "POST /federation/on-message-sent : Receive a message from another backend" onMessageSent,
      test s "POST /federation/send-message : Post a message sent from another backend" sendMessage
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  bob <- randomUser

  -- create & get group conv
  aliceQ <- Qualified <$> randomId <*> pure (Domain "far-away.example.com")
  carlQ <- randomQualifiedUser

  connectUsers bob (singleton (qUnqualified carlQ))
  connectWithRemoteUser bob aliceQ

  cnv2 <-
    responseJsonError
      =<< postConvWithRemoteUsers
        (qDomain aliceQ)
        [mkProfile aliceQ (Name "alice")]
        bob
        defNewConv {newConvQualifiedUsers = [aliceQ, carlQ]}

  getConvs bob (Just $ Left [qUnqualified (cnvQualifiedId cnv2)]) Nothing !!! do
    const 200 === statusCode
    const (Just (Just [cnvQualifiedId cnv2]))
      === fmap (fmap (map cnvQualifiedId . convList)) . responseJsonMaybe

  -- FUTUREWORK: also create a one2one conversation

  -- get conversations

  fedGalleyClient <- view tsFedGalleyClient
  GetConversationsResponse cs <-
    FedGalley.getConversations
      fedGalleyClient
      (qDomain aliceQ)
      ( GetConversationsRequest
          (qUnqualified aliceQ)
          (map (qUnqualified . cnvQualifiedId) [cnv2])
      )

  let c2 = find ((== qUnqualified (cnvQualifiedId cnv2)) . rcnvId) cs

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

getConversationsNotPartOf :: TestM ()
getConversationsNotPartOf = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [qUnqualified . cnvQualifiedId $ cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv1]) === fmap (map cnvQualifiedId . convList) . responseJsonUnsafe

  fedGalleyClient <- view tsFedGalleyClient
  localDomain <- viewFederationDomain
  rando <- Id <$> liftIO nextRandom
  GetConversationsResponse cs <-
    FedGalley.getConversations
      fedGalleyClient
      localDomain
      (GetConversationsRequest rando [qUnqualified . cnvQualifiedId $ cnv1])
  liftIO $ assertEqual "conversation list not empty" [] cs

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
      let expectedSelf = alice
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
              ConversationActionAddMembers (qalice :| [qdee]) roleNameWireMember
          }
  WS.bracketRN c [alice, charlie, dee] $ \[wsA, wsC, wsD] -> do
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain cu
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
      let expectedSelf = alice
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
                ConversationActionAddMembers (qCharlie :| []) roleNameWireMember
            }
    -- Alice receives no notifications from this
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain cu
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
              ConversationActionAddMembers (pure qAlice) roleNameWireMember
          }
      cuRemove =
        FedGalley.ConversationUpdate
          { FedGalley.cuTime = addUTCTime (secondsToNominalDiffTime 5) now,
            FedGalley.cuOrigUserId = qBob,
            FedGalley.cuConvId = conv,
            FedGalley.cuAlreadyPresentUsers = [alice],
            FedGalley.cuAction =
              ConversationActionRemoveMembers (pure qAlice)
          }

  connectWithRemoteUser alice qBob
  WS.bracketR c alice $ \ws -> do
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain cuAdd
    afterAddition <- listRemoteConvs remoteDomain alice
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain cuRemove
    liftIO $ do
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMemberJoinWithRole qconv qBob [qAlice] roleNameWireMember
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMembersLeave qconv qBob [qAlice]
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
              ConversationActionRemoveMembers (pure user)
          }

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain (cuRemove qEve)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qEve]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain (cuRemove qDee)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA, wsD] $
        wsAssertMembersLeave qconv qBob [qDee]
      WS.assertNoEvent (1 # Second) [wsC, wsF]

  WS.bracketRN c [alice, charlie, dee, flo] $ \[wsA, wsC, wsD, wsF] -> do
    FedGalley.onConversationUpdated fedGalleyClient remoteDomain (cuRemove qFlo)
    liftIO $ do
      WS.assertMatchN_ (3 # Second) [wsA] $
        wsAssertMembersLeave qconv qBob [qFlo]
      WS.assertNoEvent (1 # Second) [wsC, wsF, wsD]

notifyUpdate :: [Qualified UserId] -> ConversationAction -> EventType -> EventData -> TestM ()
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
    FedGalley.onConversationUpdated fedGalleyClient bdom cu
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
  notifyUpdate [] (ConversationActionRename d) ConvRename (EdConvRename d)

notifyMessageTimer :: TestM ()
notifyMessageTimer = do
  let d = ConversationMessageTimerUpdate (Just 5000)
  notifyUpdate
    []
    (ConversationActionMessageTimerUpdate d)
    ConvMessageTimerUpdate
    (EdConvMessageTimerUpdate d)

notifyReceiptMode :: TestM ()
notifyReceiptMode = do
  let d = ConversationReceiptModeUpdate (ReceiptMode 42)
  notifyUpdate
    []
    (ConversationActionReceiptModeUpdate d)
    ConvReceiptModeUpdate
    (EdConvReceiptModeUpdate d)

notifyAccess :: TestM ()
notifyAccess = do
  let d = ConversationAccessData (Set.fromList [InviteAccess, LinkAccess]) TeamAccessRole
  notifyUpdate
    []
    (ConversationActionAccessUpdate d)
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
    (ConversationActionMemberUpdate qdee (OtherMemberUpdate (Just roleNameWireAdmin)))
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
              FedGalley.cuAction = ConversationActionDelete
            }
    FedGalley.onConversationUpdated fedGalleyClient bobDomain cu

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
              ConversationActionAddMembers (qdee :| [qeve, qflo]) roleNameWireMember
          }
  WS.bracketRN c (map qUnqualified [qalice, qcharlie, qdee, qflo]) $ \[wsA, wsC, wsD, wsF] -> do
    FedGalley.onConversationUpdated fedGalleyClient bdom cu
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

  opts <- view tsGConf
  let mockedResponse fedReq = do
        let success :: ToJSON a => a -> IO F.OutwardResponse
            success = pure . F.OutwardResponseBody . LBS.toStrict . A.encode
            getUsersPath = Just "/federation/get-users-by-ids"
        case (F.domain fedReq, F.path <$> F.request fedReq) of
          (d, mp)
            | d == domainText remoteDomain1 && mp == getUsersPath ->
              success [mkProfile qChad (Name "Chad"), mkProfile qDee (Name "Dee")]
          (d, mp)
            | d == domainText remoteDomain2 && mp == getUsersPath ->
              success [mkProfile qEve (Name "Eve")]
          _ -> success ()

  (convId, _) <-
    withTempMockFederator' opts remoteDomain1 mockedResponse $
      decodeConvId
        <$> postConvQualified
          alice
          defNewConv
            { newConvQualifiedUsers = [qBob, qChad, qDee, qEve]
            }
  let qconvId = Qualified convId localDomain

  (_, federatedRequests) <-
    WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
      withTempMockFederator' opts remoteDomain1 mockedResponse $ do
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

  let [remote1GalleyFederatedRequest] = fedRequestsForDomain remoteDomain1 F.Galley federatedRequests
      [remote2GalleyFederatedRequest] = fedRequestsForDomain remoteDomain2 F.Galley federatedRequests
  assertRemoveUpdate remote1GalleyFederatedRequest qconvId qChad [qUnqualified qChad, qUnqualified qDee] qChad
  assertRemoveUpdate remote2GalleyFederatedRequest qconvId qChad [qUnqualified qEve] qChad

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
              ConversationActionAddMembers (pure qalice) roleNameWireMember
          }
  FedGalley.onConversationUpdated fedGalleyClient bdom cu

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
    FedGalley.onMessageSent fedGalleyClient bdom rm
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
sendMessage :: HasCallStack => TestM ()
sendMessage = do
  cannon <- view tsCannon
  let remoteDomain = Domain "far-away.example.com"
  localDomain <- viewFederationDomain

  -- users and clients
  (alice, aliceClient) <- randomUserWithClientQualified (someLastPrekeys !! 0)
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
  opts <- view tsGConf
  let responses1 req
        | fmap F.component (F.request req) == Just F.Brig =
          toJSON [bobProfile, chadProfile]
        | otherwise = toJSON ()
  (convId, requests1) <-
    withTempMockFederator opts remoteDomain responses1 $
      fmap decodeConvId $
        postConvQualified
          aliceId
          defNewConv
            { newConvQualifiedUsers = [bob, chad]
            }
          <!! const 201 === statusCode

  liftIO $ do
    [brigReq, galleyReq] <- case requests1 of
      xs@[_, _] -> pure xs
      _ -> assertFailure "unexpected number of requests"
    fmap F.component (F.request brigReq) @?= Just F.Brig
    fmap F.path (F.request brigReq) @?= Just "/federation/get-users-by-ids"
    fmap F.component (F.request galleyReq) @?= Just F.Galley
    fmap F.path (F.request galleyReq) @?= Just "/federation/on-conversation-created"
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
        FedGalley.MessageSendRequest
          { FedGalley.msrConvId = convId,
            FedGalley.msrSender = bobId,
            FedGalley.msrRawMessage =
              Base64ByteString
                (LBS.fromStrict (Protolens.encodeMessage msg))
          }
  let responses2 req
        | fmap F.component (F.request req) == Just F.Brig =
          toJSON
            ( Map.fromList
                [ (chadId, Set.singleton (PubClient chadClient Nothing)),
                  (bobId, Set.singleton (PubClient bobClient Nothing))
                ]
            )
        | otherwise = toJSON ()
  (_, requests2) <- withTempMockFederator opts remoteDomain responses2 $ do
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
    fmap F.component (F.request receiveReq) @?= Just F.Galley
    fmap F.path (F.request receiveReq) @?= Just "/federation/on-message-sent"
    rm <- case A.decode . LBS.fromStrict . F.body =<< F.request receiveReq of
      Nothing -> assertFailure "invalid federated request body"
      Just x -> pure (x :: FedGalley.RemoteMessage ConvId)
    FedGalley.rmSender rm @?= bob
    Map.keysSet (userClientMap (FedGalley.rmRecipients rm))
      @?= Set.singleton chadId
