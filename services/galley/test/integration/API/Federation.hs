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
import Data.Id (ConvId, Id (..), newClientId, randomId)
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
      test s "POST /federation/on-conversation-memberships-changed : Add local user to remote conversation" addLocalUser,
      test s "POST /federation/on-conversation-memberships-changed : Remove a local user from a remote conversation" removeLocalUser,
      test s "POST /federation/on-conversation-memberships-changed : Remove a remote user from a remote conversation" removeRemoteUser,
      test s "POST /federation/on-conversation-memberships-changed : Notify local user about other members joining" notifyLocalUser,
      test s "POST /federation/leave-conversation : Success" leaveConversationSuccess,
      test s "POST /federation/on-message-sent : Receive a message from another backend" onMessageSent,
      test s "POST /federation/send-message : Post a message sent from another backend" sendMessage
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  [alice, bob] <- randomUsers 2
  let aliceQ = Qualified alice (Domain "far-away.example.com")

  -- create & get group conv
  localDomain <- viewFederationDomain
  carlQ <- Qualified <$> randomUser <*> pure localDomain
  connectUsers bob (singleton (qUnqualified carlQ))

  putStrLn $ "alice: " <> show (qUnqualified aliceQ)
  putStrLn $ "bob: " <> show bob
  putStrLn $ "carl: " <> show (qUnqualified carlQ)

  cnv2 <-
    responseJsonError
      =<< postConvWithRemoteUser (qDomain aliceQ) (mkProfile aliceQ (Name "alice")) bob [aliceQ, carlQ]

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
      (GetConversationsRequest alice $ qUnqualified . cnvQualifiedId <$> [cnv2])

  let c2 = find ((== cnvQualifiedId cnv2) . cnvmQualifiedId . rcnvMetadata) cs

  liftIO $ do
    assertEqual
      "name mismatch"
      (Just $ cnvName cnv2)
      (cnvmName . rcnvMetadata <$> c2)
    assertEqual
      "self member role mismatch"
      (Just . memConvRoleName . cmSelf $ cnvMembers cnv2)
      (rcmSelfRole . rcnvMembers <$> c2)
    putStrLn $ "actual members " <> show (fmap (rcmOthers . rcnvMembers) c2)
    putStrLn $ "expected members " <> show (cmOthers (cnvMembers cnv2))
    assertEqual
      "other members mismatch"
      (Just (sort [bob, qUnqualified carlQ]))
      -- (fmap (sort . (map (qUnqualified . omQualifiedId) . rcmOthers . rcnvMembers) cnv2))
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

addLocalUser :: TestM ()
addLocalUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  let qalice = Qualified alice localDomain
  let remoteDomain = Domain "bobland.example.com"
  bob <- randomId
  let qbob = Qualified bob remoteDomain
  conv <- randomId
  let qconv = Qualified conv remoteDomain
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionAdd (pure (qalice, roleNameWireMember))
          }
  WS.bracketR c alice $ \ws -> do
    FedGalley.onConversationMembershipsChanged fedGalleyClient remoteDomain cmu
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertMemberJoinWithRole qconv qbob [qalice] roleNameWireMember
  convs <- listRemoteConvs remoteDomain alice
  liftIO $ convs @?= [Qualified conv remoteDomain]

-- | This test invokes the federation endpoint:
--
--   'POST /federation/on-conversation-memberships-changed'
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
  let cmuAdd =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qBob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionAdd (pure (qAlice, roleNameWireMember))
          }
      cmuRemove =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = addUTCTime (secondsToNominalDiffTime 5) now,
            FedGalley.cmuOrigUserId = qBob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [alice],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionRemove (pure qAlice)
          }

  WS.bracketR c alice $ \ws -> do
    FedGalley.onConversationMembershipsChanged fedGalleyClient remoteDomain cmuAdd
    afterAddition <- listRemoteConvs remoteDomain alice
    FedGalley.onConversationMembershipsChanged fedGalleyClient remoteDomain cmuRemove
    liftIO $ do
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMemberJoinWithRole qconv qBob [qAlice] roleNameWireMember
      void . WS.assertMatch (3 # Second) ws $
        wsAssertMembersLeave qconv qBob [qAlice]
    afterRemoval <- listRemoteConvs remoteDomain alice
    liftIO $ do
      afterAddition @?= [qconv]
      afterRemoval @?= []

-- | This test invokes the federation endpoint:
--
--   'POST /federation/on-conversation-memberships-changed'
--
-- two times in a row: first adding a local and a remote user to a remote
-- conversation, and then removing the remote user. The test asserts the
-- expected list of conversations in between the calls and at the end from the
-- point of view of the local backend and that the local conversation member got
-- notified of the removal.
removeRemoteUser :: TestM ()
removeRemoteUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  [bob, eve] <- replicateM 2 randomId
  let qAlice = Qualified alice localDomain
      remoteDomain = Domain "bobland.example.com"
      qBob = Qualified bob remoteDomain
      qEve = Qualified eve remoteDomain
  conv <- randomId
  let qconv = Qualified conv remoteDomain
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmuAdd =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qBob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionAdd
                ((qAlice, roleNameWireMember) :| [(qEve, roleNameWireMember)])
          }
      cmuRemove =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = addUTCTime (secondsToNominalDiffTime 5) now,
            FedGalley.cmuOrigUserId = qBob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [alice],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionRemove (pure qEve)
          }

  WS.bracketR c alice $ \ws -> do
    FedGalley.onConversationMembershipsChanged fedGalleyClient remoteDomain cmuAdd
    afterAddition <- listRemoteConvs remoteDomain alice
    void . liftIO . WS.assertMatch (3 # Second) ws $
      wsAssertMemberJoinWithRole qconv qBob [qAlice, qEve] roleNameWireMember
    FedGalley.onConversationMembershipsChanged fedGalleyClient remoteDomain cmuRemove
    afterRemoval <- listRemoteConvs remoteDomain alice
    void . liftIO $
      WS.assertMatch (3 # Second) ws $
        wsAssertMembersLeave qconv qBob [qEve]
    liftIO $ do
      afterAddition @?= [qconv]
      afterRemoval @?= [qconv]

notifyLocalUser :: TestM ()
notifyLocalUser = do
  c <- view tsCannon
  alice <- randomUser
  bob <- randomId
  charlie <- randomId
  conv <- randomId
  let bdom = Domain "bob.example.com"
      cdom = Domain "charlie.example.com"
      qbob = Qualified bob bdom
      qconv = Qualified conv bdom
      qcharlie = Qualified charlie cdom
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [alice],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionAdd (pure (qcharlie, roleNameWireMember))
          }
  WS.bracketR c alice $ \ws -> do
    FedGalley.onConversationMembershipsChanged fedGalleyClient bdom cmu
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertMemberJoinWithRole qconv qbob [qcharlie] roleNameWireMember

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
      decodeConvId <$> postConvQualified alice [qBob, qChad, qDee, qEve] Nothing [] Nothing Nothing
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
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = conv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuAction =
              FedGalley.ConversationMembersActionAdd (pure (qalice, roleNameWireMember))
          }
  FedGalley.onConversationMembershipsChanged fedGalleyClient bdom cmu

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

  -- conversation
  opts <- view tsGConf
  let responses1 req
        | fmap F.component (F.request req) == Just F.Brig =
          toJSON [bobProfile, chadProfile]
        | otherwise = toJSON ()
  (convId, requests1) <-
    withTempMockFederator opts remoteDomain responses1 $
      fmap decodeConvId $
        postConvQualified aliceId [bob, chad] Nothing [] Nothing Nothing
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
