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

module API.Federation where

import API.Util
import Bilge
import Bilge.Assert
import qualified Cassandra as Cql
import Control.Lens hiding ((#))
import Data.Aeson (ToJSON (..), (.:))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Id (ConvId, Id (..), newClientId, randomId)
import Data.Json.Util (Base64ByteString (..))
import Data.List1
import qualified Data.List1 as List1
import qualified Data.Map as Map
import qualified Data.ProtoLens as Protolens
import Data.Qualified (Qualified (..))
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Timeout (TimeoutUnit (..), (#))
import Data.UUID.V4 (nextRandom)
import qualified Galley.Data.Queries as Cql
import Galley.Types
import Gundeck.Types.Notification
import Imports
import Test.QuickCheck (arbitrary, generate)
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley (GetConversationsRequest (..), GetConversationsResponse (..))
import qualified Wire.API.Federation.API.Galley as FedGalley
import qualified Wire.API.Federation.GRPC.Types as F
import Wire.API.Message (ClientMismatchStrategy (..), mkQualifiedOtrPayload)
import Wire.API.User.Profile

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "federation"
    [ test s "POST /federation/get-conversations : All Found" getConversationsAllFound,
      test s "POST /federation/get-conversations : Conversations user is not a part of are excluded from result" getConversationsNotPartOf,
      test
        s
        "POST /federation/update-conversation-memberships : Add local user to remote conversation"
        addLocalUser,
      test
        s
        "POST /federation/update-conversation-memberships : Notify local user about other members joining"
        notifyLocalUser,
      test s "POST /federation/receive-message : Receive a message from another backend" receiveMessage,
      test s "POST /federation/send-message : Post a message sent from another backend" sendMessage
    ]

getConversationsAllFound :: TestM ()
getConversationsAllFound = do
  -- FUTUREWORK: make alice / bob remote users
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonUnsafeWithMsg "conversation" <$> postO2OConv alice bob (Just "gossip1")
  getConvs alice (Just $ Left [qUnqualified . cnvQualifiedId $ cnv1]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv1]) === fmap (map cnvQualifiedId . convList) . responseJsonUnsafe
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <- responseJsonUnsafeWithMsg "conversation" <$> postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
  getConvs alice (Just $ Left [qUnqualified . cnvQualifiedId $ cnv2]) Nothing !!! do
    const 200 === statusCode
    const (Just [cnvQualifiedId cnv2]) === fmap (map cnvQualifiedId . convList) . responseJsonUnsafe
  -- get both

  fedGalleyClient <- view tsFedGalleyClient
  localDomain <- viewFederationDomain
  let aliceQualified = Qualified alice localDomain
  GetConversationsResponse cs <-
    FedGalley.getConversations
      fedGalleyClient
      (GetConversationsRequest aliceQualified $ qUnqualified . cnvQualifiedId <$> [cnv1, cnv2])
  let c1 = find ((== cnvQualifiedId cnv1) . cnvQualifiedId) cs
  let c2 = find ((== cnvQualifiedId cnv2) . cnvQualifiedId) cs
  liftIO . forM_ [(cnv1, c1), (cnv2, c2)] $ \(expected, actual) -> do
    assertEqual
      "name mismatch"
      (Just $ cnvName expected)
      (cnvName <$> actual)
    assertEqual
      "self member mismatch"
      (Just . cmSelf $ cnvMembers expected)
      (cmSelf . cnvMembers <$> actual)
    assertEqual
      "other members mismatch"
      (Just [])
      ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)

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
  let randoQualified = Qualified rando localDomain
  GetConversationsResponse cs <-
    FedGalley.getConversations
      fedGalleyClient
      (GetConversationsRequest randoQualified [qUnqualified . cnvQualifiedId $ cnv1])
  liftIO $ assertEqual "conversation list not empty" [] cs

addLocalUser :: TestM ()
addLocalUser = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  let qalice = Qualified alice localDomain
  let dom = Domain "bobland.example.com"
  bob <- randomId
  let qbob = Qualified bob dom
  conv <- randomId
  let qconv = Qualified conv dom
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cmu =
        FedGalley.ConversationMemberUpdate
          { FedGalley.cmuTime = now,
            FedGalley.cmuOrigUserId = qbob,
            FedGalley.cmuConvId = qconv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuUsersAdd = [(qalice, roleNameWireMember)],
            FedGalley.cmuUsersRemove = []
          }
  WS.bracketR c alice $ \ws -> do
    FedGalley.updateConversationMemberships fedGalleyClient cmu
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertMemberJoinWithRole qconv qbob [qalice] roleNameWireMember
  cassState <- view tsCass
  convs <-
    Cql.runClient cassState
      . Cql.query Cql.selectUserRemoteConvs
      $ Cql.params Cql.Quorum (Identity alice)
  liftIO $ [(dom, conv)] @?= convs

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
            FedGalley.cmuConvId = qconv,
            FedGalley.cmuAlreadyPresentUsers = [alice],
            FedGalley.cmuUsersAdd = [(qcharlie, roleNameWireMember)],
            FedGalley.cmuUsersRemove = []
          }
  WS.bracketR c alice $ \ws -> do
    FedGalley.updateConversationMemberships fedGalleyClient cmu
    void . liftIO $
      WS.assertMatch (5 # Second) ws $
        wsAssertMemberJoinWithRole qconv qbob [qcharlie] roleNameWireMember

receiveMessage :: TestM ()
receiveMessage = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  alice <- randomUser
  eve <- randomUser
  bob <- randomId
  conv <- randomId
  let fromc = newClientId 0
      alicec = newClientId 0
      evec = newClientId 0
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
            FedGalley.cmuConvId = qconv,
            FedGalley.cmuAlreadyPresentUsers = [],
            FedGalley.cmuUsersAdd = [(qalice, roleNameWireMember)],
            FedGalley.cmuUsersRemove = []
          }
  FedGalley.updateConversationMemberships fedGalleyClient cmu

  let txt = "Hello from another backend"
      msg client = Map.fromList [(client, txt)]
      rcpts =
        UserClientMap $
          Map.fromList [(alice, msg alicec), (eve, msg evec)]
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
  WS.bracketR2 c alice eve $ \(wsA, wsE) -> do
    FedGalley.receiveMessage fedGalleyClient bdom rm
    liftIO $ do
      -- alice should receive the message
      WS.assertMatch_ (5 # Second) wsA $ \n ->
        do
          let e = List1.head (WS.unpackPayload n)
          ntfTransient n @?= False
          evtConv e @?= qconv
          evtType e @?= OtrMessageAdd
          evtFrom e @?= qbob
          evtData e @?= EdOtrMessage (OtrMessage fromc alicec txt Nothing)
      -- eve should not receive the message
      WS.assertNoEvent (1 # Second) [wsE]

-- alice local, bob and chad remote in a local conversation
-- bob sends a message (using the RPC), we test that alice receives it and that
-- a call is made to the receiveMessage RPC to inform chad
sendMessage :: TestM ()
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
          toJSON $ [bobProfile, chadProfile]
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
    fmap F.path (F.request galleyReq) @?= Just "/federation/register-conversation"
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
            FedGalley.msrSender = alice,
            FedGalley.msrRawMessage =
              Base64ByteString
                (LBS.fromStrict (Protolens.encodeMessage msg))
          }
  let responses2 _ = toJSON ()
  (_, requests2) <- withTempMockFederator opts remoteDomain responses2 $ do
    WS.bracketR cannon aliceId $ \ws -> do
      g <- viewGalley
      msresp <-
        post
          ( g
              . paths ["federation", "send-message"]
              . content "application/json"
              . json msr
          )
          <!! do
            const 200 === statusCode
      (status, value :: A.Value) <- either fail pure $
        flip A.parseEither (responseJsonUnsafe msresp) $
          A.withObject "Union" $ \obj -> do
            s <- obj .: "status"
            v <- obj .: "value"
            pure (s, v)
      print value
      liftIO $ status @?= (201 :: Int)

      -- check that alice received the message
      when False $ do
        WS.assertMatch_ (5 # Second) ws $
          wsAssertOtr' "" conv bob bobClient aliceClient "hi alice"

  -- check that a request to propagate message to chad has been made
  liftIO $ do
    print requests2
    [req] <- case requests2 of
      xs@[_] -> pure xs
      _ -> assertFailure "unexpected number of requests"
    fmap F.component (F.request req) @?= Just F.Galley
    fmap F.path (F.request req) @?= Just "/federation/receive-message"
    rm <- case (A.decode . LBS.fromStrict) =<< fmap F.body (F.request req) of
      Nothing -> assertFailure "invalid federated request body"
      Just x -> pure (x :: FedGalley.RemoteMessage ConvId)
    FedGalley.rmSender rm @?= bob
    Map.keysSet (userClientMap (FedGalley.rmRecipients rm))
      @?= Set.singleton chadId
