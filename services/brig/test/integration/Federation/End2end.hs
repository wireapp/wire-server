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

module Federation.End2end where

import API.Search.Util
import API.User.Util (getUserClientsQualified)
import Bilge
import Bilge.Assert ((!!!), (<!!), (===))
import Brig.API.Client (pubClient)
import qualified Brig.Options as BrigOpts
import Brig.Types
import Control.Arrow ((&&&))
import Control.Lens (sequenceAOf, _1)
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Domain (Domain)
import Data.Handle
import Data.Id (ClientId, ConvId)
import Data.Json.Util (toBase64Text)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List1 as List1
import qualified Data.Map as Map
import qualified Data.ProtoLens as Protolens
import Data.Qualified
import qualified Data.Set as Set
import Federation.Util (generateClientPrekeys, getConvQualified)
import Gundeck.Types.Notification (ntfTransient)
import Imports
import qualified System.Logger as Log
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import Wire.API.Conversation
import Wire.API.Conversation.Role (roleNameWireAdmin)
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.User (ListUsersQuery (ListUsersByIds))
import Wire.API.User.Client

-- NOTE: These federation tests require deploying two sets of (some) services
-- This might be best left to a kubernetes setup.
--
-- While individual functions can and should be tested in a more unit-testy way,
-- these more end-to-end integration test serve as a way to test the overall
-- network flow
--
-- FUTUREWORK(federation): Add tests for these scenarios (but not here in end2end, but in unit/1-backend-integration tests):
-- - Remote discovery fails
-- - Remote discovery succeeds but server doesn't exist
-- - Remote federator fails to respond in many ways (protocol error, timeout, etc.)
-- - SRV record has two servers but higher priority one always fails
--
-- See https://wearezeta.atlassian.net/browse/SQCORE-914
spec ::
  BrigOpts.Opts ->
  Manager ->
  Brig ->
  Galley ->
  Cannon ->
  Endpoint ->
  Brig ->
  Galley ->
  IO TestTree
spec _brigOpts mg brig galley cannon _federator brigTwo galleyTwo =
  pure $
    testGroup
      "federation-end2end-user"
      [ test mg "lookup user by qualified handle on remote backend" $ testHandleLookup brig brigTwo,
        test mg "search users on remote backend" $ testSearchUsers brig brigTwo,
        test mg "get users by ids on multiple backends" $ testGetUsersById brig brigTwo,
        test mg "claim client prekey" $ testClaimPrekeySuccess brig brigTwo,
        test mg "claim prekey bundle" $ testClaimPrekeyBundleSuccess brig brigTwo,
        test mg "claim multi-prekey bundle" $ testClaimMultiPrekeyBundleSuccess brig brigTwo,
        test mg "list user clients" $ testListUserClients brig brigTwo,
        test mg "list own conversations" $ testListConversations brig brigTwo galley galleyTwo,
        test mg "add remote users to local conversation" $ testAddRemoteUsersToLocalConv brig galley brigTwo galleyTwo,
        test mg "remove remote user from a local conversation" $ testRemoveRemoteUserFromLocalConv brig galley brigTwo galleyTwo,
        test mg "leave a remote conversation" $ leaveRemoteConversation brig galley brigTwo galleyTwo,
        test mg "include remote users to new conversation" $ testRemoteUsersInNewConv brig galley brigTwo galleyTwo,
        test mg "send a message to a remote user" $ testSendMessage brig brigTwo galleyTwo cannon,
        test mg "send a message in a remote conversation" $ testSendMessageToRemoteConv brig brigTwo galley galleyTwo cannon
      ]

-- | Path covered by this test:
--
-- +------+         +---------+        +---------+          +------+
-- | brig |   grpc  |federator| grpc   |federator|   http   | brig |
-- |      +-------->+         +------->+         +--------->+      |
-- +------+         +-+-------+        +---------+          +------+
testHandleLookup :: Brig -> Brig -> Http ()
testHandleLookup brig brigTwo = do
  -- Create a user on the "other side" using an internal brig endpoint from a
  -- second brig instance in backendTwo (in another namespace in kubernetes)
  (handle, userBrigTwo) <- createUserWithHandle brigTwo
  -- Get result from brig two for comparison
  let domain = qDomain $ userQualifiedId userBrigTwo
  resultViaBrigTwo <- getUserInfoFromHandle brigTwo domain handle

  -- query the local-namespace brig for a user sitting on the other backend
  -- (which will exercise the network traffic via two federators to the remote brig)
  resultViaBrigOne <- getUserInfoFromHandle brig domain handle

  liftIO $ assertEqual "remote handle lookup via federator should work in the happy case" (profileQualifiedId resultViaBrigOne) (userQualifiedId userBrigTwo)
  liftIO $ assertEqual "querying brig1 or brig2 about the same user should give same result" resultViaBrigTwo resultViaBrigOne

testSearchUsers :: Brig -> Brig -> Http ()
testSearchUsers brig brigTwo = do
  -- Create a user on the "other side" using an internal brig endpoint from a
  -- second brig instance in backendTwo (in another namespace in kubernetes)
  (handle, userBrigTwo) <- createUserWithHandle brigTwo

  searcher <- userId <$> randomUser brig
  let expectedUserId = userQualifiedId userBrigTwo
      searchTerm = fromHandle handle
      domain = qDomain expectedUserId
  liftIO $ putStrLn "search for user on brigTwo (directly)..."
  assertCanFindWithDomain brigTwo searcher expectedUserId searchTerm domain

  -- exercises multi-backend network traffic
  liftIO $ putStrLn "search for user on brigOne via federators to remote brig..."
  assertCanFindWithDomain brig searcher expectedUserId searchTerm domain

testGetUsersById :: Brig -> Brig -> Http ()
testGetUsersById brig1 brig2 = do
  users <- traverse randomUser [brig1, brig2]
  let self = Imports.head users
      q = ListUsersByIds (map userQualifiedId users)
      expected = sort (map userQualifiedId users)
  post
    ( brig1
        . path "list-users"
        . zUser (userId self)
        . json q
        . contentJson
        . acceptJson
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just expected)
        === fmap (sort . map profileQualifiedId)
          . responseJsonMaybe

testClaimPrekeySuccess :: Brig -> Brig -> Http ()
testClaimPrekeySuccess brig1 brig2 = do
  self <- randomUser brig1
  user <- randomUser brig2
  let new = defNewClient TemporaryClientType (take 1 somePrekeys) (Imports.head someLastPrekeys)
  c <- responseJsonError =<< addClient brig2 (userId user) new
  let cpk = ClientPrekey (clientId c) (Imports.head somePrekeys)
  let quser = userQualifiedId user
  get
    ( brig1
        . zUser (userId self)
        . paths
          [ "users",
            toByteString' (qDomain quser),
            toByteString' (qUnqualified quser),
            "prekeys",
            toByteString' (clientId c)
          ]
    )
    !!! do
      const 200 === statusCode
      const (Just cpk) === responseJsonMaybe

testClaimPrekeyBundleSuccess :: Brig -> Brig -> Http ()
testClaimPrekeyBundleSuccess brig1 brig2 = do
  qself <- userQualifiedId <$> randomUser brig1
  let prekeys = take 5 (zip somePrekeys someLastPrekeys)
  (quser, clients) <- generateClientPrekeys brig2 prekeys
  let sortClients = sortBy (compare `on` prekeyClient)
  get
    ( brig1
        . zUser (qUnqualified qself)
        . paths
          [ "users",
            toByteString' (qDomain quser),
            toByteString' (qUnqualified quser),
            "prekeys"
          ]
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just (sortClients clients))
        === fmap (sortClients . prekeyClients) . responseJsonMaybe

testClaimMultiPrekeyBundleSuccess :: Brig -> Brig -> Http ()
testClaimMultiPrekeyBundleSuccess brig1 brig2 = do
  let prekeys = zip somePrekeys someLastPrekeys
      (prekeys1, prekeys') = splitAt 5 prekeys
      prekeys2 = take 4 prekeys'
      mkClients = Set.fromList . map prekeyClient
      mkClientMap :: [ClientPrekey] -> Map ClientId (Maybe Prekey)
      mkClientMap = Map.fromList . map (prekeyClient &&& Just . prekeyData)
      qmap :: Ord a => [(Qualified a, b)] -> Map Domain (Map a b)
      qmap = fmap Map.fromList . partitionQualified . map (sequenceAOf _1)
  c1 <- generateClientPrekeys brig1 prekeys1
  c2 <- generateClientPrekeys brig2 prekeys2
  let uc =
        QualifiedUserClients . qmap $
          [mkClients <$> c1, mkClients <$> c2]
      ucm =
        mkQualifiedUserClientPrekeyMap . fmap mkUserClientPrekeyMap . qmap $
          [mkClientMap <$> c1, mkClientMap <$> c2]
  post
    ( brig1
        . zUser (qUnqualified (fst c1))
        . paths ["users", "list-prekeys"]
        . body (RequestBodyLBS (Aeson.encode uc))
        . contentJson
        . acceptJson
        . expect2xx
    )
    !!! do
      const 200 === statusCode
      const (Just ucm) === responseJsonMaybe

testAddRemoteUsersToLocalConv :: Brig -> Galley -> Brig -> Galley -> Http ()
testAddRemoteUsersToLocalConv brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2

  let newConv = NewConvUnmanaged $ NewConv [] [] (Just "gossip") mempty Nothing Nothing Nothing Nothing roleNameWireAdmin
  convId <-
    cnvQualifiedId . responseJsonUnsafe
      <$> post
        ( galley1
            . path "/conversations"
            . zUser (userId alice)
            . zConn "conn"
            . header "Z-Type" "access"
            . json newConv
        )

  let invite = InviteQualified (userQualifiedId bob :| []) roleNameWireAdmin
  post
    ( galley1
        . paths ["conversations", (toByteString' . qUnqualified) convId, "members", "v2"]
        . zUser (userId alice)
        . zConn "conn"
        . header "Z-Type" "access"
        . json invite
    )
    !!! (const 200 === statusCode)

  -- test GET /conversations/:domain/:cnv -- Alice's domain is used here
  liftIO $ putStrLn "search for conversation on backend 1..."
  res <- getConvQualified galley1 (userId alice) convId <!! (const 200 === statusCode)
  let conv = responseJsonUnsafeWithMsg "backend 1 - get /conversations/domain/cnvId" res
      actual = cmOthers $ cnvMembers conv
      expected = [OtherMember (userQualifiedId bob) Nothing roleNameWireAdmin]
  liftIO $ actual @?= expected

  liftIO $ putStrLn "search for conversation on backend 2..."
  res' <- getConvQualified galley2 (userId bob) convId <!! (const 200 === statusCode)
  let conv' = responseJsonUnsafeWithMsg "backend 2 - get /conversations/domain/cnvId" res'
      actual' = cmOthers $ cnvMembers conv'
      expected' = [OtherMember (userQualifiedId alice) Nothing roleNameWireAdmin]
  liftIO $ actual' @?= expected'

testRemoveRemoteUserFromLocalConv :: Brig -> Galley -> Brig -> Galley -> Http ()
testRemoveRemoteUserFromLocalConv brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  let aliceId = userQualifiedId alice
  let bobId = userQualifiedId bob

  convId <- cnvQualifiedId . responseJsonUnsafe <$> createConversation galley1 (userId alice) [bobId]

  aliceConvBeforeDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvBeforeDelete)) @?= [bobId]

  bobConvBeforeDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley2 (userId bob) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers bobConvBeforeDelete)) @?= [aliceId]

  -- Alice kicks Bob out of the conversation
  delete
    ( galley1
        . paths
          [ "conversations",
            (toByteString' . qDomain) convId,
            (toByteString' . qUnqualified) convId,
            "members",
            (toByteString' . qDomain) bobId,
            (toByteString' . qUnqualified) bobId
          ]
        . zUser (userId alice)
        . zConn "conn"
    )
    !!! const 200 === statusCode

  aliceConvAfterDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvAfterDelete)) @?= []

  getConversationQualified galley2 (userId bob) convId
    !!! const 404 === statusCode

leaveRemoteConversation :: Brig -> Galley -> Brig -> Galley -> Http ()
leaveRemoteConversation brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  let aliceId = userQualifiedId alice
  let bobId = userQualifiedId bob

  convId <- cnvQualifiedId . responseJsonUnsafe <$> createConversation galley1 (userId alice) [bobId]

  aliceConvBeforeDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvBeforeDelete)) @?= [bobId]

  bobConvBeforeDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley2 (userId bob) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers bobConvBeforeDelete)) @?= [aliceId]

  -- Bob leaves the conversation
  delete
    ( galley2
        . paths
          [ "conversations",
            (toByteString' . qDomain) convId,
            (toByteString' . qUnqualified) convId,
            "members",
            (toByteString' . qDomain) bobId,
            (toByteString' . qUnqualified) bobId
          ]
        . zUser (userId bob)
        . zConn "conn"
    )
    !!! const 200 === statusCode

  aliceConvAfterDelete :: Conversation <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvAfterDelete)) @?= []

  getConversationQualified galley2 (userId bob) convId
    !!! const 404 === statusCode

-- | This creates a new conversation with a remote user. The test checks that
-- Galleys on both ends of the federation see the same conversation members.
testRemoteUsersInNewConv :: Brig -> Galley -> Brig -> Galley -> Http ()
testRemoteUsersInNewConv brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  convId <-
    cnvQualifiedId . responseJsonUnsafe
      <$> createConversation galley1 (userId alice) [userQualifiedId bob]
  -- test GET /conversations/:backend1Domain/:cnv
  testQualifiedGetConversation galley1 "galley1" alice bob convId
  testQualifiedGetConversation galley2 "galley2" bob alice convId

-- | Test a scenario of a two-user conversation.
testQualifiedGetConversation ::
  -- | A Galley to get information from
  Galley ->
  -- | A message to display during response parsing
  String ->
  -- | The user making the request
  User ->
  -- | The other user in the conversation
  User ->
  -- | A qualified conversation ID
  Qualified ConvId ->
  Http ()
testQualifiedGetConversation galley msg alice bob qconvId = do
  res <- getConvQualified galley (userId alice) qconvId <!! (const 200 === statusCode)
  let conv = responseJsonUnsafeWithMsg (msg <> " - get /conversations/domain/cnvId") res
      actual = cmOthers $ cnvMembers conv
      expected = [OtherMember (userQualifiedId bob) Nothing roleNameWireAdmin]
  liftIO $ actual @?= expected

testListUserClients :: Brig -> Brig -> Http ()
testListUserClients brig1 brig2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  let prekeys = take 3 (zip somePrekeys someLastPrekeys)
  let mkClient (pk, lpk) = defNewClient PermanentClientType [pk] lpk
      nclients = map mkClient prekeys
  clients <- traverse (responseJsonError <=< addClient brig2 (userId bob)) nclients
  resp <- getUserClientsQualified brig1 (userId alice) (qDomain (userQualifiedId bob)) (userId bob)
  let expected = map pubClient clients
      actual = responseJsonUnsafe resp
  liftIO $ Set.fromList actual @?= Set.fromList expected

testListConversations :: Brig -> Brig -> Galley -> Galley -> Http ()
testListConversations brig1 brig2 galley1 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2

  -- create two group conversations with alice & bob, on each of domain1, domain2
  cnv1 <- responseJsonUnsafe <$> createConversation galley1 (userId alice) [userQualifiedId bob]
  cnv2 <- responseJsonUnsafe <$> createConversation galley2 (userId bob) [userQualifiedId alice]

  --  Expect both group conversations containing alice and bob
  --  to pop up for alice (on galley1)
  --  when she request her own conversations ( GET /conversations )
  debug "list all convs galley 1..."
  -- From Alice's point of view
  -- both conversations should show her as the self member and bob as Othermember.
  let expected = cnv1
  rs <- listAllConvs galley1 (userId alice) <!! (const 200 === statusCode)
  let cs = convList <$> responseJsonUnsafe rs
  let c1 = cs >>= find ((== cnvQualifiedId cnv1) . cnvQualifiedId)
  let c2 = cs >>= find ((== cnvQualifiedId cnv2) . cnvQualifiedId)
  liftIO . forM_ [c1, c2] $ \actual -> do
    assertEqual
      "self member mismatch"
      (Just . cmSelf $ cnvMembers expected)
      (cmSelf . cnvMembers <$> actual)
    assertEqual
      "other members mismatch"
      (Just [])
      ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)

debug :: Text -> Http ()
debug text = do
  l <- Log.new Log.defSettings
  liftIO $ Log.warn l (Log.msg text)

-- bob creates a conversation on domain 2 with alice on domain 1, then sends a
-- message to alice
testSendMessage :: Brig -> Brig -> Galley -> Cannon -> Http ()
testSendMessage brig1 brig2 galley2 cannon1 = do
  -- create alice user and client on domain 1
  alice <- randomUser brig1
  aliceClient <-
    clientId . responseJsonUnsafe
      <$> addClient
        brig1
        (userId alice)
        (defNewClient PermanentClientType [] (someLastPrekeys !! 0))

  -- create bob user and client on domain 2
  bob <- randomUser brig2
  bobClient <-
    clientId . responseJsonUnsafe
      <$> addClient
        brig2
        (userId bob)
        (defNewClient PermanentClientType [] (someLastPrekeys !! 1))

  -- create conversation on domain 2
  convId <-
    qUnqualified . cnvQualifiedId . responseJsonUnsafe
      <$> createConversation galley2 (userId bob) [userQualifiedId alice]

  -- send a message from bob at domain 2 to alice at domain 1
  let qconvId = Qualified convId (qDomain (userQualifiedId bob))
      msgText = "🕊️"
      rcpts = [(userQualifiedId alice, aliceClient, msgText)]
      msg = mkQualifiedOtrPayload bobClient rcpts "" MismatchReportAll

  WS.bracketR cannon1 (userId alice) $ \(wsAlice) -> do
    post
      ( galley2
          . paths
            [ "conversations",
              toByteString' (qDomain qconvId),
              toByteString' convId,
              "proteus",
              "messages"
            ]
          . zUser (userId bob)
          . zConn "conn"
          . header "Z-Type" "access"
          . contentProtobuf
          . acceptJson
          . bytes (Protolens.encodeMessage msg)
      )
      !!! const 201 === statusCode

    -- verify that alice received the message
    WS.assertMatch_ (5 # Second) wsAlice $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconvId
      evtType e @?= OtrMessageAdd
      evtFrom e @?= userQualifiedId bob
      evtData e
        @?= EdOtrMessage
          ( OtrMessage bobClient aliceClient (toBase64Text msgText) (Just "")
          )

-- alice creates a conversation on domain 1 with bob on domain 2, then bob
-- sends a message to alice
testSendMessageToRemoteConv :: Brig -> Brig -> Galley -> Galley -> Cannon -> Http ()
testSendMessageToRemoteConv brig1 brig2 galley1 galley2 cannon1 = do
  -- create alice user and client on domain 1
  alice <- randomUser brig1
  aliceClient <-
    clientId . responseJsonUnsafe
      <$> addClient
        brig1
        (userId alice)
        (defNewClient PermanentClientType [] (someLastPrekeys !! 0))

  -- create bob user and client on domain 2
  bob <- randomUser brig2
  bobClient <-
    clientId . responseJsonUnsafe
      <$> addClient
        brig2
        (userId bob)
        (defNewClient PermanentClientType [] (someLastPrekeys !! 1))

  -- create conversation on domain 1
  convId <-
    qUnqualified . cnvQualifiedId . responseJsonUnsafe
      <$> createConversation galley1 (userId alice) [userQualifiedId bob]

  -- send a message from bob at domain 2 to alice at domain 1
  let qconvId = Qualified convId (qDomain (userQualifiedId alice))
      msgText = "🕊️"
      rcpts = [(userQualifiedId alice, aliceClient, msgText)]
      msg = mkQualifiedOtrPayload bobClient rcpts "" MismatchReportAll

  WS.bracketR cannon1 (userId alice) $ \(wsAlice) -> do
    post
      ( galley2
          . paths
            [ "conversations",
              toByteString' (qDomain qconvId),
              toByteString' convId,
              "proteus",
              "messages"
            ]
          . zUser (userId bob)
          . zConn "conn"
          . header "Z-Type" "access"
          . contentProtobuf
          . acceptJson
          . bytes (Protolens.encodeMessage msg)
      )
      !!! const 201 === statusCode

    -- verify that alice received the message
    WS.assertMatch_ (5 # Second) wsAlice $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconvId
      evtType e @?= OtrMessageAdd
      evtFrom e @?= userQualifiedId bob
      evtData e
        @?= EdOtrMessage
          ( OtrMessage bobClient aliceClient (toBase64Text msgText) (Just "")
          )
