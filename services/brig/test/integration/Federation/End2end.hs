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

module Federation.End2end where

import API.MLS.Util
import API.User.Util
import Bilge
import Bilge.Assert ((!!!), (<!!), (===))
import Brig.API.Client (pubClient)
import Brig.Options qualified as BrigOpts
import Control.Arrow ((&&&))
import Control.Lens hiding ((#))
import Data.Aeson qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util (toBase64Text)
import Data.List1 as List1
import Data.Map qualified as Map
import Data.ProtoLens qualified as Protolens
import Data.Qualified
import Data.Range (checked)
import Data.Set qualified as Set
import Federation.Util
import Imports
import System.IO.Temp
import System.Logger qualified as Log
import Test.Tasty
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import Util
import Util.Options (Endpoint)
import Wire.API.Asset
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Event.LeaveReason
import Wire.API.Internal.Notification
import Wire.API.MLS.KeyPackage
import Wire.API.Message
import Wire.API.Routes.MultiTablePaging
import Wire.API.User hiding (assetKey)
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

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
  CargoHold ->
  Cannon ->
  Endpoint ->
  Brig ->
  Galley ->
  CargoHold ->
  Cannon ->
  IO TestTree
spec _brigOpts mg brig galley cargohold cannon _federator brigTwo galleyTwo cargoholdTwo cannonTwo =
  pure $
    testGroup
      "federation-end2end-user"
      [ test mg "get users by ids on multiple backends" $ testGetUsersById brig brigTwo,
        test mg "claim client prekey" $ testClaimPrekeySuccess brig brigTwo,
        test mg "claim prekey bundle" $ testClaimPrekeyBundleSuccess brig brigTwo,
        test mg "claim multi-prekey bundle" $ testClaimMultiPrekeyBundleSuccess brig brigTwo,
        test mg "list user clients" $ testListUserClients brig brigTwo,
        test mg "list own conversations" $ testListConversations brig brigTwo galley galleyTwo,
        test mg "remove remote user from a local conversation" $ testRemoveRemoteUserFromLocalConv brig galley brigTwo galleyTwo,
        test mg "leave a remote conversation" $ leaveRemoteConversation brig galley brigTwo galleyTwo,
        test mg "include remote users to new conversation" $ testRemoteUsersInNewConv brig galley brigTwo galleyTwo,
        test mg "send a message to a remote user" $ testSendMessage brig brigTwo galleyTwo cannon,
        test mg "send a message in a remote conversation" $ testSendMessageToRemoteConv brig brigTwo galley galleyTwo cannon,
        test mg "delete user connected to remotes and in conversation with remotes" $ testDeleteUser brig brigTwo galley galleyTwo cannon,
        test mg "download remote asset" $ testRemoteAsset brig brigTwo cargohold cargoholdTwo,
        test mg "claim remote key packages" $ claimRemoteKeyPackages brig brigTwo,
        test mg "remote typing indicator" $
          testRemoteTypingIndicator brig brigTwo galley galleyTwo cannon cannonTwo
      ]

-- | Path covered by this test:
--
-- +------+         +---------+        +---------+          +------+
-- | brig |  http2  |federator| http2  |federator|   http   | brig |
-- |      +-------->+         +------->+         +--------->+      |
-- +------+         +-+-------+        +---------+          +------+
testGetUsersById :: Brig -> Brig -> Http ()
testGetUsersById brig1 brig2 = do
  users <- traverse randomUser [brig1, brig2]
  let self = Imports.head users
      q = ListUsersByIds (map userQualifiedId users)
      expected = sort (map userQualifiedId users)
  post
    ( apiVersion "v3"
        . brig1
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
      qmap :: (Ord a) => [(Qualified a, b)] -> Map Domain (Map a b)
      qmap = fmap Map.fromList . indexQualified . map (sequenceAOf _1)
  c1 <- generateClientPrekeys brig1 prekeys1
  c2 <- generateClientPrekeys brig2 prekeys2
  let uc =
        QualifiedUserClients . qmap $
          [mkClients <$> c1, mkClients <$> c2]
      ucm =
        mkQualifiedUserClientPrekeyMap . fmap mkUserClientPrekeyMap . qmap $
          [mkClientMap <$> c1, mkClientMap <$> c2]
  post
    ( apiVersion "v3"
        . brig1
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

testRemoveRemoteUserFromLocalConv :: Brig -> Galley -> Brig -> Galley -> Http ()
testRemoveRemoteUserFromLocalConv brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  let aliceId = userQualifiedId alice
  let bobId = userQualifiedId bob

  connectUsersEnd2End brig1 brig2 aliceId bobId

  convId <-
    fmap (.qualifiedId) . responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [bobId]
        <!! const 201 === statusCode

  aliceConvBeforeDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvBeforeDelete)) @?= [bobId]

  bobConvBeforeDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley2 (userId bob) convId
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

  aliceConvAfterDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvAfterDelete)) @?= []

  getConversationQualified galley2 (userId bob) convId
    !!! const 404 === statusCode

leaveRemoteConversation :: Brig -> Galley -> Brig -> Galley -> Http ()
leaveRemoteConversation brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2
  let aliceId = userQualifiedId alice
  let bobId = userQualifiedId bob

  connectUsersEnd2End brig1 brig2 aliceId bobId

  convId <-
    fmap (.qualifiedId) . responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [bobId]
        <!! const 201 === statusCode

  aliceConvBeforeDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvBeforeDelete)) @?= [bobId]

  bobConvBeforeDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley2 (userId bob) convId
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

  aliceConvAfterDelete :: ConversationV8 <- responseJsonUnsafe <$> getConversationQualified galley1 (userId alice) convId
  liftIO $ map omQualifiedId (cmOthers (cnvMembers aliceConvAfterDelete)) @?= []

  getConversationQualified galley2 (userId bob) convId
    !!! const 404 === statusCode

-- | This creates a new conversation with a remote user. The test checks that
-- Galleys on both ends of the federation see the same conversation members.
testRemoteUsersInNewConv :: Brig -> Galley -> Brig -> Galley -> Http ()
testRemoteUsersInNewConv brig1 galley1 brig2 galley2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2

  connectUsersEnd2End brig1 brig2 (userQualifiedId alice) (userQualifiedId bob)
  convId <-
    fmap (.qualifiedId) . responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [userQualifiedId bob]
        <!! const 201 === statusCode

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

  connectUsersEnd2End brig1 brig2 (userQualifiedId alice) (userQualifiedId bob)

  -- create two group conversations with alice & bob, on each of domain1, domain2
  cnv1 <-
    responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [userQualifiedId bob]
        <!! const 201 === statusCode
  cnv2 <-
    responseJsonError @_ @ConversationV9
      =<< createConversation galley2 (userId bob) [userQualifiedId alice]
        <!! const 201 === statusCode

  --  Expect both group conversations containing alice and bob
  --  to pop up for alice (on galley1)
  --  when she request her own conversations ( GET /conversations )
  debug "list all convs galley 1..."
  -- From Alice's point of view
  -- both conversations should show her as the self member and bob as Othermember.
  let expected = cnv1
  rs <- listConvIdsFirstPage galley1 (userId alice) <!! (const 200 === statusCode)
  (page :: ConvIdsPage) <- responseJsonError rs
  liftIO $ assertBool "conversations should fit in a single page" (not (mtpHasMore page))
  cids <- liftIO $ case checked (mtpResults page) of
    Nothing -> assertFailure "too many conversations"
    Just r -> pure r
  (cs :: [ConversationV8]) <-
    (fmap crFound . responseJsonError)
      =<< listConvs galley1 (userId alice) cids <!! (const 200 === statusCode)
  let c1 = find ((== cnv1.qualifiedId) . cnvQualifiedId) cs
  let c2 = find ((== cnv2.qualifiedId) . cnvQualifiedId) cs
  liftIO . forM_ [c1, c2] $ \mActual -> do
    case mActual of
      Nothing -> assertFailure "An expected conversation was not found"
      Just actual -> do
        let actualMemIds = Set.fromList $ actual.cnvMembers.cmSelf.memId : map (.omQualifiedId) actual.cnvMembers.cmOthers
            expectedMemIds = Set.fromList $ map (.omQualifiedId) expected.otherMembers
        assertEqual "member mismatch" actualMemIds expectedMemIds

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
        (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))

  -- create bob user and client on domain 2
  bob <- randomUser brig2
  bobClient <-
    clientId . responseJsonUnsafe
      <$> addClient
        brig2
        (userId bob)
        (defNewClient PermanentClientType [] (someLastPrekeys !! 1))

  connectUsersEnd2End brig1 brig2 (userQualifiedId alice) (userQualifiedId bob)

  -- create conversation on domain 2
  convId <-
    fmap (qUnqualified . (.qualifiedId)) . responseJsonError @_ @ConversationV9
      =<< createConversation galley2 (userId bob) [userQualifiedId alice]
        <!! const 201 === statusCode

  -- send a message from bob at domain 2 to alice at domain 1
  let qconvId = Qualified convId (qDomain (userQualifiedId bob))
      msgText = "🕊️"
      rcpts = [(userQualifiedId alice, aliceClient, msgText)]
      msg = mkQualifiedOtrPayload bobClient rcpts "" MismatchReportAll

  WS.bracketR cannon1 (userId alice) $ \wsAlice -> do
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
    fmap clientId . responseJsonError
      =<< addClient brig1 (userId alice) (defNewClient PermanentClientType [] (Imports.head someLastPrekeys))
        <!! const 201 === statusCode

  -- create bob user and client on domain 2
  bob <- randomUser brig2
  bobClient <-
    fmap clientId . responseJsonError
      =<< addClient brig2 (userId bob) (defNewClient PermanentClientType [] (someLastPrekeys !! 1))
        <!! const 201 === statusCode

  connectUsersEnd2End brig1 brig2 (userQualifiedId alice) (userQualifiedId bob)

  -- create conversation on domain 1
  convId <-
    fmap (qUnqualified . (.qualifiedId)) . responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [userQualifiedId bob]
        <!! const 201 === statusCode

  -- send a message from bob at domain 2 to alice at domain 1
  let qconvId = Qualified convId (qDomain (userQualifiedId alice))
      msgText = "🕊️"
      rcpts = [(userQualifiedId alice, aliceClient, msgText)]
      msg = mkQualifiedOtrPayload bobClient rcpts "" MismatchReportAll

  WS.bracketR cannon1 (userId alice) $ \wsAlice -> do
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

testDeleteUser :: Brig -> Brig -> Galley -> Galley -> Cannon -> Http ()
testDeleteUser brig1 brig2 galley1 galley2 cannon1 = do
  alice <- userQualifiedId <$> randomUser brig1
  bobDel <- userQualifiedId <$> randomUser brig2

  connectUsersEnd2End brig1 brig2 alice bobDel

  conv1 <-
    fmap (.qualifiedId) . responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (qUnqualified alice) [bobDel]
        <!! const 201 === statusCode

  conv2 <-
    fmap (.qualifiedId) . responseJsonError @_ @ConversationV9
      =<< createConversation galley2 (qUnqualified bobDel) [alice]
        <!! const 201 === statusCode

  WS.bracketR cannon1 (qUnqualified alice) $ \wsAlice -> do
    deleteUser (qUnqualified bobDel) (Just defPassword) brig2 !!! const 200 === statusCode
    WS.assertMatch_ (5 # Second) wsAlice $ matchDeleteUserNotification bobDel
    WS.assertMatch_ (5 # Second) wsAlice $ matchConvLeaveNotification conv1 bobDel [bobDel] EdReasonLeft
    WS.assertMatch_ (5 # Second) wsAlice $ matchConvLeaveNotification conv2 bobDel [bobDel] EdReasonLeft

testRemoteAsset :: Brig -> Brig -> CargoHold -> CargoHold -> Http ()
testRemoteAsset brig1 brig2 ch1 ch2 = do
  alice <- userQualifiedId <$> randomUser brig1
  bob <- userQualifiedId <$> randomUser brig2

  let sts = defAssetSettings & setAssetPublic .~ True
  ast <- responseJsonError =<< uploadAsset ch2 (qUnqualified bob) sts "hello world"
  let qkey = view assetKey ast

  downloadAsset ch1 (qUnqualified alice) qkey
    !!! do
      const 200 === statusCode
      const (Just "hello world") === responseBody

claimRemoteKeyPackages :: Brig -> Brig -> Http ()
claimRemoteKeyPackages brig1 brig2 = do
  alice <- userQualifiedId <$> randomUser brig1

  bob <- userQualifiedId <$> randomUser brig2
  bobClients <- for (take 3 someLastPrekeys) $ \lpk -> do
    let new = defNewClient PermanentClientType [] lpk
    fmap clientId $ responseJsonError =<< addClient brig2 (qUnqualified bob) new

  withSystemTempDirectory "mls" $ \tmp ->
    for_ bobClients $ \c ->
      uploadKeyPackages brig2 tmp def bob c 5

  bundle :: KeyPackageBundle <-
    responseJsonError
      =<< post
        ( brig1
            . paths ["mls", "key-packages", "claim", toByteString' (qDomain bob), toByteString' (qUnqualified bob)]
            . queryItem "ciphersuite" "0x0001"
            . zUser (qUnqualified alice)
        )
        <!! const 200 === statusCode

  liftIO $
    Set.map (\e -> (e.user, e.client)) bundle.entries
      @?= Set.fromList [(bob, c) | c <- bobClients]

testRemoteTypingIndicator ::
  (HasCallStack) =>
  Brig ->
  Brig ->
  Galley ->
  Galley ->
  Cannon ->
  Cannon ->
  Http ()
testRemoteTypingIndicator brig1 brig2 galley1 galley2 cannon1 cannon2 = do
  alice <- randomUser brig1
  bob <- randomUser brig2

  connectUsersEnd2End brig1 brig2 (userQualifiedId alice) (userQualifiedId bob)

  cnv <-
    responseJsonError @_ @ConversationV9
      =<< createConversation galley1 (userId alice) [userQualifiedId bob]
        <!! const 201 === statusCode
  let isTyping g u s =
        post
          ( g
              . paths
                [ "conversations",
                  toByteString' (qDomain cnv.qualifiedId),
                  toByteString' (qUnqualified cnv.qualifiedId),
                  "typing"
                ]
              . zUser (userId u)
              . zConn "conn"
              . json s
          )
          !!! const 200 === statusCode
  let checkEvent ws u s =
        WS.assertMatch_ (8 # Second) ws $ \n -> do
          let e = List1.head (WS.unpackPayload n)
          ntfTransient n @?= True
          evtConv e @?= cnv.qualifiedId
          evtType e @?= Typing
          evtFrom e @?= userQualifiedId u
          evtData e @?= EdTyping s

  -- -- alice is typing, bob gets events
  WS.bracketR cannon2 (userId bob) $ \wsBob -> do
    isTyping galley1 alice StartedTyping
    checkEvent wsBob alice StartedTyping
    isTyping galley1 alice StoppedTyping
    checkEvent wsBob alice StoppedTyping

  -- bob is typing, alice gets events
  WS.bracketR cannon1 (userId alice) $ \wsAlice -> do
    isTyping galley2 bob StartedTyping
    checkEvent wsAlice bob StartedTyping
    isTyping galley2 bob StoppedTyping
    checkEvent wsAlice bob StoppedTyping
