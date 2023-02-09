{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module PostMessage
  ( tests,
  )
where

import API.Util
import Bilge hiding (head)
import Bilge.Assert
import Control.Exception
import Control.Lens hiding ((#))
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.Json.Util hiding ((#))
import Data.List1 hiding (head)
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Timeout
import Federator.MockServer
import Imports
import qualified Network.HTTP.Types as HTTP
import Test.QuickCheck hiding ((===))
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Federation.API
import qualified Wire.API.Federation.API.Brig as F
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Message
import qualified Wire.API.Message as Message
import Wire.API.User.Client
import Wire.API.UserMap

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Post message"
    [ test s "post conversations/:cnv/otr/message: message delivery and missing clients" postCryptoMessageVerifyMsgSentAndRejectIfMissingClient,
      test s "post conversations/:cnv/otr/message: mismatch and prekey fetching" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson,
      test s "post conversations/:cnv/otr/message: mismatch with protobuf" postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto,
      test s "post conversations/:cnv/otr/message: unknown sender client" postCryptoMessageNotAuthorizeUnknownClient,
      test s "post conversations/:cnv/otr/message: ignore_missing and report_missing" postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam,
      test s "post message qualified - local owning backend - success" postMessageQualifiedLocalOwningBackendSuccess,
      test s "post message qualified - local owning backend - missing clients" postMessageQualifiedLocalOwningBackendMissingClients,
      test s "post message qualified - local owning backend - redundant and deleted clients" postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients,
      test s "post message qualified - local owning backend - ignore missing" postMessageQualifiedLocalOwningBackendIgnoreMissingClients,
      test s "post message qualified - local owning backend - failed to send clients" postMessageQualifiedLocalOwningBackendFailedToSendClients,
      test s "post message qualified - remote owning backend - federation failure" postMessageQualifiedRemoteOwningBackendFailure,
      test s "post message qualified - remote owning backend - success" postMessageQualifiedRemoteOwningBackendSuccess,
      test s "post message - reject if missing client" postMessageRejectIfMissingClients,
      test s "post message - client that is not in group doesn't receive message" postMessageClientNotInGroupDoesNotReceiveMsg
    ]

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies whether a message actually gets sent all the way to
-- cannon.
postCryptoMessageVerifyMsgSentAndRejectIfMissingClient :: TestM ()
postCryptoMessageVerifyMsgSentAndRejectIfMissingClient = do
  localDomain <- viewFederationDomain
  c <- view tsCannon
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  let qalice = Qualified alice localDomain
      qconv = Qualified conv localDomain
  -- WS receive timeout
  let t = 5 # Second
  -- Missing eve
  let m1 = [(bob, bc, "ciphertext1")]
  postOtrMessage id alice ac conv m1 !!! do
    const 412 === statusCode
    assertMismatch [(eve, Set.singleton ec)] [] []
  -- Complete
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    let m2 = [(bob, bc, toBase64Text "ciphertext2"), (eve, ec, toBase64Text "ciphertext2")]
    postOtrMessage id alice ac conv m2 !!! do
      const 201 === statusCode
      assertMismatch [] [] []
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext2"))
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext2"))
  -- Redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m3 = [(alice, ac, toBase64Text "ciphertext3"), (bob, bc, toBase64Text "ciphertext3"), (eve, ec, toBase64Text "ciphertext3")]
    postOtrMessage id alice ac conv m3 !!! do
      const 201 === statusCode
      assertMismatch [] [(alice, Set.singleton ac)] []
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext3"))
    void . liftIO $ WS.assertMatch t wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext3"))
    -- Alice should not get it
    assertNoMsg wsA (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext3"))
  -- Deleted eve
  WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
    deleteClient eve ec (Just defPassword) !!! const 200 === statusCode
    liftIO $
      WS.assertMatch_ (5 # WS.Second) wsE $
        wsAssertClientRemoved ec
    let m4 = [(bob, bc, toBase64Text "ciphertext4"), (eve, ec, toBase64Text "ciphertext4")]
    postOtrMessage id alice ac conv m4 !!! do
      const 201 === statusCode
      assertMismatch [] [] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext4"))
    -- Eve should not get it
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext4"))
  -- Deleted eve & redundant self
  WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
    let m5 = [(bob, bc, toBase64Text "ciphertext5"), (eve, ec, toBase64Text "ciphertext5"), (alice, ac, toBase64Text "ciphertext5")]
    postOtrMessage id alice ac conv m5 !!! do
      const 201 === statusCode
      assertMismatch [] [(alice, Set.singleton ac)] [(eve, Set.singleton ec)]
    void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext5"))
    -- Neither Alice nor Eve should get it
    assertNoMsg wsA (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext5"))
    assertNoMsg wsE (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext5"))
  -- Missing Bob, deleted eve & redundant self
  let m6 = [(eve, ec, toBase64Text "ciphertext6"), (alice, ac, toBase64Text "ciphertext6")]
  postOtrMessage id alice ac conv m6 !!! do
    const 412 === statusCode
    assertMismatch
      [(bob, Set.singleton bc)]
      [(alice, Set.singleton ac)]
      [(eve, Set.singleton ec)]
  -- A second client for Bob
  bc2 <- randomClient bob (someLastPrekeys !! 3)
  -- The first client listens for all messages of Bob
  WS.bracketR c bob $ \wsB -> do
    let cipher = toBase64Text "ciphertext7"
    -- The second client listens only for his own messages
    WS.bracketR (c . queryItem "client" (toByteString' bc2)) bob $ \wsB2 -> do
      let m7 = [(bob, bc, cipher), (bob, bc2, cipher)]
      postOtrMessage id alice ac conv m7 !!! do
        const 201 === statusCode
        assertMismatch [] [] []
      -- Bob's first client gets both messages
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc cipher)
      void . liftIO $ WS.assertMatch t wsB (wsAssertOtr qconv qalice ac bc2 cipher)
      -- Bob's second client gets only the message destined for him
      void . liftIO $ WS.assertMatch t wsB2 (wsAssertOtr qconv qalice ac bc2 cipher)
      liftIO $ assertBool "unexpected equal clients" (bc /= bc2)
      assertNoMsg wsB2 (wsAssertOtr qconv qalice ac bc cipher)

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies basic mismatch behavior of the the JSON endpoint.
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson :: TestM ()
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysJson = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let m = [(bob, bc, toBase64Text "hello bob")]
  r1 <-
    postOtrMessage id alice ac conv m <!! do
      const 412 === statusCode
      assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec)] [] []
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  -- Fetch all missing clients prekeys
  b <- view tsUnversionedBrig
  r2 <-
    post (b . zUser alice . path "v1/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies basic mismatch behaviour of the protobuf endpoint.
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto :: TestM ()
postCryptoMessageVerifyRejectMissingClientAndRepondMissingPrekeysProto = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  connectUsers alice (list1 bob [eve])
  conv <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, bc, ciphertext)]
  r1 <-
    postProtoOtrMessage alice ac conv m
      <!! const 412 === statusCode
  let x = responseJsonUnsafeWithMsg "ClientMismatch" r1
  pure r1
    !!! assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec)] [] []
  -- Fetch all missing clients prekeys
  b <- view tsUnversionedBrig
  r2 <-
    post (b . zUser alice . path "v1/users/prekeys" . json (missingClients x))
      <!! const 200 === statusCode
  let p = responseJsonUnsafeWithMsg "prekeys" r2 :: UserClientPrekeyMap
  liftIO $ do
    Map.keys (userClientMap (getUserClientPrekeyMap p)) @=? [eve]
    Map.keys <$> Map.lookup eve (userClientMap (getUserClientPrekeyMap p)) @=? Just [ec]

-- @END

-- | This test verifies behaviour when an unknown client posts the message. Only
-- tests the Protobuf endpoint.
postCryptoMessageNotAuthorizeUnknownClient :: TestM ()
postCryptoMessageNotAuthorizeUnknownClient = do
  alice <- randomUser
  bob <- randomUser
  bc <- randomClient bob (head someLastPrekeys)
  connectUsers alice (list1 bob [])
  conv <- decodeConvId <$> postConv alice [bob] (Just "gossip") [] Nothing Nothing
  -- Unknown client ID => 403
  let ciphertext = toBase64Text "hello bob"
  let m = otrRecipients [(bob, bc, ciphertext)]
  postProtoOtrMessage alice (ClientId "172618352518396") conv m
    !!! const 403 === statusCode

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies the following scenario.
-- A client sends a message to all clients of a group and one more who is not part of the group.
-- The server must not send this message to client ids not part of the group.
postMessageClientNotInGroupDoesNotReceiveMsg :: TestM ()
postMessageClientNotInGroupDoesNotReceiveMsg = do
  localDomain <- viewFederationDomain
  cannon <- view tsCannon
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 2)
  (chad, cc) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 bob [eve, chad])
  conversationWithAllButChad <- decodeConvId <$> postConv alice [bob, eve] (Just "gossip") [] Nothing Nothing
  let qalice = Qualified alice localDomain
      qconv = Qualified conversationWithAllButChad localDomain
  WS.bracketR3 cannon bob eve chad $ \(wsBob, wsEve, wsChad) -> do
    let msgToAllIncludingChad = [(bob, bc, toBase64Text "ciphertext2"), (eve, ec, toBase64Text "ciphertext2"), (chad, cc, toBase64Text "ciphertext2")]
    postOtrMessage id alice ac conversationWithAllButChad msgToAllIncludingChad !!! const 201 === statusCode
    let checkBobGetsMsg = void . liftIO $ WS.assertMatch (5 # Second) wsBob (wsAssertOtr qconv qalice ac bc (toBase64Text "ciphertext2"))
    let checkEveGetsMsg = void . liftIO $ WS.assertMatch (5 # Second) wsEve (wsAssertOtr qconv qalice ac ec (toBase64Text "ciphertext2"))
    let checkChadDoesNotGetMsg = assertNoMsg wsChad (wsAssertOtr qconv qalice ac ac (toBase64Text "ciphertext2"))
    checkBobGetsMsg
    checkEveGetsMsg
    checkChadDoesNotGetMsg

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies that when a client sends a message not to all clients of a group then the server should reject the message and sent a notification to the sender (412 Missing clients).
-- The test is somewhat redundant because this is already tested as part of other tests already. This is a stand alone test that solely tests the behavior described above.
postMessageRejectIfMissingClients :: TestM ()
postMessageRejectIfMissingClients = do
  (sender, senderClient) : allReceivers <- randomUserWithClient `traverse` someLastPrekeys
  let (receiver1, receiverClient1) : otherReceivers = allReceivers
  connectUsers sender (list1 receiver1 (fst <$> otherReceivers))
  conv <- decodeConvId <$> postConv sender (receiver1 : (fst <$> otherReceivers)) (Just "gossip") [] Nothing Nothing
  let msgToAllClients = mkMsg "hello!" <$> allReceivers
  let msgMissingClients = mkMsg "hello!" <$> drop 1 allReceivers

  let checkSendToAllClientShouldBeSuccessful =
        postOtrMessage id sender senderClient conv msgToAllClients !!! do
          const 201 === statusCode
          assertMismatch [] [] []

  let checkSendWitMissingClientsShouldFail =
        postOtrMessage id sender senderClient conv msgMissingClients !!! do
          const 412 === statusCode
          assertMismatch [(receiver1, Set.singleton receiverClient1)] [] []

  checkSendToAllClientShouldBeSuccessful
  checkSendWitMissingClientsShouldFail
  where
    mkMsg :: ByteString -> (UserId, ClientId) -> (UserId, ClientId, Text)
    mkMsg text (userId, clientId) = (userId, clientId, toBase64Text text)

-- @END

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- This test verifies behaviour under various values of ignore_missing and
-- report_missing. Only tests the JSON endpoint.
postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam :: TestM ()
postCryptoMessageVerifyCorrectResponseIfIgnoreAndReportMissingQueryParam = do
  (alice, ac) <- randomUserWithClient (head someLastPrekeys)
  (bob, bc) <- randomUserWithClient (someLastPrekeys !! 1)
  (chad, cc) <- randomUserWithClient (someLastPrekeys !! 2)
  (eve, ec) <- randomUserWithClient (someLastPrekeys !! 3)
  connectUsers alice (list1 bob [chad, eve])
  conv <- decodeConvId <$> postConv alice [bob, chad, eve] (Just "gossip") [] Nothing Nothing
  -- Missing eve
  let msgMissingChadAndEve = [(bob, bc, toBase64Text "hello bob")]
  let m' = otrRecipients [(bob, bc, toBase64Text "hello bob")]
  -- These three are equivalent (i.e. report all missing clients)
  postOtrMessage id alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  postOtrMessage (queryItem "ignore_missing" "false") alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  postOtrMessage (queryItem "report_missing" "true") alice ac conv msgMissingChadAndEve
    !!! const 412 === statusCode
  -- These two are equivalent (i.e. ignore all missing clients)
  postOtrMessage (queryItem "ignore_missing" "true") alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  postOtrMessage (queryItem "report_missing" "false") alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Report missing clients of a specific user only
  postOtrMessage (queryItem "report_missing" (toByteString' bob)) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Let's make sure that the same logic using protobuf in the body works too
  postProtoOtrMessage' Nothing (queryItem "report_missing" (toByteString' bob)) alice ac conv m'
    !!! const 201 === statusCode
  -- Body takes precedence
  postOtrMessage' (Just [bob]) (queryItem "report_missing" (listToByteString [eve, chad])) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Set it only in the body of the message
  postOtrMessage' (Just [bob]) id alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  -- Let's make sure that protobuf works too, when specified in the body only
  postProtoOtrMessage' (Just [bob]) id alice ac conv m'
    !!! const 201 === statusCode
  reportEveAndChad <-
    -- send message with no clients
    postOtrMessage (queryItem "report_missing" (listToByteString [eve, chad])) alice ac conv []
      <!! const 412 === statusCode
  pure reportEveAndChad
    !!! assertMismatchWithMessage (Just "client mismatch") [(eve, Set.singleton ec), (chad, Set.singleton cc)] [] []
  -- Ignore missing clients of a specific user only
  postOtrMessage (queryItem "ignore_missing" (listToByteString [chad, eve])) alice ac conv msgMissingChadAndEve
    !!! const 201 === statusCode
  ignoreEveAndChadButNotBob <-
    postOtrMessage (queryItem "ignore_missing" (listToByteString [chad, eve])) alice ac conv []
      <!! const 412 === statusCode
  pure ignoreEveAndChadButNotBob
    !!! assertMismatchWithMessage (Just "client mismatch") [(bob, Set.singleton bc)] [] []
  where
    listToByteString = BS.intercalate "," . map toByteString'

-- @END

-- | Sets up a conversation on Backend A known as "owning backend". All user's
-- on this backend have names begining with 'A'. The conversation has a couple
-- of users from backend B and one user from backend C.
--
-- One of the users from Backend A will send the message, it is expected that
-- message will be sent successfully.
postMessageQualifiedLocalOwningBackendSuccess :: TestM ()
postMessageQualifiedLocalOwningBackendSuccess = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (alice, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (alex, alexClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  alexClient2 <- randomClient (qUnqualified alex) (someLastPrekeys !! 2)
  (amy, amyClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)

  let bDomain = Domain "b.far-away.example.com"
      cDomain = Domain "c.far-away.example.com"
      randomQuidAndClients d n = (,) <$> randomQualifiedId d <*> liftIO (replicateM n $ generate arbitrary)
  (bob, [bobClient]) <- randomQuidAndClients bDomain 1
  (bart, [bartClient1, bartClient2]) <- randomQuidAndClients bDomain 2
  (carl, [carlClient]) <- randomQuidAndClients cDomain 1

  let aliceU = qUnqualified alice
      alexU = qUnqualified alex
      amyU = qUnqualified amy

  connectLocalQualifiedUsers aliceU (list1 alex [amy])
  forM_ [bob, bart, carl] $ connectWithRemoteUser aliceU

  resp <-
    postConvWithRemoteUsers
      aliceU
      defNewProteusConv {newConvQualifiedUsers = [alex, amy, bob, bart, carl]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketAsClientRN cannon [(alexU, alexClient), (alexU, alexClient2), (amyU, amyClient)] $ \[wsAlex1, wsAlex2, wsAmy] -> do
    let message =
          [ (alex, alexClient, "text-for-alex"),
            (alex, alexClient2, "text-for-alex2"),
            (amy, amyClient, "text-for-amy"),
            (bob, bobClient, "text-for-bob"),
            (bart, bartClient1, "text-for-bart1"),
            (bart, bartClient2, "text-for-bart2"),
            (carl, carlClient, "text-for-carl")
          ]

    let mkPubClient c = PubClient c Nothing
        brigMock = do
          guardRPC "get-user-clients"
          d <- frTargetDomain <$> getRequest
          asum
            [ do
                guard (d == bDomain)

                mockReply $
                  UserMap . Map.fromList $
                    [ (qUnqualified bob, Set.singleton (mkPubClient bobClient)),
                      (qUnqualified bart, Set.fromList (map mkPubClient [bartClient1, bartClient2]))
                    ],
              do
                guard (d == cDomain)
                mockReply $
                  UserMap
                    ( Map.singleton
                        (qUnqualified carl)
                        (Set.singleton (PubClient carlClient Nothing))
                    )
            ]
        galleyMock = "on-message-sent" ~> ()

    (resp2, requests) <- postProteusMessageQualifiedWithMockFederator aliceU aliceClient convId message "data" Message.MismatchReportAll (brigMock <|> galleyMock)
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForAlex1 = toBase64Text "text-for-alex"
        encodedTextForAlex2 = toBase64Text "text-for-alex2"
        encodedTextForAmy = toBase64Text "text-for-amy"
        encodedTextForBob = toBase64Text "text-for-bob"
        encodedTextForBart1 = toBase64Text "text-for-bart1"
        encodedTextForBart2 = toBase64Text "text-for-bart2"
        encodedTextForCarl = toBase64Text "text-for-carl"
        encodedData = toBase64Text "data"
    liftIO $ do
      let matchReq domain component r = frTargetDomain r == domain && frComponent r == component
          filterReq domain component = filter (matchReq domain component) requests
      bBrigReq <- assertOne $ filterReq bDomain Brig
      bGalleyReq <- assertOne $ filterReq bDomain Galley
      cBrigReq <- assertOne $ filterReq cDomain Brig
      cGalleyReq <- assertOne $ filterReq cDomain Galley

      frRPC bBrigReq @?= "get-user-clients"
      (sort . F.gucUsers <$> parseFedRequest bBrigReq) @?= Right (sort $ qUnqualified <$> [bob, bart])
      frRPC cBrigReq @?= "get-user-clients"
      parseFedRequest cBrigReq @?= Right (F.GetUserClients [qUnqualified carl])

      frRPC bGalleyReq @?= "on-message-sent"
      bActualNotif <- assertRight $ parseFedRequest bGalleyReq
      let bExpectedNotif =
            F.RemoteMessage
              { rmTime = F.rmTime bActualNotif,
                rmData = Just $ toBase64Text "data",
                rmSender = alice,
                rmSenderClient = aliceClient,
                rmConversation = qUnqualified convId,
                rmPriority = Nothing,
                rmPush = True,
                rmTransient = False,
                rmRecipients =
                  UserClientMap $
                    Map.fromList
                      [ (qUnqualified bob, Map.singleton bobClient encodedTextForBob),
                        ( qUnqualified bart,
                          Map.fromList
                            [ (bartClient1, encodedTextForBart1),
                              (bartClient2, encodedTextForBart2)
                            ]
                        )
                      ]
              }
      bActualNotif @?= bExpectedNotif
      frRPC cGalleyReq @?= "on-message-sent"
      cActualNotif <- assertRight $ parseFedRequest cGalleyReq
      let cExpectedNotif =
            bExpectedNotif
              { F.rmRecipients =
                  UserClientMap $ Map.fromList [(qUnqualified carl, Map.singleton carlClient encodedTextForCarl)]
              }
      cActualNotif @?= cExpectedNotif

      WS.assertMatch_ t wsAlex1 (wsAssertOtr' encodedData convId alice aliceClient alexClient encodedTextForAlex1)
      WS.assertMatch_ t wsAlex2 (wsAssertOtr' encodedData convId alice aliceClient alexClient2 encodedTextForAlex2)
      WS.assertMatch_ t wsAmy (wsAssertOtr' encodedData convId alice aliceClient amyClient encodedTextForAmy)

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message but have a missing client. It is
-- expected that the message will not be sent.
postMessageQualifiedLocalOwningBackendMissingClients :: TestM ()
postMessageQualifiedLocalOwningBackendMissingClients = do
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  chadClient2 <- randomClient (qUnqualified chadOwningDomain) (someLastPrekeys !! 2)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let mock = "get-user-clients" ~> UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients $
              Map.fromList
                [ ( owningDomain,
                    Map.fromList
                      [ (bobUnqualified, Set.singleton bobClient),
                        (chadUnqualified, Set.singleton chadClient2)
                      ]
                  ),
                  ( remoteDomain,
                    Map.singleton (qUnqualified deeRemote) (Set.singleton deeClient)
                  )
                ]
      assertMismatchQualified mempty expectedMissing mempty mempty
    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

-- @END

-- | Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message, it is expected that message will
-- be sent successfully.
postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients :: TestM ()
postMessageQualifiedLocalOwningBackendRedundantAndDeletedClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain
  let remoteDomain = Domain "far-away.example.com"

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 2)
  chadClientNonExistent <- liftIO $ generate arbitrary
  deeRemote <- (`Qualified` remoteDomain) <$> randomId
  deeClient <- liftIO $ generate arbitrary
  (nonMemberOwningDomain, nonMemberOwningDomainClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  nonMemberRemote <- (`Qualified` remoteDomain) <$> randomId
  nonMemberRemoteClient <- liftIO $ generate arbitrary
  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain
      deeRemoteUnqualified = qUnqualified deeRemote
      nonMemberUnqualified = qUnqualified nonMemberOwningDomain
      nonMemberRemoteUnqualified = qUnqualified nonMemberRemote

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR3 cannon bobUnqualified chadUnqualified nonMemberUnqualified $ \(wsBob, wsChad, wsNonMember) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (chadOwningDomain, chadClientNonExistent, "text-for-chad-non-existent"),
            (deeRemote, deeClient, "text-for-dee"),
            (nonMemberOwningDomain, nonMemberOwningDomainClient, "text-for-non-member-owning-domain"),
            (nonMemberRemote, nonMemberRemoteClient, "text-for-non-member-remote")
          ]

    -- FUTUREWORK: Mock federator and ensure that a message to Dee is sent
    let brigMock = do
          guardRPC "get-user-clients"
          getUserClients <- getRequestBody
          let lookupClients uid
                | uid == deeRemoteUnqualified = Just (uid, Set.fromList [PubClient deeClient Nothing])
                | uid == nonMemberRemoteUnqualified = Just (uid, Set.fromList [PubClient nonMemberRemoteClient Nothing])
                | otherwise = Nothing
          mockReply $ UserMap . Map.fromList . mapMaybe lookupClients $ F.gucUsers getUserClients
        galleyMock = "on-message-sent" ~> ()

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll (brigMock <|> galleyMock)
    pure resp2 !!! do
      const 201 === statusCode
      let expectedRedundant =
            QualifiedUserClients . Map.fromList $
              [ ( owningDomain,
                  Map.fromList
                    [ (nonMemberUnqualified, Set.singleton nonMemberOwningDomainClient)
                    ]
                ),
                ( remoteDomain,
                  Map.fromList
                    [ (nonMemberRemoteUnqualified, Set.singleton nonMemberRemoteClient)
                    ]
                )
              ]
          expectedDeleted =
            QualifiedUserClients . Map.singleton owningDomain . Map.fromList $
              [(chadUnqualified, Set.singleton chadClientNonExistent)]
      assertMismatchQualified mempty mempty expectedRedundant expectedDeleted
    liftIO $ do
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
      -- Wait less for no message
      WS.assertNoEvent (1 # Second) [wsNonMember]

-- @SF.Separation @TSFI.RESTfulAPI @S2
-- Sets up a conversation on Backend A known as "owning backend". One of the
-- users from Backend A will send the message but have a missing client. It is
-- expected that the message will be sent except when it is specifically
-- requested to report on missing clients of a user.
postMessageQualifiedLocalOwningBackendIgnoreMissingClients :: TestM ()
postMessageQualifiedLocalOwningBackendIgnoreMissingClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (bobOwningDomain, _bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  chadClient2 <- randomClient (qUnqualified chadOwningDomain) (someLastPrekeys !! 2)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  let mock =
        "get-user-clients" ~>
          UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))

  -- Missing Bob, chadClient2 and Dee
  let message = [(chadOwningDomain, chadClient, "text-for-chad")]
  -- FUTUREWORK: Mock federator and ensure that clients of Dee are checked. Also
  -- ensure that message is not propagated to remotes
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchIgnoreAll mock
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Another way to ignore all is to report nobody
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" (Message.MismatchReportOnly mempty) mock
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- Yet another way to ignore all is to ignore specific users
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchIgnoreOnly (Set.fromList [bobOwningDomain, chadOwningDomain, deeRemote]))
        mock
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified mempty mempty mempty mempty
    let encodedTextForChad = toBase64Text "text-for-chad"
        encodedData = toBase64Text "data"
    WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)
    WS.assertNoEvent (1 # Second) [wsBob]

  -- When we ask only chad be reported, but one of their clients is missing, the
  -- message shouldn't be sent!
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchReportOnly (Set.fromList [chadOwningDomain]))
        mock
    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients . Map.singleton owningDomain . Map.fromList $
              [(chadUnqualified, Set.singleton chadClient2)]
      assertMismatchQualified mempty expectedMissing mempty mempty

    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

  -- Same as above, but with a remote user's client
  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    (resp2, _requests) <-
      postProteusMessageQualifiedWithMockFederator
        aliceUnqualified
        aliceClient
        convId
        message
        "data"
        (Message.MismatchReportOnly (Set.fromList [deeRemote]))
        mock
    pure resp2 !!! do
      const 412 === statusCode
      let expectedMissing =
            QualifiedUserClients . Map.singleton remoteDomain . Map.fromList $
              [(qUnqualified deeRemote, Set.singleton deeClient)]
      assertMismatchQualified mempty expectedMissing mempty mempty
    WS.assertNoEvent (1 # Second) [wsBob, wsChad]

-- @END

postMessageQualifiedLocalOwningBackendFailedToSendClients :: TestM ()
postMessageQualifiedLocalOwningBackendFailedToSendClients = do
  -- WS receive timeout
  let t = 5 # Second
  -- Cannon for local users
  cannon <- view tsCannon
  -- Domain which owns the converstaion
  owningDomain <- viewFederationDomain

  (aliceOwningDomain, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  bobClient2 <- randomClient (qUnqualified bobOwningDomain) (someLastPrekeys !! 2)
  (chadOwningDomain, chadClient) <- randomUserWithClientQualified (someLastPrekeys !! 3)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary
  let remoteDomain = Domain "far-away.example.com"
      deeRemote = Qualified deeId remoteDomain

  let aliceUnqualified = qUnqualified aliceOwningDomain
      bobUnqualified = qUnqualified bobOwningDomain
      chadUnqualified = qUnqualified chadOwningDomain

  connectLocalQualifiedUsers aliceUnqualified (list1 bobOwningDomain [chadOwningDomain])
  connectWithRemoteUser aliceUnqualified deeRemote

  -- FUTUREWORK: Do this test with more than one remote domains
  resp <-
    postConvWithRemoteUsers
      aliceUnqualified
      defNewProteusConv {newConvQualifiedUsers = [bobOwningDomain, chadOwningDomain, deeRemote]}
  let convId = (`Qualified` owningDomain) . decodeConvId $ resp

  WS.bracketR2 cannon bobUnqualified chadUnqualified $ \(wsBob, wsChad) -> do
    let message =
          [ (bobOwningDomain, bobClient, "text-for-bob"),
            (bobOwningDomain, bobClient2, "text-for-bob2"),
            (chadOwningDomain, chadClient, "text-for-chad"),
            (deeRemote, deeClient, "text-for-dee")
          ]

    let mock =
          ( "get-user-clients" ~>
              UserMap (Map.singleton (qUnqualified deeRemote) (Set.singleton (PubClient deeClient Nothing)))
          )
            <|> ( guardRPC "on-message-sent"
                    *> throw (MockErrorResponse HTTP.status503 "Down for maintenance.")
                )

    (resp2, _requests) <- postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

    let expectedFailedToSend =
          QualifiedUserClients . Map.fromList $
            [ ( remoteDomain,
                Map.fromList
                  [ (deeId, Set.singleton deeClient)
                  ]
              )
            ]
    pure resp2 !!! do
      const 201 === statusCode
      assertMismatchQualified expectedFailedToSend mempty mempty mempty

    liftIO $ do
      let encodedTextForBob = toBase64Text "text-for-bob"
          encodedTextForChad = toBase64Text "text-for-chad"
          encodedData = toBase64Text "data"
      WS.assertMatch_ t wsBob (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient bobClient encodedTextForBob)
      WS.assertMatch_ t wsChad (wsAssertOtr' encodedData convId aliceOwningDomain aliceClient chadClient encodedTextForChad)

postMessageQualifiedRemoteOwningBackendFailure :: TestM ()
postMessageQualifiedRemoteOwningBackendFailure = do
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  let aliceUnqualified = qUnqualified aliceLocal
  convIdUnqualified <- randomId
  let remoteDomain = Domain "far-away.example.com"
      convId = Qualified convIdUnqualified remoteDomain

  let mock =
        guardRPC "send-message"
          *> throw (MockErrorResponse HTTP.status503 "Down for maintenance.")

  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId [] "data" Message.MismatchReportAll mock

  pure resp2 !!! do
    const 503 === statusCode

postMessageQualifiedRemoteOwningBackendSuccess :: TestM ()
postMessageQualifiedRemoteOwningBackendSuccess = do
  (aliceLocal, aliceClient) <- randomUserWithClientQualified (head someLastPrekeys)
  (bobOwningDomain, bobClient) <- randomUserWithClientQualified (someLastPrekeys !! 1)
  deeId <- randomId
  deeClient <- liftIO $ generate arbitrary

  let aliceUnqualified = qUnqualified aliceLocal
  convIdUnqualified <- randomId
  let remoteDomain = Domain "far-away.example.com"
      convId = Qualified convIdUnqualified remoteDomain
      deeRemote = Qualified deeId remoteDomain

  now <- toUTCTimeMillis <$> liftIO getCurrentTime
  let redundant =
        QualifiedUserClients
          . Map.singleton remoteDomain
          . Map.singleton deeId
          . Set.singleton
          $ deeClient
      mss =
        Message.MessageSendingStatus
          { Message.mssTime = now,
            Message.mssMissingClients = mempty,
            Message.mssRedundantClients = redundant,
            Message.mssDeletedClients = mempty,
            Message.mssFailedToSend = mempty
          }
      message = [(bobOwningDomain, bobClient, "text-for-bob"), (deeRemote, deeClient, "text-for-dee")]
      mock = "send-message" ~> F.MessageSendResponse (Right mss)
  (resp2, _requests) <-
    postProteusMessageQualifiedWithMockFederator aliceUnqualified aliceClient convId message "data" Message.MismatchReportAll mock

  pure resp2 !!! do
    const 201 === statusCode
    assertMismatchQualified mempty mempty redundant mempty
