module GetConversation
  ( tests,
  )
where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens
import Data.Aeson
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.List1
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import Federator.MockServer
import Galley.API.Mapping
import Galley.Types.Conversations.Members
import Imports
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Federation.API.Galley

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Get conversation"
    [ test s "get conversation" getConvOk,
      test s "get qualified conversation" getConvQualifiedOk,
      test s "get empty conversations" getConvsOk,
      test s "get conversation ids with v2 API" testGetConvIdsV2,
      test s "get conversations by ids" getConvsOk2,
      test s "get conversations/:domain/:cnv - local" testGetQualifiedLocalConv,
      test s "get conversations/:domain/:cnv - local, not found" testGetQualifiedLocalConvNotFound,
      test s "get conversations/:domain/:cnv - local, not participating" testGetQualifiedLocalConvNotParticipating,
      test s "get conversations/:domain/:cnv - remote" testGetQualifiedRemoteConv,
      test s "get conversations/:domain/:cnv - remote, not found" testGetQualifiedRemoteConvNotFound,
      test s "get conversations/:domain/:cnv - remote, not found on remote" testGetQualifiedRemoteConvNotFoundOnRemote,
      test s "fail to get >1000 conversation ids" getConvIdsFailMaxSize,
      test s "fail to get >1000 conversation ids with v2 API" getConvIdsFailMaxSizeV2,
      test s "fail to get >500 conversations with v2 API" getConvsFailMaxSizeV2
    ]

getConvOk :: TestM ()
getConvOk = do
  alice <- randomUser
  bob <- randomUser
  chuck <- randomUser
  connectUsers alice (list1 bob [chuck])
  conv <- decodeConvId <$> postConv alice [bob, chuck] (Just "gossip") [] Nothing Nothing
  getConv alice conv !!! const 200 === statusCode
  getConv bob conv !!! const 200 === statusCode
  getConv chuck conv !!! const 200 === statusCode

getConvQualifiedOk :: TestM ()
getConvQualifiedOk = do
  alice <- randomUser
  bob <- randomQualifiedUser
  chuck <- randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob [chuck])
  conv <-
    decodeConvId
      <$> postConvQualified
        alice
        defNewProteusConv
          { newConvQualifiedUsers = [bob, chuck],
            newConvName = checked "gossip"
          }
  getConv alice conv !!! const 200 === statusCode
  getConv (qUnqualified bob) conv !!! const 200 === statusCode
  getConv (qUnqualified chuck) conv !!! const 200 === statusCode

getConvsOk :: TestM ()
getConvsOk = do
  usr <- randomUser
  convs <- getAllConvs usr
  liftIO $
    [selfConv usr, mlsSelfConvId usr]
      @?= map (qUnqualified . cnvQualifiedId) convs

getConvsOk2 :: TestM ()
getConvsOk2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  -- create & get one2one conv
  cnv1 <- responseJsonError =<< postO2OConv alice bob (Just "gossip1") <!! const 200 === statusCode
  do
    r <-
      responseJsonError
        =<< getConvs alice [cnvQualifiedId cnv1] <!! do
          const 200 === statusCode
    liftIO $
      [cnvQualifiedId cnv1] @=? map cnvQualifiedId (crFound r)
  -- create & get group conv
  carl <- randomUser
  connectUsers alice (singleton carl)
  cnv2 <-
    responseJsonError
      =<< postConv alice [bob, carl] (Just "gossip2") [] Nothing Nothing
        <!! const 201 === statusCode
  do
    r <-
      responseJsonError
        =<< getConvs alice [cnvQualifiedId cnv2] <!! do
          const 200 === statusCode
    liftIO $
      [cnvQualifiedId cnv2] @=? map cnvQualifiedId (crFound r)
  -- get both
  convs <- getAllConvs alice
  let c1 = find ((== cnvQualifiedId cnv1) . cnvQualifiedId) convs
  let c2 = find ((== cnvQualifiedId cnv2) . cnvQualifiedId) convs
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

getConvsFailMaxSizeV2 :: TestM ()
getConvsFailMaxSizeV2 = do
  usr <- randomUser
  g <- view tsUnversionedGalley
  get
    ( g
        . path "/v2/conversations"
        . zUser usr
        . zConn "conn"
        . queryItem "size" (toByteString' (501 :: Int32))
    )
    !!! const 400 === statusCode

testGetConvIdsV2 :: TestM ()
testGetConvIdsV2 = do
  [alice, bob] <- randomUsers 2
  connectUsers alice (singleton bob)
  void $ postO2OConv alice bob (Just "gossip")
  getConvIdsV2 alice Nothing Nothing !!! do
    const 200 === statusCode
    const 2 === length . decodeConvIdList
  getConvIdsV2 bob Nothing Nothing !!! do
    const 200 === statusCode
    const 2 === length . decodeConvIdList

getConvIdsFailMaxSizeV2 :: TestM ()
getConvIdsFailMaxSizeV2 = do
  usr <- randomUser
  getConvIdsV2 usr Nothing (Just 1001)
    !!! const 400 === statusCode

getConvIdsFailMaxSize :: TestM ()
getConvIdsFailMaxSize = do
  usr <- randomUser
  getConvPage usr Nothing (Just 1001)
    !!! const 400 === statusCode

testGetQualifiedRemoteConvNotFound :: TestM ()
testGetQualifiedRemoteConvNotFound = do
  aliceId <- randomUser
  let remoteDomain = Domain "far-away.example.com"
  remoteConvId <- (`Qualified` remoteDomain) <$> randomId
  -- No need to mock federator as we don't expect a call to be made
  getConvQualified aliceId remoteConvId !!! do
    const 404 === statusCode
    const (Just "no-conversation") === view (at "label") . responseJsonUnsafe @Object

testGetQualifiedRemoteConvNotFoundOnRemote :: TestM ()
testGetQualifiedRemoteConvNotFoundOnRemote = do
  aliceQ <- randomQualifiedUser
  let aliceId = qUnqualified aliceQ
  bobId <- randomId
  convId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteConvId = Qualified convId remoteDomain
      aliceAsOtherMember = OtherMember aliceQ Nothing roleNameWireAdmin

  registerRemoteConv remoteConvId bobId Nothing (Set.fromList [aliceAsOtherMember])

  void . withTempMockFederator' (mockReply (GetConversationsResponse [])) $ do
    getConvQualified aliceId remoteConvId !!! do
      const 404 === statusCode
      const (Just "no-conversation") === view (at "label") . responseJsonUnsafe @Object

testGetQualifiedLocalConv :: TestM ()
testGetQualifiedLocalConv = do
  alice <- randomUser
  convId <- decodeQualifiedConvId <$> postConv alice [] (Just "gossip") [] Nothing Nothing
  conv :: Conversation <- fmap responseJsonUnsafe $ getConvQualified alice convId <!! const 200 === statusCode
  liftIO $ do
    assertEqual "conversation id" convId (cnvQualifiedId conv)
    assertEqual "conversation name" (Just "gossip") (cnvName conv)

testGetQualifiedLocalConvNotFound :: TestM ()
testGetQualifiedLocalConvNotFound = do
  alice <- randomUser
  localDomain <- viewFederationDomain
  convId <- (`Qualified` localDomain) <$> randomId
  getConvQualified alice convId !!! do
    const 404 === statusCode
    const (Right (Just "no-conversation")) === fmap (view (at "label")) . responseJsonEither @Object

testGetQualifiedLocalConvNotParticipating :: TestM ()
testGetQualifiedLocalConvNotParticipating = do
  alice <- randomUser
  bob <- randomUser
  convId <- decodeQualifiedConvId <$> postConv bob [] (Just "gossip about alice") [] Nothing Nothing
  getConvQualified alice convId !!! do
    const 403 === statusCode
    const (Just "access-denied") === view (at "label") . responseJsonUnsafe @Object

testGetQualifiedRemoteConv :: TestM ()
testGetQualifiedRemoteConv = do
  aliceQ <- randomQualifiedUser
  let aliceId = qUnqualified aliceQ
  loc <- flip toLocalUnsafe () <$> viewFederationDomain
  bobId <- randomId
  convId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      bobQ = Qualified bobId remoteDomain
      remoteConvId = Qualified convId remoteDomain
      bobAsOtherMember = OtherMember bobQ Nothing roleNameWireAdmin
      aliceAsLocal =
        LocalMember aliceId defMemberStatus Nothing roleNameWireAdmin
      aliceAsOtherMember = localMemberToOther (qDomain aliceQ) aliceAsLocal
      aliceAsSelfMember = localMemberToSelf loc aliceAsLocal

  connectWithRemoteUser aliceId bobQ
  registerRemoteConv remoteConvId bobId Nothing (Set.fromList [aliceAsOtherMember])

  let mockConversation = mkProteusConv convId bobId roleNameWireAdmin [bobAsOtherMember]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
      expected =
        Conversation
          remoteConvId
          (rcnvMetadata mockConversation)
          (ConvMembers aliceAsSelfMember (rcmOthers (rcnvMembers mockConversation)))
          ProtocolProteus

  (respAll, _) <-
    withTempMockFederator'
      (mockReply remoteConversationResponse)
      (getConvQualified aliceId remoteConvId)

  conv <- responseJsonUnsafe <$> (pure respAll <!! const 200 === statusCode)
  liftIO $ do assertEqual "conversation metadata" expected conv
