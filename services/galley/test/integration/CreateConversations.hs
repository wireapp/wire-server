module CreateConversations
  ( tests,
  )
where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens hiding ((#))
import Data.Domain
import Data.Id
import Data.List1
import qualified Data.List1 as List1
import Data.Qualified
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Timeout
import Federator.MockServer
import Imports
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import qualified UnliftIO.Async as Async
import Wire.API.Conversation
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Internal.Notification
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.API.Conversation.Role
import Network.Wai.Utilities (label)
import Wire.API.Connection
import Galley.Options
import Wire.API.Conversation.Protocol

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Create conversations"
    [ test s "create Proteus conversation" postProteusConvOk,
      test s "create conversation with remote users" postConvWithRemoteUsersOk,
      test s "fail to create conversation when not connected" postConvFailNotConnected,
      test s "fail to create conversation with qualified users when not connected" postConvQualifiedFailNotConnected,
      test s "M:N conversation creation with N - 1 invitees should be allowed" postConvLimitOk,
      test s "M:N conversation creation must have <N members" postConvFailNumMembers,
      test s "M:N conversation creation must have <N qualified members" postConvQualifiedFailNumMembers,
      test s "fail to create conversation when blocked" postConvFailBlocked,
      test s "fail to create conversation when blocked by qualified member" postConvQualifiedFailBlocked,
      test s "fail to create conversation with remote users when remote user is not connected" postConvQualifiedNoConnection,
      test s "fail to create team conversation with remote users when remote user is not connected" postTeamConvQualifiedNoConnection,
      test s "fail to create conversation with remote users when remote user's domain doesn't exist" postConvQualifiedNonExistentDomain,
      test s "fail to create conversation with remote users when federation not configured" postConvQualifiedFederationNotEnabled,
      test s "create self conversation" postSelfConvOk,
      test s "create 1:1 conversation" postO2OConvOk,
      test s "fail to create 1:1 conversation with yourself" postConvO2OFailWithSelf,
      test s "create connect conversation" postConnectConvOk,
      test s "create connect conversation with email" postConnectConvOk2,
      test s "create mutual connect conversation" postMutualConnectConvOk
    ]

postProteusConvOk :: TestM ()
postProteusConvOk = do
  c <- view tsCannon
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  (jane, qjane) <- randomUserTuple
  connectUsers alice (list1 bob [jane])
  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR3 c alice bob jane $ \(wsA, wsB, wsJ) -> do
    rsp <-
      postConv alice [bob, jane] (Just nameMaxSize) [] Nothing Nothing
        <!! const 201 === statusCode
    qcid <- assertConv rsp RegularConv alice qalice [qbob, qjane] (Just nameMaxSize) Nothing
    let cid = qUnqualified qcid
    cvs <- mapM (convView cid) [alice, bob, jane]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs qalice) (zip cvs [wsA, wsB, wsJ])
  where
    convView cnv usr = do
      r <- getConv usr cnv <!! const 200 === statusCode
      unVersioned @'V2 <$> responseJsonError r
    checkWs qalice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvQualifiedId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= qalice
      case evtData e of
        EdConversation c' -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

postConvWithRemoteUsersOk :: TestM ()
postConvWithRemoteUsersOk = do
  c <- view tsCannon
  (alice, qAlice) <- randomUserTuple
  (alex, qAlex) <- randomUserTuple
  (amy, qAmy) <- randomUserTuple
  connectUsers alice (list1 alex [amy])
  let cDomain = Domain "c.example.com"
      dDomain = Domain "d.example.com"
  qChad <- randomQualifiedId cDomain
  qCharlie <- randomQualifiedId cDomain
  qDee <- randomQualifiedId dDomain
  mapM_ (connectWithRemoteUser alice) [qChad, qCharlie, qDee]

  let nameMaxSize = T.replicate 256 "a"
  WS.bracketR3 c alice alex amy $ \(wsAlice, wsAlex, wsAmy) -> do
    (rsp, federatedRequests) <-
      withTempMockFederator' (mockReply ()) $
        postConvQualified alice defNewProteusConv {newConvName = checked nameMaxSize, newConvQualifiedUsers = [qAlex, qAmy, qChad, qCharlie, qDee]}
          <!! const 201 === statusCode
    qcid <- assertConv rsp RegularConv alice qAlice [qAlex, qAmy, qChad, qCharlie, qDee] (Just nameMaxSize) Nothing
    let cid = qUnqualified qcid
    cvs <- mapM (convView cid) [alice, alex, amy]
    liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs qAlice) (zip cvs [wsAlice, wsAlex, wsAmy])

    cFedReq <- assertOne $ filter (\r -> frTargetDomain r == cDomain) federatedRequests
    cFedReqBody <- assertRight $ parseFedRequest cFedReq

    dFedReq <- assertOne $ filter (\r -> frTargetDomain r == dDomain) federatedRequests
    dFedReqBody <- assertRight $ parseFedRequest dFedReq

    liftIO $ do
      length federatedRequests @?= 2

      F.ccOrigUserId cFedReqBody @?= alice
      F.ccCnvId cFedReqBody @?= cid
      F.ccCnvType cFedReqBody @?= RegularConv
      F.ccCnvAccess cFedReqBody @?= [InviteAccess]
      F.ccCnvAccessRoles cFedReqBody @?= Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole, ServiceAccessRole]
      F.ccCnvName cFedReqBody @?= Just nameMaxSize
      F.ccNonCreatorMembers cFedReqBody @?= Set.fromList (toOtherMember <$> [qAlex, qAmy, qChad, qCharlie, qDee])
      F.ccMessageTimer cFedReqBody @?= Nothing
      F.ccReceiptMode cFedReqBody @?= Nothing

      dFedReqBody @?= cFedReqBody
  where
    toOtherMember qid = OtherMember qid Nothing roleNameWireAdmin
    convView cnv usr = do
      r <- getConv usr cnv <!! const 200 === statusCode
      unVersioned @'V2 <$> responseJsonError r
    checkWs qalice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
      ntfTransient n @?= False
      let e = List1.head (WS.unpackPayload n)
      evtConv e @?= cnvQualifiedId cnv
      evtType e @?= ConvCreate
      evtFrom e @?= qalice
      case evtData e of
        EdConversation c' -> assertConvEquals cnv c'
        _ -> assertFailure "Unexpected event data"

postConvFailNotConnected :: TestM ()
postConvFailNotConnected = do
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  postConv alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedFailNotConnected :: TestM ()
postConvQualifiedFailNotConnected = do
  alice <- randomUser
  bob <- randomQualifiedUser
  jane <- randomQualifiedUser
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvLimitOk :: TestM ()
postConvLimitOk = do
  n <- fromIntegral . pred <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomUser
  connectUsers alice (list1 bob others)
  postConv alice (bob : others) Nothing [] Nothing Nothing !!! do
    const 201 === statusCode

postConvFailNumMembers :: TestM ()
postConvFailNumMembers = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomUser
  connectUsers alice (list1 bob others)
  postConv alice (bob : others) Nothing [] Nothing Nothing !!! do
    const 400 === statusCode
    const (Just "client-error") === fmap label . responseJsonUnsafe

postConvQualifiedFailNumMembers :: TestM ()
postConvQualifiedFailNumMembers = do
  n <- fromIntegral <$> view tsMaxConvSize
  alice <- randomUser
  bob : others <- replicateM n randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob others)
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = bob : others} !!! do
    const 400 === statusCode
    const (Just "client-error") === fmap label . responseJsonUnsafe

-- | If somebody has blocked a user, that user shouldn't be able to create a
-- group conversation which includes them.
postConvFailBlocked :: TestM ()
postConvFailBlocked = do
  alice <- randomUser
  bob <- randomUser
  jane <- randomUser
  connectUsers alice (list1 bob [jane])
  putConnection jane alice Blocked
    !!! const 200 === statusCode
  postConv alice [bob, jane] Nothing [] Nothing Nothing !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

--- | If somebody has blocked a user, that user shouldn't be able to create a
-- group conversation which includes them.
postConvQualifiedFailBlocked :: TestM ()
postConvQualifiedFailBlocked = do
  alice <- randomUser
  bob <- randomQualifiedUser
  jane <- randomQualifiedUser
  connectLocalQualifiedUsers alice (list1 bob [jane])
  putConnectionQualified jane alice Blocked
    !!! const 200 === statusCode
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob, jane]} !!! do
    const 403 === statusCode
    const (Just "not-connected") === fmap label . responseJsonUnsafe

postConvQualifiedNoConnection :: TestM ()
postConvQualifiedNoConnection = do
  alice <- randomUser
  bob <- flip Qualified (Domain "far-away.example.com") <$> randomId
  postConvQualified alice defNewProteusConv {newConvQualifiedUsers = [bob]}
    !!! const 403 === statusCode

postTeamConvQualifiedNoConnection :: TestM ()
postTeamConvQualifiedNoConnection = do
  (tid, alice, _) <- createBindingTeamWithQualifiedMembers 1
  bob <- randomQualifiedId (Domain "bob.example.com")
  charlie <- randomQualifiedUser
  postConvQualified
    (qUnqualified alice)
    defNewProteusConv
      { newConvQualifiedUsers = [bob],
        newConvTeam = Just (ConvTeamInfo tid)
      }
    !!! const 403 === statusCode
  postConvQualified
    (qUnqualified alice)
    defNewProteusConv
      { newConvQualifiedUsers = [charlie],
        newConvTeam = Just (ConvTeamInfo tid)
      }
    !!! const 403 === statusCode

postConvQualifiedNonExistentDomain :: TestM ()
postConvQualifiedNonExistentDomain = do
  alice <- randomUser
  bob <- flip Qualified (Domain "non-existent.example.com") <$> randomId
  connectWithRemoteUser alice bob
  postConvQualified
    alice
    defNewProteusConv {newConvQualifiedUsers = [bob]}
    !!! do
      const 422 === statusCode

postConvQualifiedFederationNotEnabled :: TestM ()
postConvQualifiedFederationNotEnabled = do
  alice <- randomUser
  bob <- flip Qualified (Domain "some-remote-backend.example.com") <$> randomId
  connectWithRemoteUser alice bob
  let federatorNotConfigured = optFederator .~ Nothing
  withSettingsOverrides federatorNotConfigured $ do
    g <- viewGalley
    postConvHelper g alice [bob] !!! do
      const 400 === statusCode
      const (Just "federation-not-enabled") === fmap label . responseJsonUnsafe

-- like postConvQualified
-- FUTUREWORK: figure out how to use functions in the TestM monad inside withSettingsOverrides and remove this duplication
postConvHelper :: (MonadIO m, MonadHttp m) => (Request -> Request) -> UserId -> [Qualified UserId] -> m ResponseLBS
postConvHelper g zusr newUsers = do
  let conv = NewConv [] newUsers (checked "gossip") (Set.fromList []) Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag Nothing
  post $ g . path "/conversations" . zUser zusr . zConn "conn" . zType "access" . json conv

postSelfConvOk :: TestM ()
postSelfConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  m <- postSelfConv alice <!! const 200 === statusCode
  n <- postSelfConv alice <!! const 200 === statusCode
  mId <- assertConv m SelfConv alice qalice [] Nothing Nothing
  nId <- assertConv n SelfConv alice qalice [] Nothing Nothing
  liftIO $ mId @=? nId

postO2OConvOk :: TestM ()
postO2OConvOk = do
  (alice, qalice) <- randomUserTuple
  (bob, qbob) <- randomUserTuple
  connectUsers alice (singleton bob)
  a <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  c <- postO2OConv alice bob Nothing <!! const 200 === statusCode
  aId <- assertConv a One2OneConv alice qalice [qbob] Nothing Nothing
  cId <- assertConv c One2OneConv alice qalice [qbob] Nothing Nothing
  liftIO $ aId @=? cId

postConvO2OFailWithSelf :: TestM ()
postConvO2OFailWithSelf = do
  g <- viewGalley
  alice <- randomUser
  let inv = NewConv [alice] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag Nothing
  post (g . path "/conversations/one2one" . zUser alice . zConn "conn" . zType "access" . json inv) !!! do
    const 403 === statusCode
    const (Just "invalid-op") === fmap label . responseJsonUnsafe

postConnectConvOk :: TestM ()
postConnectConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  bob <- randomUser
  m <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 201 === statusCode
  n <-
    postConnectConv alice bob "Alice" "connect with me!" Nothing
      <!! const 200 === statusCode
  mId <- assertConv m ConnectConv alice qalice [] (Just "Alice") Nothing
  nId <- assertConv n ConnectConv alice qalice [] (Just "Alice") Nothing
  liftIO $ mId @=? nId

postConnectConvOk2 :: TestM ()
postConnectConvOk2 = do
  alice <- randomUser
  bob <- randomUser
  m <- decodeConvId <$> req alice bob
  n <- decodeConvId <$> req alice bob
  liftIO $ m @=? n
  where
    req alice bob =
      postConnectConv alice bob "Alice" "connect with me!" (Just "me@me.com")

postMutualConnectConvOk :: TestM ()
postMutualConnectConvOk = do
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  ac <-
    postConnectConv alice bob "A" "a" Nothing
      <!! const 201 === statusCode
  acId <- assertConv ac ConnectConv alice qalice [] (Just "A") Nothing
  bc <-
    postConnectConv bob alice "B" "b" Nothing
      <!! const 200 === statusCode
  -- The connect conversation was simply accepted, thus the
  -- conversation name and message sent in Bob's request ignored.
  bcId <- assertConv bc One2OneConv alice qbob [qalice] (Just "A") Nothing
  liftIO $ acId @=? bcId
