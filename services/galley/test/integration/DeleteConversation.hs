{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module DeleteConversation
  ( tests,
  )
where

import API.Util
import Bilge hiding (head)
import Bilge.Assert
import Data.Aeson
import Data.Domain
import Data.Either.Extra
import Data.Id
import Data.List.NonEmpty (NonEmpty (..))
import Data.List1 hiding (head)
import Data.Qualified
import Data.Range
import Data.Singletons
import Federator.MockServer
import Galley.API.Action
import Imports
import Network.Wai.Utilities
import Test.Tasty
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.MakesFederatedCall
import Wire.API.User

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Delete conversation"
    [ test s "delete conversation with remote members" testDeleteTeamConversationWithRemoteMembers,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - fail, self conv" deleteMembersQualifiedFailSelf,
      test s "delete conversations/:domain:/cnv/members/:domain/:usr - fail, 1:1 conv" deleteMembersQualifiedFailO2O,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with all locals" deleteMembersConvLocalQualifiedOk,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with locals and remote, delete local" deleteLocalMemberConvLocalQualifiedOk,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - local conv with locals and remote, delete remote" deleteRemoteMemberConvLocalQualifiedOk,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv" leaveRemoteConvQualifiedOk,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, non-existent" leaveNonExistentRemoteConv,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, leave conv, denied" leaveRemoteConvDenied,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove local user, fail" removeLocalMemberConvQualifiedFail,
      test s "delete conversations/:domain/:cnv/members/:domain/:usr - remote conv, remove remote user, fail" removeRemoteMemberConvQualifiedFail
    ]

testDeleteTeamConversationWithRemoteMembers :: TestM ()
testDeleteTeamConversationWithRemoteMembers = do
  (alice, tid) <- createBindingTeam
  localDomain <- viewFederationDomain
  let qalice = Qualified alice localDomain

  bobId <- randomId
  let remoteDomain = Domain "far-away.example.com"
      remoteBob = Qualified bobId remoteDomain

  convId <- decodeConvId <$> postTeamConv tid alice [] (Just "remote gossip") [] Nothing Nothing
  let _qconvId = Qualified convId localDomain

  connectWithRemoteUser alice remoteBob

  let mock =
        ("on-new-remote-conversation" ~> EmptyResponse)
          <|> ("on-conversation-updated" ~> ())
  (_, received) <- withTempMockFederator' mock $ do
    postQualifiedMembers alice (remoteBob :| []) convId
      !!! const 200 === statusCode

    deleteTeamConv tid convId alice
      !!! const 200 === statusCode

  liftIO $ do
    let convUpdates = mapMaybe (eitherToMaybe . parseFedRequest) received
    convUpdate <- case filter ((== SomeConversationAction (sing @'ConversationDeleteTag) ()) . cuAction) convUpdates of
      [] -> assertFailure "No ConversationUpdate requests received"
      [convDelete] -> pure convDelete
      _ -> assertFailure "Multiple ConversationUpdate requests received"
    cuAlreadyPresentUsers convUpdate @?= [bobId]
    cuOrigUserId convUpdate @?= qalice

-- Creates a conversation with three users from the same domain. Then it uses a
-- qualified endpoint for deleting a conversation member:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteMembersConvLocalQualifiedOk :: TestM ()
deleteMembersConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob, eve] <- randomUsers 3
  let [qAlice, qBob, qEve] = (`Qualified` localDomain) <$> [alice, bob, eve]
  connectUsers alice (list1 bob [eve])
  conv <-
    decodeConvId
      <$> postConvQualified
        alice
        defNewProteusConv
          { newConvQualifiedUsers = [qBob, qEve],
            newConvName = checked "federated gossip"
          }
  let qconv = Qualified conv localDomain
  deleteMemberQualified bob qBob qconv !!! const 200 === statusCode
  deleteMemberQualified bob qBob qconv !!! const 404 === statusCode
  -- if the conversation still exists, don't respond with 404, but with 403.
  getConv bob conv !!! const 403 === statusCode
  deleteMemberQualified alice qEve qconv !!! const 200 === statusCode
  deleteMemberQualified alice qEve qconv !!! const 204 === statusCode
  deleteMemberQualified alice qAlice qconv !!! const 200 === statusCode
  deleteMemberQualified alice qAlice qconv !!! const 404 === statusCode

-- Creates a conversation with three users. Alice and Bob are on the local
-- domain, while Eve is on a remote domain. It uses a qualified endpoint for
-- removing Bob from the conversation:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteLocalMemberConvLocalQualifiedOk :: TestM ()
deleteLocalMemberConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  eve <- randomId
  let [qAlice, qBob] = (`Qualified` localDomain) <$> [alice, bob]
      remoteDomain = Domain "far-away.example.com"
      qEve = Qualified eve remoteDomain

  connectUsers alice (singleton bob)
  connectWithRemoteUser alice qEve
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob, qEve]}
  let qconvId = Qualified convId localDomain

  let mockReturnEve =
        mockedFederatedBrigResponse [(qEve, "Eve")]
          <|> mockReply ()
  (respDel, fedRequests) <-
    withTempMockFederator' mockReturnEve $
      deleteMemberQualified alice qBob qconvId
  let [galleyFederatedRequest] = fedRequestsForDomain remoteDomain Galley fedRequests
  assertRemoveUpdate galleyFederatedRequest qconvId qAlice [qUnqualified qEve] qBob

  liftIO $ do
    statusCode respDel @?= 200
    case responseJsonEither respDel of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconvId qAlice [qBob] e

  -- Now that Bob is gone, try removing him once again
  deleteMemberQualified alice qBob qconvId !!! do
    const 204 === statusCode
    const Nothing === responseBody

-- Creates a conversation with five users. Alice and Bob are on the local
-- domain. Chad and Dee are on far-away-1.example.com. Eve is on
-- far-away-2.example.com. It uses a qualified endpoint to remove Chad from the
-- conversation:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
deleteRemoteMemberConvLocalQualifiedOk :: TestM ()
deleteRemoteMemberConvLocalQualifiedOk = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let [qAlice, qBob] = (`Qualified` localDomain) <$> [alice, bob]
      remoteDomain1 = Domain "far-away-1.example.com"
      remoteDomain2 = Domain "far-away-2.example.com"
  qChad <- (`Qualified` remoteDomain1) <$> randomId
  qDee <- (`Qualified` remoteDomain1) <$> randomId
  qEve <- (`Qualified` remoteDomain2) <$> randomId
  connectUsers alice (singleton bob)
  mapM_ (connectWithRemoteUser alice) [qChad, qDee, qEve]

  let mockedResponse = do
        guardRPC "get-users-by-ids"
        d <- frTargetDomain <$> getRequest
        asum
          [ guard (d == remoteDomain1)
              *> mockReply [mkProfile qChad (Name "Chad"), mkProfile qDee (Name "Dee")],
            guard (d == remoteDomain2)
              *> mockReply [mkProfile qEve (Name "Eve")]
          ]
  (convId, _) <-
    withTempMockFederator' (mockedResponse <|> mockReply ()) $
      fmap decodeConvId $
        postConvQualified
          alice
          defNewProteusConv {newConvQualifiedUsers = [qBob, qChad, qDee, qEve]}
          <!! const 201 === statusCode
  let qconvId = Qualified convId localDomain

  (respDel, federatedRequests) <-
    withTempMockFederator' (mockedResponse <|> mockReply ()) $
      deleteMemberQualified alice qChad qconvId
  liftIO $ do
    statusCode respDel @?= 200
    case responseJsonEither respDel of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconvId qAlice [qChad] e

  remote1GalleyFederatedRequest <-
    assertOne (filter ((== "on-conversation-updated") . frRPC) (fedRequestsForDomain remoteDomain1 Galley federatedRequests))
  remote2GalleyFederatedRequest <-
    assertOne (filter ((== "on-conversation-updated") . frRPC) (fedRequestsForDomain remoteDomain2 Galley federatedRequests))
  assertRemoveUpdate remote1GalleyFederatedRequest qconvId qAlice [qUnqualified qChad, qUnqualified qDee] qChad
  assertRemoveUpdate remote2GalleyFederatedRequest qconvId qAlice [qUnqualified qEve] qChad

  -- Now that Chad is gone, try removing him once again
  deleteMemberQualified alice qChad qconvId !!! do
    const 204 === statusCode
    const Nothing === responseBody

-- Alice, a local user, leaves a remote conversation. Bob's domain is the same
-- as that of the conversation. The test uses the following endpoint:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
leaveRemoteConvQualifiedOk :: TestM ()
leaveRemoteConvQualifiedOk = do
  localDomain <- viewFederationDomain
  alice <- randomUser
  let qAlice = Qualified alice localDomain
  conv <- randomId
  bob <- randomId
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain
      qBob = Qualified bob remoteDomain
  let mockedFederatedGalleyResponse = do
        guardComponent Galley
        mockReply (F.LeaveConversationResponse (Right ()))
      mockResponses =
        mockedFederatedBrigResponse [(qBob, "Bob")]
          <|> mockedFederatedGalleyResponse

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      deleteMemberQualified alice qAlice qconv
  let leaveRequest =
        fromJust . decode . frBody . head $
          fedRequests
  liftIO $ do
    statusCode resp @?= 200
    case responseJsonEither resp of
      Left err -> assertFailure err
      Right e -> assertLeaveEvent qconv qAlice [qAlice] e
    F.lcConvId leaveRequest @?= conv
    F.lcLeaver leaveRequest @?= alice

-- Alice tries to leave a non-existent remote conversation
leaveNonExistentRemoteConv :: TestM ()
leaveNonExistentRemoteConv = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          F.LeaveConversationResponse (Left F.RemoveFromConversationErrorNotFound)

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 404 === statusCode
  let leaveRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "no-conversation"
    F.lcConvId leaveRequest @?= qUnqualified conv
    F.lcLeaver leaveRequest @?= qUnqualified alice

-- Alice tries to leave a conversation of the wrong type
leaveRemoteConvDenied :: TestM ()
leaveRemoteConvDenied = do
  alice <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
  conv <- randomQualifiedId remoteDomain

  let mockResponses = do
        guardComponent Galley
        mockReply $
          F.LeaveConversationResponse
            ( Left F.RemoveFromConversationErrorRemovalNotAllowed
            )

  (resp, fedRequests) <-
    withTempMockFederator' mockResponses $
      responseJsonError
        =<< deleteMemberQualified (qUnqualified alice) alice conv
          <!! const 403 === statusCode
  let leaveRequest =
        fromJust . decode . frBody . Imports.head $
          fedRequests
  liftIO $ do
    fmap label resp @?= Just "action-denied"
    F.lcConvId leaveRequest @?= qUnqualified conv
    F.lcLeaver leaveRequest @?= qUnqualified alice

-- Alice, a user remote to the conversation, tries to remove someone on her own
-- backend other than herself via:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
removeLocalMemberConvQualifiedFail :: TestM ()
removeLocalMemberConvQualifiedFail = do
  alice <- randomUser
  conv <- randomId
  qBob <- randomQualifiedUser
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain

  deleteMemberQualified alice qBob qconv !!! do
    const 403 === statusCode
    const (Just "action-denied") === fmap label . responseJsonUnsafe

-- Alice, a user remote to the conversation, tries to remove someone on a remote
-- backend via:
--
-- DELETE /conversations/:domain/:cnv/members/:domain/:usr
removeRemoteMemberConvQualifiedFail :: TestM ()
removeRemoteMemberConvQualifiedFail = do
  alice <- randomUser
  conv <- randomId
  bob <- randomId
  let remoteDomain = Domain "faraway.example.com"
      qconv = Qualified conv remoteDomain
      qBob = Qualified bob remoteDomain

  deleteMemberQualified alice qBob qconv !!! do
    const 403 === statusCode
    const (Just "action-denied") === fmap label . responseJsonUnsafe

deleteMembersQualifiedFailSelf :: TestM ()
deleteMembersQualifiedFailSelf = do
  (alice, qalice) <- randomUserTuple
  self <- decodeConvId <$> postSelfConv alice
  qself <- Qualified self <$> viewFederationDomain
  deleteMemberQualified alice qalice qself !!! const 403 === statusCode

deleteMembersQualifiedFailO2O :: TestM ()
deleteMembersQualifiedFailO2O = do
  alice <- randomUser
  (bob, qbob) <- randomUserTuple
  connectUsers alice (singleton bob)
  o2o <- decodeConvId <$> postO2OConv alice bob (Just "foo")
  qo2o <- Qualified o2o <$> viewFederationDomain
  deleteMemberQualified alice qbob qo2o !!! const 403 === statusCode
