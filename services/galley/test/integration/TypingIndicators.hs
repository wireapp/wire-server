module TypingIndicators
  ( tests,
  )
where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens hiding ((#), (.=))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Domain
import Data.Id
import Data.List1
import Data.Qualified
import Data.Singletons
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Timeout
import Federator.MockServer
import Galley.Types.Conversations.Members
import Imports
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Federation.API.Galley
import qualified Wire.API.Federation.API.Galley as F

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Typing indicators"
    [ test s "send typing indicators" postTypingIndicators,
      test s "send typing indicators without domain" postTypingIndicatorsV2,
      test s "send typing indicators with invalid pyaload" postTypingIndicatorsHandlesNonsense,
      test s "POST /federation/on-typing-indicator-updated : Update typing indicator by remote user" updateTypingIndicatorFromRemoteUser,
      test s "POST /federation/on-typing-indicator-updated : Update typing indicator to remote user" updateTypingIndicatorToRemoteUser,
      test s "send typing indicator update from local to remote on remote conv" updateTypingIndicatorToRemoteUserRemoteConv
    ]

postTypingIndicatorsV2 :: TestM ()
postTypingIndicatorsV2 = do
  c <- view tsCannon
  g <- view tsUnversionedGalley

  alice <- randomUser
  bob <- randomUser

  aliceL <- qualifyLocal alice
  bobL <- qualifyLocal bob

  connectUsers alice (singleton bob)

  conv <- decodeConvId <$> postO2OConv alice bob Nothing
  lcnv <- qualifyLocal conv

  WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
    post
      ( g
          . paths ["v2", "conversations", toByteString' conv, "typing"]
          . zUser alice
          . zConn "conn"
          . zType "access"
          . json StartedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged aliceL) StartedTyping n

    post
      ( g
          . paths ["v2", "conversations", toByteString' conv, "typing"]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json StoppedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged bobL) StoppedTyping n

postTypingIndicators :: TestM ()
postTypingIndicators = do
  domain <- viewFederationDomain
  c <- view tsCannon
  g <- viewGalley

  alice <- randomUser
  bob <- randomUser

  aliceL <- qualifyLocal alice
  bobL <- qualifyLocal bob

  connectUsers alice (singleton bob)

  conv <- decodeConvId <$> postO2OConv alice bob Nothing
  lcnv <- qualifyLocal conv

  WS.bracketR2 c alice bob $ \(wsAlice, wsBob) -> do
    -- to alice from bob
    post
      ( g
          . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json StoppedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged bobL) StoppedTyping n

    -- to bob from alice
    post
      ( g
          . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
          . zUser alice
          . zConn "conn"
          . zType "access"
          . json StartedTyping
      )
      !!! const 200 === statusCode

    void . liftIO $
      WS.assertMatchN (5 # Second) [wsAlice, wsBob] $ \n ->
        wsAssertTyping (tUntagged lcnv) (tUntagged aliceL) StartedTyping n

postTypingIndicatorsHandlesNonsense :: TestM ()
postTypingIndicatorsHandlesNonsense = do
  domain <- viewFederationDomain
  g <- viewGalley

  alice <- randomUser
  bob <- randomUser

  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob Nothing

  post
    ( g
        . paths ["conversations", toByteString' domain, toByteString' conv, "typing"]
        . zUser bob
        . zConn "conn"
        . zType "access"
        . json (object ["status" .= ("dummy" :: T.Text)])
    )
    !!! const 400 === statusCode

updateTypingIndicatorToRemoteUserRemoteConv :: TestM ()
updateTypingIndicatorToRemoteUserRemoteConv = do
  c <- view tsCannon
  qalice <- randomQualifiedUser
  let alice = qUnqualified qalice

  -- create a remote conversation with alice
  let remoteDomain = Domain "bobland.example.com"
  qbob <- Qualified <$> randomId <*> pure remoteDomain
  qconv <- Qualified <$> randomId <*> pure remoteDomain
  connectWithRemoteUser alice qbob

  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        F.ConversationUpdate
          { cuTime = now,
            cuOrigUserId = qbob,
            cuConvId = qUnqualified qconv,
            cuAlreadyPresentUsers = [],
            cuAction =
              SomeConversationAction (sing @'ConversationJoinTag) (ConversationJoin (pure qalice) roleNameWireMember)
          }
  runFedClient @"on-conversation-updated" fedGalleyClient remoteDomain cu

  -- Fetch remote conversation
  let bobAsLocal =
        LocalMember
          (qUnqualified qbob)
          defMemberStatus
          Nothing
          roleNameWireAdmin
  let mockConversation =
        mkProteusConv
          (qUnqualified qconv)
          (qUnqualified qbob)
          roleNameWireMember
          [localMemberToOther remoteDomain bobAsLocal]
      remoteConversationResponse = GetConversationsResponse [mockConversation]
  void
    $ withTempMockFederator'
      (mockReply remoteConversationResponse)
    $ getConvQualified alice qconv
      <!! const 200 === statusCode

  WS.bracketR c alice $ \wsAlice -> do
    -- Started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = qUnqualified qbob,
                  tdurConvId = qUnqualified qconv
                }

        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qalice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconv qalice StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = qUnqualified qbob,
                  tdurConvId = qUnqualified qconv
                }

        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qalice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconv qalice StoppedTyping n

updateTypingIndicatorFromRemoteUser :: TestM ()
updateTypingIndicatorFromRemoteUser = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let qAlice = Qualified alice localDomain
      remoteDomain = Domain "far-away.example.com"
      qBob = Qualified bob remoteDomain

  connectWithRemoteUser alice qBob
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob]}
  let qconvId = Qualified convId localDomain

  c <- view tsCannon
  WS.bracketR c alice $ \wsAlice -> do
    -- Started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = bob,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qAlice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconvId qAlice StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from bob to alice
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = bob,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qAlice) tcReq

    -- backend A generates a notification for alice
    void $
      WS.awaitMatch (5 # Second) wsAlice $ \n -> do
        liftIO $ wsAssertTyping qconvId qAlice StoppedTyping n

updateTypingIndicatorToRemoteUser :: TestM ()
updateTypingIndicatorToRemoteUser = do
  localDomain <- viewFederationDomain
  [alice, bob] <- randomUsers 2
  let remoteDomain = Domain "far-away.example.com"
      qBob = Qualified bob remoteDomain

  connectWithRemoteUser alice qBob
  convId <-
    decodeConvId
      <$> postConvWithRemoteUsers
        alice
        defNewProteusConv {newConvQualifiedUsers = [qBob]}
  let qconvId = Qualified convId localDomain

  c <- view tsCannon
  WS.bracketR c bob $ \wsBob -> do
    -- started
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from alice to bob
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StartedTyping,
                  tdurUserId = alice,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qBob) tcReq

    -- backend A generates a notification for bob
    void $
      WS.awaitMatch (5 # Second) wsBob $ \n -> do
        liftIO $ wsAssertTyping qconvId qBob StartedTyping n

    -- stopped
    void $
      withTempMockFederator' (mockReply ()) $ do
        -- post typing indicator from alice to bob
        let tcReq =
              TypingDataUpdateRequest
                { tdurTypingStatus = StoppedTyping,
                  tdurUserId = alice,
                  tdurConvId = convId
                }

        fedGalleyClient <- view tsFedGalleyClient
        runFedClient @"on-typing-indicator-updated" fedGalleyClient (qDomain qBob) tcReq

    -- backend A generates a notification for bob
    void $
      WS.awaitMatch (5 # Second) wsBob $ \n -> do
        liftIO $ wsAssertTyping qconvId qBob StoppedTyping n
