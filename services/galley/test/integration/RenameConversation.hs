{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module RenameConversation
  ( tests,
  )
where

import API.Util
import Bilge hiding (head, timeout)
import Bilge.Assert
import Control.Lens hiding ((#))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Domain
import Data.Either.Extra
import Data.Id
import Data.List1 hiding (head)
import qualified Data.List1 as List1
import Data.Qualified
import Data.Singletons
import Data.Timeout
import Federator.MockServer
import Galley.API.Action
import Imports
import Network.Wai.Utilities.Error
import Test.Tasty
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import qualified Wire.API.Federation.API.Galley as F
import Wire.API.Internal.Notification
import Wire.API.MakesFederatedCall

tests :: IO TestSetup -> TestTree
tests s =
  testGroup
    "Rename conversation"
    [ test s "rename conversation (deprecated endpoint)" putConvDeprecatedRenameOk,
      test s "rename conversation" putConvRenameOk,
      test s "rename qualified conversation" putQualifiedConvRenameOk,
      test s "rename qualified conversation with remote members" putQualifiedConvRenameWithRemotesOk,
      test s "rename qualified conversation failure" putQualifiedConvRenameFailure
    ]

putConvDeprecatedRenameOk :: TestM ()
putConvDeprecatedRenameOk = do
  c <- view tsCannon
  g <- viewGalley
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    -- This endpoint is deprecated but clients still use it
    put
      ( g
          . paths ["conversations", toByteString' conv]
          . zUser bob
          . zConn "conn"
          . zType "access"
          . json (ConversationRename "gossip++")
      )
      !!! const 200
        === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putConvRenameOk :: TestM ()
putConvRenameOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putConversationName bob conv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putQualifiedConvRenameFailure :: TestM ()
putQualifiedConvRenameFailure = do
  conv <- randomId
  qbob <- randomQualifiedUser
  let qconv = Qualified conv (qDomain qbob)
  putQualifiedConversationName (qUnqualified qbob) qconv "gossip"
    !!! do
      const 404 === statusCode
      const (Just "no-conversation") === fmap label . responseJsonUnsafe

putQualifiedConvRenameOk :: TestM ()
putQualifiedConvRenameOk = do
  c <- view tsCannon
  alice <- randomUser
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob
  connectUsers alice (singleton bob)
  conv <- decodeConvId <$> postO2OConv alice bob (Just "gossip")
  let qconv = Qualified conv (qDomain qbob)
  WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
    void $ putQualifiedConversationName bob qconv "gossip++" !!! const 200 === statusCode
    void . liftIO . WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")

putQualifiedConvRenameWithRemotesOk :: TestM ()
putQualifiedConvRenameWithRemotesOk = do
  c <- view tsCannon
  let remoteDomain = Domain "alice.example.com"
  qalice <- Qualified <$> randomId <*> pure remoteDomain
  qbob <- randomQualifiedUser
  let bob = qUnqualified qbob

  connectWithRemoteUser bob qalice

  resp <-
    postConvWithRemoteUsers
      bob
      defNewProteusConv {newConvQualifiedUsers = [qalice]}
      <!! const 201 === statusCode
  let qconv = decodeQualifiedConvId resp

  WS.bracketR c bob $ \wsB -> do
    (_, requests) <-
      withTempMockFederator' (mockReply ()) $
        putQualifiedConversationName bob qconv "gossip++" !!! const 200 === statusCode

    req <- assertOne requests
    liftIO $ do
      frTargetDomain req @?= remoteDomain
      frComponent req @?= Galley
      frRPC req @?= "on-conversation-updated"
      Right cu <- pure . eitherDecode . frBody $ req
      F.cuConvId cu @?= qUnqualified qconv
      F.cuAction cu @?= SomeConversationAction (sing @'ConversationRenameTag) (ConversationRename "gossip++")

    void . liftIO . WS.assertMatch (5 # Second) wsB $ \n -> do
      let e = List1.head (WS.unpackPayload n)
      ntfTransient n @?= False
      evtConv e @?= qconv
      evtType e @?= ConvRename
      evtFrom e @?= qbob
      evtData e @?= EdConvRename (ConversationRename "gossip++")
