{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Cells where

import API.Galley
import qualified API.GalleyInternal as I
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Retry
import qualified Data.Aeson as A
import Network.AMQP
import Network.AMQP.Extended
import SetupHelpers
import System.Timeout
import Testlib.Prelude
import Testlib.ResourcePool

testCellsEvent :: App ()
testCellsEvent = do
  (alice, tid, [bob, chaz, dean]) <- createTeam OwnDomain 4
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201

  bobId <- bob %. "qualified_id"
  chazId <- chaz %. "qualified_id"
  deanId <- dean %. "qualified_id"

  addMembers alice conv def {role = Just "wire_member", users = [bobId]} >>= assertSuccess

  I.setCellsState alice conv "pending" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [chazId]} >>= assertSuccess

  I.setCellsState alice conv "ready" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [deanId]} >>= assertSuccess

  runCodensity (connectToCellsQueue backendA) $ \q -> do
    event <- getMessage q %. "payload.0"
    event %. "type" `shouldMatch` "conversation.member-join"
    event %. "conversation" `shouldMatch` (conv %. "id")
    event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")
    users <- event %. "data.users" & asList
    assertOne users %. "qualified_id" `shouldMatch` deanId

testCellsFeatureCheck :: App ()
testCellsFeatureCheck = do
  (alice, tid, _) <- createTeam OwnDomain 1
  I.patchTeamFeatureConfig OwnDomain tid "cells" (object ["status" .= "disabled"]) >>= assertSuccess
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  bindResponse (I.setCellsState alice conv "ready") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "invalid-op"

testCellsTeamConversationCheck :: App ()
testCellsTeamConversationCheck = do
  alice <- randomUser OwnDomain def
  conv <- postConversation alice defProteus >>= getJSON 201
  bindResponse (I.setCellsState alice conv "ready") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "invalid-op"

testCellsIgnoredEvents :: App ()
testCellsIgnoredEvents = do
  (alice, tid, _) <- createTeam OwnDomain 1
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  I.setCellsState alice conv "ready" >>= assertSuccess
  void $ updateMessageTimer alice conv 1000 >>= getBody 200
  runCodensity (connectToCellsQueue backendA) assertNoMessage

--------------------------------------------------------------------------------
-- Utilities

data CellsQueue = CellsQueue
  { msgVar :: MVar Message,
    queueTimeout :: Int
  }

connectToCellsQueue :: BackendResource -> Codensity App CellsQueue
connectToCellsQueue resource = do
  queueName <- lift $ asks (.cellsEventQueue)

  env <- lift ask
  let createConnection :: IO Connection
      createConnection =
        recovering
          (limitRetries 5)
          skipAsyncExceptions
          ( const $ do
              connOpts <-
                mkConnectionOpts
                  (demoteOpts env.rabbitMQConfig)
                    { vHost = fromString resource.berVHost
                    }
              liftIO $ openConnection'' connOpts
          )
  conn <- Codensity $ \k -> do
    iok <- appToIOKleisli k
    liftIO $ bracket createConnection closeConnection iok
  chan <- Codensity $ \k -> do
    iok <- appToIOKleisli k
    liftIO $ bracket (openChannel conn) closeChannel iok

  msgVar <- liftIO newEmptyMVar
  let handler (m, e) = do
        putMVar msgVar m
        ackMsg chan e.envDeliveryTag False
  void $ Codensity $ \k -> do
    iok <- appToIOKleisli k
    liftIO
      $ bracket
        (consumeMsgs chan (fromString queueName) Ack handler)
        (cancelConsumer chan)
        iok
  pure CellsQueue {msgVar, queueTimeout = 1000000}

getMessageMaybe :: CellsQueue -> App (Maybe Value)
getMessageMaybe q = runMaybeT $ do
  m <- MaybeT . liftIO . timeout q.queueTimeout $ takeMVar q.msgVar
  either (lift . assertFailure) pure $ A.eitherDecode m.msgBody

getMessage :: CellsQueue -> App Value
getMessage q = getMessageMaybe q >>= assertJust "Cells queue timeout"

assertNoMessage :: CellsQueue -> App ()
assertNoMessage q =
  getMessageMaybe q >>= \case
    Nothing -> pure ()
    Just m -> do
      j <- prettyJSON m
      assertFailure $ "Expected no message, got:\n" <> j
