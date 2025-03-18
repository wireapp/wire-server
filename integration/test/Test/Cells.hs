{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Cells where

import API.Galley
import qualified API.GalleyInternal as I
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Retry
import qualified Data.Aeson as A
import Data.IORef
import qualified Data.Map as Map
import Network.AMQP
import Network.AMQP.Extended
import Notifications
import SetupHelpers
import System.Timeout
import Testlib.Prelude
import Testlib.ResourcePool

testCellsEvent :: App ()
testCellsEvent = do
  (alice, tid, [bob, chaz, dean]) <- createTeam OwnDomain 4
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  q <- watchCellsEvents backendA

  bobId <- bob %. "qualified_id"
  chazId <- chaz %. "qualified_id"
  deanId <- dean %. "qualified_id"

  addMembers alice conv def {role = Just "wire_member", users = [bobId]} >>= assertSuccess

  I.setCellsState alice conv "pending" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [chazId]} >>= assertSuccess

  I.setCellsState alice conv "ready" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [deanId]} >>= assertSuccess

  event <- getMessage q (MessageFilter (isNotifConv conv)) %. "payload.0"
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
  q <- watchCellsEvents backendA
  void $ updateMessageTimer alice conv 1000 >>= getBody 200
  assertNoMessage q (MessageFilter (isNotifConv conv))

--------------------------------------------------------------------------------
-- Utilities

connectToCellsQueue :: BackendResource -> TChan Message -> Codensity App ()
connectToCellsQueue resource messages = do
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
  conn <-
    hoistCodensity
      $ Codensity
      $ E.bracket createConnection closeConnection

  chan <-
    hoistCodensity
      $ Codensity
      $ E.bracket (openChannel conn) closeChannel

  handler <- lift $ appToIOKleisli $ \(m, e) -> do
    liftIO $ atomically $ writeTChan messages m
    liftIO $ ackMsg chan e.envDeliveryTag False

  void
    . hoistCodensity
    $ Codensity
    $ E.bracket
      (consumeMsgs chan (fromString queueName) Ack handler)
      (cancelConsumer chan)

newtype MessageFilter = MessageFilter
  { applyMessageFilter :: Value -> App Bool
  }

instance Default MessageFilter where
  def = MessageFilter {applyMessageFilter = const (pure True)}

getNextMessage :: QueueConsumer -> MessageFilter -> App Value
getNextMessage q f = do
  m <- liftIO $ atomically $ readTChan q.chan
  v <- either assertFailure pure $ A.eitherDecode m.msgBody
  ok <- applyMessageFilter f v
  if ok
    then pure v
    else getNextMessage q f

getMessageMaybe :: QueueConsumer -> MessageFilter -> App (Maybe Value)
getMessageMaybe q f = do
  timeOutSeconds <- asks (.timeOutSeconds)
  next <- appToIO (getNextMessage q f)
  liftIO $ timeout (timeOutSeconds * 1000000) next

getMessage :: QueueConsumer -> MessageFilter -> App Value
getMessage q f = getMessageMaybe q f >>= assertJust "Cells queue timeout"

assertNoMessage :: QueueConsumer -> MessageFilter -> App ()
assertNoMessage q f =
  getMessageMaybe q f >>= \case
    Nothing -> pure ()
    Just m -> do
      j <- prettyJSON m
      assertFailure $ "Expected no message, got:\n" <> j

--------------------------------------------------------------------------------
-- Queue watcher

data QueueConsumer = QueueConsumer
  { chan :: TChan Message
  }

startQueueWatcher :: BackendResource -> App QueueWatcher
startQueueWatcher resource = do
  broadcast <- liftIO newBroadcastTChanIO
  readyVar <- liftIO $ newEmptyMVar
  doneVar <- liftIO $ newEmptyMVar

  startIO <- appToIO $ lowerCodensity $ do
    void $ connectToCellsQueue resource broadcast
    liftIO $ putMVar readyVar ()
    liftIO $ takeMVar doneVar

  void $ liftIO $ async startIO
  liftIO $ takeMVar readyVar

  pure QueueWatcher {doneVar, broadcast}

ensureWatcher :: BackendResource -> App QueueWatcher
ensureWatcher resource = do
  watchersLock <- asks (.cellsEventWatchersLock)
  watchersRef <- asks (.cellsEventWatchers)
  start <- appToIO (startQueueWatcher resource)

  liftIO
    $ E.bracket
      (putMVar watchersLock ())
      (\_ -> tryTakeMVar watchersLock)
    $ \_ -> do
      watchers <- liftIO $ readIORef watchersRef
      case Map.lookup resource watchers of
        Nothing -> do
          watcher <- start
          let watchers' = Map.insert resource watcher watchers
          writeIORef watchersRef watchers'
          pure watcher
        Just watcher -> pure watcher

watchCellsEvents :: BackendResource -> App QueueConsumer
watchCellsEvents resource = do
  watcher <- ensureWatcher resource
  chan <- liftIO $ atomically $ dupTChan watcher.broadcast
  pure QueueConsumer {chan}
