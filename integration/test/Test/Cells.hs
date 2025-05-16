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

testCellsEvent :: (HasCallStack) => App ()
testCellsEvent = do
  (alice, tid, [bob, chaz, dean, eve]) <- createTeam OwnDomain 5
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  q <- watchCellsEvents (convEvents conv)

  bobId <- bob %. "qualified_id"
  chazId <- chaz %. "qualified_id"
  deanId <- dean %. "qualified_id"
  eveId <- eve %. "qualified_id"

  addMembers alice conv def {role = Just "wire_member", users = [bobId]} >>= assertSuccess

  I.setCellsState alice conv "pending" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [chazId]} >>= assertSuccess

  do
    event <- getMessage q %. "payload.0"
    event %. "type" `shouldMatch` "conversation.member-join"
    event %. "conversation" `shouldMatch` (conv %. "qualified_id" & objId)
    event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")
    users <- event %. "data.users" & asList
    assertOne users %. "qualified_id" `shouldMatch` chazId

  I.setCellsState alice conv "ready" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [deanId]} >>= assertSuccess

  do
    event <- getMessage q %. "payload.0"
    event %. "type" `shouldMatch` "conversation.member-join"
    event %. "conversation" `shouldMatch` (conv %. "qualified_id" & objId)
    event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")
    users <- event %. "data.users" & asList
    assertOne users %. "qualified_id" `shouldMatch` deanId

  I.setCellsState alice conv "disabled" >>= assertSuccess
  addMembers alice conv def {role = Just "wire_member", users = [eveId]} >>= assertSuccess

  assertNoMessage q

testCellsCreationEvent :: (HasCallStack) => App ()
testCellsCreationEvent = do
  -- start watcher before creating conversation
  q0 <- watchCellsEvents def
  (alice, tid, _) <- createTeam OwnDomain 1
  conv <- postConversation alice defProteus {team = Just tid, cells = True} >>= getJSON 201

  let q = q0 {filter = isNotifConv conv} :: QueueConsumer

  event <- getMessage q %. "payload.0"
  event %. "type" `shouldMatch` "conversation.create"
  event %. "qualified_conversation.id" `shouldMatch` (conv %. "qualified_id.id")
  event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")

  assertNoMessage q

testCellsCreationEventIsSentOnlyOnce :: (HasCallStack) => App ()
testCellsCreationEventIsSentOnlyOnce = do
  -- start watcher before creating conversation
  q0 <- watchCellsEvents def
  (alice, tid, members) <- createTeam OwnDomain 2
  conv <- postConversation alice defProteus {team = Just tid, cells = True, qualifiedUsers = members} >>= getJSON 201

  let q = q0 {filter = isNotifConv conv} :: QueueConsumer

  event <- getMessage q %. "payload.0"
  event %. "type" `shouldMatch` "conversation.create"
  event %. "qualified_conversation.id" `shouldMatch` (conv %. "qualified_id.id")
  event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")

  assertNoMessage q

testCellsFeatureCheck :: (HasCallStack) => App ()
testCellsFeatureCheck = do
  (alice, tid, _) <- createTeam OwnDomain 1
  I.patchTeamFeatureConfig OwnDomain tid "cells" (object ["status" .= "disabled"]) >>= assertSuccess
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  bindResponse (I.setCellsState alice conv "ready") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "invalid-op"

testCellsTeamConversationCheck :: (HasCallStack) => App ()
testCellsTeamConversationCheck = do
  alice <- randomUser OwnDomain def
  conv <- postConversation alice defProteus >>= getJSON 201
  bindResponse (I.setCellsState alice conv "ready") $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "invalid-op"

testCellsIgnoredEvents :: (HasCallStack) => App ()
testCellsIgnoredEvents = do
  (alice, tid, _) <- createTeam OwnDomain 1
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  I.setCellsState alice conv "ready" >>= assertSuccess
  q <- watchCellsEvents (convEvents conv)
  void $ updateMessageTimer alice conv 1000 >>= getBody 200
  assertNoMessage q

--------------------------------------------------------------------------------
-- Utilities

connectToCellsQueue :: ServiceMap -> TChan Message -> Codensity App ()
connectToCellsQueue sm messages = do
  queueName <- lift $ asks (.cellsEventQueue)

  env <- lift ask
  let opts = (demoteOpts env.rabbitMQConfig :: AmqpEndpoint) {vHost = sm.rabbitMqVHost}
  let createConnection :: IO Connection
      createConnection =
        recovering
          (limitRetries 5)
          skipAsyncExceptions
          ( const $ do
              connOpts <-
                mkConnectionOpts opts
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

getNextMessage :: QueueConsumer -> App Value
getNextMessage q = do
  m <- liftIO $ atomically $ readTChan q.chan
  v <- either assertFailure pure $ A.eitherDecode m.msgBody
  ok <- q.filter v
  if ok
    then pure v
    else getNextMessage q

getMessageMaybe :: QueueConsumer -> App (Maybe Value)
getMessageMaybe q = do
  timeOutSeconds <- asks (.timeOutSeconds)
  next <- appToIO (getNextMessage q)
  liftIO $ timeout (timeOutSeconds * 1000000) next

getMessage :: QueueConsumer -> App Value
getMessage q = getMessageMaybe q >>= assertJust "Cells queue timeout"

assertNoMessage :: QueueConsumer -> App ()
assertNoMessage f =
  getMessageMaybe f >>= \case
    Nothing -> pure ()
    Just m -> do
      j <- prettyJSON m
      assertFailure $ "Expected no message, got:\n" <> j

--------------------------------------------------------------------------------
-- Queue watcher

data QueueConsumer = QueueConsumer
  { chan :: TChan Message,
    filter :: Value -> App Bool
  }

startQueueWatcher :: ServiceMap -> App QueueWatcher
startQueueWatcher sm = do
  broadcast <- liftIO newBroadcastTChanIO
  readyVar <- liftIO $ newEmptyMVar
  doneVar <- liftIO $ newEmptyMVar

  startIO <- appToIO $ lowerCodensity $ do
    void $ connectToCellsQueue sm broadcast
    liftIO $ putMVar readyVar ()
    liftIO $ takeMVar doneVar

  void $ liftIO $ async startIO
  liftIO $ takeMVar readyVar

  pure QueueWatcher {doneVar, broadcast}

ensureWatcher :: String -> App QueueWatcher
ensureWatcher domain = do
  watchersLock <- asks (.cellsEventWatchersLock)
  watchersRef <- asks (.cellsEventWatchers)
  serviceMaps <- asks (.serviceMap)
  sm <- assertOne $ Map.lookup domain serviceMaps

  start <- appToIO (startQueueWatcher sm)

  liftIO
    $ E.bracket
      (putMVar watchersLock ())
      (\_ -> tryTakeMVar watchersLock)
    $ \_ -> do
      watchers <- liftIO $ readIORef watchersRef
      case Map.lookup domain watchers of
        Nothing -> do
          watcher <- start
          let watchers' = Map.insert domain watcher watchers
          writeIORef watchersRef watchers'
          pure watcher
        Just watcher -> pure watcher

data WatchCellsEvents = WatchCellsEvents
  { domain :: Either Domain String,
    filter :: Value -> App Bool
  }

instance Default WatchCellsEvents where
  def =
    WatchCellsEvents
      { domain = Left OwnDomain,
        filter = const (pure True)
      }

convEvents :: (MakesValue conv) => conv -> WatchCellsEvents
convEvents conv = def {filter = isNotifConv conv}

watchCellsEvents :: WatchCellsEvents -> App QueueConsumer
watchCellsEvents opts = do
  domain <- either (asString . make) pure opts.domain
  watcher <- ensureWatcher domain
  chan <- liftIO $ atomically $ dupTChan watcher.broadcast
  pure QueueConsumer {filter = opts.filter, chan}
