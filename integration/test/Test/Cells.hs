{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Cells where

import API.Galley
import qualified API.GalleyInternal as I
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Reader
import Control.Retry
import qualified Data.Aeson as A
import Data.Text (Text)
import Network.AMQP
import Network.AMQP.Extended
import SetupHelpers
import System.Timeout
import Testlib.Prelude
import Testlib.ResourcePool

testCellsEvent :: App ()
testCellsEvent = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  conv <- postConversation alice defProteus {team = Just tid} >>= getJSON 201
  I.setCellsState alice conv "ready" >>= assertSuccess
  bobId <- bob %. "qualified_id"
  addMembers alice conv def {role = Just "wire_member", users = [bobId]} >>= assertSuccess

  config <- getServiceConfig backendA Gundeck
  queueName <- config %. "settings.cellsEventQueue" & asText
  runCodensity (connectToRabbitMQ backendA queueName) $ \q -> do
    event <- getMessage q %. "payload.0"
    event %. "type" `shouldMatch` "conversation.member-join"
    event %. "conversation" `shouldMatch` (conv %. "id")
    event %. "qualified_from" `shouldMatch` (alice %. "qualified_id")

data CellsQueue = CellsQueue
  { msgVar :: MVar Message,
    queueTimeout :: Int
  }

connectToRabbitMQ :: BackendResource -> Text -> Codensity App CellsQueue
connectToRabbitMQ resource queueName = do
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
        (consumeMsgs chan queueName Ack handler)
        (cancelConsumer chan)
        iok
  pure CellsQueue {msgVar, queueTimeout = 1000000}

getMessage :: CellsQueue -> App Value
getMessage q = do
  msg <-
    liftIO (timeout q.queueTimeout (takeMVar q.msgVar))
      >>= assertJust "Cells queue timeout"
  either assertFailure pure $ A.eitherDecode msg.msgBody
