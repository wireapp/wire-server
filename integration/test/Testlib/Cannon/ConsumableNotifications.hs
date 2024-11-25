-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Testlib.Cannon.ConsumableNotifications where

import Control.Retry
import Data.ByteString.Conversion (toByteString')
import qualified Data.Text as Text
import qualified Network.WebSockets as WS
import Testlib.Prelude hiding (assertNoEvent, awaitAnyEvent, awaitNMatches, awaitNMatchesResult)
import Testlib.Printing
import UnliftIO hiding (handle)

withEventsWebSockets ::
  forall user a.
  (HasCallStack, MakesValue user) =>
  [(user, String)] ->
  ([(TChan Value, TChan Value)] -> App a) ->
  App a
withEventsWebSockets userClients k = go [] $ reverse userClients
  where
    go :: [(TChan Value, TChan Value)] -> [(user, String)] -> App a
    go chans [] = k chans
    go chans ((user, cid) : remaining) =
      withEventsWebSocket user cid $ \eventsChan ackChan ->
        go ((eventsChan, ackChan) : chans) remaining

withEventsWebSocket :: (HasCallStack, MakesValue user) => user -> String -> (TChan Value -> TChan Value -> App a) -> App a
withEventsWebSocket user cid k = do
  closeWS <- liftIO newEmptyMVar
  bracket (setup closeWS) (\(_, _, wsThread) -> liftIO $ cancel wsThread) $ \(eventsChan, ackChan, wsThread) -> do
    x <- k eventsChan ackChan

    -- Ensure all the acks are sent before closing the websocket
    isAckChanEmpty <-
      retrying
        (limitRetries 5 <> constantDelay 10_000)
        (\_ isEmpty -> pure $ not isEmpty)
        (\_ -> liftIO $ atomically $ isEmptyTChan ackChan)
    unless isAckChanEmpty $ do
      liftIO $ putStrLn $ colored yellow $ "The ack chan is not empty after 50ms, some acks may not make it to the server"

    liftIO $ void $ tryPutMVar closeWS ()

    liftIO
      $ timeout 1_000_000 (wait wsThread)
      >>= \case
        Nothing ->
          putStrLn $ colored yellow $ "The websocket thread did not close after waiting for 1s"
        Just () -> pure ()

    pure x
  where
    setup :: (HasCallStack) => MVar () -> App (TChan Value, TChan Value, Async ())
    setup closeWS = do
      (eventsChan, ackChan) <- liftIO $ (,) <$> newTChanIO <*> newTChanIO
      wsThread <- eventsWebSocket user cid eventsChan ackChan closeWS
      pure (eventsChan, ackChan, wsThread)

eventsWebSocket :: (MakesValue user) => user -> String -> TChan Value -> TChan Value -> MVar () -> App (Async ())
eventsWebSocket user clientId eventsChan ackChan closeWS = do
  serviceMap <- getServiceMap =<< objDomain user
  uid <- objId =<< objQidObject user
  let HostPort caHost caPort = serviceHostPort serviceMap Cannon
      path = "/events?client=" <> clientId
      caHdrs = [(fromString "Z-User", toByteString' uid)]
      app conn = do
        r <-
          async
            $ wsRead conn
            `catch` \(e :: WS.ConnectionException) ->
              case e of
                WS.CloseRequest {} -> pure ()
                _ -> throwIO e
        w <- async $ wsWrite conn
        void $ waitAny [r, w]

      wsRead conn = forever $ do
        bs <- WS.receiveData conn
        case decodeStrict' bs of
          Just n -> atomically $ writeTChan eventsChan n
          Nothing ->
            error $ "Failed to decode events: " ++ show bs

      wsWrite conn = forever $ do
        eitherAck <- race (readMVar closeWS) (atomically $ readTChan ackChan)
        case eitherAck of
          Left () -> WS.sendClose conn (Text.pack "")
          Right ack -> WS.sendBinaryData conn (encode ack)
  liftIO
    $ async
    $ WS.runClientWith
      caHost
      (fromIntegral caPort)
      path
      WS.defaultConnectionOptions
      caHdrs
      app

sendMsg :: (HasCallStack) => TChan Value -> Value -> App ()
sendMsg eventsChan msg = liftIO $ atomically $ writeTChan eventsChan msg

ackFullSync :: (HasCallStack) => TChan Value -> App ()
ackFullSync ackChan = do
  sendMsg ackChan
    $ object ["type" .= "ack_full_sync"]

ackEvent :: (HasCallStack) => TChan Value -> Value -> App ()
ackEvent ackChan event = do
  deliveryTag <- event %. "data.delivery_tag"
  sendAck ackChan deliveryTag False

sendAck :: (HasCallStack) => TChan Value -> Value -> Bool -> App ()
sendAck ackChan deliveryTag multiple = do
  sendMsg ackChan
    $ object
      [ "type" .= "ack",
        "data"
          .= object
            [ "delivery_tag" .= deliveryTag,
              "multiple" .= multiple
            ]
      ]

assertEvent :: (HasCallStack) => TChan Value -> ((HasCallStack) => Value -> App a) -> App a
assertEvent eventsChan expectations = do
  liftIO (timeout 10_000_000 (atomically (readTChan eventsChan))) >>= \case
    Nothing -> assertFailure "No event received for 10s"
    Just e -> do
      pretty <- prettyJSON e
      addFailureContext ("event:\n" <> pretty)
        $ expectations e

assertNoEvent :: (HasCallStack) => TChan Value -> App ()
assertNoEvent eventsChan = do
  timeout 1_000_000 (atomically (readTChan eventsChan)) >>= \case
    Nothing -> pure ()
    Just e -> do
      eventJSON <- prettyJSON e
      assertFailure $ "Did not expect event: \n" <> eventJSON

consumeAllEvents :: TChan Value -> TChan Value -> App ()
consumeAllEvents eventsChan ackChan = do
  timeout 1_000_000 (atomically (readTChan eventsChan)) >>= \case
    Nothing -> pure ()
    Just e -> do
      ackEvent ackChan e
      consumeAllEvents eventsChan ackChan

awaitEvent :: TChan Value -> TChan Value -> (Value -> App Bool) -> App Value
awaitEvent eventsChan ackChan selector = do
  timeout 10_000_000 (atomically (readTChan eventsChan)) >>= \case
    Nothing -> assertFailure "No event received for 10s"
    Just e -> do
      ackEvent ackChan e
      matches <- selector e
      if matches
        then pure e
        else awaitEvent eventsChan ackChan selector

awaitAnyEvent :: (HasCallStack) => Int -> TChan Value -> App (Maybe Value)
awaitAnyEvent tSecs eventsChan = do
  timeout tSecs (atomically (readTChan eventsChan))

awaitNMatchesResult :: (HasCallStack) => TChan Value -> Int -> (Value -> App Bool) -> App AwaitResult
awaitNMatchesResult eventsChan n selector = go n [] []
  where
    go 0 nonMatches matches = do
      refill nonMatches
      pure
        $ AwaitResult
          { success = True,
            nMatchesExpected = n,
            matches = reverse matches,
            nonMatches = reverse nonMatches
          }
    go nLeft nonMatches matches = do
      let tSecs = 10_000_000
      mEvent <- awaitAnyEvent tSecs eventsChan
      case mEvent of
        Just event ->
          do
            isMatch <- selector event
            if isMatch
              then go (nLeft - 1) nonMatches (event : matches)
              else go nLeft (event : nonMatches) matches
        Nothing -> do
          refill nonMatches
          pure
            $ AwaitResult
              { success = False,
                nMatchesExpected = n,
                matches = reverse matches,
                nonMatches = reverse nonMatches
              }
    refill = mapM_ (liftIO . atomically . writeTChan eventsChan)

awaitNMatches ::
  (HasCallStack) =>
  -- | Number of matches
  Int ->
  -- | Selection function. Should not throw any exceptions
  (Value -> App Bool) ->
  TChan Value ->
  App [Value]
awaitNMatches nExpected checkMatch chan = do
  res <- awaitNMatchesResult chan nExpected checkMatch
  assertAwaitResult res

awaitMatch :: (HasCallStack) => (Value -> App Bool) -> TChan Value -> App Value
awaitMatch selector chan = do
  head <$> awaitNMatches 1 selector chan
