-- | Bounded, stop-and-wait experimental delivery protocol. One page is in flight.
module FanInPerf.Delivery where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (throwSTM)
import Control.Exception (evaluate, throwIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.UUID (UUID)
import FanInPerf.ServerTiming
import FanInPerf.Wakeup
import GHC.Clock (getMonotonicTimeNSec)
import Imports
import System.IO.Error (userError)
import System.Timeout (timeout)

-- Receive must be interruptible. The WebSocket adapter limits incoming frames.
-- A separate reader detects disconnects even while waiting for a wakeup.
deliverEvents :: WakeupHub -> UUID -> (UUID -> IO [(UUID, Value)]) -> (UUID -> IO ()) -> (Value -> IO ()) -> IO BL.ByteString -> IO ()
deliverEvents = deliverEventsWithTiming (\_ _ -> pure ())

-- | The receive timestamp is captured in the reader thread, before enqueueing.
deliverEventsWithTiming :: ObserveAck -> WakeupHub -> UUID -> (UUID -> IO [(UUID, Value)]) -> (UUID -> IO ()) -> (Value -> IO ()) -> IO BL.ByteString -> IO ()
deliverEventsWithTiming observe hub initial fetch persist send receive = do
  incoming <- newTBQueueIO 1
  let receiveLoop = forever $ do
        bytes <- receive
        receivedAt <- getMonotonicTimeNSec
        let parsed =
              if BL.length bytes > 1024
                then Nothing
                else
                  decode bytes
                    >>= parseMaybe
                      ( withObject "ack" $ \o -> do
                          kind <- o .: "type"
                          unless (kind == ("ack" :: Text)) $ fail "expected ack"
                          o .: "tag"
                      )
        -- Force both the Maybe and its Integer here; otherwise decoding can
        -- migrate into the delivery thread and obscure the handoff measurement.
        forced <- evaluate $ case parsed :: Maybe Integer of
          Nothing -> Nothing
          Just ackTag -> ackTag `seq` Just ackTag
        parsedAt <- getMonotonicTimeNSec
        atomically $ writeTBQueue incoming (forced, receivedAt, parsedAt)
        enqueuedAt <- getMonotonicTimeNSec
        observeBetween observe AckParse receivedAt parsedAt
        observeBetween observe AckEnqueue parsedAt enqueuedAt
      invalid = throwIO $ userError "invalid experimental acknowledgement"
      loop cursor tag = do
        -- Snapshot before fetching so a wakeup during catch-up cannot be lost.
        generation <- atomically $ currentGeneration hub
        page <- fetch cursor
        case page of
          [] -> do
            send $ object ["type" .= ("ready" :: Text)]
            atomically $
              void (awaitWakeup hub generation)
                `orElse` (readTBQueue incoming >> throwSTM (userError "unexpected acknowledgement"))
            loop cursor tag
          _ -> do
            eventSendStart <- getMonotonicTimeNSec
            send $ object ["type" .= ("events" :: Text), "tag" .= tag, "events" .= [object ["id" .= nid, "payload" .= payload] | (nid, payload) <- page]]
            eventSendEnd <- getMonotonicTimeNSec
            acknowledgement <- timeout 30_000_000 $ do
              queued <- atomically $ readTBQueue incoming
              dequeuedAt <- getMonotonicTimeNSec
              pure (queued, dequeuedAt)
            timeoutExitedAt <- getMonotonicTimeNSec
            (receivedAt, parsedAt, dequeuedAt) <- case acknowledgement of
              Just ((Just ackTag, receivedAt, parsedAt), dequeuedAt) | ackTag == tag -> pure (receivedAt, parsedAt, dequeuedAt)
              _ -> invalid
            validatedAt <- getMonotonicTimeNSec
            -- EnqueueToDequeue starts before the STM write, not at its commit:
            -- no unsafe IO in STM or additional synchronisation is needed.
            observeBetween observe EnqueueToDequeue parsedAt dequeuedAt
            observeBetween observe AckTimeoutExit dequeuedAt timeoutExitedAt
            observeBetween observe AckValidate timeoutExitedAt validatedAt
            observeBetween observe EventPageSend eventSendStart eventSendEnd
            observeBetween observe EventSendAfterAckRead receivedAt eventSendEnd
            observeBetween observe ReceiveQueue receivedAt validatedAt
            let next = fst (last page)
            persistStart <- getMonotonicTimeNSec
            persist next
            observeSince observe Persistence persistStart
            sendStart <- getMonotonicTimeNSec
            send $ object ["type" .= ("acked" :: Text), "tag" .= tag]
            observeSince observe ConfirmationSend sendStart
            observeSince observe ReceiveToConfirmation receivedAt
            loop next (tag + 1)
  -- Propagate the original receive exception synchronously. Linking wraps it in
  -- ExceptionInLinkedThread, which escapes the adapter's synchronous exception
  -- handler and incorrectly turns ordinary disconnects into HTTP 500 errors.
  race_ receiveLoop (loop initial (1 :: Integer))

-- | Availability-only stream. The initial signal carries the durable cursor for
-- reconnect catch-up. While the device fetches/acks via HTTP, hub generations
-- coalesce further signals. A caught-up message releases the next signal; it is
-- not a durable event acknowledgement. No payloads are fetched here.
deliverWakeups :: WakeupHub -> UUID -> (Value -> IO ()) -> IO BL.ByteString -> IO ()
deliverWakeups hub cursor send receive = do
  incoming <- newTBQueueIO 1
  let receiveLoop = forever $ do
        bytes <- receive
        let parsed =
              if BL.length bytes > 1024
                then Nothing
                else
                  decode bytes
                    >>= parseMaybe
                      ( withObject "caught-up" $ \o -> do
                          kind <- o .: "type"
                          unless (kind == ("caught-up" :: Text)) $ fail "expected caught-up"
                          o .: "tag"
                      )
        atomically $ writeTBQueue incoming (parsed :: Maybe Integer)
      loop tag = do
        generation <- atomically $ currentGeneration hub
        send $ object ["type" .= ("wakeup" :: Text), "tag" .= tag, "cursor" .= cursor]
        received <- atomically $ readTBQueue incoming
        unless (received == Just tag) $ throwIO $ userError "invalid experimental caught-up message"
        atomically $
          void (awaitWakeup hub generation)
            `orElse` (readTBQueue incoming >> throwSTM (userError "unexpected caught-up message"))
        loop (tag + 1)
  race_ receiveLoop (loop (1 :: Integer))
