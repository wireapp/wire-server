-- | Bounded, stop-and-wait experimental delivery protocol. One page is in flight.
module FanInPerf.Delivery where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM (throwSTM)
import Control.Exception (throwIO)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Lazy qualified as BL
import Data.UUID (UUID)
import FanInPerf.Wakeup
import Imports
import System.IO.Error (userError)
import System.Timeout (timeout)

-- Receive must be interruptible. The WebSocket adapter limits incoming frames.
-- A separate reader detects disconnects even while waiting for a wakeup.
deliverEvents :: WakeupHub -> UUID -> (UUID -> IO [(UUID, Value)]) -> (UUID -> IO ()) -> (Value -> IO ()) -> IO BL.ByteString -> IO ()
deliverEvents hub initial fetch persist send receive = do
  incoming <- newTBQueueIO 1
  let receiveLoop = forever $ do
        bytes <- receive
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
        atomically $ writeTBQueue incoming (parsed :: Maybe Integer)
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
            send $ object ["type" .= ("events" :: Text), "tag" .= tag, "events" .= [object ["id" .= nid, "payload" .= payload] | (nid, payload) <- page]]
            acknowledgement <- timeout 30_000_000 (atomically $ readTBQueue incoming)
            unless (acknowledgement == Just (Just tag)) invalid
            let next = fst (last page)
            persist next
            send $ object ["type" .= ("acked" :: Text), "tag" .= tag]
            loop next (tag + 1)
  -- Propagate the original receive exception synchronously. Linking wraps it in
  -- ExceptionInLinkedThread, which escapes the adapter's synchronous exception
  -- handler and incorrectly turns ordinary disconnects into HTTP 500 errors.
  race_ receiveLoop (loop initial (1 :: Integer))
