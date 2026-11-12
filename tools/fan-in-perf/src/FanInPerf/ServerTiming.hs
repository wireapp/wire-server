-- | Fixed-cardinality timing metrics for the experimental Cannon ack path.
module FanInPerf.ServerTiming where

import GHC.Clock (getMonotonicTimeNSec)
import Imports
import Prometheus qualified as P

data AckStage = AckParse | AckEnqueue | EnqueueToDequeue | AckValidate | AckTimeoutExit | EventPageSend | EventSendAfterAckRead | PoolAcquire | DatabaseWrite | Persistence | ReceiveQueue | ConfirmationSend | ReceiveToConfirmation
  deriving (Eq, Show)

type ObserveAck = AckStage -> Double -> IO ()

stageLabel :: AckStage -> Text
stageLabel = \case
  AckParse -> "ack_parse"
  AckEnqueue -> "ack_enqueue"
  EnqueueToDequeue -> "enqueue_to_dequeue"
  AckValidate -> "ack_validate"
  AckTimeoutExit -> "ack_timeout_exit"
  EventPageSend -> "event_page_send"
  EventSendAfterAckRead -> "event_send_after_ack_read"
  PoolAcquire -> "pool_acquire"
  DatabaseWrite -> "database_write"
  Persistence -> "persistence"
  ReceiveQueue -> "receive_queue"
  ConfirmationSend -> "confirmation_send"
  ReceiveToConfirmation -> "receive_to_confirmation"

registerAckTimings :: IO ObserveAck
registerAckTimings = do
  metric <-
    P.register $
      P.vector "stage" $
        P.histogram
          (P.Info "fanin_perf_server_ack_seconds" "Experimental Cannon acknowledgement stage duration in seconds; successful stages only")
          [0.001, 0.0025, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10, 30, 120]
  pure $ \stage seconds -> P.withLabel metric (stageLabel stage) (`P.observe` seconds)

observeSince :: ObserveAck -> AckStage -> Word64 -> IO ()
observeSince observe stage start = do
  end <- getMonotonicTimeNSec
  observeBetween observe stage start end

observeBetween :: ObserveAck -> AckStage -> Word64 -> Word64 -> IO ()
observeBetween observe stage start end =
  observe stage (fromIntegral (end - min start end) / 1000000000)
