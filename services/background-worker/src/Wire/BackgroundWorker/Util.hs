module Wire.BackgroundWorker.Util where
-- NOTE: This file needs substantial rework for NATS
-- RabbitMQ consumer logic (Q.consumeMsgs, Q.ConsumerTag, etc.) 
-- needs to be replaced with NATS subscription logic
-- NATS.subscribe returns a subscription ID, and messages are received differently
-- TODO: Implement NATS-based message consumption

import Imports
import Network.AMQP qualified as Q

-- | This class exists to help with testing, making the envelope in unit test is
-- too difficult. So we use fake envelopes in the unit tests.
class RabbitMQEnvelope e where
  ack :: e -> IO ()
  reject :: e -> Bool -> IO ()

instance RabbitMQEnvelope Q.Envelope where
  ack = Q.ackEnv
  reject = Q.rejectEnv

type CleanupAction = IO ()
