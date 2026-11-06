-- | Experimental worker: route wakeups through current participant presences.
module Gundeck.FanInWorker (runWorker) where

import Data.Aeson (encode)
import Data.Id
import Data.Set qualified as Set
import FanInPerf.Service (wakeupPayload)
import FanInPerf.Wakeup
import Imports
import Network.AMQP qualified as Q
import Network.AMQP.Extended
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (statusCode)
import Network.URI qualified as URI
import System.Logger qualified as Log
import UnliftIO.Exception (tryAny)
import Wire.API.Presence

runWorker :: Log.Logger -> AmqpEndpoint -> HTTP.Manager -> IO [(UserId, ClientId)] -> ([UserId] -> IO [[Presence]]) -> IO ()
runWorker logger rabbit manager participants lookupPresences = do
  let send address = do
        req <- HTTP.parseRequest address
        response <-
          HTTP.httpNoBody
            req
              { HTTP.method = "POST",
                HTTP.requestHeaders = [("Content-Type", "application/json")],
                HTTP.requestBody = HTTP.RequestBodyLBS (encode wakeupPayload),
                HTTP.responseTimeout = HTTP.responseTimeoutMicro 2_000_000,
                HTTP.redirectCount = 0
              }
            manager
        let code = statusCode (HTTP.responseStatus response)
        pure (code >= 200 && code < 300)
      route body
        | not (validWakeup body) = pure Invalid
        | otherwise = do
            members <- participants
            presences <- concat <$> lookupPresences (Set.toList $ Set.fromList $ map fst members)
            let address p = show $ (fromURI p.resource) {URI.uriPath = "/i/fan-in-perf/wakeup", URI.uriQuery = "", URI.uriFragment = ""}
                destinations = participantDestinations members [(p.userId, p.clientId, address p) | p <- presences]
            -- Offline clients catch up on connect. An empty successful lookup
            -- must not block the queue; lookup failures instead cause a retry.
            if null destinations then pure Delivered else forwardWakeup (map send destinations) body
      consume (message, envelope) = do
        outcome <- either (const Retry) id <$> tryAny (route message.msgBody)
        case outcome of
          Delivered -> Q.ackEnv envelope
          Invalid -> do
            Log.warn logger $ Log.msg (Log.val "Invalid experimental fan-in wakeup discarded")
            Q.rejectEnv envelope False
          Retry -> do
            Log.warn logger $ Log.msg (Log.val "Experimental fan-in wakeup failed; requeueing")
            threadDelay 1_000_000
            Q.rejectEnv envelope True
  openConnectionWithRetries
    logger
    rabbit
    (Just "fan-in-perf-worker")
    RabbitMqHooks
      { onNewChannel = \channel -> do
          void $ Q.declareQueue channel Q.newQueue {Q.queueName = wakeupQueue, Q.queueDurable = True, Q.queueAutoDelete = False}
          Q.qos channel 0 1 False
          void $ Q.consumeMsgs channel wakeupQueue Q.Ack consume
          forever (threadDelay 60_000_000),
        onConnectionClose = pure (),
        onChannelException = const (pure ())
      }
