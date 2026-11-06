-- | Wakeup transport for the fixed experimental epoch stream.
module FanInPerf.Wakeup where

import Control.Concurrent.STM (check)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import FanInPerf.Service (boundedBody, wakeupPayload)
import Imports
import Network.HTTP.Types
import Network.HTTP.Types.Header (hAllow)
import Network.Wai qualified as Wai
import UnliftIO.Exception (tryAny)

wakeupQueue :: Text
wakeupQueue = "fan-in-perf-wakeups"

-- A generation counter coalesces signals without keeping an unbounded queue per
-- client. Each reader keeps its own last generation, so readers cannot steal
-- each other's wakeups. Registration must be followed by an initial DB catch-up.
newtype WakeupHub = WakeupHub (TVar Integer)

newWakeupHub :: IO WakeupHub
newWakeupHub = WakeupHub <$> newTVarIO 0

currentGeneration :: WakeupHub -> STM Integer
currentGeneration (WakeupHub generation) = readTVar generation

signal :: WakeupHub -> STM ()
signal (WakeupHub generation) = modifyTVar' generation (+ 1)

awaitWakeup :: WakeupHub -> Integer -> STM Integer
awaitWakeup hub previous = do
  generation <- currentGeneration hub
  check (generation > previous)
  pure generation

data Delivery = Delivered | Retry | Invalid
  deriving (Eq, Show)

-- Payloads are signals only; arbitrary event payloads must never enter this path.
validWakeup :: BL.ByteString -> Bool
validWakeup body = BL.length body <= 1024 && decode body == Just wakeupPayload

forwardWakeup :: [IO Bool] -> BL.ByteString -> IO Delivery
forwardWakeup destinations body
  | not (validWakeup body) = pure Invalid
  | null destinations = pure Retry
  | otherwise = do
      results <- for destinations $ \send -> either (const False) id <$> tryAny send
      pure $ if and results then Delivered else Retry

wakeupMiddleware :: WakeupHub -> Wai.Middleware
wakeupMiddleware hub fallback req respond
  | Wai.pathInfo req /= ["i", "fan-in-perf", "wakeup"] = fallback req respond
  | Wai.requestMethod req /= methodPost = respond $ Wai.responseLBS status405 [(hAllow, "POST")] ""
  | otherwise = do
      body <- boundedBody 1024 req
      case body of
        Nothing -> respond $ Wai.responseLBS status413 [] ""
        Just bytes
          | validWakeup bytes -> do
              atomically $ signal hub
              respond $ Wai.responseLBS status204 [] ""
          | otherwise -> respond $ Wai.responseLBS status400 [] ""

-- Match exact devices, then notify each hosting instance once.
participantDestinations :: (Ord u, Ord c, Ord a) => [(u, c)] -> [(u, Maybe c, a)] -> [a]
participantDestinations participants presences =
  Set.toList $ Set.fromList [address | (user, Just client, address) <- presences, Set.member (user, client) members]
  where
    members = Set.fromList participants
