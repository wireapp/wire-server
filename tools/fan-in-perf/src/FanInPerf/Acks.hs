-- | When a simulated client acknowledges. Like the RFC's delivery tags (per
-- websocket, across streams) with @multiple = true@: once the client received
-- N notifications since its last ack, it acks every stream it received
-- something on, each up to the highest id it got there.
module FanInPerf.Acks
  ( Pending (..),
    AckState,
    emptyAckState,
    unacked,
    deliver,
    restoreFailed,
  )
where

import Data.Map.Strict qualified as Map
import Data.UUID.Types (UUID)
import FanInPerf.Population (Key, streamKey)
import Imports
import Wire.FanInNotificationsStore (Stream)

-- | Received, not yet acknowledged notifications of one stream.
data Pending = Pending
  { stream :: !Stream,
    -- | highest delivered id
    maxId :: !UUID,
    -- | delivered since this stream's last ack
    count :: !Int
  }
  deriving (Eq, Show)

-- | Streams with unacknowledged notifications
newtype AckState = AckState (Map Key Pending)
  deriving (Eq, Show)

emptyAckState :: AckState
emptyAckState = AckState mempty

unacked :: AckState -> Int
unacked (AckState m) = sum (map (.count) (Map.elems m))

-- | Merges entries into the state: higher id, counts added.
restore :: [Pending] -> AckState -> AckState
restore ps (AckState m) = AckState (foldl' (\acc p -> Map.insertWith merge (streamKey p.stream) p acc) m ps)
  where
    merge new old = old {maxId = max old.maxId new.maxId, count = old.count + new.count}

-- | Failed acks stay pending with their id but start counting from zero: the
-- retry comes with the next regular ack point (after N further deliveries),
-- not on the next delivery (which would retry every pending stream ~N times
-- as often while the store fails). New deliveries of the stream since keep
-- their count; the higher id wins.
restoreFailed :: [Pending] -> AckState -> AckState
restoreFailed ps = restore (map (\p -> p {count = 0}) ps)

-- | @deliver n stream id state@: one received notification. Once 'unacked'
-- reaches @n@, returns every pending stream and clears the state.
deliver :: Int -> Stream -> UUID -> AckState -> ([Pending], AckState)
deliver n s i st =
  let AckState m = restore [Pending s i 1] st
   in if unacked (AckState m) >= n then (Map.elems m, emptyAckState) else ([], AckState m)
