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
    restore,
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

-- | Streams with unacknowledged notifications; the single source of truth
-- (no separate counter that could drift).
newtype AckState = AckState (Map Key Pending)
  deriving (Eq, Show)

emptyAckState :: AckState
emptyAckState = AckState mempty

unacked :: AckState -> Int
unacked (AckState m) = sum (map (.count) (Map.elems m))

-- | Merges entries (e.g. of failed acks) into the state: higher id, counts
-- added, so their notifications stay unacknowledged.
restore :: [Pending] -> AckState -> AckState
restore ps (AckState m) = AckState (foldl' (\acc p -> Map.insertWith merge (streamKey p.stream) p acc) m ps)
  where
    merge new old = old {maxId = max old.maxId new.maxId, count = old.count + new.count}

-- | @deliver n stream id state@: one received notification. Once 'unacked'
-- reaches @n@, returns every pending stream and clears the state.
deliver :: Int -> Stream -> UUID -> AckState -> ([Pending], AckState)
deliver n s i st =
  let AckState m = restore [Pending s i 1] st
   in if unacked (AckState m) >= n then (Map.elems m, emptyAckState) else ([], AckState m)
