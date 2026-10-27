-- | Does the TTL garbage collection (pg_partman dropping a partition) disturb a
-- run? Compares the per-second figures of the seconds around a drop with all
-- others. Pure: the run only collects one 'TickSample' per tick and the drops
-- (one per table and poll that saw its oldest partition change).
module FanInPerf.GcImpact
  ( TickSample (..),
    DropEvent (..),
    detectDrops,
    GcWindow (..),
    GcImpact (..),
    gcWindowSeconds,
    sampleOf,
    gcImpact,
    jsonOptions,
  )
where

import Data.Aeson (ToJSON (..), camelTo2, genericToJSON)
import Data.Aeson qualified as A
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import FanInPerf.ConsumeStats
import FanInPerf.Stats
import Imports

-- | A tick is "around a drop" if it ended at most this many seconds before or
-- after it.
gcWindowSeconds :: Double
gcWindowSeconds = 5

-- | What one status line of a consume run says, with the time (seconds since
-- the start of the run) it ended. Strict, so that a sample does not keep the
-- reports it was taken from (cumulative snapshots and histograms) alive.
data TickSample = TickSample
  { at :: !Double,
    push :: !Double,
    ack :: !Double,
    ackP99 :: !(Maybe Double),
    lagP99 :: !(Maybe Double),
    backlog :: !Int
  }
  deriving (Eq, Show)

sampleOf :: TickReport -> ConsumeReport -> TickSample
sampleOf r c =
  TickSample
    { at = r.elapsed,
      push = r.pushRate,
      ack = c.ackRate,
      ackP99 = c.ackP99,
      lagP99 = c.lagP99,
      backlog = c.backlog
    }

-- | Medians (maximum for @ackP99Max@) over the ticks of a group; 'Nothing' where
-- no tick has a value (a percentile is missing in a second without any).
data GcWindow = GcWindow
  { seconds :: Int,
    pushRate :: Maybe Double,
    ackRate :: Maybe Double,
    ackP99 :: Maybe Double,
    ackP99Max :: Maybe Double,
    lagP99 :: Maybe Double,
    backlog :: Maybe Double
  }
  deriving stock (Eq, Show, Generic)

-- | A partition of a table was dropped, seen by the poll at @at@ (seconds
-- since the start of the run).
data DropEvent = DropEvent {table :: !Text, at :: !Double}
  deriving (Eq, Show)

-- | @detectDrops now previous current@: the polls of the oldest partition of
-- every table, by table. One event for each table whose oldest partition
-- changed (a table missing from one poll counts as having no partition).
-- Creating a partition never changes the oldest one, so only drops fire.
detectDrops :: Double -> Map Text Text -> Map Text Text -> [DropEvent]
detectDrops now previous current =
  [ DropEvent {table, at = now}
  | table <- Set.toList (Map.keysSet previous <> Map.keysSet current),
    oldest previous table /= oldest current table
  ]
  where
    oldest polls table = Map.findWithDefault "" table polls

data GcImpact = GcImpact
  { -- | all tables together
    drops :: Int,
    dropsByTable :: Map Text Int,
    windowSeconds :: Double,
    -- | ticks around a drop
    within :: GcWindow,
    -- | all other ticks
    other :: GcWindow
  }
  deriving stock (Eq, Show, Generic)

-- | snake_case field names, for all summary JSON.
jsonOptions :: A.Options
jsonOptions = A.defaultOptions {A.fieldLabelModifier = camelTo2 '_'}

instance ToJSON GcWindow where toJSON = genericToJSON jsonOptions

instance ToJSON GcImpact where toJSON = genericToJSON jsonOptions

-- | @gcImpact warmup duration drops samples@: only the ticks after the warmup
-- and while the writers ran (@at <= duration@) count. A tick is within a drop
-- window if it is near the drop of any table.
gcImpact :: Double -> Double -> [DropEvent] -> [TickSample] -> GcImpact
gcImpact warmup duration drops samples =
  GcImpact
    { drops = length drops,
      dropsByTable = Map.fromListWith (+) [(d.table, 1) | d <- drops],
      windowSeconds = gcWindowSeconds,
      within = summarize near,
      other = summarize far
    }
  where
    counted = [s | s <- samples, s.at > warmup, s.at <= duration]
    (near, far) = partition (\s -> any (\d -> abs (s.at - d.at) <= gcWindowSeconds) drops) counted

summarize :: [TickSample] -> GcWindow
summarize ss =
  GcWindow
    { seconds = length ss,
      pushRate = median (map (.push) ss),
      ackRate = median (map (.ack) ss),
      ackP99 = median p99s,
      ackP99Max = if null p99s then Nothing else Just (maximum p99s),
      lagP99 = median (mapMaybe (.lagP99) ss),
      backlog = median (map (fromIntegral . (.backlog)) ss)
    }
  where
    p99s = mapMaybe (.ackP99) ss

median :: [Double] -> Maybe Double
median [] = Nothing
median xs =
  let sorted = sort xs
      n = length sorted
   in Just $
        if even n
          then (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2
          else sorted !! (n `div` 2)
