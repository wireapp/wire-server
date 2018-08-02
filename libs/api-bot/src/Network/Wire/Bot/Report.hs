{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.Wire.Bot.Report
    ( -- * Create Reports
      Report (reportTitle, reportDate, reportSections)
    , createReport

      -- * Access Report Data
    , reportCounter
    , reportLabel
    , reportGauge
    , reportBucket

      -- * Structure Reports
    , SectionS
    , Section (sectionName, sectionMetrics)
    , Metric  (..)
    , section

      -- ** Predefined Sections
    , defaultSections
    , botsSection
    , exceptionsSection
    , assertionsSection
    , eventsTotalSection
    , eventTypeSection
    ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Metrics
import Data.Metrics.Buckets
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Network.Wire.Client.API.Push (EventType (..), eventTypeText)
import Network.Wire.Bot.Metrics

import qualified Data.HashMap.Strict as HashMap

-------------------------------------------------------------------------------
-- * Create Reports

data Report = Report
    { reportTitle    :: !Text
    , reportDate     :: !UTCTime
    , reportSections :: [Section]
    , _data          :: !Data
    } deriving (Eq)

-- | Create a 'Report' of the metrics in the given 'Section's.
-- Note that reports are not created atomically, i.e. reports that
-- are created while bots are still active may not be fully consistent.
createReport :: MonadIO m => Text -> Metrics -> SectionS -> m Report
createReport t m (SectionS (Endo f)) = do
    d <- liftIO getCurrentTime
    v <- liftIO $ foldM go mempty (concatMap sectionMetrics s)
    return $! Report t d s v
  where
    s = f []
    go (Data cs ls bs gs) metric = case metric of
        Counter _ p -> do
            v <- counterValue =<< counterGet p m
            return $! Data (HashMap.insert p v cs) ls bs gs
        Label   _ p -> do
            v <- labelValue =<< labelGet p m
            return $! Data cs (HashMap.insert p v ls) bs gs
        Gauge   _ p -> do
            v <- gaugeValue =<< gaugeGet p m
            return $! Data cs ls bs (HashMap.insert p v gs)
        Buckets _ p -> do
            v <- snapshot =<< bucketsGet 0 0 p m
            return $! Data cs ls (HashMap.insert p v bs) gs

-------------------------------------------------------------------------------
-- * Access Report Data

data Data = Data
    { _counters :: HashMap Path Word
    , _labels   :: HashMap Path Text
    , _buckets  :: HashMap Path (HashMap Int Word)
    , _gauges   :: HashMap Path Int
    } deriving (Eq)

instance Semigroup Data where
    (<>) (Data a b c d) (Data w x y z) =
        Data (a <> w) (b <> x) (c <> y) (d <> z)

instance Monoid Data where
    mempty = Data mempty mempty mempty mempty

reportCounter :: Report -> Path -> Word
reportCounter r p = fromMaybe 0 $ HashMap.lookup p (_counters (_data r))

reportLabel :: Report -> Path -> Text
reportLabel r p = fromMaybe "" $ HashMap.lookup p (_labels (_data r))

reportGauge :: Report -> Path -> Int
reportGauge r p = fromMaybe 0 $ HashMap.lookup p (_gauges (_data r))

reportBucket :: Report -> Path -> HashMap Int Word
reportBucket r p = fromMaybe mempty $ HashMap.lookup p (_buckets (_data r))

-------------------------------------------------------------------------------
-- * Structure Reports

newtype SectionS = SectionS (Endo [Section]) deriving Monoid

data Section = Section
    { sectionName    :: !Text
    , sectionMetrics :: [Metric]
    } deriving (Eq)

data Metric
    = Counter !Text !Path
    | Gauge   !Text !Path
    | Buckets !Text !Path
    | Label   !Text !Path
    deriving (Eq)

section :: Text -> [Metric] -> SectionS
section t m = SectionS $ Endo (Section t m:)

defaultSections :: SectionS
defaultSections =
       botsSection
    <> assertionsSection
    <> exceptionsSection
    <> eventsTotalSection
    <> foldMap eventTypeSection [(minBound :: EventType)..]

botsSection :: SectionS
botsSection = section "Bots"
    [ Counter "Created (New)" botsCreatedNew
    , Counter "Created (Cached)" botsCreatedCached
    , Counter "Alive" botsAlive
    ]

exceptionsSection :: SectionS
exceptionsSection = section "Exceptions"
    [ Counter "Total" exceptionsTotal
    ]

assertionsSection :: SectionS
assertionsSection = section "Assertions"
    [ Counter "Total" assertionsTotal
    , Counter "Failed" assertionsFailed
    ]

eventsTotalSection :: SectionS
eventsTotalSection = section "Events (Total)"
    [ Counter "Received" eventsTotalRcvd
    , Counter "Acknowledged" eventsTotalAckd
    , Counter "Ignored" eventsTotalIgnd
    , Counter "Missed" eventsTotalMssd
    ]

eventTypeSection :: EventType -> SectionS
eventTypeSection t = section ("Event (" <> eventTypeText t <> ")")
    [ Counter "Received" (eventTypeRcvd t)
    , Counter "Acknowledged" (eventTypeAckd t)
    , Counter "Ignored" (eventTypeIgnd t)
    , Counter "Missed" (eventTypeMssd t)
    ]
