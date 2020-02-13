{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wire.Bot.Report
  ( -- * Create Reports
    Report (reportTitle, reportDate, reportSections),
    createReport,

    -- * Access Report Data
    reportCounter,
    reportLabel,
    reportGauge,
    reportBucket,

    -- * Structure Reports
    SectionS,
    Section (sectionName, sectionMetrics),
    Metric (..),
    section,

    -- ** Predefined Sections
    defaultSections,
    botsSection,
    exceptionsSection,
    assertionsSection,
    eventsTotalSection,
    eventTypeSection,
  )
where

import qualified Data.HashMap.Strict as HashMap
import Data.Metrics
import Data.Time.Clock
import Imports
import Network.Wire.Bot.Metrics
import Network.Wire.Client.API.Push (EventType (..), eventTypeText)

-------------------------------------------------------------------------------

-- * Create Reports

data Report
  = Report
      { reportTitle :: !Text,
        reportDate :: !UTCTime,
        reportSections :: [Section],
        _data :: !Data
      }
  deriving (Eq)

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
      Gauge _ p -> do
        v <- gaugeValue =<< gaugeGet p m
        return $! Data cs ls bs (HashMap.insert p v gs)
      Histogram _ p hi -> do
        v <- histoGet hi m >>= histoValue
        return $! Data cs ls (HashMap.insert p v bs) gs

-------------------------------------------------------------------------------

-- * Access Report Data

data Data
  = Data
      { _counters :: HashMap Path Double,
        _labels :: HashMap Path Text,
        _histograms :: HashMap Path (Map Bucket Int),
        _gauges :: HashMap Path Double
      }
  deriving (Eq)

instance Semigroup Data where
  (<>) (Data a b c d) (Data w x y z) =
    Data (a <> w) (b <> x) (c <> y) (d <> z)

instance Monoid Data where
  mempty = Data mempty mempty mempty mempty

reportCounter :: Report -> Path -> Double
reportCounter r p = fromMaybe 0 $ HashMap.lookup p (_counters (_data r))

reportLabel :: Report -> Path -> Text
reportLabel r p = fromMaybe "" $ HashMap.lookup p (_labels (_data r))

reportGauge :: Report -> Path -> Double
reportGauge r p = fromMaybe 0 $ HashMap.lookup p (_gauges (_data r))

reportBucket :: Report -> Path -> Map Bucket Int
reportBucket r p = fromMaybe mempty $ HashMap.lookup p (_histograms (_data r))

-------------------------------------------------------------------------------

-- * Structure Reports

newtype SectionS = SectionS (Endo [Section]) deriving (Semigroup, Monoid)

data Section
  = Section
      { sectionName :: !Text,
        sectionMetrics :: [Metric]
      }
  deriving (Eq)

data Metric
  = Counter !Text !Path
  | Gauge !Text !Path
  | Histogram !Text !Path !HistogramInfo
  deriving (Eq)

section :: Text -> [Metric] -> SectionS
section t m = SectionS $ Endo (Section t m :)

defaultSections :: SectionS
defaultSections =
  botsSection
    <> assertionsSection
    <> exceptionsSection
    <> eventsTotalSection
    <> foldMap eventTypeSection [(minBound :: EventType) ..]

botsSection :: SectionS
botsSection =
  section
    "Bots"
    [ Counter "Created (New)" botsCreatedNew,
      Counter "Created (Cached)" botsCreatedCached,
      Counter "Alive" botsAlive
    ]

exceptionsSection :: SectionS
exceptionsSection =
  section
    "Exceptions"
    [ Counter "Total" exceptionsTotal
    ]

assertionsSection :: SectionS
assertionsSection =
  section
    "Assertions"
    [ Counter "Total" assertionsTotal,
      Counter "Failed" assertionsFailed
    ]

eventsTotalSection :: SectionS
eventsTotalSection =
  section
    "Events (Total)"
    [ Counter "Received" eventsTotalRcvd,
      Counter "Acknowledged" eventsTotalAckd,
      Counter "Ignored" eventsTotalIgnd,
      Counter "Missed" eventsTotalMssd
    ]

eventTypeSection :: EventType -> SectionS
eventTypeSection t =
  section
    ("Event (" <> eventTypeText t <> ")")
    [ Counter "Received" (eventTypeRcvd t),
      Counter "Acknowledged" (eventTypeAckd t),
      Counter "Ignored" (eventTypeIgnd t),
      Counter "Missed" (eventTypeMssd t)
    ]
