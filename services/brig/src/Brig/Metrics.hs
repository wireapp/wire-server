module Brig.Metrics
    ( Metrics
    , initMetrics
    , withMetric
    , withMetric_
    , HasMetrics(..)
    , module Prometheus
    ) where

import Imports
import Control.Lens as L
import Prometheus

data Metrics = Metrics
    { _budget_sms_exhausted_count :: Counter
    }

initMetrics :: IO Metrics
initMetrics =
    Metrics
    <$> register (counter (Info "budget_sms_exhausted_count"
                                "Count of cases when budget for sending sms's has been exhausted"))

L.makeClassy ''Metrics

{-|
Pass a metric to a function which alters that metric in some way.

Examples:

> withMetric_ budget_sms_exhausted_count incCounter
> withMetric_ budget_sms_exhausted_count $ \m -> addCounter m 1
> withMetric_ budget_sms_exhausted_count $ flip addDurationToCounter $ do long action
-}
withMetric :: (MonadReader s m) => Lens' s a -> (a -> m b) -> m b
withMetric metricLens action = view metricLens >>= action

-- | Voided 'withMetric'
withMetric_ :: (MonadReader s m) => Lens' s a -> (a -> m b) -> m ()
withMetric_ metricLens action = void $ withMetric metricLens action
