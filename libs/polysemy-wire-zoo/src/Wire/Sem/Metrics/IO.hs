module Wire.Sem.Metrics.IO where

import Imports
import Polysemy
import qualified Prometheus as Prom
import Wire.Sem.Metrics

runMetricsToIO :: (Member (Embed IO) r) => InterpreterFor Metrics r
runMetricsToIO = interpret $ \case
  AddCounter c n -> embed . void $ Prom.addCounter @IO c n
  AddGauge g n -> embed $ Prom.addGauge @IO g n

ignoreMetrics :: InterpreterFor Metrics r
ignoreMetrics = interpret $ \case
  AddCounter _ _ -> pure ()
  AddGauge _ _ -> pure ()
