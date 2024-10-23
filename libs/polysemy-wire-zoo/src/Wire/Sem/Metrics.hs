{-# LANGUAGE TemplateHaskell #-}

module Wire.Sem.Metrics where

import Imports
import Polysemy
import Prometheus (Counter, Gauge)

-- | NOTE: Vectors would require non trival changes because
-- 'Prometheus.withLabel' take a paramter of type 'metric -> IO ()'.
data Metrics m a where
  AddCounter :: Counter -> Double -> Metrics m ()
  AddGauge :: Gauge -> Double -> Metrics m ()

makeSem ''Metrics

incCounter :: (Member Metrics r) => Counter -> Sem r ()
incCounter c = addCounter c 1

incGauge :: (Member Metrics r) => Gauge -> Sem r ()
incGauge c = addGauge c 1
