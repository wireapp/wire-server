module Pulsar.Client.Logging where

import Imports
import Pulsar.Client qualified as Pulsar
import System.Logger qualified as Log

onPulsarError :: (MonadIO m) => String -> Log.Logger -> Pulsar.RawResult -> m ()
onPulsarError provenance logger result =
  Log.err logger $
    Log.msg message
      . Log.field "provenance" provenance
  where
    message =
      "error: " <> pulsarResultToString result

pulsarResultToString :: Pulsar.RawResult -> String
pulsarResultToString result = case Pulsar.renderResult result of
  Just r -> show r
  Nothing -> (show . Pulsar.unRawResult) result

pulsarClientLogger :: (MonadIO m) => String -> Log.Logger -> Pulsar.LogLevel -> Pulsar.LogFile -> Pulsar.LogLine -> Pulsar.LogMessage -> m ()
pulsarClientLogger provenance logger level file line message =
  Log.log logger (toLogLevel level) $
    Log.msg message
      . Log.field "file" file
      . Log.field "line" (show line)
      . Log.field "provenance" provenance
  where
    toLogLevel :: Pulsar.LogLevel -> Log.Level
    toLogLevel 0 = Log.Debug
    toLogLevel 1 = Log.Info
    toLogLevel 2 = Log.Warn
    toLogLevel 3 = Log.Error
    toLogLevel n = error ("Unknown Pulsar log level" <> show n)

logPulsarResult :: (MonadIO m) => String -> Log.Logger -> Pulsar.RawResult -> m ()
logPulsarResult provenance logger result =
  Log.debug logger $
    Log.msg message
      . Log.field "provenance" provenance
  where
    message =
      "result: " <> pulsarResultToString result
