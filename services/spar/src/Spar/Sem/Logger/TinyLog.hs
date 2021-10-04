module Spar.Sem.Logger.TinyLog (loggerToTinyLog, stringLoggerToTinyLog, toLevel, fromLevel) where

import Imports
import Polysemy
import Spar.Sem.Logger (Level (..), Logger (..), mapLogger)
import qualified System.Logger as Log

loggerToTinyLog ::
  Member (Embed IO) r =>
  Log.Logger ->
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
loggerToTinyLog tinylog = interpret $ \case
  Log lvl msg ->
    embed @IO $ Log.log tinylog (toLevel lvl) msg

stringLoggerToTinyLog :: Member (Logger (Log.Msg -> Log.Msg)) r => Sem (Logger String ': r) a -> Sem r a
stringLoggerToTinyLog = mapLogger @String Log.msg

toLevel :: Level -> Log.Level
toLevel = \case
  Fatal -> Log.Fatal
  Error -> Log.Error
  Warn -> Log.Warn
  Info -> Log.Info
  Debug -> Log.Debug
  Trace -> Log.Trace

fromLevel :: Log.Level -> Level
fromLevel = \case
  Log.Fatal -> Fatal
  Log.Error -> Error
  Log.Warn -> Warn
  Log.Info -> Info
  Log.Debug -> Debug
  Log.Trace -> Trace
