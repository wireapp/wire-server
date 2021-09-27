module Spar.Sem.Logger.TinyLog (loggerToTinyLog, toLevel) where

import Imports
import Polysemy
import Spar.Sem.Logger (Level (..), Logger (..))
import qualified System.Logger as Log

loggerToTinyLog ::
  Member (Embed IO) r =>
  Log.Logger ->
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
loggerToTinyLog tinylog = interpret $ \case
  Log lvl msg ->
    embed @IO $ Log.log tinylog (toLevel lvl) msg

toLevel :: Level -> Log.Level
toLevel = \case
  Fatal -> Log.Fatal
  Error -> Log.Error
  Warn -> Log.Warn
  Info -> Log.Info
  Debug -> Log.Debug
  Trace -> Log.Trace
