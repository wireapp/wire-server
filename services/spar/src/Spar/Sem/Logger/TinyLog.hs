module Spar.Sem.Logger.TinyLog (loggerToTinyLog) where

import Imports
import Polysemy
import qualified System.Logger as Log
import Spar.Sem.Logger (Logger (..), Level(..))


loggerToTinyLog
    :: Member (Embed IO) r
    => Log.Logger
    -> Sem (Logger (Log.Msg -> Log.Msg) ': r) a -> Sem r a
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

