module Wire.MockInterpreters.TinyLog where

import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger qualified as Log

noopLogger ::
  Sem (Logger (Log.Msg -> Log.Msg) ': r) a ->
  Sem r a
noopLogger = interpret $ \case
  Log _lvl _msg -> pure ()
