module Brig.PolyLog where

import Imports
import Polysemy
import qualified System.Logger as Log

-- | This effect will help us write tests for log messages
--
-- FUTUREWORK: Move this to a separate module if it is required
--
-- FUTUREWORK: Either write an orphan instance for MonadLogger or provide
-- equivalent functions in System.Logger.Class
data PolyLog m a where
  PolyLog :: Log.Level -> (Log.Msg -> Log.Msg) -> PolyLog m ()

makeSem 'PolyLog

runPolyLog :: Member (Embed IO) r => Log.Logger -> Sem (PolyLog ': r) a -> Sem r a
runPolyLog logger = interpret $ \(PolyLog lvl msg) -> Log.log logger lvl msg
