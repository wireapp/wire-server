module Spar.Sem.Reporter.Wai where

import Imports
import qualified Network.Wai.Utilities.Server as Wai
import Polysemy
import Polysemy.Input
import Spar.Sem.Reporter
import qualified System.Logger as TinyLog

reporterToTinyLogWai :: Members '[Embed IO, Input TinyLog.Logger] r => Sem (Reporter ': r) a -> Sem r a
reporterToTinyLogWai = interpret $ \case
  Report req err -> do
    logger <- input
    embed @IO $ Wai.logError logger req err
