module Main (main) where

import FanInPerf.Options (Invocation (..), invocationInfo)
import FanInPerf.Run (run)
import FanInPerf.Smoke (runSmoke)
import FanInPerf.Terminal (newConsole)
import Imports
import Options.Applicative (customExecParser, prefs, showHelpOnError)

main :: IO ()
main = do
  invocation <- customExecParser (prefs showHelpOnError) invocationInfo
  case invocation of
    SmokeRun opts -> runSmoke opts
    DatabaseRun opts -> newConsole >>= \console -> run console opts
