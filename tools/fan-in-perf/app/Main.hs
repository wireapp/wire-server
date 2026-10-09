module Main (main) where

import FanInPerf.Options (optionsInfo)
import FanInPerf.Run (run)
import FanInPerf.Terminal (newConsole)
import Imports
import Options.Applicative (customExecParser, prefs, showHelpOnError)

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnError) optionsInfo
  console <- newConsole
  run console opts
