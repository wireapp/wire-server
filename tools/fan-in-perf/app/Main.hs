module Main (main) where

import FanInPerf.Options (optionsInfo)
import FanInPerf.Run (run)
import FanInPerf.Terminal (newConsole)
import Imports
import Options.Applicative (execParser)

main :: IO ()
main = do
  opts <- execParser optionsInfo
  console <- newConsole
  run console opts
