module Main (main) where

import Brig.Index.Eval
import Brig.Index.Options
import Imports
import Options.Applicative
import System.Exit
import qualified System.Logger.Class as Log

main :: IO ()
main = do
  cmd <- execParser (info (helper <*> commandParser) desc)
  lgr <- initLogger
  runCommand lgr cmd
  -- TODO: dump metrics in a suitable format (NOT json)
  exitSuccess
  where
    desc =
      header "brig-index"
        <> progDesc "Brig Search Index Utilities"
        <> fullDesc
    initLogger =
      Log.new -- TODO: use mkLogger'?
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
