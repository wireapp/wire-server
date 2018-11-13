{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Imports
import           Eval
import           Options
import           Options.Applicative
import           System.Exit
import qualified System.Logger       as Log

main :: IO ()
main = do
    cmd <- execParser (info (helper <*> commandParser) desc)
    lgr <- initLogger
    runCommand lgr cmd
    -- TODO: dump metrics in a suitable format (NOT json)
    exitSuccess
  where
    desc = header   "brig-index"
        <> progDesc "Brig Search Index Utilities"
        <> fullDesc

    initLogger
        = Log.new
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
