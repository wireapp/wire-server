module Main where

import Imports
import Options.Applicative
import System.MakeDeb

main :: IO ()
main = execParser (info (helper <*> options) desc) >>= makeDeb
  where
    desc = header "Create binary debian archive." <> fullDesc
