module Main where

import Data.Monoid
import System.MakeDeb
import Options.Applicative

main :: IO ()
main = execParser (info (helper <*> options) desc) >>= makeDeb
  where
    desc = header "Create binary debian archive." <> fullDesc

