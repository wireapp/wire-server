module Main where

import Imports
import qualified Spec
import System.Environment (withArgs)
import Test.Hspec.Runner

-- See https://hspec.github.io/hspec-discover.html#using-a-custom-main-function and
-- /services/spar/test-integration/Main.hs
main :: IO ()
main = do
  -- (we don't need wire args, appearently.  but if we ever should, here we have them.)
  (_wireArgs, hspecArgs) <- partitionArgs <$> getArgs
  withArgs hspecArgs . hspec $ Spec.spec

partitionArgs :: [String] -> ([String], [String])
partitionArgs = go [] []
  where
    go wireArgs hspecArgs ("-s" : x : xs) = go (wireArgs <> ["-s", x]) hspecArgs xs
    go wireArgs hspecArgs ("-i" : x : xs) = go (wireArgs <> ["-i", x]) hspecArgs xs
    go wireArgs hspecArgs (x : xs) = go wireArgs (hspecArgs <> [x]) xs
    go wireArgs hspecArgs [] = (wireArgs, hspecArgs)