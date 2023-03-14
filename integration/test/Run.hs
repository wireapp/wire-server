module Run (main) where

import App
import Imports
import qualified Moo
import qualified Test.Client

main :: IO ()
main = do
  putStrLn ("Moo is" <> show Moo.x)
  runApp $ Test.Client.testCantDeleteLHClient
