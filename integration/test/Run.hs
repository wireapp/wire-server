module Run (main) where

import App
import Imports
import qualified Test.Client

main :: IO ()
main = runApp $ Test.Client.testCantDeleteLHClient
