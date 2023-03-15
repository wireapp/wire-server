module Run (main) where

import App
import Imports
import RunAllTests

main :: IO ()
main = runApp runAllTests
