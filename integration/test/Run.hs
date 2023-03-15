module Run (main) where

import App
import Imports
import RunAllTests

runAllTests :: App ()
runAllTests = do
  for_ allTests $ \(_module_, _name, _summary, _full, action) -> do
    putStrLn _name
    action

main :: IO ()
main = runApp runAllTests
