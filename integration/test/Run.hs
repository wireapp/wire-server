module Run (main) where

import Imports
import RunAllTests
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Providers (singleTest)

main :: IO ()
main = do
  let tree =
        testGroup "Tests" $
          allTests <&> \(module_, name, _summary, _full, action) ->
            let qualifiedName = module_ <> "." <> name
             in singleTest qualifiedName action
  defaultMain tree
