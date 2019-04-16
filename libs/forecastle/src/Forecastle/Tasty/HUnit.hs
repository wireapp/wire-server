module Forecastle.Tasty.HUnit
    ( test
    , module Test.Tasty
    ) where

import Imports
import Forecastle
import Test.Tasty.HUnit
import Test.Tasty

test :: IO e -> TestName -> TestM e a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
        setup <- s
        void . flip runReaderT setup . runTestM $ h
