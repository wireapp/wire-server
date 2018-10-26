module Main (main) where

import Imports
import OpenSSL (withOpenSSL)
import Test.Tasty

import qualified DelayQueue
import qualified Json
import qualified Native

main :: IO ()
main = withOpenSSL . defaultMain $
    testGroup "Main"
        [ Native.tests
        , DelayQueue.tests
        , Json.tests
        ]
