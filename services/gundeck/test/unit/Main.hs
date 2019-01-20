module Main (main) where

import Imports
import OpenSSL (withOpenSSL)
import Test.Tasty

import qualified DelayQueue
import qualified Json
import qualified Native
import qualified Push

main :: IO ()
main = withOpenSSL . defaultMain $
    testGroup "Main"
        [ DelayQueue.tests
        , Json.tests
        , Native.tests
        , Push.tests
        ]
