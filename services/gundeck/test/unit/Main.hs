module Main (main) where

import Test.Tasty
import OpenSSL (withOpenSSL)

import qualified DelayQueue
import qualified Json
import qualified Native

main :: IO ()
main = withOpenSSL $ defaultMain $
    testGroup "Main"
        [ Native.tests
        , DelayQueue.tests
        , Json.tests
        ]
