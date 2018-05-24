module Main (main) where

import OpenSSL (withOpenSSL)
import Test.Tasty
import Util.Test (withWireTastyPatternEnv)

import qualified DelayQueue
import qualified Json
import qualified Native

main :: IO ()
main = withOpenSSL . withWireTastyPatternEnv . defaultMain $
    testGroup "Main"
        [ Native.tests
        , DelayQueue.tests
        , Json.tests
        ]
