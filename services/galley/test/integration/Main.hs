{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Bilge (host, port)
import Control.Monad
import Data.Maybe (isJust)
import OpenSSL
import System.Environment
import Test.Tasty

import qualified API

main :: IO ()
main = withOpenSSL $ do
    integrationTest <- lookupEnv "INTEGRATION_TEST"
    when (isJust integrationTest) $ do
        g <- (host "localhost" .) . port . read <$> getEnv "GALLEY_WEB_PORT"
        b <- (host "localhost" .) . port . read <$> getEnv "BRIG_WEB_PORT"
        c <- (host "localhost" .) . port . read <$> getEnv "CANNON_WEB_PORT"
        defaultMain =<< API.tests g b c

