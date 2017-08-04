{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Bilge (host, port)
import Control.Monad
import Data.Maybe (isJust)
import OpenSSL
import System.Environment
import Test.Tasty
import Data.Text

import qualified API
import qualified API.SQS as SQS

main :: IO ()
main = withOpenSSL $ do
    integrationTest <- lookupEnv "INTEGRATION_TEST"
    when (isJust integrationTest) $ do
        g <- (host "localhost" .) . port . read <$> getEnv "GALLEY_WEB_PORT"
        b <- (host "localhost" .) . port . read <$> getEnv "BRIG_WEB_PORT"
        c <- (host "localhost" .) . port . read <$> getEnv "CANNON_WEB_PORT"
        -- unset this env variable to disable testing SQS team events:
        q <- fmap pack <$> lookupEnv "GALLEY_SQS_TEAM_EVENTS"
        awsEnv <- maybe (return Nothing) (fmap Just . SQS.mkAWSEnv) q
        defaultMain =<< API.tests g b c awsEnv
