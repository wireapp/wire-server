{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bilge (Request, host, port, header)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Control.Monad (unless)
import Data.ByteString.Char8 (pack)
import Data.Maybe (isJust)
import System.Environment
import System.Exit (exitSuccess)
import Test.Tasty
import Types
import qualified API
import qualified System.Logger as Logger

main :: IO ()
main = do
    intTest <- lookupEnv "INTEGRATION_TEST"
    unless (isJust intTest) exitSuccess
    gd <- Gundeck <$> service "GUNDECK" Nothing
    ca <- Cannon  <$> service "CANNON"  Nothing
    br <- Brig    <$> service "BRIG"    Nothing
    gg <- Logger.new Logger.defSettings
    cs <- cassandra gg
    tests <- API.tests gd ca br cs
    defaultMain tests

service :: String -> Maybe Int -> IO (Request -> Request)
service n i = do
    h <- getEnv $ n ++ "_WEB_HOST"
    p <- getEnv $ n ++ maybe "" show i ++ "_WEB_PORT"
    return $ host (pack h) . port (read p) . header "Accept-Encoding" ""

cassandra :: Logger.Logger -> IO Cql.ClientState
cassandra g = do
    h <- getEnv "GUNDECK_CASSANDRA_HOST"
    p <- fromInteger . read <$> getEnv "GUNDECK_CASSANDRA_PORT"
    Cql.init g $ Cql.setPortNumber p
        . Cql.addContact h
        . Cql.setKeyspace (Cql.Keyspace "gundeck_test")
        $ Cql.defSettings
