{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bilge (newManager, host, port, Request)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Data.Word
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import System.Environment
import System.Logger (Logger)
import Test.Tasty

import qualified API           as User
import qualified API.Provider  as Provider
import qualified API.Search    as Search
import qualified API.Team      as Team
import qualified API.TURN      as TURN
import qualified API.User.Auth as UserAuth
import qualified System.Logger as Logger

main :: IO ()
main = lookupEnv "INTEGRATION_TEST" >>= maybe (return ()) (const run)
  where
    run = withOpenSSL $ do
        brig     <- mkEndpoint . read <$> getEnv "BRIG_WEB_PORT"
        cannon   <- mkEndpoint . read <$> getEnv "CANNON_WEB_PORT"
        galley   <- mkEndpoint . read <$> getEnv "GALLEY_WEB_PORT"
        turnFile <- getEnv "TURN_SERVERS"

        lg <- Logger.new Logger.defSettings
        db <- initCassandra lg
        mg <- newManager tlsManagerSettings

        userApi     <- User.tests mg brig cannon galley
        userAuthApi <- UserAuth.tests mg lg brig
        providerApi <- Provider.tests mg db brig cannon galley
        searchApis  <- Search.tests mg brig
        teamApis    <- Team.tests mg brig galley
        turnApi     <- TURN.tests mg brig turnFile

        defaultMain $ testGroup "Brig API Integration"
            [ userApi
            , userAuthApi
            , providerApi
            , searchApis
            , teamApis
            , turnApi
            ]

initCassandra :: Logger -> IO Cql.ClientState
initCassandra lg = do
    h <- getEnv "BRIG_CASSANDRA_HOST"
    p <- fromInteger . read <$> getEnv "BRIG_CASSANDRA_PORT"
    Cql.init lg $ Cql.setPortNumber p
                . Cql.addContact h
                . Cql.setKeyspace (Cql.Keyspace "brig_test")
                $ Cql.defSettings

mkEndpoint :: Word16 -> Request -> Request
mkEndpoint p = host "127.0.0.1" . port p

