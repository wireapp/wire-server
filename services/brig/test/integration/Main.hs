{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import Bilge (newManager, host, port, Request)
import Cassandra as Cql
import Cassandra.Settings as Cql
import Control.Monad (join)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (unpack, pack)
import Data.Word
import Data.Yaml (decodeFileEither, ParseException)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (getArgs)
import System.Logger (Logger)
import Test.Tasty

import qualified API                   as User
import qualified API.Provider          as Provider
import qualified API.Search            as Search
import qualified API.Team              as Team
import qualified API.TURN              as TURN
import qualified API.User.Auth         as UserAuth
import qualified Data.ByteString.Char8 as B
import qualified Brig.Options          as Opts
import qualified System.Logger         as Logger

data Endpoint = Endpoint
    { epHost :: String
    , epPort :: Word16
    } deriving (Show, Generic)

data Config = Config
  -- internal endpoints
  { confBrig      :: Endpoint
  , confCannon    :: Endpoint
  , confGalley    :: Endpoint

  , provider      :: Provider.Config
  } deriving (Show, Generic)

instance FromJSON Endpoint
instance FromJSON Config

decodeConfigFile :: (FromJSON c) => FilePath -> IO (Either ParseException c)
decodeConfigFile = decodeFileEither

runTests :: Maybe Config -> Maybe Opts.Opts -> IO ()
runTests iConf bConf = do
    let local p = Endpoint { epHost = "127.0.0.1", epPort = p }
    brig      <- mkRequest <$> Opts.optOrEnv confBrig iConf (local . read) "BRIG_WEB_PORT"
    cannon    <- mkRequest <$> Opts.optOrEnv confCannon iConf (local . read) "CANNON_WEB_PORT"
    galley    <- mkRequest <$> Opts.optOrEnv confGalley iConf (local . read) "GALLEY_WEB_PORT"
    turnFile  <- Opts.optOrEnv (Opts.servers . Opts.turn) bConf id "TURN_SERVERS"
    casHost   <- Opts.optOrEnv (Opts.host . Opts.endpoint . Opts.cassandra) bConf pack "BRIG_CASSANDRA_HOST"
    casPort   <- Opts.optOrEnv (Opts.port . Opts.endpoint . Opts.cassandra) bConf read "BRIG_CASSANDRA_PORT"

    lg <- Logger.new Logger.defSettings
    db <- initCassandra (Opts.Endpoint casHost casPort) lg
    mg <- newManager tlsManagerSettings

    userApi     <- User.tests bConf mg brig cannon galley
    userAuthApi <- UserAuth.tests bConf mg lg brig
    providerApi <- Provider.tests (provider <$> iConf) mg db brig cannon galley
    searchApis  <- Search.tests mg brig
    teamApis    <- Team.tests mg brig cannon galley
    turnApi     <- TURN.tests mg brig turnFile

    defaultMain $ testGroup "Brig API Integration"
        [ userApi
        , userAuthApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        ]

main :: IO ()
main = withOpenSSL $ do
  (iPath, bPath) <- parseConfigPaths
  iConf <- join $ handleParseError <$> decodeConfigFile iPath
  bConf <- join $ handleParseError <$> decodeConfigFile bPath

  runTests iConf bConf

initCassandra :: Opts.Endpoint -> Logger -> IO Cql.ClientState
initCassandra ep lg =
    Cql.init lg $ Cql.setPortNumber (fromIntegral $ Opts.port ep)
                . Cql.setContacts (unpack (Opts.host ep)) []
                . Cql.setKeyspace (Cql.Keyspace "brig_test")
                $ Cql.defSettings

mkRequest :: Endpoint -> Request -> Request
mkRequest (Endpoint h p) = host (B.pack h) . port p

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val

parseConfigPaths :: IO (String, String)
parseConfigPaths = do
  args <- getArgs
  let desc = header "Brig Integration tests" <> fullDesc
      res = getParseResult $ execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
  pure $ fromMaybe (defaultIntPath, defaultBrigPath) res
  where
    defaultBrigPath = "/etc/wire/brig/conf/brig.yaml"
    defaultIntPath = "/etc/wire/brig/conf/integration.yaml"
    pathParser :: Parser (String, String)
    pathParser = (,) <$>
                 (strOption $
                 long "integration-config-file"
                 <> short 'c'
                 <> help "Integration config to load"
                 <> showDefault
                 <> value defaultIntPath)
                 <*>
                 (strOption $
                 long "brig-config-file"
                 <> short 'c'
                 <> help "Brig application config to load"
                 <> showDefault
                 <> value defaultBrigPath)
