{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main (main) where

import Bilge hiding (header)
import Cassandra.Util
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (getArgs)
import Test.Tasty
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API                 as User
import qualified API.Provider        as Provider
import qualified API.Search          as Search
import qualified API.Team            as Team
import qualified API.TURN            as TURN
import qualified API.User.Auth       as UserAuth
import qualified Brig.Options        as Opts
import qualified System.Logger       as Logger

data Config = Config
  -- internal endpoints
  { brig     :: Endpoint
  , cannon   :: Endpoint
  , galley   :: Endpoint
  -- external provider
  , provider :: Provider.Config
  } deriving (Show, Generic)

instance FromJSON Config

runTests :: Maybe Config -> Maybe Opts.Opts -> IO ()
runTests iConf bConf = do
    let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
    b <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
    c <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
    g <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
    turnFile <- optOrEnv (Opts.servers . Opts.turn) bConf id "TURN_SERVERS"
    casHost  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epHost) bConf pack "BRIG_CASSANDRA_HOST"
    casPort  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epPort) bConf read "BRIG_CASSANDRA_PORT"
    casKey   <- optOrEnv (\v -> (Opts.cassandra v)^.casKeyspace) bConf pack "BRIG_CASSANDRA_KEYSPACE"

    lg <- Logger.new Logger.defSettings
    db <- defInitCassandra casKey casHost casPort lg
    mg <- newManager tlsManagerSettings

    userApi     <- User.tests bConf mg b c g
    userAuthApi <- UserAuth.tests bConf mg lg b
    providerApi <- Provider.tests (provider <$> iConf) mg db b c g
    searchApis  <- Search.tests mg b
    teamApis    <- Team.tests bConf mg b c g
    turnApi     <- TURN.tests mg b turnFile

    defaultMain $ testGroup "Brig API Integration"
        [ userApi
        , userAuthApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        ]
  where
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

main :: IO ()
main = withOpenSSL $ do
  (iPath, bPath) <- parseConfigPaths
  iConf <- join $ handleParseError <$> decodeFileEither iPath
  bConf <- join $ handleParseError <$> decodeFileEither bPath

  runTests iConf bConf

parseConfigPaths :: IO (String, String)
parseConfigPaths = do
  args <- getArgs
  let desc = header "Brig Integration tests" <> fullDesc
      res = getParseResult $ execParserPure defaultPrefs (info (helper <*> pathParser) desc) args
  pure $ fromMaybe (defaultIntPath, defaultBrigPath) res
  where
    defaultBrigPath = "/etc/wire/brig/conf/brig.yaml"
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    pathParser :: Parser (String, String)
    pathParser = (,) <$>
                 (strOption $
                 long "integration-config-file"
                 <> short 'i'
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
