{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bilge hiding (header)
import Cassandra.Util
import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.ByteString.Conversion
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.List (foldl', (\\))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import GHC.Generics
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (getArgs, withArgs)
import Test.Tasty
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API.Provider          as Provider
import qualified API.Search            as Search
import qualified API.Team              as Team
import qualified API.TURN              as TURN
import qualified API.User              as User
import qualified Brig.AWS              as AWS
import qualified Brig.Options          as Opts
import qualified Data.ByteString.Char8 as BS
import qualified System.Logger         as Logger

data Config = Config
  -- internal endpoints
  { brig      :: Endpoint
  , cannon    :: Endpoint
  , cargohold :: Endpoint
  , galley    :: Endpoint
  -- external provider
  , provider  :: Provider.Config
  } deriving (Show, Generic)

instance FromJSON Config

runTests :: Maybe Config -> Maybe Opts.Opts -> [String] -> IO ()
runTests iConf bConf otherArgs = do
    let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
    b  <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
    c  <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
    ch <- mkRequest <$> optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
    g  <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
    turnFile <- optOrEnv (Opts.servers . Opts.turn) bConf id "TURN_SERVERS"
    casHost  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epHost) bConf pack "BRIG_CASSANDRA_HOST"
    casPort  <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epPort) bConf read "BRIG_CASSANDRA_PORT"
    casKey   <- optOrEnv (\v -> (Opts.cassandra v)^.casKeyspace) bConf pack "BRIG_CASSANDRA_KEYSPACE"
    awsOpts  <- parseAWSEnv (Opts.aws <$> bConf)

    lg <- Logger.new Logger.defSettings
    db <- defInitCassandra casKey casHost casPort lg
    mg <- newManager tlsManagerSettings
    awsEnv <- AWS.mkEnv lg awsOpts mg

    userApi     <- User.tests bConf mg b c ch g awsEnv
    providerApi <- Provider.tests (provider <$> iConf) mg db b c g
    searchApis  <- Search.tests mg b
    teamApis    <- Team.tests bConf mg b c g awsEnv
    turnApi     <- TURN.tests mg b turnFile

    withArgs otherArgs . defaultMain $ testGroup "Brig API Integration"
        [ userApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        ]
  where
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

    -- Using config files only would certainly simplify this quite a bit
    parseAWSEnv :: Maybe Opts.AWSOpts -> IO (Opts.AWSOpts)
    parseAWSEnv (Just o) = return o
    parseAWSEnv Nothing  = do
        sqsEnd   <- optOrEnv (Opts.sqsEndpoint . Opts.aws)      bConf parseEndpoint "AWS_SQS_ENDPOINT"
        dynEnd   <- optOrEnv (Opts.dynamoDBEndpoint . Opts.aws) bConf parseEndpoint "AWS_DYNAMODB_ENDPOINT"
        sesEnd   <- optOrEnv (Opts.sesEndpoint . Opts.aws)      bConf parseEndpoint "AWS_SES_ENDPOINT"
        sesQueue <- optOrEnv (Opts.sesQueue . Opts.aws)         bConf pack          "AWS_USER_SES_QUEUE"
        sqsIntQ  <- optOrEnv (Opts.internalQueue . Opts.aws)    bConf pack          "AWS_USER_INTERNAL_QUEUE"
        sqsJrnlQ <- join <$> optOrEnvSafe (Opts.userJournalQueue . Opts.aws) bConf (Just . pack) "AWS_USER_JOURNAL_QUEUE"
        dynBlTbl <- optOrEnv (Opts.blacklistTable . Opts.aws)   bConf pack          "AWS_USER_BLACKLIST_TABLE"
        dynPkTbl <- optOrEnv (Opts.prekeyTable . Opts.aws)      bConf pack          "AWS_USER_PREKEYS_TABLE"
        return $ Opts.AWSOpts sesQueue sqsIntQ sqsJrnlQ dynBlTbl dynPkTbl sesEnd sqsEnd dynEnd

    parseEndpoint :: String -> AWSEndpoint
    parseEndpoint e = fromMaybe (error ("Not a valid AWS endpoint: " ++ show e))
                    $ fromByteString (BS.pack e)

main :: IO ()
main = withOpenSSL $ do
    -- For command line arguments to the configPaths and tasty parser not to interfere,
    -- split arguments into configArgs and otherArgs
    args <- getArgs
    let configArgs = getConfigArgs args
    let otherArgs = args \\ configArgs

    (iPath, bPath) <- withArgs configArgs parseConfigPaths
    iConf <- join $ handleParseError <$> decodeFileEither iPath
    bConf <- join $ handleParseError <$> decodeFileEither bPath

    runTests iConf bConf otherArgs
  where
    getConfigArgs args = reverse $ snd $ foldl' filterConfig (False, []) args

    filterConfig :: (Bool, [String]) -> String -> (Bool, [String])
    filterConfig (True, xs) a = (False, a:xs)
    filterConfig (False, xs) a = if configOption a then (True, a:xs) else (False, xs)

    configOption s = (s == "-i") || (s == "-s") || (s == "--integration-config") || (s == "--service-config")

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
                 long "integration-config"
                 <> short 'i'
                 <> help "Integration config to load"
                 <> showDefault
                 <> value defaultIntPath)
                 <*>
                 (strOption $
                 long "service-config"
                 <> short 's'
                 <> help "Brig application config to load"
                 <> showDefault
                 <> value defaultBrigPath)
