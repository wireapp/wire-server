module Main (main) where

import Imports hiding (local)

import Bilge hiding (header)
import Brig.API (sitemap)
import Cassandra.Util (defInitCassandra)
import Control.Lens
import Data.Aeson
import Data.ByteString.Conversion
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)
import Data.Yaml (decodeFileEither)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (withArgs)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options
import Util.Options.Common
import Util.Test

import qualified API.Provider          as Provider
import qualified API.Search            as Search
import qualified API.Team              as Team
import qualified API.TURN              as TURN
import qualified API.User              as User
import qualified API.Metrics           as Metrics
import qualified API.Settings          as Settings
import qualified Brig.AWS              as AWS
import qualified Brig.Options          as Opts
import qualified Data.ByteString.Char8 as BS
import qualified System.Logger         as Logger
import qualified UnliftIO.Temporary    as Temp

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

lookupTempDir :: IO FilePath
lookupTempDir = fromMaybe "/tmp" . getFirst . mconcat . map First
                <$> lookupEnv `mapM` ["TMP", "TEMP", "TMPDIR", "TEMPDIR"]

withTurnFiles :: (FilePath -> FilePath -> IO TestTree) -> IO TestTree
withTurnFiles act = do
    tmpdirRoot <- lookupTempDir
    Temp.withTempDirectory tmpdirRoot "wire.temp" $ \tempdir -> do
        let turn = tempdir </> "servers.txt"
        let turnv2 = tempdir </> "servers-v2.txt"
        writeFile turn   "turn:127.0.0.1:3478"
        writeFile turnv2 "turn:localhost:3478"
        act turn turnv2

runTests :: Maybe Config -> Maybe Opts.Opts -> [String] -> IO ()
runTests iConf bConf otherArgs = do
    -- TODO: Pass Opts instead of Maybe Opts through tests now that we no longer use ENV vars
    -- for config.
    -- Involves removing a bunch of 'optOrEnv' calls
    brigOpts <- maybe (fail "failed to parse options file") pure bConf
    let local p = Endpoint { _epHost = "127.0.0.1", _epPort = p }
    b  <- mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
    c  <- mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
    ch <- mkRequest <$> optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
    g  <- mkRequest <$> optOrEnv galley iConf (local . read) "GALLEY_WEB_PORT"
    casHost <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epHost) bConf pack "BRIG_CASSANDRA_HOST"
    casPort <- optOrEnv (\v -> (Opts.cassandra v)^.casEndpoint.epPort) bConf read "BRIG_CASSANDRA_PORT"
    casKey  <- optOrEnv (\v -> (Opts.cassandra v)^.casKeyspace) bConf pack "BRIG_CASSANDRA_KEYSPACE"
    awsOpts <- parseAWSEnv (Opts.aws <$> bConf)

    lg <- Logger.new Logger.defSettings  -- TODO: use mkLogger'?
    db <- defInitCassandra casKey casHost casPort lg
    mg <- newManager tlsManagerSettings
    emailAWSOpts <- parseEmailAWSOpts
    awsEnv <- AWS.mkEnv lg awsOpts emailAWSOpts mg

    userApi     <- User.tests bConf mg b c ch g awsEnv
    providerApi <- Provider.tests (provider <$> iConf) mg db b c g
    searchApis  <- Search.tests mg b
    teamApis    <- Team.tests bConf mg b c g awsEnv
    turnApi     <- withTurnFiles $ TURN.tests mg b
    metricsApi  <- Metrics.tests mg b
    settingsApi <- Settings.tests brigOpts mg b g

    withArgs otherArgs . defaultMain $ testGroup "Brig API Integration"
        [ testCase "sitemap" $ assertEqual "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Brig.API.sitemap brigOpts)
        , userApi
        , providerApi
        , searchApis
        , teamApis
        , turnApi
        , metricsApi
        , settingsApi
        ]
  where
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

    -- Using config files only would certainly simplify this quite a bit
    parseAWSEnv :: Maybe Opts.AWSOpts -> IO (Opts.AWSOpts)
    parseAWSEnv (Just o) = return o
    parseAWSEnv Nothing  = do
        sqsEnd   <- optOrEnv (Opts.sqsEndpoint . Opts.aws)      bConf parseEndpoint "AWS_SQS_ENDPOINT"
        dynEnd   <- optOrEnv (Opts.dynamoDBEndpoint . Opts.aws) bConf parseEndpoint "AWS_DYNAMODB_ENDPOINT"
        sqsJrnlQ <- join <$> optOrEnvSafe (Opts.userJournalQueue . Opts.aws) bConf (Just . pack) "AWS_USER_JOURNAL_QUEUE"
        dynPkTbl <- optOrEnv (Opts.prekeyTable . Opts.aws)      bConf pack          "AWS_USER_PREKEYS_TABLE"
        return $ Opts.AWSOpts sqsJrnlQ dynPkTbl sqsEnd dynEnd

    parseEndpoint :: String -> AWSEndpoint
    parseEndpoint e = fromMaybe (error ("Not a valid AWS endpoint: " ++ show e))
                    $ fromByteString (BS.pack e)

    parseEmailAWSOpts :: IO (Maybe Opts.EmailAWSOpts)
    parseEmailAWSOpts = case Opts.email . Opts.emailSMS <$> bConf of
        Just (Opts.EmailAWS aws) -> return (Just aws)
        Just (Opts.EmailSMTP  _) -> return Nothing
        _                        -> do
            sesEnd   <- parseEndpoint <$> getEnv "AWS_SES_ENDPOINT"
            sesQueue <- pack          <$> getEnv "AWS_USER_SES_QUEUE"
            return . Just $ Opts.EmailAWSOpts sesQueue sesEnd

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
