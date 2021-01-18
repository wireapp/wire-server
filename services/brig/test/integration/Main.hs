-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Main
  ( main,
  )
where

import qualified API.Calling as Calling
import qualified API.Metrics as Metrics
import qualified API.Provider as Provider
import qualified API.Search as Search
import qualified API.Settings as Settings
import qualified API.Team as Team
import qualified API.TeamUserSearch as TeamUserSearch
import qualified API.User as User
import qualified API.UserPendingActivation as UserPendingActivation
import Bilge hiding (header)
import Brig.API (sitemap)
import qualified Brig.AWS as AWS
import qualified Brig.Options as Opts
import Cassandra.Util (defInitCassandra)
import Control.Lens
import Data.Aeson
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import qualified Federation.User
import Imports hiding (local)
import qualified Index.Create
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Utilities.Server (compile)
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Environment (withArgs)
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options
import Util.Test

data BackendConf = BackendConf
  { remoteBrig :: Endpoint,
    remoteFederator :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON BackendConf where
  parseJSON = withObject "BackendConf" $ \o ->
    BackendConf
      <$> o .: "brig"
      <*> o .: "federator"

data Config = Config
  -- internal endpoints
  { brig :: Endpoint,
    cannon :: Endpoint,
    cargohold :: Endpoint,
    federator :: Endpoint,
    galley :: Endpoint,
    nginz :: Endpoint,
    spar :: Endpoint,
    -- external provider
    provider :: Provider.Config,
    -- for federation
    backendTwo :: BackendConf
  }
  deriving (Show, Generic)

instance FromJSON Config

runTests :: Config -> Opts.Opts -> [String] -> IO ()
runTests iConf brigOpts otherArgs = do
  let b = mkRequest $ brig iConf
      c = mkRequest $ cannon iConf
      ch = mkRequest $ cargohold iConf
      g = mkRequest $ galley iConf
      n = mkRequest $ nginz iConf
      s = mkRequest $ spar iConf
      f = federator iConf
      brigTwo = mkRequest $ remoteBrig (backendTwo iConf)

  let turnFile = Opts.servers . Opts.turn $ brigOpts
      turnFileV2 = (Opts.serversV2 . Opts.turn) brigOpts
      casHost = (\v -> (Opts.cassandra v) ^. casEndpoint . epHost) brigOpts
      casPort = (\v -> (Opts.cassandra v) ^. casEndpoint . epPort) brigOpts
      casKey = (\v -> (Opts.cassandra v) ^. casKeyspace) brigOpts
      awsOpts = Opts.aws brigOpts
  lg <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  db <- defInitCassandra casKey casHost casPort lg
  mg <- newManager tlsManagerSettings
  emailAWSOpts <- parseEmailAWSOpts
  awsEnv <- AWS.mkEnv lg awsOpts emailAWSOpts mg
  userApi <- User.tests brigOpts mg b c ch g n awsEnv
  providerApi <- Provider.tests (provider iConf) mg db b c g
  searchApis <- Search.tests brigOpts mg g b
  teamApis <- Team.tests brigOpts mg n b c g awsEnv
  turnApi <- Calling.tests mg b brigOpts turnFile turnFileV2
  metricsApi <- Metrics.tests mg b
  settingsApi <- Settings.tests brigOpts mg b g
  createIndex <- Index.Create.spec brigOpts
  browseTeam <- TeamUserSearch.tests brigOpts mg g b
  userPendingActivation <- UserPendingActivation.tests brigOpts mg db b g s
  federationUser <- Federation.User.spec brigOpts mg b f brigTwo
  withArgs otherArgs . defaultMain $
    testGroup
      "Brig API Integration"
      [ testCase "sitemap" $
          assertEqual
            "inconcistent sitemap"
            mempty
            (pathsConsistencyCheck . treeToPaths . compile $ Brig.API.sitemap brigOpts),
        userApi,
        providerApi,
        searchApis,
        teamApis,
        turnApi,
        metricsApi,
        settingsApi,
        createIndex,
        userPendingActivation,
        browseTeam,
        federationUser
      ]
  where
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p

    parseEmailAWSOpts :: IO (Maybe Opts.EmailAWSOpts)
    parseEmailAWSOpts = case Opts.email . Opts.emailSMS $ brigOpts of
      (Opts.EmailAWS aws) -> return (Just aws)
      (Opts.EmailSMTP _) -> return Nothing

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
  brigOpts <- maybe (fail "failed to parse brig options file") pure bConf
  integrationConfig <- maybe (fail "failed to parse integration.yaml file") pure iConf
  runTests integrationConfig brigOpts otherArgs
  where
    getConfigArgs args = reverse $ snd $ foldl' filterConfig (False, []) args
    filterConfig :: (Bool, [String]) -> String -> (Bool, [String])
    filterConfig (True, xs) a = (False, a : xs)
    filterConfig (False, xs) a = if configOption a then (True, a : xs) else (False, xs)
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
    pathParser =
      (,)
        <$> ( strOption $
                long "integration-config"
                  <> short 'i'
                  <> help "Integration config to load"
                  <> showDefault
                  <> value defaultIntPath
            )
        <*> ( strOption $
                long "service-config"
                  <> short 's'
                  <> help "Brig application config to load"
                  <> showDefault
                  <> value defaultBrigPath
            )
