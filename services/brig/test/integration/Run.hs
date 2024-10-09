-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Run
  ( main,
  )
where

import API.Calling qualified as Calling
import API.Federation qualified
import API.Internal qualified
import API.Metrics qualified as Metrics
import API.OAuth qualified
import API.Provider qualified as Provider
import API.Search qualified as Search
import API.Settings qualified as Settings
import API.SystemSettings qualified as SystemSettings
import API.Team qualified as Team
import API.TeamUserSearch qualified as TeamUserSearch
import API.User qualified as User
import API.UserPendingActivation qualified as UserPendingActivation
import Bilge hiding (header, host, port)
import Bilge qualified
import Brig.AWS qualified as AWS
import Brig.App (initHttpManagerWithTLSConfig)
import Brig.Options qualified as Opts
import Cassandra.Util (defInitCassandra)
import Control.Lens
import Data.Aeson
import Data.ByteString.Char8 qualified as B8
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Federation.End2end qualified
import Imports hiding (local)
import Index.Create qualified
import Network.HTTP.Client qualified as HTTP
import Network.URI (pathSegments)
import OpenSSL (withOpenSSL)
import Options.Applicative hiding (action)
import SMTP qualified
import System.Environment (withArgs)
import System.Logger qualified as Logger
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Runners
import Test.Tasty.Runners.AntXML
import Util
import Util.Options
import Util.Test
import Util.Test.SQS qualified as SQS
import Web.HttpApiData
import Wire.API.Federation.API
import Wire.API.Routes.Version

data BackendConf = BackendConf
  { remoteBrig :: Endpoint,
    remoteGalley :: Endpoint,
    remoteCargohold :: Endpoint,
    remoteCannon :: Endpoint,
    remoteFederatorInternal :: Endpoint,
    remoteFederatorExternal :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON BackendConf where
  parseJSON = withObject "BackendConf" $ \o ->
    BackendConf
      <$> o .: "brig"
      <*> o .: "galley"
      <*> o .: "cargohold"
      <*> o .: "cannon"
      <*> o .: "federatorInternal"
      <*> o .: "federatorExternal"

data Config = Config
  -- internal endpoints
  { brig :: Endpoint,
    cannon :: Endpoint,
    gundeck :: Endpoint,
    cargohold :: Endpoint,
    federatorInternal :: Endpoint,
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
  let b = mkVersionedRequest $ brig iConf
      c = mkVersionedRequest $ cannon iConf
      gd = mkVersionedRequest $ gundeck iConf
      ch = mkVersionedRequest $ cargohold iConf
      g = mkVersionedRequest $ galley iConf
      n = mkVersionedRequest $ nginz iConf
      s = mkVersionedRequest $ spar iConf
      f = federatorInternal iConf
      brigTwo = mkVersionedRequest $ remoteBrig (backendTwo iConf)
      cannonTwo = mkVersionedRequest $ remoteCannon (backendTwo iConf)
      galleyTwo = mkVersionedRequest $ remoteGalley (backendTwo iConf)
      ch2 = mkVersionedRequest $ remoteCargohold (backendTwo iConf)

  let Opts.TurnServersFiles turnFile turnFileV2 = case Opts.serversSource $ Opts.turn brigOpts of
        Opts.TurnSourceFiles files -> files
        Opts.TurnSourceDNS _ -> error "The integration tests can only be run when TurnServers are sourced from files"
      localDomain = brigOpts.settings.federationDomain
      awsOpts = Opts.aws brigOpts
  lg <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  db <- defInitCassandra (brigOpts.cassandra) lg
  mg <- initHttpManagerWithTLSConfig False Nothing
  let fedBrigClient = FedClient @'Brig mg (brig iConf)
  emailAWSOpts <- parseEmailAWSOpts
  awsEnv <- AWS.mkEnv lg awsOpts emailAWSOpts mg
  mUserJournalWatcher <- for (Opts.userJournalQueue awsOpts) $ SQS.watchSQSQueue (view AWS.amazonkaEnv awsEnv)
  userApi <- User.tests brigOpts fedBrigClient mg b c ch g n awsEnv db mUserJournalWatcher
  providerApi <- Provider.tests localDomain (provider iConf) mg db b c g n
  searchApis <- Search.tests brigOpts mg g b
  teamApis <- Team.tests brigOpts mg n b c g mUserJournalWatcher
  turnApi <- Calling.tests mg b brigOpts turnFile turnFileV2
  metricsApi <- Metrics.tests mg brigOpts b
  systemSettingsApi <- SystemSettings.tests brigOpts mg
  settingsApi <- Settings.tests brigOpts mg b g
  createIndex <- Index.Create.spec brigOpts
  browseTeam <- TeamUserSearch.tests brigOpts mg g b
  userPendingActivation <- UserPendingActivation.tests brigOpts mg db b g s
  federationEnd2End <- Federation.End2end.spec brigOpts mg b g ch c f brigTwo galleyTwo ch2 cannonTwo
  federationEndpoints <- API.Federation.tests mg brigOpts b fedBrigClient
  internalApi <- API.Internal.tests brigOpts mg db b (brig iConf) gd g

  let smtp = SMTP.tests mg lg
      oauthAPI = API.OAuth.tests mg db b n brigOpts

  withArgs otherArgs . defaultMainWithIngredients (listingTests : (composeReporters antXMLRunner consoleTestReporter) : defaultIngredients)
    $ testGroup
      "Brig API Integration"
    $ [ userApi,
        providerApi,
        searchApis,
        teamApis,
        turnApi,
        metricsApi,
        systemSettingsApi,
        settingsApi,
        createIndex,
        userPendingActivation,
        browseTeam,
        federationEndpoints,
        internalApi,
        smtp,
        oauthAPI,
        federationEnd2End
      ]
  where
    mkRequest (Endpoint h p) = Bilge.host (encodeUtf8 h) . Bilge.port p

    mkVersionedRequest ep = maybeAddPrefix . mkRequest ep

    maybeAddPrefix :: Request -> Request
    maybeAddPrefix r = case pathSegments $ getUri r of
      ("i" : _) -> r
      ("api-internal" : _) -> r
      _ -> addPrefix r

    addPrefix :: Request -> Request
    addPrefix r = r {HTTP.path = toHeader latestVersion <> "/" <> removeSlash (HTTP.path r)}
      where
        removeSlash s = case B8.uncons s of
          Just ('/', s') -> s'
          _ -> s
        latestVersion :: Version
        latestVersion = maxBound

    parseEmailAWSOpts :: IO (Maybe Opts.EmailAWSOpts)
    parseEmailAWSOpts = case Opts.email . Opts.emailSMS $ brigOpts of
      (Opts.EmailAWS aws) -> pure (Just aws)
      (Opts.EmailSMTP _) -> pure Nothing

main :: IO ()
main = withOpenSSL $ do
  -- For command line arguments to the configPaths and tasty parser not to interfere,
  -- split arguments into configArgs and otherArgs
  args <- getArgs
  let configArgs = getConfigArgs args
  let otherArgs = args \\ configArgs
  (iPath, bPath) <- withArgs configArgs parseConfigPaths
  iConf <- handleParseError =<< decodeFileEither iPath
  bConf <- handleParseError =<< decodeFileEither bPath
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
        <$> strOption
          ( long "integration-config"
              <> short 'i'
              <> help "Integration config to load"
              <> showDefault
              <> value defaultIntPath
          )
        <*> strOption
          ( long "service-config"
              <> short 's'
              <> help "Brig application config to load"
              <> showDefault
              <> value defaultBrigPath
          )
