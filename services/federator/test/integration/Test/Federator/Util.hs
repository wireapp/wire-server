{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

-- FUTUREWORK: this is all copied from galley and spar. Maybe at some point reduce duplication?
module Test.Federator.Util where

import Bilge
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Data.Aeson.TH
import Data.String.Conversions
import qualified Data.Yaml as Yaml
import Federator.Options
import Imports hiding (head)
import qualified Options.Applicative as OPA
import Test.Federator.JSON
import Util.Options

type BrigReq = Request -> Request

newtype TestFederator m a = TestFederator {unwrapTestFederator :: ReaderT TestEnv m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadTrans,
      MonadReader TestEnv,
      MonadFail,
      MonadThrow,
      MonadCatch
    )

instance MonadRandom m => MonadRandom (TestFederator m) where
  getRandomBytes = lift . getRandomBytes

instance MonadIO m => MonadHttp (TestFederator m) where
  handleRequestWithCont req handler = do
    manager <- _teMgr <$> ask
    liftIO $ withResponse req manager handler

runTestFederator :: TestEnv -> TestFederator IO a -> IO a
runTestFederator env = flip runReaderT env . unwrapTestFederator

-- | See 'mkEnv' about what's in here.
data TestEnv = TestEnv
  { _teMgr :: Manager,
    _teBrig :: BrigReq,
    -- | federator config
    _teOpts :: Opts,
    -- | integration test config
    _teTstOpts :: IntegrationConfig
  }

type Select = TestEnv -> (Request -> Request)

newtype IntegrationConfig = IntegrationConfig
  { cfgBrig :: Endpoint
  }
  deriving (Show, Generic)

deriveFromJSON deriveJSONOptions ''IntegrationConfig

makeLenses ''TestEnv

-- | Call 'mkEnv' with options from config files.
mkEnvFromOptions :: IO TestEnv
mkEnvFromOptions = do
  let desc = "Federator - SSO Service Integration Test Suite"
  (integrationCfgFilePath, cfgFilePath) <- OPA.execParser (OPA.info (OPA.helper <*> cliOptsParser) (OPA.header desc <> OPA.fullDesc))
  integrationOpts :: IntegrationConfig <- Yaml.decodeFileEither integrationCfgFilePath >>= either (error . show) pure
  serviceOpts :: Opts <- Yaml.decodeFileEither cfgFilePath >>= either (throwIO . ErrorCall . show) pure
  mkEnv integrationOpts serviceOpts

-- | Accept config file locations as cli options.
cliOptsParser :: OPA.Parser (String, String)
cliOptsParser =
  (,)
    <$> OPA.strOption
      ( OPA.long "integration-config"
          <> OPA.short 'i'
          <> OPA.help "Integration config to load"
          <> OPA.showDefault
          <> OPA.value defaultIntPath
      )
    <*> OPA.strOption
      ( OPA.long "service-config"
          <> OPA.short 's'
          <> OPA.help "Federator application config to load"
          <> OPA.showDefault
          <> OPA.value defaultFederatorPath
      )
  where
    defaultIntPath = "/etc/wire/integration/integration.yaml"
    defaultFederatorPath = "/etc/wire/federator/conf/federator.yaml"

-- | Create an environment for integration tests from integration and federator config files.
mkEnv :: HasCallStack => IntegrationConfig -> Opts -> IO TestEnv
mkEnv _teTstOpts _teOpts = do
  _teMgr :: Manager <- newManager defaultManagerSettings
  let _teBrig = endpointToReq (cfgBrig _teTstOpts)
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv _ = pure ()

endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. epHost . to cs) . Bilge.port (ep ^. epPort)
