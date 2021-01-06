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
-- import Bilge.Assert (Assertions, (!!!), (<!!), (===))
import Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.Except
import Control.Retry
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.TH
-- import Data.Aeson.Lens as Aeson
-- import qualified Data.ByteString as SBS
-- import qualified Data.ByteString.Base64.Lazy as EL
-- import Data.ByteString.Conversion
-- import Data.Handle (Handle (Handle))
import Data.Id
-- import Data.Misc (PlainTextPassword (..))
-- import Data.Proxy
-- import Data.Range
import Data.String.Conversions
-- import qualified Data.Text.Ascii as Ascii
-- import Data.Text.Encoding (encodeUtf8)
-- import Data.Time
-- import Data.UUID as UUID hiding (fromByteString, null)
-- import Data.UUID.V4 as UUID (nextRandom)
import qualified Data.Yaml as Yaml
-- import GHC.TypeLits

-- import qualified Network.Wai.Handler.Warp as Warp
-- import qualified Network.Wai.Handler.Warp.Internal as Warp

import Federator.Options
import Imports hiding (head)
import qualified Options.Applicative as OPA
import qualified System.Logger.Extended as Log
import System.Random (randomRIO)
import Test.Federator.Utilly
import Test.Hspec hiding (it, pending, pendingWith, xit)
import Util.Options

-- import Util.Types
-- import qualified Wire.API.User as User

type BrigReq = Request -> Request

type TestFederator = ReaderT TestEnv IO

instance MonadRandom TestFederator where
  getRandomBytes = lift . getRandomBytes

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

data IntegrationConfig = IntegrationConfig
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
    <$> ( OPA.strOption $
            OPA.long "integration-config"
              <> OPA.short 'i'
              <> OPA.help "Integration config to load"
              <> OPA.showDefault
              <> OPA.value defaultIntPath
        )
    <*> ( OPA.strOption $
            OPA.long "service-config"
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
  -- federatorCtxLogger <- Log.mkLogger (toLevel $ saml _teOpts ^. SAML.cfgLogLevel) (logNetStrings _teOpts) (logFormat _teOpts)
  let _teBrig = endpointToReq (cfgBrig _teTstOpts)
      -- _teFederator = endpointToReq (federator _teTstOpts)
      -- _teFederatorEnv = Federator.Env {..}
      federatorCtxOpts = _teOpts
      federatorCtxHttpManager = _teMgr
      federatorCtxHttpBrig = _teBrig empty
      federatorCtxRequestId = RequestId "<fake request id>"
  pure TestEnv {..}

destroyEnv :: HasCallStack => TestEnv -> IO ()
destroyEnv _ = pure ()

endpointToReq :: Endpoint -> (Bilge.Request -> Bilge.Request)
endpointToReq ep = Bilge.host (ep ^. epHost . to cs) . Bilge.port (ep ^. epPort)
