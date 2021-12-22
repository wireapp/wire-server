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

module TestSetup
  ( test,
    tsManager,
    tsCargohold,
    TestSetup (..),
    Cargohold,
    TestM,
    viewCargohold,
  )
where

import Bilge hiding (body)
import Control.Lens
import Data.Text.Encoding
import Data.Yaml
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options
import Util.Options.Common
import Util.Test

type Cargohold = Request -> Request

type TestM = ReaderT TestSetup Http

data TestSetup = TestSetup
  { _tsManager :: Manager,
    _tsCargohold :: Cargohold
  }

makeLenses ''TestSetup

viewCargohold :: TestM Cargohold
viewCargohold = view tsCargohold

data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

runTestM :: FilePath -> TestM a -> IO a
runTestM configPath testM = do
  ts <- createTestSetup
  runHttpT (view tsManager ts) (runReaderT testM ts)
  where
    createTestSetup :: IO TestSetup
    createTestSetup = do
      -- TODO: It would actually be useful to read some
      -- values from cargohold (max bytes, for instance)
      -- so that tests do not need to keep those values
      -- in sync and the user _knows_ what they are
      m <-
        newManager
          tlsManagerSettings
            { managerResponseTimeout = responseTimeoutMicro 300000000
            }
      let localEndpoint p = Endpoint {_epHost = "127.0.0.1", _epPort = p}
      iConf <- handleParseError =<< decodeFileEither configPath
      cargo <- mkRequest <$> optOrEnv cargohold iConf (localEndpoint . read) "CARGOHOLD_WEB_PORT"
      return $ TestSetup m cargo

    mkRequest :: Endpoint -> Request -> Request
    mkRequest (Endpoint h p) = Bilge.host (encodeUtf8 h) . Bilge.port p

test :: FilePath -> TestName -> TestM () -> TestTree
test configPath n h = testCase n $ runTestM configPath h
