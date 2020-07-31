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

import qualified API.V3
import Bilge hiding (body, header)
import qualified CargoHold.API (sitemap)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.WaiRoute (treeToPaths)
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml hiding (Parser)
import Imports hiding (local)
import qualified Metrics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.Wai.Utilities.Server (compile)
import Options.Applicative
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Options
import TestSetup
import Util.Options
import Util.Options.Common
import Util.Test

data IntegrationConfig = IntegrationConfig
  -- internal endpoint
  { cargohold :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/cargohold/conf/cargohold.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = return "service-config"
  optionHelp = return "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $
      strOption $
        ( short (untag (return 's' :: Tagged ServiceConfigFile Char))
            <> long (untag (optionName :: Tagged ServiceConfigFile String))
            <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

runTests :: (String -> String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $
  askOption $
    \(ServiceConfigFile c) ->
      askOption $ \(IntegrationConfigFile i) -> run c i
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy ServiceConfigFile),
          Option (Proxy :: Proxy IntegrationConfigFile)
        ] :
      defaultIngredients

main :: IO ()
main = runTests go
  where
    go c i = withResource (getOpts c i) releaseOpts $ \opts ->
      testGroup
        "Cargohold"
        [ testCase "sitemap" $
            assertEqual
              "inconcistent sitemap"
              mempty
              (pathsConsistencyCheck . treeToPaths . compile $ CargoHold.API.sitemap),
          API.V3.tests opts,
          Metrics.tests opts
        ]
    getOpts _ i = do
      -- TODO: It would actually be useful to read some
      -- values from cargohold (max bytes, for instance)
      -- so that tests do not need to keep those values
      -- in sync and the user _knows_ what they are
      m <-
        newManager
          tlsManagerSettings
            { managerResponseTimeout = responseTimeoutMicro 300000000
            }
      let local p = Endpoint {_epHost = "127.0.0.1", _epPort = p}
      iConf <- handleParseError =<< decodeFileEither i
      cargo <- mkRequest <$> optOrEnv cargohold iConf (local . read) "CARGOHOLD_WEB_PORT"
      return $ TestSetup m cargo
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
    releaseOpts _ = return ()
