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

import qualified API
import Bilge hiding (body, header)
import Cassandra.Util
import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Tagged
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Gundeck.Options
import Imports hiding (local)
import qualified Metrics
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Options.Applicative
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.Options
import TestSetup
import Util.Options
import Util.Options.Common
import Util.Test

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { gundeck :: Endpoint,
    cannon :: Endpoint,
    cannon2 :: Endpoint,
    brig :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/gundeck/conf/gundeck.yaml"
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
main = withOpenSSL $ runTests go
  where
    go g i = withResource (getOpts g i) releaseOpts $ \opts ->
      testGroup
        "Gundeck"
        [ API.tests opts,
          Metrics.tests opts
        ]
    getOpts :: FilePath -> FilePath -> IO TestSetup
    getOpts gFile iFile = do
      m <-
        newManager
          tlsManagerSettings
            { managerResponseTimeout = responseTimeoutMicro 300000000
            }
      let local p = Endpoint {_epHost = "127.0.0.1", _epPort = p}
      gConf <- handleParseError =<< decodeFileEither gFile
      iConf <- handleParseError =<< decodeFileEither iFile
      g <- GundeckR . mkRequest <$> optOrEnv gundeck iConf (local . read) "GUNDECK_WEB_PORT"
      c <- CannonR . mkRequest <$> optOrEnv cannon iConf (local . read) "CANNON_WEB_PORT"
      c2 <- CannonR . mkRequest <$> optOrEnv cannon2 iConf (local . read) "CANNON2_WEB_PORT"
      b <- BrigR . mkRequest <$> optOrEnv brig iConf (local . read) "BRIG_WEB_PORT"
      ch <- optOrEnv (\v -> v ^. optCassandra . casEndpoint . epHost) gConf pack "GUNDECK_CASSANDRA_HOST"
      cp <- optOrEnv (\v -> v ^. optCassandra . casEndpoint . epPort) gConf read "GUNDECK_CASSANDRA_PORT"
      ck <- optOrEnv (\v -> v ^. optCassandra . casKeyspace) gConf pack "GUNDECK_CASSANDRA_KEYSPACE"
      lg <- Logger.new Logger.defSettings
      db <- defInitCassandra ck ch cp lg
      return $ TestSetup m g c c2 b db lg
    releaseOpts _ = return ()
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
