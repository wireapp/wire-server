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
import Util.Test

data IntegrationConfig = IntegrationConfig
  -- internal endpoints
  { gundeck :: Endpoint,
    cannon :: Endpoint,
    cannon2 :: Endpoint,
    brig :: Endpoint,
    redis2 :: RedisEndpoint
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
    ServiceConfigFile
      <$> strOption
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
      gConf <- handleParseError =<< decodeFileEither gFile
      iConf <- handleParseError =<< decodeFileEither iFile
      let g = GundeckR . mkRequest $ gundeck iConf
          c = CannonR . mkRequest $ cannon iConf
          c2 = CannonR . mkRequest $ cannon2 iConf
          b = BrigR . mkRequest $ brig iConf
          ch = gConf ^. optCassandra . casEndpoint . epHost
          cp = gConf ^. optCassandra . casEndpoint . epPort
          ck = gConf ^. optCassandra . casKeyspace
      lg <- Logger.new Logger.defSettings
      db <- defInitCassandra ck ch cp lg
      return $ TestSetup m g c c2 b db lg gConf (redis2 iConf)
    releaseOpts _ = return ()
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
