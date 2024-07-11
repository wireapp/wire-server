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

import API qualified
import Bilge hiding (body, header, host, port)
import Bilge qualified
import Cassandra.Util
import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Gundeck.Options
import Imports hiding (local)
import Metrics qualified
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.TLS
import OpenSSL (withOpenSSL)
import Options.Applicative
import System.Logger qualified as Logger
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Runners.AntXML
import TestSetup
import Util.Options
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
  optionName = pure "service-config"
  optionHelp = pure "Service config file to read from"
  optionCLParser =
    ServiceConfigFile
      <$> strOption
        ( short (untag (pure 's' :: Tagged ServiceConfigFile Char))
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
        ]
        : listingTests
        : composeReporters antXMLRunner consoleTestReporter
        : defaultIngredients

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
      let g = GundeckR $ mkRequest iConf.gundeck
          c = CannonR . mkRequest $ cannon iConf
          c2 = CannonR . mkRequest $ cannon2 iConf
          b = BrigR $ mkRequest iConf.brig
      lg <- Logger.new Logger.defSettings
      db <- defInitCassandra (gConf ^. cassandra) lg
      pure $ TestSetup m g c c2 b db lg gConf
    releaseOpts _ = pure ()
    mkRequest (Endpoint h p) = Bilge.host (encodeUtf8 h) . Bilge.port p
