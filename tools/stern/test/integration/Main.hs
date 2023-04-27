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
import Data.Aeson
import Data.Proxy
import Data.Tagged
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml (decodeFileEither)
import Imports hiding (local)
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
  { stern :: Endpoint,
    brig :: Endpoint,
    galley :: Endpoint
  }
  deriving (Show, Generic)

instance FromJSON IntegrationConfig

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/integration/integration.yaml"
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

runTests :: (String -> TestTree) -> IO ()
runTests run = defaultMainWithIngredients ings $
  askOption $
    \(ServiceConfigFile c) -> run c
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy ServiceConfigFile),
          Option (Proxy :: Proxy IntegrationConfigFile)
        ]
        : defaultIngredients

main :: IO ()
main = withOpenSSL $ runTests go
  where
    go i = withResource (getOpts i) releaseOpts $ \opts ->
      testGroup
        "Stern"
        [ API.tests opts
        ]
    getOpts :: FilePath -> IO TestSetup
    getOpts iFile = do
      m <-
        newManager
          tlsManagerSettings
            { managerResponseTimeout = responseTimeoutMicro 300000000
            }
      iConf <- handleParseError =<< decodeFileEither iFile
      let s = mkRequest $ stern iConf
      let b = mkRequest $ brig iConf
      let g = mkRequest $ galley iConf
      lg <- Logger.new Logger.defSettings
      pure $ TestSetup m s b g lg
    releaseOpts _ = pure ()
    mkRequest (Endpoint h p) = host (encodeUtf8 h) . port p
