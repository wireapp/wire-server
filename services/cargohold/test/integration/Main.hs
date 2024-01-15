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
import qualified App
import Data.Proxy
import Data.Tagged
import Imports hiding (local)
import Options.Applicative
import Test.Tasty
import Test.Tasty.Ingredients
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Runners.AntXML
import TestSetup
import Util.Test

newtype ServiceConfigFile = ServiceConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption ServiceConfigFile where
  defaultValue = ServiceConfigFile "/etc/wire/cargohold/conf/cargohold.yaml"
  parseValue = fmap ServiceConfigFile . safeRead
  optionName = pure "service-config"
  optionHelp = pure "Service config file to read from"
  optionCLParser =
    fmap ServiceConfigFile $
      strOption $
        ( short (untag (pure 's' :: Tagged ServiceConfigFile Char))
            <> long (untag (optionName :: Tagged ServiceConfigFile String))
            <> help (untag (optionHelp :: Tagged ServiceConfigFile String))
        )

main :: IO ()
main = do
  defaultMainWithIngredients ings $
    askOption $ \(IntegrationConfigFile configPath) ->
      askOption $ \(ServiceConfigFile optsPath) ->
        -- we treat the configuration file as a tasty "resource", so that we can
        -- read it once before all tests
        withResource
          (createTestSetup optsPath configPath)
          (const (pure ()))
          $ \ts ->
            testGroup
              "Cargohold"
              [ API.tests ts,
                App.tests ts
              ]
  where
    ings =
      includingOptions
        [ Option (Proxy :: Proxy ServiceConfigFile),
          Option (Proxy :: Proxy IntegrationConfigFile)
        ]
        : listingTests
        : composeReporters antXMLRunner consoleTestReporter
        : defaultIngredients
