{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Util.Test where

import Data.Tagged
import Imports
import Options.Applicative
import Test.Tasty.Options

newtype IntegrationConfigFile = IntegrationConfigFile String
  deriving (Eq, Ord, Typeable)

instance IsOption IntegrationConfigFile where
  defaultValue = IntegrationConfigFile "/etc/wire/integration/integration.yaml"
  parseValue = fmap IntegrationConfigFile . safeRead
  optionName = return "integration-config"
  optionHelp = return "Integration config file to read from"
  optionCLParser =
    fmap IntegrationConfigFile $
      strOption $
        ( short (untag (return 'i' :: Tagged IntegrationConfigFile Char))
            <> long (untag (optionName :: Tagged IntegrationConfigFile String))
            <> help (untag (optionHelp :: Tagged IntegrationConfigFile String))
        )

handleParseError :: (Show a) => Either a b -> IO (Maybe b)
handleParseError (Left err) = do
  putStrLn $ "Parse failed: " ++ show err ++ "\nFalling back to environment variables"
  pure Nothing
handleParseError (Right val) = pure $ Just val
