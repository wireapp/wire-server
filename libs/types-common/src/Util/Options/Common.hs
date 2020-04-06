{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

module Util.Options.Common where

import Data.Aeson.TH
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Imports hiding (reader)
import Options.Applicative
import qualified System.Posix.Env as Posix

-- | Convenient helper to convert record field names to use as YAML fields.
-- NOTE: We typically use this for options in the configuration files!
-- If you are looking into converting record field name to JSON to be used
-- over the API, look for toJSONFieldName in the Data.Json.Util module.
-- It removes the prefix (assumed to be anything before an uppercase
-- character) and lowers the first character
--
-- Example:
-- newtype TeamName = TeamName { tnTeamName :: Text }
-- deriveJSON toJSONFieldName ''tnTeamName
--
-- would generate {To/From}JSON instances where
-- the field name is "teamName"
toOptionFieldName :: Options
toOptionFieldName = defaultOptions {fieldLabelModifier = lowerFirst . dropPrefix}
  where
    lowerFirst :: String -> String
    lowerFirst (x : xs) = toLower x : xs
    lowerFirst [] = ""
    dropPrefix :: String -> String
    dropPrefix = dropWhile (not . isUpper)

optOrEnv :: (a -> b) -> (Maybe a) -> (String -> b) -> String -> IO b
optOrEnv getter conf reader var = case conf of
  Nothing -> reader <$> getEnv var
  Just c -> pure $ getter c

optOrEnvSafe :: (a -> b) -> Maybe a -> (String -> b) -> String -> IO (Maybe b)
optOrEnvSafe getter conf reader var = case conf of
  Nothing -> fmap reader <$> Posix.getEnv var
  Just c -> pure $ Just (getter c)

bytesOption :: Mod OptionFields String -> Parser ByteString
bytesOption = fmap C.pack . strOption

textOption :: Mod OptionFields String -> Parser Text
textOption = fmap T.pack . strOption
