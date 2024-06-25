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

module Util.Options.Common
  ( module Cassandra.Helpers,
    module Util.Options.Common,
  )
where

import Cassandra.Helpers (toOptionFieldName)
import Imports hiding (reader)
import System.Posix.Env qualified as Posix

optOrEnv :: (a -> b) -> Maybe a -> (String -> b) -> String -> IO b
optOrEnv getter conf reader var = case conf of
  Nothing -> reader <$> getEnv var
  Just c -> pure $ getter c

optOrEnvSafe :: (a -> b) -> Maybe a -> (String -> b) -> String -> IO (Maybe b)
optOrEnvSafe getter conf reader var = case conf of
  Nothing -> fmap reader <$> Posix.getEnv var
  Just c -> pure $ Just (getter c)
