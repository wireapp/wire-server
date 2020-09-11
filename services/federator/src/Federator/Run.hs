{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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

module Federator.Run
  ( run,
    mkApp,

    -- * App Environment
    newEnv,
    closeEnv,
  )
where

import Control.Lens ((^.))
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text (unpack)
import qualified Federator.Impl as Impl
import Federator.Options as Opt
import Federator.Types
import Imports
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Server as Server
import qualified System.Logger.Extended as Log
import Util.Options

run :: Opts -> IO ()
run opts = do
  (app, env) <- mkApp opts
  settings <- Server.newSettings (server env)
  Warp.runSettings settings app
  where
    endpoint = federator opts
    server env = defaultServer (unpack $ endpoint ^. epHost) (endpoint ^. epPort) (env ^. applog) (env ^. metrics)

mkApp :: Opts -> IO (Application, Env)
mkApp opts = do
  env <- newEnv opts
  pure (Impl.app env, env)

-------------------------------------------------------------------------------
-- Environment

newEnv :: Opts -> IO Env
newEnv o = do
  _metrics <- Metrics.metrics
  _applog <- Log.mkLogger (Opt.logLevel o) (Opt.logNetStrings o) (Opt.logFormat o)
  let _requestId = def
  return Env {..}

closeEnv :: Env -> IO ()
closeEnv e = do
  Log.flush $ e ^. applog
  Log.close $ e ^. applog
