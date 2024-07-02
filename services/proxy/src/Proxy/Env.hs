{-# LANGUAGE TemplateHaskell #-}

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

module Proxy.Env
  ( Env,
    createEnv,
    destroyEnv,
    reqId,
    options,
    applog,
    manager,
    secrets,
  )
where

import Control.Lens (makeLenses, (^.))
import Data.Configurator
import Data.Configurator.Types
import Data.Id (RequestId (..))
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Proxy.Options
import System.Logger.Extended qualified as Logger

data Env = Env
  { _reqId :: !RequestId,
    _options :: !Opts,
    _applog :: !Logger.Logger,
    _manager :: !Manager,
    _secrets :: !Config,
    _loader :: !ThreadId
  }

makeLenses ''Env

createEnv :: Opts -> IO Env
createEnv o = do
  g <- Logger.mkLogger (o ^. logLevel) (o ^. logNetStrings) (o ^. logFormat)
  n <-
    newManager
      tlsManagerSettings
        { managerConnCount = o ^. httpPoolSize,
          managerIdleConnectionCount = 3 * (o ^. httpPoolSize),
          managerResponseTimeout = responseTimeoutMicro 5000000
        }
  let ac = AutoConfig 60 (reloadError g)
  (c, t) <- autoReload ac [Required $ o ^. secretsConfig]
  let rid = RequestId "N/A"
  pure $! Env rid o g n c t
  where
    reloadError g x =
      Logger.err g (Logger.msg $ Logger.val "Failed reloading config: " Logger.+++ show x)

destroyEnv :: Env -> IO ()
destroyEnv e = do
  killThread (e ^. loader)
  Logger.close (e ^. applog)
