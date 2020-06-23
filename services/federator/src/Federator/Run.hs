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

    -- * App Monad
    AppT,
    AppIO,
    runAppT,
    runAppResourceT,
  )
where

import Bilge (RequestId (unRequestId))
import Bilge.RPC (HasRequestId (..))
import Control.Error
import Control.Lens ((^.), view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Trans.Resource
import Data.Default (def)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text (unpack)
import qualified Federator.App as App
import Federator.Options as Opt
import Federator.Types
import Imports
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Utilities.Server as Server
import System.Logger.Class as LC
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
  pure (App.app env, env)

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

-------------------------------------------------------------------------------
-- App Monad

-- FUTUREWORK: this code re-occurs in every service.  introduce 'MkAppT' in types-common that
-- takes 'Env' as one more argument.
newtype AppT m a = AppT
  { unAppT :: ReaderT Env m a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env
    )

type AppIO = AppT IO

instance MonadIO m => LC.MonadLogger (AppT m) where
  log l m = do
    g <- view applog
    r <- view requestId
    Log.log g l $ field "request" (unRequestId r) ~~ m

instance MonadIO m => LC.MonadLogger (ExceptT err (AppT m)) where
  log l m = lift (LC.log l m)

instance Monad m => HasRequestId (AppT m) where
  getRequestId = view requestId

instance MonadUnliftIO m => MonadUnliftIO (AppT m) where
  withRunInIO inner =
    AppT . ReaderT $ \r ->
      withRunInIO $ \runner ->
        inner (runner . flip runReaderT r . unAppT)

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

runAppResourceT :: ResourceT AppIO a -> AppIO a
runAppResourceT ma = do
  e <- ask
  liftIO . runResourceT $ transResourceT (runAppT e) ma
