{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Federator.App
  ( AppT,
    Federator,
    runAppT,
  )
where

import Bilge (MonadHttp (..), RequestId (unRequestId), withResponse)
import Bilge.RPC (HasRequestId (..))
import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Except
import Federator.Env (Env, applog, httpManager, requestId)
import Imports
import Mu.Server (ServerError, ServerErrorIO)
import Servant.API.Generic ()
import Servant.Server ()
import System.Logger.Class as LC
import qualified System.Logger.Extended as Log

-- FUTUREWORK: this code re-occurs in every service.  introduce 'MkAppT' in types-common that
-- takes 'Env' as one more argument.
newtype AppT m a = AppT
  { unAppT :: ReaderT Env m a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env
    )

type Federator = AppT ServerErrorIO

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

instance MonadError ServerError (AppT ServerErrorIO) where
  throwError = lift . throwError @_ @ServerErrorIO
  catchError a f = do
    env <- ask
    lift $ catchError (runAppT env a) (runAppT env . f)

instance MonadTrans AppT where
  lift = AppT . lift

instance (Monad m, MonadIO m) => MonadHttp (AppT m) where
  handleRequestWithCont req handler = do
    manager <- view httpManager <$> ask
    liftIO $ withResponse req manager handler

runAppT :: forall m a. Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e
