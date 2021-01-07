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
    AppIO,
    runAppT,
    runAppResourceT,
  )
where

import Bilge (RequestId (unRequestId))
import Bilge.RPC (HasRequestId (..))
import Control.Lens (view)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Resource (ResourceT, runResourceT, transResourceT)
import Federator.Types (Env, applog, requestId)
import Imports
import Mu.Server (ServerError)
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

-- TODO dear reviewer, I don't know what I'm doing here
-- but this instance made 'singleService' (which seems to need an instance of MonadError for ServerError) calls in Run.hs compile again
instance (MonadThrow m, MonadCatch m) => MonadError ServerError (AppT m) where
  throwError = throwM
  catchError = catch

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

runAppResourceT :: ResourceT AppIO a -> AppIO a
runAppResourceT ma = do
  e <- ask
  liftIO . runResourceT $ transResourceT (runAppT e) ma
