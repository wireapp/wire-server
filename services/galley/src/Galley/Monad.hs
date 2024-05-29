{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Galley.Monad where

import Bilge.IO hiding (options)
import Bilge.RPC
import Cassandra
import Control.Lens
import Control.Monad.Catch
import Galley.Env
import Imports hiding (log)
import Polysemy
import Polysemy.Input
import Prometheus
import System.Logger
import System.Logger.Class qualified as LC

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader Env,
      MonadThrow,
      MonadUnliftIO,
      MonadMonitor
    )

runApp :: Env -> App a -> IO a
runApp env = flip runReaderT env . unApp

instance HasRequestId App where
  getRequestId = App $ view reqId

instance MonadHttp App where
  handleRequestWithCont req h = do
    m <- view manager
    liftIO $ withResponse req m h

instance MonadClient App where
  liftClient m = do
    cs <- view cstate
    liftIO $ runClient cs m
  localState f = locally cstate f

instance LC.MonadLogger App where
  log lvl m = do
    env <- ask
    log (env ^. applog) lvl (reqIdMsg (env ^. reqId) . m)

embedApp ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  App a ->
  Sem r a
embedApp action = do
  env <- input
  embed $ runApp env action
