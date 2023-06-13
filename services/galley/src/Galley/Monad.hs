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
import Control.Monad.Except
import Data.Id
import Data.Misc
import Galley.Env
import Galley.Options (optSettings, setDeleteConvThrottleMillis)
import Imports hiding (cs, log)
import qualified OpenSSL.Session as SSL
import Polysemy
import Polysemy.Input
import System.Logger
import qualified System.Logger.Class as LC

-- Keep a compatible App kind around so existing code doesn't break.
-- also pin the Reader input just to be safe.
type App = App' Env

unApp :: App' Env a -> ReaderT Env IO a
unApp = unApp'

runApp :: Env -> App a -> IO a
runApp env = flip runReaderT env . unApp

embedApp ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  App a ->
  Sem r a
embedApp = embedApp'

-- This newtype is very much like the original App, except that it isn't pinned to the Galley Env object.
-- A type alias keeps old code working while also allowing more flexible inputs for the Reader
newtype App' c a = App {unApp' :: ReaderT c IO a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadIO,
      MonadMask,
      MonadReader c,
      MonadThrow,
      MonadUnliftIO
    )

embedApp' ::
  forall c r a.
  ( Member (Embed IO) r,
    Member (Input c) r
  ) =>
  App' c a ->
  Sem r a
embedApp' action = do
  o <- input
  embed $ runReaderT (unApp' action) o

--
-- Classes and Instances
--
-- Keep all of the existing instances and add more so that we can be more flexible in
-- what we need to give to interpreters so we aren't stuck with just `Env`
-- This is so we can more easily call galley code in background worker.

instance HasRequestId' c => HasRequestId (App' c) where
  getRequestId = App $ view requestId

class HasManager c where
  manager' :: Lens' c Manager

instance HasManager Env where
  manager' = manager

instance HasManager c => MonadHttp (App' c) where
  handleRequestWithCont req h = do
    m <- view manager'
    liftIO $ withResponse req m h

class HasCassandra c where
  cassandra :: Lens' c ClientState

instance HasCassandra Env where
  cassandra = cstate

instance HasCassandra c => MonadClient (App' c) where
  liftClient m = do
    cs <- view cassandra
    liftIO $ runClient cs m
  localState f = locally cassandra f

class HasLogger c where
  logger' :: Lens' c Logger

class HasRequestId' c where
  requestId :: Lens' c RequestId

instance HasLogger Env where
  logger' = applog

instance HasRequestId' Env where
  requestId = reqId

instance (HasLogger c, HasRequestId' c) => LC.MonadLogger (App' c) where
  log lvl m = do
    c <- ask
    log (c ^. logger') lvl (reqIdMsg (c ^. requestId) . m)

class DeleteConvThrottle c where
  deleteConvThrottleMillis :: c -> Maybe Int

instance DeleteConvThrottle Env where
  deleteConvThrottleMillis = view (options . optSettings . setDeleteConvThrottleMillis)

class HasExtGetManager c where
  getExtGetManager :: c -> (Manager, [Fingerprint Rsa] -> SSL.SSL -> IO ())

instance HasExtGetManager Env where
  getExtGetManager = view (extEnv . extGetManager)
