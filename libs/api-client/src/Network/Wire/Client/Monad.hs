{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wire.Client.Monad
  ( Server (..),
    setServer,
    Client,
    MonadClient (..),
    liftClient,
    asyncClient,
    runClient,
    ClientException (..),
  )
where

import Bilge
import Control.Concurrent.Async
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Imports hiding (log)
import Network.HTTP.Types
import qualified System.Logger as Logger
import System.Logger.Class

data Env
  = Env
      { clientServer :: Server,
        clientLogger :: Logger
      }

newtype Client a = Client (ReaderT Env IO a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader Env,
      MonadUnliftIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

data Server
  = Server
      { serverHost :: ByteString,
        serverPort :: Word16,
        serverWsHost :: Maybe ByteString,
        serverWsPort :: Maybe Word16,
        serverSSL :: Bool,
        serverManager :: Manager
      }

class (MonadHttp m, MonadLogger m, MonadIO m) => MonadClient m where
  getServer :: m Server
  getLogger :: m Logger

-- | Allows running a 'Client' inside a 'MonadClient'.
-- This is sometimes useful to regain a MonadUnliftIO instance
-- which is invalid for the 'MonadClient' itself.
liftClient :: MonadClient m => Client a -> m a
liftClient m = do
  s <- getServer
  l <- getLogger
  liftIO $ runClient s l m

instance MonadHttp Client where
  handleRequestWithCont req handler = do
    manager <- asks (serverManager . clientServer)
    liftIO $ withResponse req manager handler

instance MonadClient Client where
  getServer = Client $ asks clientServer
  getLogger = Client $ asks clientLogger

instance MonadLogger Client where
  log l m = getLogger >>= \lg -> Logger.log lg l m

setServer :: Server -> Request -> Request
setServer (Server h p _ _ s _) = host h . port p . (if s then secure else id)

asyncClient :: Client a -> Client (Async a)
asyncClient (Client c) = Client $ mapReaderT async c

runClient :: MonadIO m => Server -> Logger -> Client a -> m a
runClient s l (Client c) = liftIO $ runReaderT c (Env s l)

data ClientException
  = -- | An error response from the API (code + label + message)
    ErrorResponse Int Text Text
  | -- | An unexpected response from the API.
    UnexpectedResponse Status ResponseHeaders Text
  | -- | A response body could not be parsed.
    ParseError Text
  deriving (Show, Typeable)

instance Exception ClientException
