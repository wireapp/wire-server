{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

module Stern.App where

import Bilge qualified
import Bilge.RPC (HasRequestId (..))
import Control.Error
import Control.Lens (lensField, lensRules, makeLensesWith, (.~))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Text.Encoding (encodeUtf8)
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.Wai (Response, ResponseReceived)
import Network.Wai.Utilities (Error (..))
import Stern.Options as Opts
import System.Logger qualified as Log
import System.Logger.Class hiding (Error, info)
import System.Logger.Class qualified as LC
import System.Logger.Extended qualified as Log
import Util.Options
import Util.SuffixNamer

data Env = Env
  { brig :: !Bilge.Request,
    galley :: !Bilge.Request,
    gundeck :: !Bilge.Request,
    ibis :: !Bilge.Request,
    galeb :: !Bilge.Request,
    appLogger :: !Logger,
    requestId :: !Bilge.RequestId,
    httpManager :: !Bilge.Manager
  }

makeLensesWith (lensRules & lensField .~ suffixNamer) ''Env

newEnv :: Opts -> IO Env
newEnv opts = do
  l <- Log.mkLogger opts.logLevel opts.logNetStrings opts.logFormat
  Env
    (mkRequest opts.brig)
    (mkRequest opts.galley)
    (mkRequest opts.gundeck)
    (mkRequest opts.ibis)
    (mkRequest opts.galeb)
    l
    (RequestId "N/A")
    <$> newManager
  where
    mkRequest s = Bilge.host (encodeUtf8 s.host) . Bilge.port s.port $ Bilge.empty
    newManager = Bilge.newManager (Bilge.defaultManagerSettings {Bilge.managerResponseTimeout = responseTimeoutMicro 10000000})

-- Monads
newtype AppT m a = AppT (ReaderT Env m a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadReader Env
    )

deriving instance MonadUnliftIO App

type App = AppT IO

instance (MonadIO m) => MonadLogger (AppT m) where
  log l m = do
    g <- asks (.appLogger)
    r <- asks (.requestId)
    Log.log g l $ "request" .= Bilge.unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
  log l m = lift (LC.log l m)

instance (MonadIO m) => Bilge.MonadHttp (AppT m) where
  handleRequestWithCont req h = do
    m <- asks (.httpManager)
    liftIO $ Bilge.withResponse req m h

instance (Monad m) => HasRequestId (AppT m) where
  getRequestId = asks (.requestId)

instance HasRequestId (ExceptT e App) where
  getRequestId = asks (.requestId)

instance Bilge.MonadHttp (ExceptT e App) where
  handleRequestWithCont req h = do
    m <- asks (.httpManager)
    liftIO $ Bilge.withResponse req m h

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

-------------------------------------------------------------------------------
-- Handler Monad

type Handler = ExceptT Error App

runHandler :: Env -> Handler a -> IO (Either Error a)
runHandler env = runAppT env . runExceptT

type Continue m = Response -> m ResponseReceived

userMsg :: UserId -> Msg -> Msg
userMsg = field "user" . toByteString'
