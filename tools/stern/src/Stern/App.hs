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
import Control.Error (ExceptT)
import Control.Lens (makeLenses, view, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.ByteString.Conversion (toByteString')
import Data.Id (RequestId (RequestId), UserId)
import Data.Text.Encoding (encodeUtf8)
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.Wai (Response, ResponseReceived)
import Network.Wai.Utilities (Error (..))
import Stern.Options as O
import System.Logger qualified as Log
import System.Logger.Class (Logger, MonadLogger (..), Msg, field, (.=), (~~))
import System.Logger.Class qualified as LC
import System.Logger.Extended qualified as Log
import Util.Options (host, port)

data Env = Env
  { _brig :: !Bilge.Request,
    _galley :: !Bilge.Request,
    _gundeck :: !Bilge.Request,
    _ibis :: !Bilge.Request,
    _galeb :: !Bilge.Request,
    _applog :: !Logger,
    _requestId :: !Bilge.RequestId,
    _httpManager :: !Bilge.Manager
  }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
  l <- Log.mkLogger (O.logLevel o) (O.logNetStrings o) (O.logFormat o)
  Env (mkRequest $ O.brig o) (mkRequest $ O.galley o) (mkRequest $ O.gundeck o) (mkRequest $ O.ibis o) (mkRequest $ O.galeb o) l (RequestId "N/A")
    <$> newManager
  where
    mkRequest s = Bilge.host (encodeUtf8 (s ^. host)) . Bilge.port (s ^. port) $ Bilge.empty
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
    g <- view applog
    r <- view requestId
    Log.log g l $ "request" .= Bilge.unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
  log l m = lift (LC.log l m)

instance (MonadIO m) => Bilge.MonadHttp (AppT m) where
  handleRequestWithCont req h = do
    m <- view httpManager
    liftIO $ Bilge.withResponse req m h

instance (Monad m) => HasRequestId (AppT m) where
  getRequestId = view requestId

instance HasRequestId (ExceptT e App) where
  getRequestId = view requestId

instance Bilge.MonadHttp (ExceptT e App) where
  handleRequestWithCont req h = do
    m <- view httpManager
    liftIO $ Bilge.withResponse req m h

runAppT :: Env -> AppT m a -> m a
runAppT e (AppT ma) = runReaderT ma e

-------------------------------------------------------------------------------
-- Handler Monad

type Handler = ExceptT Error App

type Continue m = Response -> m ResponseReceived

userMsg :: UserId -> Msg -> Msg
userMsg = field "user" . toByteString'
