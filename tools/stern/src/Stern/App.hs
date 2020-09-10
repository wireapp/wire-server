{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Stern.App where

import qualified Bilge
import qualified Bilge.IO as Bilge (withResponse)
import Bilge.RPC (HasRequestId (..))
import Control.Error
import Control.Lens (makeLenses, set, view, (^.))
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (..), runReaderT)
import Data.ByteString.Conversion (toByteString')
import Data.Default (def)
import Data.Id (UserId)
import qualified Data.Metrics.Middleware as Metrics
import Data.Text.Encoding (encodeUtf8)
import Data.UUID (toString)
import qualified Data.UUID.V4 as UUID
import Imports
import Network.HTTP.Client (responseTimeoutMicro)
import Network.Wai (Request, ResponseReceived)
import Network.Wai.Routing (Continue)
import Network.Wai.Utilities (Error (..), lookupRequestId)
import qualified Network.Wai.Utilities.Error as WaiError
import Network.Wai.Utilities.Response (json, setStatus)
import qualified Network.Wai.Utilities.Server as Server
import Stern.Options as O
import qualified System.Logger as Log
import System.Logger.Class hiding (Error, info)
import qualified System.Logger.Class as LC
import qualified System.Logger.Extended as Log
import Util.Options

data Env = Env
  { _brig :: !Bilge.Request,
    _galley :: !Bilge.Request,
    _gundeck :: !Bilge.Request,
    _ibis :: !Bilge.Request,
    _galeb :: !Bilge.Request,
    _applog :: !Logger,
    _metrics :: !Metrics.Metrics,
    _requestId :: !Bilge.RequestId,
    _httpManager :: !Bilge.Manager
  }

makeLenses ''Env

newEnv :: Opts -> IO Env
newEnv o = do
  mt <- Metrics.metrics
  l <- Log.mkLogger (O.logLevel o) (O.logNetStrings o) (O.logFormat o)
  Env (mkRequest $ O.brig o) (mkRequest $ O.galley o) (mkRequest $ O.gundeck o) (mkRequest $ O.ibis o) (mkRequest $ O.galeb o) l mt
    <$> pure def
    <*> Bilge.newManager (Bilge.defaultManagerSettings {Bilge.managerResponseTimeout = responseTimeoutMicro 10000000})
  where
    mkRequest s = Bilge.host (encodeUtf8 (s ^. epHost)) . Bilge.port (s ^. epPort) $ Bilge.empty

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

instance (Functor m, MonadIO m) => MonadLogger (AppT m) where
  log l m = do
    g <- view applog
    r <- view requestId
    Log.log g l $ "request" .= Bilge.unRequestId r ~~ m

instance MonadLogger (ExceptT e App) where
  log l m = lift (LC.log l m)

instance MonadIO m => Bilge.MonadHttp (AppT m) where
  handleRequestWithCont req h = do
    m <- view httpManager
    liftIO $ Bilge.withResponse req m h

instance Monad m => HasRequestId (AppT m) where
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

runHandler :: Env -> Request -> Handler ResponseReceived -> Continue IO -> IO ResponseReceived
runHandler e r h k = do
  i <- reqId (lookupRequestId r)
  let e' = set requestId (Bilge.RequestId i) e
  a <- runAppT e' (runExceptT h)
  either (onError (view applog e) r k) return a
  where
    reqId (Just i) = return i
    reqId Nothing = do
      uuid <- UUID.nextRandom
      return $ toByteString' $ "stern-" ++ toString uuid

onError :: Logger -> Request -> Continue IO -> Error -> IO ResponseReceived
onError g r k e = do
  Server.logError g (Just r) e
  Server.flushRequestBody r
  k (setStatus (WaiError.code e) (json e))

userMsg :: UserId -> Msg -> Msg
userMsg = field "user" . toByteString'
