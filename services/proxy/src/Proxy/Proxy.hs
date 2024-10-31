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

module Proxy.Proxy (Proxy, appInProxy, runProxy) where

import Bilge.Request (requestIdName)
import Control.Lens hiding ((.=))
import Control.Monad.Catch
import Control.Monad.IO.Unlift ()
import Data.Id (RequestId (..))
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import Network.Wai
import Proxy.Env
import System.Logger qualified as Log
import System.Logger qualified as Logger
import System.Logger.Class hiding (Error, info)

newtype Proxy a = Proxy
  { unProxy :: ReaderT Env IO a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadUnliftIO
    )

instance MonadLogger Proxy where
  log l m = ask >>= \e -> Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

appInProxy :: Env -> Request -> Proxy ResponseReceived -> IO ResponseReceived
appInProxy e r m = do
  rid <- lookupReqId (e ^. applog) r
  runReaderT (unProxy m) (reqId .~ rid $ e)

runProxy :: Env -> Proxy a -> IO a
runProxy e m = runReaderT (unProxy m) e

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

lookupReqId :: Logger -> Request -> IO RequestId
lookupReqId l r = case lookup requestIdName (requestHeaders r) of
  Just rid -> pure $ RequestId rid
  Nothing -> do
    localRid <- RequestId . UUID.toASCIIBytes <$> UUID.nextRandom
    Log.info l $
      "request-id"
        .= localRid
        ~~ "method"
        .= requestMethod r
        ~~ "path"
        .= rawPathInfo r
        ~~ msg (val "generated a new request id for local request")
    pure localRid
