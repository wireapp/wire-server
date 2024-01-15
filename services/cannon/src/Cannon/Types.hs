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

module Cannon.Types
  ( Env,
    mon,
    opts,
    applog,
    dict,
    env,
    logger,
    Cannon,
    mapConcurrentlyCannon,
    mkEnv,
    runCannon,
    runCannon',
    options,
    clients,
    monitor,
    wsenv,
    runCannonToServant,
  )
where

import Bilge (Manager, RequestId (..), requestIdName)
import Bilge.RPC (HasRequestId (..))
import Cannon.Dict (Dict)
import Cannon.Options
import Cannon.WS (Clock, Key, Websocket)
import Cannon.WS qualified as WS
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Metrics.Middleware
import Data.Text.Encoding
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import Network.Wai
import Servant qualified
import System.Logger qualified as Log
import System.Logger qualified as Logger
import System.Logger.Class hiding (info)
import System.Random.MWC (GenIO)

-----------------------------------------------------------------------------
-- Cannon monad

data Env = Env
  { mon :: !Metrics,
    opts :: !Opts,
    applog :: !Logger,
    dict :: !(Dict Key Websocket),
    reqId :: !RequestId,
    env :: !WS.Env
  }

newtype Cannon a = Cannon
  { unCannon :: ReaderT Env IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

mapConcurrentlyCannon :: Traversable t => (a -> Cannon b) -> t a -> Cannon (t b)
mapConcurrentlyCannon action inputs =
  Cannon $
    ask >>= \e ->
      liftIO $ mapConcurrently ((`runReaderT` e) . unCannon . action) inputs

instance MonadLogger Cannon where
  log l m = Cannon $ do
    g <- asks applog
    r <- field "request" . unRequestId <$> asks reqId
    liftIO $ Logger.log g l (r . m)

instance HasRequestId Cannon where
  getRequestId = Cannon $ asks reqId

mkEnv ::
  Metrics ->
  ByteString ->
  Opts ->
  Logger ->
  Dict Key Websocket ->
  Manager ->
  GenIO ->
  Clock ->
  Env
mkEnv m external o l d p g t =
  Env m o l d (RequestId "N/A") $
    WS.env external (o ^. cannon . port) (encodeUtf8 $ o ^. gundeck . host) (o ^. gundeck . port) l p d g t (o ^. drainOpts)

runCannon :: Env -> Cannon a -> Request -> IO a
runCannon e c r = do
  rid <- lookupReqId e.applog r
  let e' = e {reqId = rid}
  runCannon' e' c

runCannon' :: Env -> Cannon a -> IO a
runCannon' e c = runReaderT (unCannon c) e

lookupReqId :: Logger -> Request -> IO RequestId
lookupReqId l r = case lookup requestIdName (requestHeaders r) of
  Just rid -> pure $ RequestId rid
  Nothing -> do
    localRid <- RequestId . cs . UUID.toText <$> UUID.nextRandom
    Log.info l $
      "request-id" .= localRid
        ~~ "method" .= requestMethod r
        ~~ "path" .= rawPathInfo r
        ~~ msg (val "generated a new request id for local request")
    pure localRid

options :: Cannon Opts
options = Cannon $ asks opts

clients :: Cannon (Dict Key Websocket)
clients = Cannon $ asks dict

monitor :: Cannon Metrics
monitor = Cannon $ asks mon

wsenv :: Cannon WS.Env
wsenv = Cannon $ do
  e <- asks env
  r <- asks reqId
  pure $ WS.setRequestId r e

logger :: Cannon Logger
logger = Cannon $ asks applog

-- | Natural transformation from 'Cannon' to 'Handler' monad.
-- Used to call 'Cannon' from servant.
runCannonToServant :: Cannon.Types.Env -> Cannon x -> Servant.Handler x
runCannonToServant env c = liftIO $ runCannon' env c
