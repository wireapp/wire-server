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

module Gundeck.Monad
  ( -- * Environment
    Env,
    reqId,
    options,
    applog,
    manager,
    cstate,
    createEnv,

    -- * Gundeck monad
    Gundeck,
    runDirect,
    runGundeck,
    fromJsonBody,
    ifNothing,
    posixTime,

    -- * Select which redis to target
    runWithDefaultRedis,
    runWithAdditionalRedis,
  )
where

import Bilge hiding (Request, header, options, statusCode)
import Bilge.RPC
import Cassandra
import Control.Concurrent.Async (AsyncCancelled)
import Control.Error
import Control.Exception (throwIO)
import Control.Lens (view, (.~), (^.))
import Control.Monad.Catch hiding (tryJust)
import Data.Aeson (FromJSON)
import Data.Misc (Milliseconds (..))
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Database.Redis qualified as Redis
import Gundeck.Env
import Gundeck.Redis qualified as Redis
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Utilities
import Prometheus
import System.Logger qualified as Log
import System.Logger qualified as Logger
import System.Logger.Class
import UnliftIO (async)

-- | TODO: 'Client' already has an 'Env'.  Why do we need two?  How does this even work?  We should
-- probably explain this here.
newtype Gundeck a = Gundeck
  { unGundeck :: ReaderT Env Client a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadClient,
      MonadUnliftIO
    )

-- This can be derived if we resolve the TODO above.
instance MonadMonitor Gundeck where
  doIO = liftIO

-- | 'Gundeck' doesn't have an instance for 'MonadRedis' because it contains two
-- connections to two redis instances. When using 'WithDefaultRedis', any redis
-- operation will only target the default redis instance (configured under
-- 'redis:' in the gundeck config). To write to both redises use
-- 'WithAdditionalRedis'.
newtype WithDefaultRedis a = WithDefaultRedis {runWithDefaultRedis :: Gundeck a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadClient,
      MonadUnliftIO,
      MonadLogger
    )

instance Redis.MonadRedis WithDefaultRedis where
  liftRedis action = do
    defaultConn <- view rstate
    Redis.runRobust defaultConn action

instance Redis.RedisCtx WithDefaultRedis (Either Redis.Reply) where
  returnDecode :: Redis.RedisResult a => Redis.Reply -> WithDefaultRedis (Either Redis.Reply a)
  returnDecode = Redis.liftRedis . Redis.returnDecode

-- | 'Gundeck' doesn't have an instance for 'MonadRedis' because it contains two
-- connections to two redis instances. When using 'WithAdditionalRedis', any
-- redis operation will target both redis instances (configured under 'redis:'
-- and 'redisAddtionalWrite:' in the gundeck config). To write to only the
-- default redis use 'WithDefaultRedis'.
newtype WithAdditionalRedis a = WithAdditionalRedis {runWithAdditionalRedis :: Gundeck a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadReader Env,
      MonadClient,
      MonadUnliftIO,
      MonadLogger
    )

instance Redis.MonadRedis WithAdditionalRedis where
  liftRedis action = do
    defaultConn <- view rstate
    ret <- Redis.runRobust defaultConn action

    mAdditionalRedisConn <- view rstateAdditionalWrite
    for_ mAdditionalRedisConn $ \additionalRedisConn ->
      -- We just fire and forget this call, as there is not much we can do if
      -- this fails.
      async $ Redis.runRobust additionalRedisConn action

    pure ret

instance Redis.RedisCtx WithAdditionalRedis (Either Redis.Reply) where
  returnDecode :: Redis.RedisResult a => Redis.Reply -> WithAdditionalRedis (Either Redis.Reply a)
  returnDecode = Redis.liftRedis . Redis.returnDecode

instance MonadLogger Gundeck where
  log l m = do
    e <- ask
    Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

instance MonadHttp Gundeck where
  handleRequestWithCont req handler = do
    httpManager <- view manager
    liftIO $ withResponse req httpManager handler

instance HasRequestId Gundeck where
  getRequestId = view reqId

runGundeck :: Env -> Request -> Gundeck ResponseReceived -> IO ResponseReceived
runGundeck e r m = do
  rid <- lookupReqId e._applog r
  let e' = e & reqId .~ rid
  runDirect e' m

runDirect :: Env -> Gundeck a -> IO a
runDirect e m =
  runClient (e ^. cstate) (runReaderT (unGundeck m) e)
    `catch` ( \(exception :: SomeException) -> do
                case fromException exception of
                  Nothing ->
                    Log.err (e ^. applog) $
                      Log.msg ("IO Exception occurred" :: ByteString)
                        . Log.field "message" (displayException exception)
                        . Log.field "request" (unRequestId (e ^. reqId))
                  Just (_ :: AsyncCancelled) -> pure ()
                throwIO exception
            )

lookupReqId :: Logger -> Request -> IO RequestId
lookupReqId l r = case lookup requestIdName (requestHeaders r) of
  Just rid -> pure $ RequestId rid
  Nothing -> do
    localRid <- RequestId . UUID.toASCIIBytes <$> UUID.nextRandom
    Log.info l $
      "request-id" .= localRid
        ~~ "method" .= requestMethod r
        ~~ "path" .= rawPathInfo r
        ~~ msg (val "generated a new request id for local request")
    pure localRid

fromJsonBody :: FromJSON a => JsonRequest a -> Gundeck a
fromJsonBody r = exceptT (throwM . mkError status400 "bad-request") pure (parseBody r)
{-# INLINE fromJsonBody #-}

ifNothing :: Error -> Maybe a -> Gundeck a
ifNothing e = maybe (throwM e) pure
{-# INLINE ifNothing #-}

posixTime :: Gundeck Milliseconds
posixTime = view time >>= liftIO
{-# INLINE posixTime #-}
