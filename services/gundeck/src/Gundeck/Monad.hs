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
    monitor,
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
import Control.Concurrent.Async (async)
import Control.Error hiding (err)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (tryJust)
import Data.Aeson (FromJSON)
import Data.Default (def)
import Data.Misc (Milliseconds (..))
import qualified Database.Redis as Redis
import Gundeck.Env
import qualified Gundeck.Redis as Redis
import Imports
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Utilities
import qualified System.Logger as Logger
import System.Logger.Class hiding (Error, info)

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
      MonadUnliftIO
    )

instance Redis.MonadRedis WithDefaultRedis where
  liftRedis action = do
    defaultConn <- view rstate
    liftIO $ Redis.runRobust defaultConn action

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
    ret <- liftIO $ Redis.runRobust defaultConn action

    mAdditionalRedisConn <- view rstateAdditionalWrite
    liftIO . for_ mAdditionalRedisConn $ \additionalRedisConn ->
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
runGundeck e r m = runClient (e ^. cstate) (runReaderT (unGundeck m) (e & reqId .~ lookupReqId r))

runDirect :: Env -> Gundeck a -> IO a
runDirect e m = runClient (e ^. cstate) (runReaderT (unGundeck m) e)

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders
{-# INLINE lookupReqId #-}

fromJsonBody :: FromJSON a => JsonRequest a -> Gundeck a
fromJsonBody r = exceptT (throwM . mkError status400 "bad-request") pure (parseBody r)
{-# INLINE fromJsonBody #-}

ifNothing :: Error -> Maybe a -> Gundeck a
ifNothing e = maybe (throwM e) pure
{-# INLINE ifNothing #-}

posixTime :: Gundeck Milliseconds
posixTime = view time >>= liftIO
{-# INLINE posixTime #-}
