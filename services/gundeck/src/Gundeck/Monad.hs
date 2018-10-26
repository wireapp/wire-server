{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Gundeck.Monad
    ( -- * Environment
      Env
    , reqId
    , monitor
    , options
    , applog
    , manager
    , cstate
    , fbQueue
    , createEnv

      -- * Gundeck monad
    , Gundeck
    , runDirect
    , runGundeck
    , fromBody
    , ifNothing
    , posixTime
    ) where

import Imports
import Bilge hiding (Request, header, statusCode, options)
import Bilge.RPC
import Cassandra hiding (Error)
import Control.Error hiding (err)
import Control.Lens hiding ((.=))
import Control.Monad.Catch hiding (tryJust)
import Data.Aeson (FromJSON)
import Data.Misc (Milliseconds (..))
import Gundeck.Env
import Network.Wai
import Network.Wai.Utilities
import System.Logger.Class hiding (Error, info)

import qualified Database.Redis.IO as Redis
import qualified System.Logger     as Logger

-- | TODO: 'Client' already has an 'Env'.  Why do we need two?  How does this even work?  We should
-- probably explain this here.
newtype Gundeck a = Gundeck
    { unGundeck :: ReaderT Env Client a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader Env
               , MonadClient
               )

instance MonadUnliftIO Gundeck where
    askUnliftIO =
        Gundeck $ ReaderT $ \r ->
        withUnliftIO $ \u ->
        return (UnliftIO (unliftIO u . flip runReaderT r . unGundeck))

instance Redis.MonadClient Gundeck where
    liftClient m = view rstate >>= \p -> Redis.runRedis p m

instance MonadLogger Gundeck where
    log l m = do
        e <- ask
        Logger.log (e^.applog) l (reqIdMsg (e^.reqId) . m)

instance MonadHttp Gundeck where
    getManager = view manager

instance HasRequestId Gundeck where
    getRequestId = view reqId

runGundeck :: Env -> Request -> Gundeck ResponseReceived -> IO ResponseReceived
runGundeck e r m = runClient (e^.cstate) (runReaderT (unGundeck m) (e & reqId .~ lookupReqId r))

runDirect :: Env -> Gundeck a -> IO a
runDirect e m = runClient (e^.cstate) (runReaderT (unGundeck m) e)

lookupReqId :: Request -> RequestId
lookupReqId = maybe mempty RequestId . lookup requestIdName . requestHeaders
{-# INLINE lookupReqId #-}

fromBody :: FromJSON a => Request -> (LText -> Error) -> Gundeck a
fromBody r f = exceptT (throwM . f) return (parseBody r)
{-# INLINE fromBody #-}

ifNothing :: Error -> Maybe a -> Gundeck a
ifNothing e = maybe (throwM e) return
{-# INLINE ifNothing #-}

posixTime :: Gundeck Milliseconds
posixTime = view time >>= liftIO
{-# INLINE posixTime #-}
