{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gundeck.Util.Redis where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry
import Data.ByteString.Lazy (isPrefixOf)
import Data.Monoid
import Database.Redis.IO

retry :: (MonadIO m, MonadMask m) => RetryPolicyM m -> m a -> m a
retry x = recovering x handlers . const

x1 :: RetryPolicy
x1 = limitRetries 1 <> exponentialBackoff 100000

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

handlers :: Monad m => [a -> Handler m Bool]
handlers =
    [ const . Handler $ \(e :: RedisError) -> case e of
        RedisError msg -> pure $ "READONLY" `isPrefixOf` msg
        _              -> pure False
    , const . Handler $ \(_ :: ConnectionError) -> pure True
    , const . Handler $ \(_ ::         Timeout) -> pure True
    , const . Handler $ \e ->
        case e of
            TransactionAborted -> pure True
            _                  -> pure False
    ]
