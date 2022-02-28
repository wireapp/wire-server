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

module Gundeck.Util.Redis where

import Control.Monad.Catch
import Control.Retry
import qualified Data.ByteString as BS
import Database.Redis
import Imports

retry :: (MonadIO m, MonadMask m) => RetryPolicyM m -> m a -> m a
retry x = recovering x handlers . const

x1 :: RetryPolicy
x1 = limitRetries 1 <> exponentialBackoff 100000

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

-- TODO  add tests
handlers :: Monad m => [a -> Handler m Bool]
handlers =
  [ const . Handler $ \case
      RedisSimpleError (Error msg) -> pure $ "READONLY" `BS.isPrefixOf` msg
      RedisTxError msg -> pure $ "READONLY" `isPrefixOf` msg
      _ -> pure $ False
  ]

-- [ const . Handler $ \(e :: RedisError) -> case e of
--     RedisError msg -> pure $ "READONLY" `BSL.isPrefixOf` msg
--     _ -> pure False,
--   const . Handler $ \(_ :: ConnectionError) -> pure True,
--   const . Handler $ \(_ :: Timeout) -> pure True,
--   const . Handler $ \case
--     TransactionAborted -> pure True
--     _ -> pure False
-- ]
--

-- Error -------------------------------------------------------------------

data RedisError
  = RedisSimpleError Reply
  | RedisTxAborted
  | RedisTxError String
  deriving (Show)

instance Exception RedisError

fromTxResult :: MonadThrow m => TxResult a -> m a
fromTxResult = \case
  TxSuccess a -> pure a
  TxAborted -> throwM RedisTxAborted
  TxError e -> throwM $ RedisTxError e
