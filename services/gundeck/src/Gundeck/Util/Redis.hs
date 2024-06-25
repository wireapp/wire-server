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
import Data.ByteString qualified as BS
import Database.Redis
import Imports
import System.Logger.Class (MonadLogger)
import System.Logger.Class qualified as Log
import System.Logger.Message ((.=), (~~))

retry :: (MonadIO m, MonadMask m, MonadLogger m) => RetryPolicyM m -> m a -> m a
retry x = recovering x handlers . const

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

handlers :: (MonadLogger m) => [a -> Handler m Bool]
handlers =
  [ const . Handler $ \case
      RedisSimpleError (Error err) -> pure $ "READONLY" `BS.isPrefixOf` err
      RedisTxError err -> pure $ "READONLY" `isPrefixOf` err
      err -> do
        Log.warn $
          Log.msg (Log.val "Redis error; not retrying.")
            ~~ "redis.errMsg" .= show err
        pure False
  ]

-- Error -------------------------------------------------------------------

data RedisError
  = RedisSimpleError Reply
  | RedisTxAborted
  | RedisTxError String
  deriving (Show)

instance Exception RedisError

fromTxResult :: (MonadThrow m) => TxResult a -> m a
fromTxResult = \case
  TxSuccess a -> pure a
  TxAborted -> throwM RedisTxAborted
  TxError e -> throwM $ RedisTxError e
