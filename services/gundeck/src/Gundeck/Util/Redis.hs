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

module Gundeck.Util.Redis where

import Control.Monad.Catch
import Control.Retry
import qualified Data.ByteString.Lazy as BSL
import Database.Redis.IO
import Imports

retry :: (MonadIO m, MonadMask m) => RetryPolicyM m -> m a -> m a
retry x = recovering x handlers . const

x1 :: RetryPolicy
x1 = limitRetries 1 <> exponentialBackoff 100000

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000

handlers :: Monad m => [a -> Handler m Bool]
handlers =
  [ const . Handler $ \(e :: RedisError) -> case e of
      RedisError msg -> pure $ "READONLY" `BSL.isPrefixOf` msg
      _ -> pure False,
    const . Handler $ \(_ :: ConnectionError) -> pure True,
    const . Handler $ \(_ :: Timeout) -> pure True,
    const . Handler $ \e ->
      case e of
        TransactionAborted -> pure True
        _ -> pure False
  ]
