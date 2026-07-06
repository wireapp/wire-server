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

-- | Working with remote queues (like Amazon SQS).
module Brig.Queue
  ( module Brig.Queue.Types,
    listen,
  )
where

import Brig.AWS qualified as AWS
import Brig.DeleteQueue.Interpreter (QueueEnv (..))
import Brig.Queue.Types
import Data.Aeson
import Imports

-- | Forever listen to messages coming from a queue and execute a callback
-- for each incoming message.
--
-- See documentation of underlying functions (e.g. 'AWS.listen') for
-- extra details.
listen ::
  ( Show a,
    FromJSON a,
    MonadUnliftIO m
  ) =>
  QueueEnv ->
  (a -> m ()) ->
  m ()
listen (SqsQueueEnv env throttleMillis queue) callback = do
  withRunInIO $ \lower -> AWS.execute env $ AWS.listen throttleMillis queue $ lower . callback
