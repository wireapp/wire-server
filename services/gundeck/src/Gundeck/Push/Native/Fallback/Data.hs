{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Push.Native.Fallback.Data where

import Imports
import Cassandra
import Data.Id (UserId)
import Gundeck.Types.Notification

cancel :: MonadClient m => UserId -> NotificationId -> Int32 -> m ()
cancel u n t = write insert (params Quorum (u, n, t))
             & retry (adjustConsistency One x5) -- See note [consistency]
  where
    insert :: PrepQuery W (UserId, NotificationId, Int32) ()
    insert = "INSERT INTO fallback_cancel (user, id) VALUES (?, ?) USING TTL ?"

isCancelled :: MonadClient m => UserId -> NotificationId -> m Bool
isCancelled u n = isJust <$> query1 select (params Quorum (u, n))
                & retry (adjustConsistency One x1) -- See note [consistency]
  where
    select :: PrepQuery R (UserId, NotificationId) (Identity TimeUuid)
    select = "SELECT id FROM fallback_cancel WHERE user = ? AND id = ?"

-- Note [consistency]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We prefer availability over consistency on retries. This might lead to
-- duplicate notifications due to lost writes or stale reads, but that seems
-- preferable over failing entirely.
