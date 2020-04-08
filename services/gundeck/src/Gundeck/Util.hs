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

module Gundeck.Util where

import Control.Monad.Catch
import Control.Retry
import Data.Id
import Data.UUID.V1
import Gundeck.Types.Notification
import Imports
import Network.HTTP.Types.Status
import Network.Wai.Predicate.MediaType (Media)
import Network.Wai.Utilities
import UnliftIO (async, waitCatch)

type JSON = Media "application" "json"

-- | 'Data.UUID.V1.nextUUID' is sometimes unsuccessful, so we try a few times.
mkNotificationId :: (MonadIO m, MonadThrow m) => m NotificationId
mkNotificationId = do
  ni <- fmap Id <$> retrying x10 fun (const (liftIO nextUUID))
  maybe (throwM err) return ni
  where
    x10 = limitRetries 10 <> exponentialBackoff 10
    fun = const (return . isNothing)
    err = Error status500 "internal-error" "unable to generate notification ID"

mapAsync ::
  (MonadUnliftIO m, Traversable t) =>
  (a -> m b) ->
  t a ->
  m (t (Either SomeException b))
mapAsync f = mapM waitCatch <=< mapM (async . f)
{-# INLINE mapAsync #-}

maybeEqual :: Eq a => Maybe a -> Maybe a -> Bool
maybeEqual (Just x) (Just y) = x == y
maybeEqual _ _ = False
{-# INLINE maybeEqual #-}
