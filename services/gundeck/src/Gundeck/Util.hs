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
