module Brig.Sem.UserPendingActivationStore.Cassandra
  ( userPendingActivationStoreToCassandra,
  )
where

import Brig.Sem.UserPendingActivationStore
import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports
import Polysemy

userPendingActivationStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (UserPendingActivationStore ': r) a ->
  Sem r a
userPendingActivationStoreToCassandra =
  interpret $
    embed @m . \case
      Add upa -> usersPendingActivationAdd upa
      List -> usersPendingActivationList
      RemoveMultiple uids -> usersPendingActivationRemoveMultiple uids

usersPendingActivationAdd :: MonadClient m => UserPendingActivation -> m ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params LocalQuorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

usersPendingActivationList :: MonadClient m => m (Page UserPendingActivation)
usersPendingActivationList = do
  uncurry UserPendingActivation <$$> retry x1 (paginate selectExpired (params LocalQuorum ()))
  where
    selectExpired :: PrepQuery R () (UserId, UTCTime)
    selectExpired =
      "SELECT user, expires_at FROM users_pending_activation"

usersPendingActivationRemoveMultiple :: MonadClient m => [UserId] -> m ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params LocalQuorum $ Identity uids
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
