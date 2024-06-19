{-# LANGUAGE DeepSubsumption #-}

module Brig.Effects.UserPendingActivationStore.Cassandra
  ( userPendingActivationStoreToCassandra,
  )
where

import Brig.Effects.UserPendingActivationStore
import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports
import Polysemy
import Polysemy.Internal.Tactics
import Wire.Sem.Paging.Cassandra qualified as PC

userPendingActivationStoreToCassandra ::
  forall r a.
  (Member (Embed Client) r) =>
  Sem (UserPendingActivationStore PC.InternalPaging ': r) a ->
  Sem r a
userPendingActivationStoreToCassandra =
  interpretH $
    liftT . embed @Client . \case
      Add upa -> usersPendingActivationAdd upa
      List Nothing -> flip PC.mkInternalPage pure =<< usersPendingActivationList
      List (Just ps) -> PC.ipNext ps
      RemoveMultiple uids -> usersPendingActivationRemoveMultiple uids

usersPendingActivationAdd :: (MonadClient m) => UserPendingActivation -> m ()
usersPendingActivationAdd (UserPendingActivation uid expiresAt) = do
  retry x5 . write insertExpiration . params LocalQuorum $ (uid, expiresAt)
  where
    insertExpiration :: PrepQuery W (UserId, UTCTime) ()
    insertExpiration = "INSERT INTO users_pending_activation (user, expires_at) VALUES (?, ?)"

usersPendingActivationList :: (MonadClient m) => m (Page UserPendingActivation)
usersPendingActivationList = do
  uncurry UserPendingActivation <$$> retry x1 (paginate selectExpired (params LocalQuorum ()))
  where
    selectExpired :: PrepQuery R () (UserId, UTCTime)
    selectExpired =
      "SELECT user, expires_at FROM users_pending_activation"

usersPendingActivationRemoveMultiple :: (MonadClient m) => [UserId] -> m ()
usersPendingActivationRemoveMultiple uids =
  retry x5 . write deleteExpired . params LocalQuorum $ Identity uids
  where
    deleteExpired :: PrepQuery W (Identity [UserId]) ()
    deleteExpired =
      "DELETE FROM users_pending_activation WHERE user IN ?"
