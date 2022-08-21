-- See note on 'NextPageToken'
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Brig.Sem.UserPendingActivationStore.Cassandra
  ( userPendingActivationStoreToCassandra,
  )
where

import Brig.Sem.UserPendingActivationStore hiding (Page)
import qualified Brig.Sem.UserPendingActivationStore as E
import Cassandra
import Data.Id (UserId)
import Data.Time (UTCTime)
import Imports
import Polysemy
import Polysemy.Internal.Tactics
import Unsafe.Coerce (unsafeCoerce)

userPendingActivationStoreToCassandra ::
  forall r a.
  (Member (Embed Client) r) =>
  Sem (UserPendingActivationStore ': r) a ->
  Sem r a
userPendingActivationStoreToCassandra =
  interpretH $
    liftT . embed @Client . \case
      Add upa -> usersPendingActivationAdd upa
      List ->
        fmap cassandraPageToEffectPage $ usersPendingActivationList
      GetNext nkt ->
        fmap cassandraPageToEffectPage $ unsafeFromNextKeyToken nkt
      RemoveMultiple uids -> usersPendingActivationRemoveMultiple uids

cassandraPageToEffectPage :: Page a -> E.Page unique a
cassandraPageToEffectPage (Page more results nextPage) =
  E.Page results $ bool Nothing (Just $ unsafeToNextKeyToken nextPage) more

unsafeToNextKeyToken :: Client (Page a) -> NextPageToken unique a
unsafeToNextKeyToken = ExtremelyUnsafeNextPageToken . unsafeCoerce

unsafeFromNextKeyToken :: NextPageToken unique a -> Client (Page a)
unsafeFromNextKeyToken = unsafeCoerce . getNextPage

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
