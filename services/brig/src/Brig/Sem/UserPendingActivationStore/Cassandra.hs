{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

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
import Polysemy.Internal.Tactics (liftT)
import Conduit
import Polysemy.Final

userPendingActivationStoreToCassandra ::
  forall r a.
  ( Member (Embed Client) r
  , Member (Final IO) r
  ) =>
  Sem (UserPendingActivationStore ': r) a ->
  Sem r a
userPendingActivationStoreToCassandra =
  interpretH $
    \case
      Add upa -> liftT $ embed @Client $ usersPendingActivationAdd upa
      (List :: UserPendingActivationStore (Sem r0) _)  -> do
        st0 <- getInitialStateT
        withWeavingToFinal @IO $ \st lower ins -> do
          fconduit
            <- fmap (fmap $ paginateToConduit @r0)
             $ lower
             $ embed @Client usersPendingActivationList <$ st
          let Just conduit = ins fconduit

          (z :: _ (ConduitT () [UserPendingActivation] (Sem r0) ())) <- _
          pure $ fmap (<$ st0) z
        -- cont <- bindT (pure . paginateToConduit @r)
        -- z <- raise $ userPendingActivationStoreToCassandra $ cont page
        -- pure $ fmap (hoistConduit _) z
        -- pure $ fmap paginateToConduit page
      RemoveMultiple uids -> do
        liftT $ embed @Client $ usersPendingActivationRemoveMultiple uids


hoistConduit :: (Monad m, Monad n) => (forall x. m x -> n x) -> ConduitT i o m a -> ConduitT i o n a
hoistConduit f = undefined

paginateToConduit :: forall r a. (Member (Embed Client) r) => Page a -> ConduitT () [a] (Sem r) ()
paginateToConduit (Page False as _) = yield as
paginateToConduit (Page True as more) = do
  yield as
  m <- lift $ embed more
  paginateToConduit m

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
