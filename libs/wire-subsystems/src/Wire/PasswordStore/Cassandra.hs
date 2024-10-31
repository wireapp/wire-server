{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wire.PasswordStore.Cassandra (interpretPasswordStore) where

import Cassandra
import Data.Id
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.Password (Password)
import Wire.PasswordStore

interpretPasswordStore :: (Member (Embed IO) r) => ClientState -> InterpreterFor PasswordStore r
interpretPasswordStore casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      UpsertHashedPassword uid password -> embed $ updatePasswordImpl uid password
      LookupHashedPassword uid -> embed $ lookupPasswordImpl uid
      LookupHashedProviderPassword pid -> embed $ lookupProviderPasswordImpl pid

lookupProviderPasswordImpl :: (MonadClient m) => ProviderId -> m (Maybe Password)
lookupProviderPasswordImpl u =
  (runIdentity =<<)
    <$> retry x1 (query1 providerPasswordSelect (params LocalQuorum (Identity u)))

lookupPasswordImpl :: (MonadClient m) => UserId -> m (Maybe Password)
lookupPasswordImpl u =
  (runIdentity =<<)
    <$> retry x1 (query1 passwordSelect (params LocalQuorum (Identity u)))

updatePasswordImpl :: (MonadClient m) => UserId -> Password -> m ()
updatePasswordImpl u p = do
  retry x5 $ write userPasswordUpdate (params LocalQuorum (p, u))

------------------------------------------------------------------------
-- Queries

providerPasswordSelect :: PrepQuery R (Identity ProviderId) (Identity (Maybe Password))
providerPasswordSelect =
  "SELECT password FROM provider WHERE id = ?"

passwordSelect :: PrepQuery R (Identity UserId) (Identity (Maybe Password))
passwordSelect = "SELECT password FROM user WHERE id = ?"

userPasswordUpdate :: PrepQuery W (Password, UserId) ()
userPasswordUpdate = {- `IF EXISTS`, but that requires benchmarking -} "UPDATE user SET password = ? WHERE id = ?"
