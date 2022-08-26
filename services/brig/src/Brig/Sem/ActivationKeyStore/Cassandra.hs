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

module Brig.Sem.ActivationKeyStore.Cassandra (activationKeyStoreToCassandra) where

import Brig.Data.Instances ()
import Brig.Sem.ActivationKeyStore
import Cassandra
import Data.Id
import Imports
import Polysemy
import Wire.API.User.Activation

activationKeyStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (ActivationKeyStore ': r) a ->
  Sem r a
activationKeyStoreToCassandra =
  interpret $
    embed @m . \case
      GetActivationKey k -> getKey k
      InsertActivationKey tuple -> keyInsertQuery tuple
      DeleteActivationPair k -> keyDelete k

getKey :: MonadClient m => ActivationKey -> m (Maybe GetKeyTuple)
getKey key = retry x1 . query1 keySelect $ params LocalQuorum (Identity key)
  where
    keySelect :: PrepQuery R (Identity ActivationKey) (Int32, Ascii, Text, ActivationCode, Maybe UserId, Int32)
    keySelect = "SELECT ttl(code) as ttl, key_type, key_text, code, user, retries FROM activation_keys WHERE key = ?"

keyInsertQuery :: MonadClient m => InsertKeyTuple -> m ()
keyInsertQuery (key, t, k, c, u, attempts, timeout) =
  retry x5 . write keyInsert $ params LocalQuorum (key, t, k, c, u, attempts, timeout)
  where
    keyInsert :: PrepQuery W (ActivationKey, Text, Text, ActivationCode, Maybe UserId, Int32, Int32) ()
    keyInsert =
      "INSERT INTO activation_keys \
      \(key, key_type, key_text, code, user, retries) VALUES \
      \(?  , ?       , ?       , ?   , ?   , ?      ) USING TTL ?"

keyDelete :: MonadClient m => ActivationKey -> m ()
keyDelete = write q . params LocalQuorum . Identity
  where
    q :: PrepQuery W (Identity ActivationKey) ()
    q = "DELETE FROM activation_keys WHERE key = ?"
