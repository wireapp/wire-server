-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.NonceStore.Cassandra
  ( interpretNonceStoreToCassandra,
    selectAllNonces,
  )
where

import Cassandra
import Data.Id (UserId)
import Data.Nonce (Nonce, NonceTtlSecs)
import Imports
import Polysemy
import Wire.NonceStore (NonceStore (..))

interpretNonceStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor NonceStore r
interpretNonceStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      InsertNonce ttl uid key nonce -> insertNonce ttl uid key nonce
      LookupAndDeleteNonce uid key -> lookupAndDeleteNonce uid key
      DeleteNonce uid key -> deleteNonce uid key

--------------------------------------------------------------------------------

insertNonce :: (MonadClient m) => NonceTtlSecs -> UserId -> Text -> Nonce -> m ()
insertNonce ttl uid key nonce = retry x5 . write insert $ params LocalQuorum (uid, key, nonce, ttl)
  where
    insert :: PrepQuery W (UserId, Text, Nonce, NonceTtlSecs) ()
    insert = "INSERT INTO nonce (user, key, nonce) VALUES (?, ?, ?) USING TTL ?"

lookupAndDeleteNonce :: (MonadClient m) => UserId -> Text -> m (Maybe Nonce)
lookupAndDeleteNonce uid key = lookupNonce uid key <* deleteNonce uid key

lookupNonce :: (MonadClient m) => UserId -> Text -> m (Maybe Nonce)
lookupNonce uid key = (runIdentity <$$>) . retry x5 . query1 get $ params LocalQuorum (uid, key)
  where
    get :: PrepQuery R (UserId, Text) (Identity Nonce)
    get = "SELECT nonce FROM nonce WHERE user = ? AND key = ?"

deleteNonce :: (MonadClient m) => UserId -> Text -> m ()
deleteNonce uid key = retry x5 . write delete $ params LocalQuorum (uid, key)
  where
    delete :: PrepQuery W (UserId, Text) ()
    delete = "DELETE FROM nonce WHERE user = ? AND key = ?"

selectAllNonces :: PrepQuery R () (UserId, Text, Nonce, Maybe Int32)
selectAllNonces = "SELECT user, key, nonce, TTL(nonce) FROM nonce"
