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

module Brig.Sem.UserKeyStore.Cassandra (userKeyStoreToCassandra) where

import Brig.Sem.UserKeyStore
import Brig.Types.Common
import Cassandra
import qualified Data.ByteString as B
import Data.Id
import qualified Data.Multihash.Digest as MH
import qualified Data.Text.Encoding as T
import Imports
import OpenSSL.EVP.Digest
import Polysemy

userKeyStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (UserKeyStore ': r) a ->
  Sem r a
userKeyStoreToCassandra =
  interpret $
    embed @m . \case
      GetKey uid -> lookupKeyQuery uid
      InsertKey d uid k -> insertKeyQuery d uid k
      DeleteKey d k -> deleteKeyQuery d k

hashKey :: Digest -> UserKey -> UserKeyHash
hashKey d uk =
  let d' = digestBS d $ T.encodeUtf8 (keyText uk)
   in UserKeyHash $ MH.MultihashDigest MH.SHA256 (B.length d') d'

lookupKeyQuery :: MonadClient m => UserKey -> m (Maybe UserId)
lookupKeyQuery k =
  fmap runIdentity
    <$> retry x1 (query1 keySelect (params LocalQuorum (Identity $ keyText k)))

keySelect :: PrepQuery R (Identity Text) (Identity UserId)
keySelect = "SELECT user FROM user_keys WHERE key = ?"

insertKeyQuery :: MonadClient m => Digest -> UserId -> UserKey -> m ()
insertKeyQuery d u k = do
  let hk = hashKey d k
      kt = foldKey (\(_ :: Email) -> UKHashEmail) (\(_ :: Phone) -> UKHashPhone) k
  retry x5 $ write insertHashed (params LocalQuorum (hk, kt, u))
  retry x5 $ write keyInsert (params LocalQuorum (keyText k, u))

insertHashed :: PrepQuery W (UserKeyHash, UKHashType, UserId) ()
insertHashed = "INSERT INTO user_keys_hash(key, key_type, user) VALUES (?, ?, ?)"

keyInsert :: PrepQuery W (Text, UserId) ()
keyInsert = "INSERT INTO user_keys (key, user) VALUES (?, ?)"

deleteKeyQuery :: MonadClient m => Digest -> UserKey -> m ()
deleteKeyQuery d k = do
  let hk = hashKey d k
  retry x5 $ write deleteHashed (params LocalQuorum (Identity hk))
  retry x5 $ write keyDelete (params LocalQuorum (Identity $ keyText k))
  where
    deleteHashed :: PrepQuery W (Identity UserKeyHash) ()
    deleteHashed = "DELETE FROM user_keys_hash WHERE key = ?"
    keyDelete :: PrepQuery W (Identity Text) ()
    keyDelete = "DELETE FROM user_keys WHERE key = ?"
