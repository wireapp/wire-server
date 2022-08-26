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

module Brig.Data.Nonce
  ( insertNonce,
    lookupAndDeleteNonce,
  )
where

import Brig.App (Env)
import Brig.Data.Instances ()
import Cassandra
import Control.Lens hiding (from)
import Data.Nonce (Nonce)
import Imports

insertNonce ::
  (MonadClient m, MonadReader Brig.App.Env m) =>
  Word64 ->
  Text ->
  Nonce ->
  m ()
insertNonce ttl key nonce = retry x5 . write insert $ params LocalQuorum (key, nonce)
  where
    insert :: PrepQuery W (Text, Nonce) ()
    insert = fromString $ "INSERT INTO nonce (key, nonce) VALUES (?) USING TTL " <> show ttl

lookupAndDeleteNonce ::
  (MonadClient m, MonadReader Env m) =>
  Text ->
  m (Maybe Nonce)
lookupAndDeleteNonce key = lookupNonce key <* deleteNonce key

lookupNonce ::
  (MonadClient m, MonadReader Env m) =>
  Text ->
  m (Maybe Nonce)
lookupNonce key = (runIdentity <$$>) . retry x5 . query1 get $ params LocalQuorum (Identity key)
  where
    get :: PrepQuery R (Identity Text) (Identity Nonce)
    get = "SELECT nonce FROM nonce WHERE key = ?"

deleteNonce ::
  (MonadClient m, MonadReader Env m) =>
  Text ->
  m ()
deleteNonce key = retry x5 . write delete $ params LocalQuorum (Identity key)
  where
    delete :: PrepQuery W (Identity Text) ()
    delete = "DELETE FROM nonce WHERE key = ?"
