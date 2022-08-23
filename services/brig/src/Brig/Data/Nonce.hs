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
  ( addNonce,
    invalidateNonce,
  )
where

import Brig.App (Env)
import Brig.Data.Instances ()
import Cassandra
import Control.Lens hiding (from)
import Data.Nonce (Nonce)
import Imports

addNonce ::
  (MonadClient m, MonadReader Brig.App.Env m) =>
  Nonce ->
  m ()
addNonce nonce = retry x5 . write insertNonce $ params LocalQuorum (Identity nonce)
  where
    insertNonce :: PrepQuery W (Identity Nonce) ()
    insertNonce = "INSERT INTO nonce (nonce) VALUES (?) using ttl 300"

invalidateNonce ::
  (MonadClient m, MonadReader Env m) =>
  Nonce ->
  m ()
invalidateNonce nonce = retry x5 . write delete $ params LocalQuorum (Identity nonce)
  where
    delete :: PrepQuery W (Identity Nonce) ()
    delete = "DELETE FROM nonce WHERE nonce = ?"
