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
{-# LANGUAGE RecordWildCards #-}

module Brig.Sem.CodeStore.Cassandra
  ( codeStoreToCassandra,
    interpretClientToIO,
  )
where

import Brig.Data.Instances ()
import Brig.Sem.CodeStore
import Brig.Types
import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy

codeStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (CodeStore ': r) a ->
  Sem r a
codeStoreToCassandra =
  interpret $
    embed @m
      . \case
        CodeSelect prk ->
          (fmap . fmap) toRecord
            . retry x1
            . query1 codeSelectQuery
            . params LocalQuorum
            . Identity
            $ prk
        CodeInsert prk (PRQueryData prc uid n ut) ttl ->
          retry x5
            . write codeInsertQuery
            . params LocalQuorum
            $ (prk, prc, uid, runIdentity n, runIdentity ut, ttl)
        CodeDelete prk ->
          retry x5
            . write codeDeleteQuery
            . params LocalQuorum
            . Identity
            $ prk
  where
    toRecord ::
      (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime) ->
      PRQueryData Maybe
    toRecord (prqdCode, prqdUser, prqdRetries, prqdTimeout) =
      PRQueryData {..}

interpretClientToIO ::
  Member (Final IO) r =>
  ClientState ->
  Sem (Embed Cassandra.Client ': r) a ->
  Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> embedFinal @IO $ runClient ctx action

---------------------------------------------------------------------------------
-- Queries

codeSelectQuery :: PrepQuery R (Identity PasswordResetKey) (PasswordResetCode, UserId, Maybe Int32, Maybe UTCTime)
codeSelectQuery = "SELECT code, user, retries, timeout FROM password_reset WHERE key = ?"

codeInsertQuery :: PrepQuery W (PasswordResetKey, PasswordResetCode, UserId, Int32, UTCTime, Int32) ()
codeInsertQuery = "INSERT INTO password_reset (key, code, user, retries, timeout) VALUES (?, ?, ?, ?, ?) USING TTL ?"

codeDeleteQuery :: PrepQuery W (Identity PasswordResetKey) ()
codeDeleteQuery = "DELETE FROM password_reset WHERE key = ?"
