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

module Brig.Data.MLS.PublicKey
  ( MLSPublicKeyDataError (..),
    addMLSPublicKeys,
  )
where

import Brig.App
import Cassandra
import Control.Monad.Except
import Data.Id
import Database.CQL.IO
import Imports
import Wire.API.MLS.Credential

data MLSPublicKeyDataError = MLSPublicKeyDuplicate

addMLSPublicKeys ::
  UserId ->
  ClientId ->
  [(SignatureSchemeTag, LByteString)] ->
  ExceptT MLSPublicKeyDataError (AppIO r) ()
addMLSPublicKeys u c = traverse_ (uncurry (addMLSPublicKey u c))

addMLSPublicKey ::
  UserId ->
  ClientId ->
  SignatureSchemeTag ->
  LByteString ->
  ExceptT MLSPublicKeyDataError (AppIO r) ()
addMLSPublicKey u c ss pk = do
  rows <- trans q (params LocalQuorum (u, c, ss, Blob pk))
  case rows of
    [row]
      | fromRow 0 row /= Right (Just True) ->
        throwError MLSPublicKeyDuplicate
    _ -> pure ()
  where
    q :: PrepQuery W (UserId, ClientId, SignatureSchemeTag, Blob) Row
    q = "INSERT INTO mls_public_keys (user, client, sig_scheme, key) VALUES (?, ?, ?, ?) IF NOT EXISTS"
