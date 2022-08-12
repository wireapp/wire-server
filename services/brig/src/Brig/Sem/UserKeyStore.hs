{-# LANGUAGE TemplateHaskell #-}

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

module Brig.Sem.UserKeyStore where

import Brig.Types.Common
import Cassandra
import Data.ByteString.Lazy
import Data.Id
import qualified Data.Multihash.Digest as MH
import Imports
import OpenSSL.EVP.Digest
import Polysemy

newtype UserKeyHash = UserKeyHash MH.MultihashDigest

instance Cql UserKeyHash where
  ctype = Tagged BlobColumn

  fromCql (CqlBlob lbs) = case MH.decode (toStrict lbs) of
    Left e -> Left ("userkeyhash: " ++ e)
    Right h -> pure $ UserKeyHash h
  fromCql _ = Left "userkeyhash: expected blob"

  toCql (UserKeyHash d) = CqlBlob $ MH.encode (MH.algorithm d) (MH.digest d)

data UKHashType
  = UKHashPhone
  | UKHashEmail
  deriving (Eq)

instance Cql UKHashType where
  ctype = Tagged IntColumn

  fromCql (CqlInt i) = case i of
    0 -> pure UKHashPhone
    1 -> pure UKHashEmail
    n -> Left $ "unexpected hashtype: " ++ show n
  fromCql _ = Left "userkeyhashtype: int expected"

  toCql UKHashPhone = CqlInt 0
  toCql UKHashEmail = CqlInt 1

data UserKeyStore m a where
  GetKey :: UserKey -> UserKeyStore m (Maybe UserId)
  InsertKey :: Digest -> UserId -> UserKey -> UserKeyStore m ()
  DeleteKey :: Digest -> UserKey -> UserKeyStore m ()

makeSem ''UserKeyStore
