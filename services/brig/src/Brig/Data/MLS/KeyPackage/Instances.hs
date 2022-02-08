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
{-# OPTIONS_GHC -Wno-orphans #-}

module Brig.Data.MLS.KeyPackage.Instances where

import Cassandra
import qualified Data.ByteString.Lazy as LBS
import Imports
import Wire.API.MLS.KeyPackage

-- kpBlob :: KeyPackageData -> Blob
-- kpBlob = Blob . kpData

-- kpFromBlob :: Blob -> KeyPackageData
-- kpFromBlob = KeyPackageData . fromBlob

-- kprBlob :: KeyPackageRef -> Blob
-- kprBlob = Blob . LBS.fromStrict . unKeyPackageRef

instance Cql KeyPackageRef where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . LBS.fromStrict . unKeyPackageRef
  fromCql (CqlBlob b) = pure . KeyPackageRef . LBS.toStrict $ b
  fromCql _ = Left "Expected CqlBlob"

instance Cql KeyPackageData where
  ctype = Tagged BlobColumn
  toCql = CqlBlob . kpData
  fromCql (CqlBlob b) = pure . KeyPackageData $ b
  fromCql _ = Left "Expected CqlBlob"
