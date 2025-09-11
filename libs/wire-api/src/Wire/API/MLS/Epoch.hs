{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Wire.API.MLS.Epoch where

import Cassandra qualified as C
import Data.Aeson qualified as A
import Data.Binary
import Data.OpenApi qualified as S
import Data.Schema
import Data.Time.Clock.POSIX
import Imports
import Wire.API.MLS.Serialisation
import Wire.API.PostgresMarshall
import Wire.Arbitrary
import Wire.Sem.FromUTC

newtype Epoch = Epoch {epochNumber :: Word64}
  deriving stock (Eq, Show)
  deriving newtype (Arbitrary, Enum, ToSchema)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via (Schema Epoch)

instance ParseMLS Epoch where
  parseMLS = Epoch <$> parseMLS

instance SerialiseMLS Epoch where
  serialiseMLS (Epoch n) = put n

instance FromUTC Epoch where
  fromUTCTime = Epoch . floor . utcTimeToPOSIXSeconds

instance C.Cql Epoch where
  ctype = C.Tagged C.BigIntColumn
  toCql = C.CqlBigInt . fromIntegral . epochNumber
  fromCql (C.CqlBigInt n) = pure (Epoch (fromIntegral n))
  fromCql _ = Left "epoch: bigint expected"

instance PostgresMarshall Epoch Int64 where
  postgresMarshall = fromIntegral . epochNumber

instance PostgresUnmarshall Int64 Epoch where
  postgresUnmarshall = Right . Epoch . fromIntegral

addToEpoch :: (Integral a) => a -> Epoch -> Epoch
addToEpoch n (Epoch e) = Epoch (e + fromIntegral n)
