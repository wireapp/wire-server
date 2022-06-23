{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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

module Brig.User.Auth.DB.Instances
  (
  )
where

import Cassandra.CQL
import Data.Id ()
import Data.Misc ()
import Data.Range ()
import Data.Text.Ascii ()
import Imports
import Wire.API.User.Auth

deriving instance Cql CookieLabel

deriving instance Cql LoginCode

instance Cql CookieId where
  ctype = Tagged BigIntColumn
  toCql = CqlBigInt . fromIntegral . cookieIdNum

  fromCql (CqlBigInt i) = pure (CookieId (fromIntegral i))
  fromCql _ = Left "fromCql: invalid cookie id"

instance Cql CookieType where
  ctype = Tagged IntColumn

  toCql SessionCookie = CqlInt 0
  toCql PersistentCookie = CqlInt 1

  fromCql (CqlInt 0) = pure SessionCookie
  fromCql (CqlInt 1) = pure PersistentCookie
  fromCql _ = Left "fromCql: invalid cookie type"
