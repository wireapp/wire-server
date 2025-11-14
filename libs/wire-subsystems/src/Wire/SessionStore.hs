{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.SessionStore where

import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Test.QuickCheck
import Wire.API.User.Auth

newtype TTL = TTL {ttlSeconds :: Int32}
  deriving (Show, Eq)
  deriving newtype (Cql, Arbitrary)

data SessionStore m a where
  InsertCookie :: UserId -> Cookie () -> Maybe TTL -> SessionStore m ()
  LookupCookie :: UserId -> UTCTime -> CookieId -> SessionStore m (Maybe (Cookie ()))
  ListCookies :: UserId -> SessionStore m [Cookie ()]
  DeleteAllCookies :: UserId -> SessionStore m ()
  DeleteCookies :: UserId -> [Cookie ()] -> SessionStore m ()

makeSem ''SessionStore
