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

-- FUTUREWORK: Migrate the remaining actions to 'Brig.Effects.CookieStore'
module Brig.User.Auth.DB.Cookie where

import Brig.User.Auth.DB.Instances ()
import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Wire.API.User.Auth

newtype TTL = TTL {ttlSeconds :: Int32}
  deriving (Cql)

insertCookie :: MonadClient m => UserId -> Cookie a -> Maybe TTL -> m ()
insertCookie u ck ttl =
  let i = cookieId ck
      x = cookieExpires ck
      c = cookieCreated ck
      t = cookieType ck
      l = cookieLabel ck
      s = cookieSucc ck
      o = fromMaybe (TTL (round (diffUTCTime x c))) ttl
   in retry x5 $ write cql (params LocalQuorum (u, x, i, t, c, l, s, o))
  where
    cql :: PrepQuery W (UserId, UTCTime, CookieId, CookieType, UTCTime, Maybe CookieLabel, Maybe CookieId, TTL) ()
    cql =
      "INSERT INTO user_cookies (user, expires, id, type, created, label, succ_id) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"

lookupCookie :: MonadClient m => UserId -> UTCTime -> CookieId -> m (Maybe (Cookie ()))
lookupCookie u t c =
  fmap mkCookie <$> retry x1 (query1 cql (params LocalQuorum (u, t, c)))
  where
    mkCookie (typ, created, label, csucc) =
      Cookie
        { cookieId = c,
          cookieCreated = created,
          cookieExpires = t,
          cookieType = typ,
          cookieLabel = label,
          cookieSucc = csucc,
          cookieValue = ()
        }
    cql :: PrepQuery R (UserId, UTCTime, CookieId) (CookieType, UTCTime, Maybe CookieLabel, Maybe CookieId)
    cql =
      "SELECT type, created, label, succ_id \
      \FROM user_cookies \
      \WHERE user = ? AND expires = ? AND id = ?"
