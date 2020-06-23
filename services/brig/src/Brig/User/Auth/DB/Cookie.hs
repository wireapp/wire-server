{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.User.Auth.DB.Cookie where

import Brig.Types.User.Auth
import Brig.User.Auth.DB.Instances ()
import Cassandra
import Data.Id
import Data.Time.Clock
import Imports

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
   in retry x5 $ write cql (params Quorum (u, x, i, t, c, l, s, o))
  where
    cql :: PrepQuery W (UserId, UTCTime, CookieId, CookieType, UTCTime, Maybe CookieLabel, Maybe CookieId, TTL) ()
    cql =
      "INSERT INTO user_cookies (user, expires, id, type, created, label, succ_id) \
      \VALUES (?, ?, ?, ?, ?, ?, ?) USING TTL ?"

lookupCookie :: MonadClient m => UserId -> UTCTime -> CookieId -> m (Maybe (Cookie ()))
lookupCookie u t c =
  fmap mkCookie <$> retry x1 (query1 cql (params Quorum (u, t, c)))
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

listCookies :: MonadClient m => UserId -> m [Cookie ()]
listCookies u =
  map toCookie <$> retry x1 (query cql (params Quorum (Identity u)))
  where
    cql :: PrepQuery R (Identity UserId) (CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId)
    cql =
      "SELECT id, created, expires, type, label, succ_id \
      \FROM user_cookies \
      \WHERE user = ? \
      \ORDER BY expires ASC"
    toCookie :: (CookieId, UTCTime, UTCTime, CookieType, Maybe CookieLabel, Maybe CookieId) -> Cookie ()
    toCookie (i, ct, et, t, l, sc) =
      Cookie
        { cookieId = i,
          cookieType = t,
          cookieCreated = ct,
          cookieExpires = et,
          cookieLabel = l,
          cookieSucc = sc,
          cookieValue = ()
        }

deleteCookies :: MonadClient m => UserId -> [Cookie a] -> m ()
deleteCookies u cs = retry x5 . batch $ do
  setType BatchUnLogged
  setConsistency Quorum
  for_ cs $ \c -> addPrepQuery cql (u, cookieExpires c, cookieId c)
  where
    cql :: PrepQuery W (UserId, UTCTime, CookieId) ()
    cql = "DELETE FROM user_cookies WHERE user = ? AND expires = ? AND id = ?"

deleteAllCookies :: MonadClient m => UserId -> m ()
deleteAllCookies u = retry x5 (write cql (params Quorum (Identity u)))
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM user_cookies WHERE user = ?"
