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
module Wire.SessionStore.Cassandra (interpretSessionStoreCassandra) where

import Cassandra
import Data.Id
import Data.Time.Clock
import Imports
import Polysemy
import Polysemy.Embed
import Wire.API.User.Auth
import Wire.SessionStore

interpretSessionStoreCassandra :: (Member (Embed IO) r) => ClientState -> InterpreterFor SessionStore r
interpretSessionStoreCassandra casClient =
  interpret $
    runEmbedded (runClient casClient) . \case
      InsertCookie uid cookie ttl -> embed $ insertCookieImpl uid cookie ttl
      LookupCookie uid utc cid -> embed $ lookupCookieImpl uid utc cid
      ListCookies uid -> embed $ listCookiesImpl uid
      DeleteAllCookies uid -> embed $ deleteAllCookiesImpl uid
      DeleteCookies uid cc -> embed $ deleteCookiesImpl uid cc

insertCookieImpl :: (MonadClient m) => UserId -> Cookie () -> Maybe TTL -> m ()
insertCookieImpl u ck ttl =
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

lookupCookieImpl :: (MonadClient m) => UserId -> UTCTime -> CookieId -> m (Maybe (Cookie ()))
lookupCookieImpl u t c =
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

listCookiesImpl :: (MonadClient m) => UserId -> m [Cookie ()]
listCookiesImpl u =
  map toCookie <$> retry x1 (query cql (params LocalQuorum (Identity u)))
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

deleteCookiesImpl :: (MonadClient m) => UserId -> [Cookie ()] -> m ()
deleteCookiesImpl u cs = retry x5 . batch $ do
  setType BatchUnLogged
  setConsistency LocalQuorum
  for_ cs $ \c -> addPrepQuery cql (u, cookieExpires c, cookieId c)
  where
    cql :: PrepQuery W (UserId, UTCTime, CookieId) ()
    cql = "DELETE FROM user_cookies WHERE user = ? AND expires = ? AND id = ?"

deleteAllCookiesImpl :: (MonadClient m) => UserId -> m ()
deleteAllCookiesImpl u = retry x5 (write cql (params LocalQuorum (Identity u)))
  where
    cql :: PrepQuery W (Identity UserId) ()
    cql = "DELETE FROM user_cookies WHERE user = ?"

--------------------------------------------------------------------------------
-- CQL Instances

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
