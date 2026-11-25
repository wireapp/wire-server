-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.SessionStore.Postgres
  ( interpretSessionStoreToPostgres,
  )
where

-- TTL/purge note: unlike the Cassandra interpreter, where row TTLs delete
-- expired cookies physically, Postgres rows survive their @invalid_after@
-- boundary and are only filtered by the read predicates (@invalid_after >
-- now()@).  Expired rows are reaped on write: every insert first deletes the
-- rows of the affected user that are invisible to every read path, which is
-- bounded per user by the @(user_id, expires_at)@ index and keeps rotation
-- from accumulating dead rows unboundedly.  Rows of users that stop writing
-- are removed by the hourly @expire-user-cookies@ pg_cron job (see the
-- @user-cookies-ttl-cleanup@ migration).

import Data.Id
import Data.Time.Clock
import Data.UUID (UUID)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Hasql.Session qualified as HasqlSession
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Auth
import Wire.Postgres
import Wire.SessionStore

interpretSessionStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor SessionStore r
interpretSessionStoreToPostgres = interpret $ \case
  InsertCookie uid cookie ttl -> insertCookieImpl uid cookie ttl
  LookupCookie uid expiry cid -> lookupCookieImpl uid expiry cid
  ListCookies uid -> listCookiesImpl uid
  DeleteAllCookies uid -> deleteAllCookiesImpl uid
  DeleteCookies uid cs -> deleteCookiesImpl uid cs

-- | Column tuple @(id, typ, created, label, succ_id, expires_at)@ produced by
-- 'cookieColumns' for the insert and returned by the select statements below
-- (user_id and invalid_after are handled separately).
type CookieRow = (Int64, Int32, UTCTime, Maybe Text, Maybe Int64, UTCTime)

-- | Marshals a cookie into a 'CookieRow'.
cookieColumns :: Cookie () -> CookieRow
cookieColumns ck =
  ( postgresMarshall (cookieId ck),
    postgresMarshall (cookieType ck),
    cookieCreated ck,
    fmap postgresMarshall (cookieLabel ck),
    fmap postgresMarshall (cookieSucc ck),
    cookieExpires ck
  )

-- | Decodes a 'CookieRow' (see 'cookieColumns').
decodeCookieRow :: CookieRow -> Either Text (Cookie ())
decodeCookieRow (i, t, created, label, csucc, expires) = do
  cid <- postgresUnmarshall i
  typ <- postgresUnmarshall t
  cookieLabel' <- traverse postgresUnmarshall label
  cookieSucc' <- traverse postgresUnmarshall csucc
  pure
    Cookie
      { cookieId = cid,
        cookieType = typ,
        cookieCreated = created,
        cookieExpires = expires,
        cookieLabel = cookieLabel',
        cookieSucc = cookieSucc',
        cookieValue = ()
      }

-- | Upsert a cookie. The Cassandra interpreter relies on a row TTL; here the
-- row carries an explicit @invalid_after@ boundary instead: cookie expiry by
-- default, or now() + TTL when a (short) TTL is passed, as renewCookie does
-- for superseded cookies. Lookups ignore rows past that boundary.
insertCookieImpl :: (PGConstraints r) => UserId -> Cookie () -> Maybe TTL -> Sem r ()
insertCookieImpl u ck ttl =
  runSessionWithRetry $ do
    HasqlSession.statement u.toUUID purgeExpiredStatement
    HasqlSession.statement (u.toUUID, i, t, c, l, s, x, ttlSecs) insertStatement
  where
    (i, t, c, l, s, x) = cookieColumns ck
    ttlSecs = fmap ttlSeconds ttl
    insertStatement :: Hasql.Statement (UUID, Int64, Int32, UTCTime, Maybe Text, Maybe Int64, UTCTime, Maybe Int32) ()
    insertStatement =
      [resultlessStatement|INSERT INTO user_cookies (user_id, id, typ, created, label, succ_id, expires_at, invalid_after)
                           VALUES ($1 :: uuid, $2 :: int8, $3 :: int4, $4 :: timestamptz, $5 :: text?, $6 :: int8?, $7 :: timestamptz,
                                   CASE WHEN $8 :: int4? IS NULL THEN $7 :: timestamptz ELSE now() + make_interval(secs => COALESCE($8 :: int4?, 0)) END)
                           ON CONFLICT (user_id, id) DO UPDATE
                           SET typ = EXCLUDED.typ,
                               created = EXCLUDED.created,
                               label = EXCLUDED.label,
                               succ_id = EXCLUDED.succ_id,
                               expires_at = EXCLUDED.expires_at,
                               invalid_after = EXCLUDED.invalid_after|]

-- | Delete exactly the rows that no read path can see: 'listCookiesImpl' requires
-- both @expires_at@ and @invalid_after@ in the future, 'lookupCookieImpl' requires
-- @invalid_after@ in the future, so the @OR@ below reaps both expired cookies
-- and superseded ones (future @expires_at@, past @invalid_after@).
purgeExpiredStatement :: Hasql.Statement UUID ()
purgeExpiredStatement =
  [resultlessStatement|DELETE FROM user_cookies WHERE user_id = ($1 :: uuid)
                       AND (expires_at <= now() OR invalid_after <= now())|]

lookupCookieImpl :: (PGConstraints r) => UserId -> UTCTime -> CookieId -> Sem r (Maybe (Cookie ()))
lookupCookieImpl u expiry cid =
  runStatement (u.toUUID, expiry, postgresMarshall cid) lookupStatement
  where
    lookupStatement :: Hasql.Statement (UUID, UTCTime, Int64) (Maybe (Cookie ()))
    lookupStatement =
      Hasql.refineResult (traverse decodeCookieRow) $
        [maybeStatement|SELECT id :: int8, typ :: int4, created :: timestamptz, label :: text?, succ_id :: int8?, expires_at :: timestamptz
                         FROM user_cookies
                         WHERE user_id = ($1 :: uuid) AND expires_at = ($2 :: timestamptz) AND id = ($3 :: int8)
                           AND invalid_after > now()|]

listCookiesImpl :: (PGConstraints r) => UserId -> Sem r [Cookie ()]
listCookiesImpl u =
  runStatement u.toUUID listStatement
  where
    listStatement :: Hasql.Statement UUID [Cookie ()]
    listStatement =
      Hasql.refineResult (traverse decodeCookieRow . Vector.toList) $
        [vectorStatement|SELECT id :: int8, typ :: int4, created :: timestamptz, label :: text?, succ_id :: int8?, expires_at :: timestamptz
                          FROM user_cookies
                          WHERE user_id = ($1 :: uuid) AND expires_at > now() AND invalid_after > now()
                          ORDER BY expires_at ASC|]

deleteCookiesImpl :: (PGConstraints r) => UserId -> [Cookie ()] -> Sem r ()
deleteCookiesImpl _ [] = pure ()
deleteCookiesImpl u cs =
  runStatement (u.toUUID, Vector.fromList (map (postgresMarshall . cookieId) cs)) deleteStatement
  where
    deleteStatement :: Hasql.Statement (UUID, Vector Int64) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM user_cookies
                           WHERE user_id = ($1 :: uuid) AND id = ANY ($2 :: int8[])|]

deleteAllCookiesImpl :: (PGConstraints r) => UserId -> Sem r ()
deleteAllCookiesImpl u =
  runStatement u.toUUID deleteAllStatement
  where
    deleteAllStatement :: Hasql.Statement UUID ()
    deleteAllStatement =
      [resultlessStatement|DELETE FROM user_cookies
                           WHERE user_id = ($1 :: uuid)|]
