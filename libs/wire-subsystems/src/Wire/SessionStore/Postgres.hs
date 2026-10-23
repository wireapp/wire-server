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
    insertCookie,
  )
where

import Data.Id
import Data.Time.Clock
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import Hasql.Session qualified as HasqlSession
import Hasql.Statement qualified as Hasql
import Hasql.TH
import Imports
import Polysemy
import Wire.API.PostgresMarshall
import Wire.API.User.Auth
import Wire.Postgres
import Wire.SessionStore hiding (insertCookie)

interpretSessionStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor SessionStore r
interpretSessionStoreToPostgres = interpret $ \case
  InsertCookie uid cookie ttl -> insertCookie uid cookie ttl
  LookupCookie uid expiry cid -> lookupImpl uid expiry cid
  ListCookies uid -> listImpl uid
  DeleteAllCookies uid -> deleteAllImpl uid
  DeleteCookies uid cs -> deleteImpl uid cs

-- | Upsert a cookie. The Cassandra interpreter relies on a row TTL; here the
-- row carries an explicit @invalid_after@ boundary instead: cookie expiry by
-- default, or now() + TTL when a (short) TTL is passed, as renewCookie does
-- for superseded cookies. Lookups ignore rows past that boundary.
insertCookie :: (PGConstraints r) => UserId -> Cookie () -> Maybe TTL -> Sem r ()
insertCookie u ck ttl =
  runStatement (u.toUUID, i, t, c, l, s, x, ttlSecs) insertStatement
  where
    i = postgresMarshall (cookieId ck)
    x = cookieExpires ck
    c = cookieCreated ck
    t = postgresMarshall (cookieType ck)
    l = fmap postgresMarshall (cookieLabel ck)
    s = fmap postgresMarshall (cookieSucc ck)
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

lookupImpl :: (PGConstraints r) => UserId -> UTCTime -> CookieId -> Sem r (Maybe (Cookie ()))
lookupImpl u expiry cid =
  runStatement (u.toUUID, expiry, postgresMarshall cid) lookupStatement
  where
    decodeRow :: (Int32, UTCTime, Maybe Text, Maybe Int64, UTCTime) -> Either Text (Cookie ())
    decodeRow (t, created, label, csucc, x) = do
      typ <- postgresUnmarshall t
      cookieLabel' <- traverse postgresUnmarshall label
      cookieSucc' <- traverse postgresUnmarshall csucc
      pure
        Cookie
          { cookieId = cid,
            cookieCreated = created,
            cookieExpires = x,
            cookieType = typ,
            cookieLabel = cookieLabel',
            cookieSucc = cookieSucc',
            cookieValue = ()
          }
    lookupStatement :: Hasql.Statement (UUID, UTCTime, Int64) (Maybe (Cookie ()))
    lookupStatement =
      Hasql.refineResult (traverse decodeRow) $
        [maybeStatement|SELECT typ :: int4, created :: timestamptz, label :: text?, succ_id :: int8?, expires_at :: timestamptz
                         FROM user_cookies
                         WHERE user_id = ($1 :: uuid) AND expires_at = ($2 :: timestamptz) AND id = ($3 :: int8)
                           AND invalid_after > now()|]

listImpl :: (PGConstraints r) => UserId -> Sem r [Cookie ()]
listImpl u =
  runStatement u.toUUID listStatement
  where
    decodeRow :: (Int64, UTCTime, UTCTime, Int32, Maybe Text, Maybe Int64) -> Either Text (Cookie ())
    decodeRow (i, ct, et, t, l, sc) = do
      cid <- postgresUnmarshall i
      typ <- postgresUnmarshall t
      cookieLabel' <- traverse postgresUnmarshall l
      cookieSucc' <- traverse postgresUnmarshall sc
      pure
        Cookie
          { cookieId = cid,
            cookieType = typ,
            cookieCreated = ct,
            cookieExpires = et,
            cookieLabel = cookieLabel',
            cookieSucc = cookieSucc',
            cookieValue = ()
          }
    listStatement :: Hasql.Statement UUID [Cookie ()]
    listStatement =
      Hasql.refineResult (traverse decodeRow . Vector.toList) $
        [vectorStatement|SELECT id :: int8, created :: timestamptz, expires_at :: timestamptz, typ :: int4, label :: text?, succ_id :: int8?
                          FROM user_cookies
                          WHERE user_id = ($1 :: uuid) AND expires_at > now()
                          ORDER BY expires_at ASC|]

deleteImpl :: (PGConstraints r) => UserId -> [Cookie ()] -> Sem r ()
deleteImpl u cs =
  runSessionWithRetry $
    traverse_ (\c -> HasqlSession.statement (u.toUUID, postgresMarshall (cookieId c)) deleteStatement) cs
  where
    deleteStatement :: Hasql.Statement (UUID, Int64) ()
    deleteStatement =
      [resultlessStatement|DELETE FROM user_cookies
                           WHERE user_id = ($1 :: uuid) AND id = ($2 :: int8)|]

deleteAllImpl :: (PGConstraints r) => UserId -> Sem r ()
deleteAllImpl u =
  runStatement u.toUUID deleteAllStatement
  where
    deleteAllStatement :: Hasql.Statement UUID ()
    deleteAllStatement =
      [resultlessStatement|DELETE FROM user_cookies
                           WHERE user_id = ($1 :: uuid)|]
