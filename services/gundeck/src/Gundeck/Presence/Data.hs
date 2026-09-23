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

module Gundeck.Presence.Data
  ( add,
    list,
    listAll,
    deleteAll,
    cleanup,
  )
where

import Control.Lens (view)
import Control.Monad.Catch
import Data.Aeson as Aeson
import Data.ByteString qualified as Strict
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as StrictChars
import Data.ByteString.Conversion hiding (fromList)
import Data.ByteString.Lazy qualified as Lazy
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Misc (Milliseconds (..))
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import Database.Redis
import Gundeck.Env (PresenceBackend (..), presenceBackend)
import Gundeck.Monad (Gundeck, posixTime, runWithAdditionalRedis, runWithDefaultRedis)
import Gundeck.Util.Redis
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import Hasql.TH
import Imports
import System.Logger.Class (MonadLogger)
import System.Logger.Class qualified as Log
import Wire.API.Presence
import Wire.Postgres qualified as Postgres

-- Note [Migration] ---------------------------------------------------------
--
-- Previous redis schema: user:<uuid>=<connection>@<cannon>=<presence-data json>
-- New redis schema:      user:<uuid>=<connection>         =<presence-data json>
--
-- The previous redis schema encodes cannon's ID in the subkey. The migration
-- proceeds as follows:
--
-- 1. When adding new entries, we only use the connection as subkey.
-- 2. When listing entries (which does not use the subkey fortunately) we
--    store the original field name in the `Presence` record property `__field`.
-- 3. When deleting entries, we use this `Presence`'s `__field` value.
-- 4. Eventually `__field` can be removed from the `Presence` type and the
--    connection can be used directly instead.
--

-- | Register (or refresh) a presence.  The server-side timestamp is stamped
-- here, the 'Presence'\'s own 'createdAt' value is ignored.
add :: Presence -> Gundeck ()
add p = do
  backend <- view presenceBackend
  case backend of
    PresenceBackendPostgres _ -> pgAdd p
    PresenceBackendRedis _ _ -> redisAdd p

-- | Read all presences of a single user.
list :: UserId -> Gundeck [Presence]
list u = do
  backend <- view presenceBackend
  case backend of
    PresenceBackendPostgres _ -> fromMaybe [] . listToMaybe <$> listAll [u]
    PresenceBackendRedis _ _ -> runWithDefaultRedis (redisList u)

-- | Read all presences of the given users, one list per user.
--
-- With postgresql, this is a single round trip; the output preserves the input
-- order and users without presences map to empty lists.  With redis, users are
-- queried individually.
listAll :: [UserId] -> Gundeck [[Presence]]
listAll [] = pure []
listAll uu = do
  backend <- view presenceBackend
  case backend of
    PresenceBackendPostgres _ -> pgListAll uu
    PresenceBackendRedis _ _ -> runWithDefaultRedis (redisListAll uu)

-- | Compare-and-delete: only delete the stored presence if it is not newer
-- than the given one (a newer re-registration with the same conn id must not
-- be deleted by a stale disconnect).
deleteAll :: [Presence] -> Gundeck ()
deleteAll [] = pure ()
deleteAll pp = do
  backend <- view presenceBackend
  case backend of
    PresenceBackendPostgres _ -> pgDeleteAll pp
    PresenceBackendRedis _ _ -> runWithAdditionalRedis (redisDeleteAll pp)

-- | Delete presences older than a week.  Only meaningful with the postgresql
-- backend (redis presences expire via key TTL).
cleanup :: Gundeck ()
cleanup = runPool $ statement () deleteStale

-- Redis implementation ------------------------------------------------------

redisAdd :: Presence -> Gundeck ()
redisAdd p = do
  now <- posixTime
  let k = toKey (userId p)
  let v = toField (connId p)
  let d = Lazy.toStrict $ Aeson.encode $ PresenceData p.resource p.clientId now
  runWithAdditionalRedis . retry x3 $ do
    void . fromTxResult <=< (liftRedis . multiExec) $ do
      void $ hset k (NonEmpty.singleton (v, d))
      -- nb. All presences of a user are expired 'maxIdleTime' after the
      -- last presence was registered. A client who keeps a presence
      -- (i.e. websocket) connected for longer than 'maxIdleTime' will be
      -- silently dropped and receives no more notifications.
      expire k maxIdleTime
  where
    maxIdleTime = 7 * 24 * 60 * 60 -- 7 days in seconds

redisDeleteAll :: (MonadMask m, MonadIO m, RedisCtx m (Either Reply), MonadLogger m) => [Presence] -> m ()
redisDeleteAll [] = pure ()
redisDeleteAll pp = for_ pp $ \p -> do
  let k = toKey (userId p)
  let f = Lazy.toStrict $ __field p
  void . retry x3 $ do
    void . liftRedis $ watch (pure k)
    value <- either (throwM . RedisSimpleError) id <$> hget k f
    void . liftRedis . multiExec $ do
      case value of
        Nothing -> pure $ pure ()
        Just v -> do
          let p' = readPresence (userId p) (f, v)
          if Just p == p'
            then void <$> hdel k (pure f)
            else pure $ pure ()

redisList :: (MonadRedis m, MonadThrow m) => UserId -> m [Presence]
redisList u = do
  ePresenses <- liftRedis $ redisList' u
  case ePresenses of
    Left r -> throwM $ RedisSimpleError r
    Right ps -> pure ps

redisList' :: (RedisCtx m f, Functor f) => UserId -> m (f [Presence])
redisList' u = mapMaybe (readPresence u) <$$> hgetall (toKey u)

-- FUTUREWORK: Make this not fail if it fails only for a few users.
redisListAll :: (MonadRedis m, MonadThrow m) => [UserId] -> m [[Presence]]
redisListAll [] = pure []
redisListAll uu = mapM redisList uu

-- Postgresql implementation -------------------------------------------------

pgAdd :: Presence -> Gundeck ()
pgAdd p = do
  nowMs <- posixTime
  runPool $
    statement
      (toUUID (userId p), connIdText (connId p), uriText (resource p), clientToText <$> p.clientId, msToUtc (fromIntegral (ms nowMs)))
      upsertPresence

-- | Read all presences of the given users, one list per user (input order,
-- empty list for users without presences).  Single round trip.
pgListAll :: [UserId] -> Gundeck [[Presence]]
pgListAll [] = pure []
pgListAll uu = do
  rows <- runPool $ statement (Vector.fromList (toUUID <$> uu)) selectByUsers
  presencesByUser <-
    foldM
      ( \acc (u, c, r, cl, t) -> case readPresenceRow u c r cl t of
          Just p -> pure $! Map.insertWith (<>) (userId p) [p] acc
          Nothing -> do
            Log.warn $
              Log.msg (Log.val "ignoring unreadable presence row")
                . Log.field "user_id" (show u)
                . Log.field "conn_id" (show c)
            pure acc
      )
      Map.empty
      (Vector.toList rows)
  pure [Map.findWithDefault [] u presencesByUser | u <- uu]

-- | Compare-and-delete, in one round trip: only delete each stored presence
-- if it is not newer than the given one (a newer re-registration with the
-- same conn id must not be deleted by a stale disconnect).
pgDeleteAll :: [Presence] -> Gundeck ()
pgDeleteAll [] = pure ()
pgDeleteAll pp =
  runPool . statement params $ deleteMany
  where
    params =
      ( Vector.fromList (toUUID . userId <$> pp),
        Vector.fromList (connIdText . connId <$> pp),
        Vector.fromList (msToUtc . fromIntegral . ms . createdAt <$> pp)
      )

-- Helpers -------------------------------------------------------------------

data PresenceData = PresenceData !URI !(Maybe ClientId) !Milliseconds
  deriving (Eq)

instance ToJSON PresenceData where
  toJSON (PresenceData r c t) =
    object
      [ "r" .= r,
        "c" .= c,
        "t" .= t
      ]

instance FromJSON PresenceData where
  parseJSON = withObject "PresenceData" $ \o ->
    PresenceData
      <$> o
        .: "r"
      <*> o
        .:? "c"
      <*> o
        .:? "t"
        .!= 0

toKey :: UserId -> ByteString
toKey u = Lazy.toStrict $ runBuilder (byteString "user:" <> builder u)

toField :: ConnId -> ByteString
toField (ConnId con) = con

fromField :: ByteString -> ConnId
fromField = ConnId . StrictChars.takeWhile (/= '@')

readPresence :: UserId -> (ByteString, ByteString) -> Maybe Presence
readPresence u (f, b) = do
  PresenceData uri clt tme <-
    if "http" `Strict.isPrefixOf` b
      then PresenceData <$> fromByteString b <*> pure Nothing <*> pure 0
      else decodeStrict' b
  pure (Presence u (fromField f) uri clt tme (Lazy.fromStrict f))

-- | Millis <-> UTC. Exact (milliseconds nest inside timestamptz's microseconds);
-- do NOT reuse 'Gundeck.Monad.msToUTCSecs', it truncates to whole seconds.
msToUtc :: Int64 -> UTCTime
msToUtc p = posixSecondsToUTCTime (fromRational (fromIntegral p / 1000 :: Rational))

utcToMs :: UTCTime -> Int64
utcToMs = floor . (* 1000) . utcTimeToPOSIXSeconds

newtype PresenceDbError = PresenceDbError Text deriving (Show)

instance Exception PresenceDbError

runPool :: Session a -> Gundeck a
runPool sess = do
  pool <-
    view presenceBackend >>= \case
      PresenceBackendPostgres p -> pure p
      PresenceBackendRedis _ _ -> error "gundeck: postgresql not configured (presenceStore is redis)"
  liftIO (Postgres.useWithResetAndRetry pool sess) >>= either (throwM . PresenceDbError . pack . show) pure

connIdText :: ConnId -> Text
connIdText = decodeUtf8 . fromConnId

uriText :: URI -> Text
uriText = decodeUtf8 . toByteString'

readPresenceRow :: UUID -> Text -> Text -> Maybe Text -> UTCTime -> Maybe Presence
readPresenceRow u c r cl t = do
  uri <- parse (unpack r)
  cid <- traverse parseClient cl
  pure (Presence (Id u) (ConnId (encodeUtf8 c)) uri cid (Ms (fromIntegral (utcToMs t))) "")
  where
    parseClient = fromByteString . encodeUtf8

upsertPresence :: Statement (UUID, Text, Text, Maybe Text, UTCTime) ()
upsertPresence =
  [resultlessStatement|
    INSERT INTO presence (user_id, conn_id, resource, client_id, created_at)
    VALUES ($1 :: uuid, $2 :: text, $3 :: text, $4 :: text?, $5 :: timestamptz)
    ON CONFLICT (user_id, conn_id) DO UPDATE
    SET resource = EXCLUDED.resource,
        client_id = EXCLUDED.client_id,
        created_at = EXCLUDED.created_at
  |]

selectByUsers :: Statement (Vector.Vector UUID) (Vector.Vector (UUID, Text, Text, Maybe Text, UTCTime))
selectByUsers =
  [vectorStatement|
    SELECT user_id :: uuid, conn_id :: text, resource :: text, client_id :: text?, created_at :: timestamptz
    FROM presence
    WHERE user_id = ANY ($1 :: uuid[])
  |]

deleteMany :: Statement (Vector.Vector UUID, Vector.Vector Text, Vector.Vector UTCTime) ()
deleteMany =
  [resultlessStatement|
    DELETE FROM presence p
    USING unnest($1 :: uuid[], $2 :: text[], $3 :: timestamptz[]) AS d (user_id, conn_id, created_at)
    WHERE p.user_id = d.user_id
      AND p.conn_id = d.conn_id
      AND p.created_at <= d.created_at
  |]

deleteStale :: Statement () ()
deleteStale =
  [resultlessStatement|
    DELETE FROM presence
    WHERE created_at < now() - interval '7 days'
  |]
