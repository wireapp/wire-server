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
import Control.Monad.Catch (throwM)
import Data.ByteString.Conversion (fromByteString, toByteString')
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Misc (Milliseconds (..))
import Data.Text (pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.UUID (UUID)
import Data.Vector qualified as Vector
import Gundeck.Env (hasqlPool)
import Gundeck.Monad
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import Hasql.TH
import Imports
import System.Logger.Class qualified as Log
import Wire.API.Presence
import Wire.Postgres qualified as Postgres

-- | Register (or refresh) a presence.  The server-side timestamp is stamped
-- here, the 'Presence'\'s own 'createdAt' value is ignored (as in the redis
-- implementation before).
add :: Presence -> Gundeck ()
add p = do
  nowMs <- posixTime
  runPool $
    statement
      (toUUID (userId p), connIdText (connId p), uriText (resource p), clientToText <$> clientId p, fromIntegral (ms nowMs))
      upsertPresence

-- | Read all presences of a single user.
list :: UserId -> Gundeck [Presence]
list u = fromMaybe [] . listToMaybe <$> listAll [u]

-- | Read all presences of the given users, one list per user (input order,
-- empty list for users without presences).  Single round trip.
listAll :: [UserId] -> Gundeck [[Presence]]
listAll [] = pure []
listAll uu = do
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

-- | Compare-and-delete: only delete the stored presence if it is not newer
-- than the given one (a newer re-registration with the same conn id must not
-- be deleted by a stale disconnect).
deleteAll :: [Presence] -> Gundeck ()
deleteAll pp = runPool $ for_ pp $ \p ->
  statement
    (toUUID (userId p), connIdText (connId p), fromIntegral (ms (createdAt p)))
    deleteOne

-- | Delete presences older than a week.  Normal disconnects delete their
-- presence rows; this only guards against leaks from abnormally dead pods
-- (replaces the redis key TTL).
cleanup :: Gundeck ()
cleanup = do
  nowMs <- posixTime
  let cutoff = fromIntegral (ms nowMs - 7 * 24 * 60 * 60 * 1000) :: Int64
  runPool $ statement cutoff deleteStale

-- Helpers -------------------------------------------------------------------

newtype PresenceDbError = PresenceDbError Text deriving (Show)

instance Exception PresenceDbError

runPool :: Session a -> Gundeck a
runPool sess = do
  pool <- view hasqlPool
  liftIO (Postgres.useWithResetAndRetry pool sess) >>= either (throwM . PresenceDbError . pack . show) pure

connIdText :: ConnId -> Text
connIdText = decodeUtf8 . fromConnId

uriText :: URI -> Text
uriText = decodeUtf8 . toByteString'

readPresenceRow :: UUID -> Text -> Text -> Maybe Text -> Int64 -> Maybe Presence
readPresenceRow u c r cl t = do
  uri <- parse (unpack r)
  cid <- traverse parseClient cl
  pure (Presence (Id u) (ConnId (encodeUtf8 c)) uri cid (Ms (fromIntegral t)))
  where
    parseClient = fromByteString . encodeUtf8

upsertPresence :: Statement (UUID, Text, Text, Maybe Text, Int64) ()
upsertPresence =
  [resultlessStatement|
    INSERT INTO presence (user_id, conn_id, resource, client_id, created_at)
    VALUES ($1 :: uuid, $2 :: text, $3 :: text, $4 :: text?, $5 :: int8)
    ON CONFLICT (user_id, conn_id) DO UPDATE
    SET resource = EXCLUDED.resource,
        client_id = EXCLUDED.client_id,
        created_at = EXCLUDED.created_at
  |]

selectByUsers :: Statement (Vector.Vector UUID) (Vector.Vector (UUID, Text, Text, Maybe Text, Int64))
selectByUsers =
  [vectorStatement|
    SELECT user_id :: uuid, conn_id :: text, resource :: text, client_id :: text?, created_at :: int8
    FROM presence
    WHERE user_id = ANY ($1 :: uuid[])
  |]

deleteOne :: Statement (UUID, Text, Int64) ()
deleteOne =
  [resultlessStatement|
    DELETE FROM presence
    WHERE user_id = ($1 :: uuid)
      AND conn_id = ($2 :: text)
      AND created_at <= ($3 :: int8)
  |]

deleteStale :: Statement Int64 ()
deleteStale =
  [resultlessStatement|
    DELETE FROM presence
    WHERE created_at < ($1 :: int8)
  |]
