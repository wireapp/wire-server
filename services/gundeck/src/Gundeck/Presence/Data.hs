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
  )
where

import Control.Monad.Catch
import Data.Aeson as Aeson
import Data.ByteString qualified as Strict
import Data.ByteString.Builder (byteString)
import Data.ByteString.Char8 qualified as StrictChars
import Data.ByteString.Conversion hiding (fromList)
import Data.ByteString.Lazy qualified as Lazy
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.Misc (Milliseconds)
import Database.Redis
import Gundeck.Monad (Gundeck, posixTime, runWithAdditionalRedis)
import Gundeck.Util.Redis
import Imports
import System.Logger.Class (MonadLogger)
import Wire.API.Presence

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

add :: Presence -> Gundeck ()
add p = do
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

deleteAll :: (MonadMask m, MonadIO m, RedisCtx m (Either Reply), MonadLogger m) => [Presence] -> m ()
deleteAll [] = pure ()
deleteAll pp = for_ pp $ \p -> do
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

list :: (MonadRedis m, MonadThrow m) => UserId -> m [Presence]
list u = do
  ePresenses <- liftRedis $ list' u
  case ePresenses of
    Left r -> throwM $ RedisSimpleError r
    Right ps -> pure ps

list' :: (RedisCtx m f, Functor f) => UserId -> m (f [Presence])
list' u = mapMaybe (readPresence u) <$$> hgetall (toKey u)

-- FUTUREWORK: Make this not fail if it fails only for a few users.
listAll :: (MonadRedis m, MonadThrow m) => [UserId] -> m [[Presence]]
listAll [] = pure []
listAll uu = mapM list uu

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
