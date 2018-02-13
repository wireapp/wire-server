{-# LANGUAGE OverloadedStrings #-}

module Gundeck.Presence.Data
    ( add
    , list
    , listAll
    , deleteAll
    ) where

import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.ByteString.Builder (byteString)
import Data.ByteString.Conversion hiding (fromList)
import Data.ByteString (ByteString, isPrefixOf)
import Data.Foldable (for_)
import Data.Id
import Data.Maybe (mapMaybe)
import Data.Monoid
import Data.Text hiding (isPrefixOf, zipWith)
import Database.Redis.IO hiding (Milliseconds)
import Gundeck.Monad (Gundeck, posixTime)
import Gundeck.Types
import Gundeck.Util.Redis

import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Char8 as LazyChars

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
    let d = encode $ PresenceData (resource p) (resourceb p) (cannonhost p) (clientId p) now
    retry x3 $ commands $ do
        multi
        void $ hset k v d
        -- nb. All presences of a user are expired 'maxIdleTime' after the
        -- last presence was registered. A client who keeps a presence
        -- (i.e. websocket) connected for longer than 'maxIdleTime' will be
        -- silently dropped and receives no more notifications.
        void $ expire k maxIdleTime
        exec
  where
    maxIdleTime = Seconds (7 * 24 * 60 * 60) -- 7 days

deleteAll :: (MonadClient m, MonadMask m) => [Presence] -> m ()
deleteAll [] = return ()
deleteAll pp = for_ pp $ \p -> do
    let k = toKey (userId p)
    let f = __field p
    retry x3 $ commands $ do
        watch (pure k)
        value <- hget k f
        multi
        for_ value $ \v -> do
            let p' = readPresence (userId p) (f, Lazy.toStrict v)
            when (Just p == p') $
                void (hdel k (pure f))
        exec

list :: MonadClient m => UserId -> m [Presence]
list u = mapMaybe (readPresence u) <$> commands (hgetall (toKey u))

listAll :: MonadClient m => [UserId] -> m [[Presence]]
listAll [] = return []
listAll uu =
    zipWith fn uu <$> commands (mapM (hgetall . toKey) uu)
  where
    fn u = mapMaybe (readPresence u)

-- Helpers -------------------------------------------------------------------

data PresenceData = PresenceData !URI !(Maybe URI) !(Maybe Text) !(Maybe ClientId) !Milliseconds
    deriving Eq

instance ToJSON PresenceData where
    toJSON (PresenceData r rb ch c t) = object
        [ "r"  .= r
        , "rb" .= rb
        , "ch" .= ch
        , "c"  .= c
        , "t"  .= t
        ]

instance FromJSON PresenceData where
    parseJSON = withObject "PresenceData" $ \o ->
        PresenceData <$> o .:  "r"
                     <*> o .:? "rb"
                     <*> o .:? "ch"
                     <*> o .:? "c"
                     <*> o .:? "t" .!= 0

toKey :: UserId -> Key
toKey u = Key $ runBuilder (byteString "user:" <> builder u)

toField :: ConnId -> Field
toField (ConnId con) = Lazy.fromStrict con

fromField :: Field -> ConnId
fromField = ConnId . Lazy.toStrict . LazyChars.takeWhile (/= '@')

readPresence :: UserId -> (Field, ByteString) -> Maybe Presence
readPresence u (f, b) = do
    PresenceData uri urib chost clt tme <- if "http" `isPrefixOf` b
        then PresenceData <$> fromByteString b <*> pure Nothing <*> pure Nothing <*> pure Nothing <*> pure 0
        else decodeStrict' b
    return (Presence u (fromField f) uri urib chost clt tme f)
