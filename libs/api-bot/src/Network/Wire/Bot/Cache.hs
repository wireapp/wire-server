{-# LANGUAGE OverloadedStrings #-}

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

module Network.Wire.Bot.Cache
  ( CachedUser (..),
    Cache,
    fromFile,
    Network.Wire.Bot.Cache.empty,
    get,
    put,
  )
where

import Data.ByteString.Conversion
import Data.LanguageCodes
import Data.Misc
import Data.Text.Encoding
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import qualified Data.Vector as V
import Imports
import Network.Wire.Client.API.User
import System.Logger hiding (new)
import System.Random.MWC
import System.Random.MWC.Distributions (uniformShuffle)

newtype Cache = Cache {cache :: IORef [CachedUser]}

data CachedUser = CachedUser !PlainTextPassword !User

-- | Load users out of a file in the following format:
--
-- @
-- user1's UUID,email1,password1
-- user2's UUID,email2,password2
-- ...
-- @
fromFile :: Logger -> GenIO -> FilePath -> IO Cache
fromFile logger gen path = do
  triples <- map (Text.splitOn ",") . Text.lines <$> Text.readFile path
  shuffled <- V.toList <$> uniformShuffle (V.fromList triples) gen
  c <- newIORef =<< foldM (toUser logger) [] shuffled
  return (Cache c)

empty :: IO Cache
empty = Cache <$> newIORef []

get :: (MonadIO m, HasCallStack) => Cache -> m CachedUser
get c = liftIO . atomicModifyIORef (cache c) $ \u ->
  case u of
    [] ->
      error
        "Cache.get: an account was requested from the cache, \
        \but the cache of available user accounts is empty"
    (x : xs) -> (xs, x)

put :: MonadIO m => Cache -> CachedUser -> m ()
put c a = liftIO . atomicModifyIORef (cache c) $ \u -> (a : u, ())

toUser :: HasCallStack => Logger -> [CachedUser] -> [LText] -> IO [CachedUser]
toUser _ acc [i, e, p] = do
  let pw = PlainTextPassword . Text.toStrict $ Text.strip p
  let iu = error "Cache.toUser: invalid user"
  let ie = error "Cache.toUser: invalid email"
  let ui = fromMaybe iu . fromByteString . encodeUtf8 . Text.toStrict . Text.strip $ i
  let em = fromMaybe ie . parseEmail . Text.toStrict . Text.strip $ e
  return . (: acc) $
    CachedUser
      pw
      User
        { userId = ui,
          userDisplayName = Name $ "Fakebot-" <> Text.toStrict (Text.strip i),
          userPict = Pict [],
          userAssets = [],
          userIdentity = Just (EmailIdentity em),
          userAccentId = ColourId 0,
          userDeleted = False,
          userLocale = Locale (Language EN) Nothing,
          userService = Nothing,
          userHandle = Nothing,
          userExpire = Nothing,
          userTeam = Nothing,
          userManagedBy = ManagedByWire
        }
toUser g acc entry = do
  warn g $ msg (val "invalid entry: " +++ show entry)
  return acc
