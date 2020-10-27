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

-- | Ownership of unique user handles.
module Brig.User.Handle
  ( claimHandleWithTTL,
    freeHandle,
    lookupHandle,
    glimpseHandle,
  )
where

import Brig.App
import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import Brig.Unique
import Cassandra
import Data.Handle (Handle, fromHandle)
import Data.Id
import Imports

-- | Claim a new handle for an existing 'User'.
claimHandleWithTTL :: Maybe Int32 -> UserId -> Maybe Handle -> Handle -> AppIO Bool
claimHandleWithTTL mbTTL uid oldHandle h = fmap isJust $ do
  owner <- lookupHandle h
  case owner of
    Just uid' | uid /= uid' -> return Nothing
    _ -> do
      env <- ask
      let key = "@" <> fromHandle h
          claimTTL = case mbTTL of
            Nothing -> (30 # Minute)
            Just ttl -> min (30 # Minute) (fromIntegral ttl # Second)
      withClaim uid key claimTTL $
        runAppT env $
          do
            -- Record ownership
            retry x5 $ write handleInsert (params Quorum (h, uid, fromMaybe 0 mbTTL))
            -- Update profile
            result <- User.updateHandleWithTTL (fromMaybe 0 mbTTL) uid h
            -- Free old handle (if it changed)
            for_ (mfilter (/= h) oldHandle) $
              freeHandle uid
            return result

-- | Free a 'Handle', making it available to be claimed again.
freeHandle :: UserId -> Handle -> AppIO ()
freeHandle uid h = do
  retry x5 $ write handleDelete (params Quorum (Identity h))
  let key = "@" <> fromHandle h
  deleteClaim uid key (30 # Minute)

-- | Lookup the current owner of a 'Handle'.
lookupHandle :: Handle -> AppIO (Maybe UserId)
lookupHandle = lookupHandleWithPolicy Quorum

-- | A weaker version of 'lookupHandle' that trades availability
-- (and potentially speed) for the possibility of returning stale data.
glimpseHandle :: Handle -> AppIO (Maybe UserId)
glimpseHandle = lookupHandleWithPolicy One

{-# INLINE lookupHandleWithPolicy #-}
lookupHandleWithPolicy :: Consistency -> Handle -> AppIO (Maybe UserId)
lookupHandleWithPolicy policy h = do
  join . fmap runIdentity
    <$> retry x1 (query1 handleSelect (params policy (Identity h)))

--------------------------------------------------------------------------------
-- Queries

handleInsert :: PrepQuery W (Handle, UserId, Int32) ()
handleInsert = "INSERT INTO user_handle (handle, user) VALUES (?, ?) USING TTL ?"

handleSelect :: PrepQuery R (Identity Handle) (Identity (Maybe UserId))
handleSelect = "SELECT user FROM user_handle WHERE handle = ?"

handleDelete :: PrepQuery W (Identity Handle) ()
handleDelete = "DELETE FROM user_handle WHERE handle = ?"
