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
  ( claimHandle,
    claimHandleInvitation,
    freeHandle,
    lookupHandle,
    glimpseHandle,
  )
where

import Brig.App
import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import qualified Brig.Team.DB as Inv
import Brig.Unique
import Cassandra
import Data.Coerce (coerce)
import Data.Handle (Handle, fromHandle)
import Data.Id
import Imports

claimHandle :: UserId -> Maybe Handle -> Handle -> AppIO Bool
claimHandle = claimHandle' (User.updateHandle)

claimHandleInvitation :: TeamId -> InvitationId -> Maybe Handle -> Handle -> AppIO Bool
claimHandleInvitation tid invid = claimHandle' (Inv.updInvitationHandle tid . back) (forth invid)
  where
    back = coerce @UserId @InvitationId
    forth = coerce @InvitationId @UserId

-- | Claim a handle for an invitation or a user.  Invitations can be referenced by the coerced
-- 'UserId'.
claimHandle' :: (UserId -> Handle -> AppIO ()) -> UserId -> Maybe Handle -> Handle -> AppIO Bool
claimHandle' updOperation uid oldHandle h = do
  owner <- lookupHandle h
  case owner of
    Just uid' | uid /= uid' -> return False
    _ -> do
      env <- ask
      let key = "@" <> fromHandle h
      claimed <- withClaim uid key (30 # Minute) $
        runAppT env $
          do
            -- Record ownership
            retry x5 $ write handleInsert (params Quorum (h, uid))
            -- Update profile
            updOperation uid h
            -- Free old handle (if it changed)
            for_ (mfilter (/= h) oldHandle) $
              freeHandle uid
      return (isJust claimed)

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

handleInsert :: PrepQuery W (Handle, UserId) ()
handleInsert = "INSERT INTO user_handle (handle, user) VALUES (?, ?)"

handleSelect :: PrepQuery R (Identity Handle) (Identity (Maybe UserId))
handleSelect = "SELECT user FROM user_handle WHERE handle = ?"

handleDelete :: PrepQuery W (Identity Handle) ()
handleDelete = "DELETE FROM user_handle WHERE handle = ?"
