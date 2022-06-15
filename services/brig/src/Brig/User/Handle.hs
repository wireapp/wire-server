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

-- | Ownership of unique user handles.
module Brig.User.Handle
  ( claimHandle,
    freeHandle,
    lookupHandle,
    glimpseHandle,
  )
where

import Brig.Data.Instances ()
import qualified Brig.Data.User as User
import Brig.Sem.UniqueClaimsStore
import Brig.Sem.UserHandleStore
  ( Consistency (..),
    UserHandleStore,
    deleteHandle,
    getHandleWithConsistency,
    insertHandle,
    lookupHandle,
  )
import Brig.Sem.UserQuery
import Brig.Unique
import Data.Handle (Handle, fromHandle)
import Data.Id
import Imports hiding (All)
import Polysemy
import Polysemy.Async
import Polysemy.Conc.Effect.Race
import Polysemy.Resource

-- | Claim a new handle for an existing 'User'.
claimHandle ::
  forall r.
  Members
    '[ Async,
       Race,
       Resource,
       UniqueClaimsStore,
       UserHandleStore,
       UserQuery
     ]
    r =>
  UserId ->
  Maybe Handle ->
  Handle ->
  Sem r Bool
claimHandle uid oldHandle newHandle =
  isJust <$> do
    owner <- lookupHandle newHandle
    case owner of
      Just uid' | uid /= uid' -> pure Nothing
      _ -> do
        let key = "@" <> fromHandle newHandle
        withClaim uid key (30 # Minute) $ do
          -- Record ownership
          insertHandle @r newHandle uid
          -- Update profile
          result <- User.updateHandle uid newHandle
          -- Free old handle (if it changed)
          for_ (mfilter (/= newHandle) oldHandle) $
            freeHandle uid
          pure result

-- | Free a 'Handle', making it available to be claimed again.
freeHandle ::
  Members '[UniqueClaimsStore, UserHandleStore] r =>
  UserId ->
  Handle ->
  Sem r ()
freeHandle uid h = do
  deleteHandle h
  let key = "@" <> fromHandle h
  deleteClaims uid (30 # Minute) key

-- | A weaker version of 'lookupHandle' that trades availability
-- (and potentially speed) for the possibility of returning stale data.
glimpseHandle :: Member UserHandleStore r => Handle -> Sem r (Maybe UserId)
glimpseHandle = getHandleWithConsistency One
