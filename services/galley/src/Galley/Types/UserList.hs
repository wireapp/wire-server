-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Types.UserList
  ( UserList (..),
    toUserList,
    ulAddLocal,
    ulAll,
  )
where

import Data.Qualified
import Data.Tagged
import Imports

-- | A list of users, partitioned into locals and remotes
data UserList a = UserList
  { ulLocals :: [a],
    ulRemotes :: [Remote a]
  }
  deriving (Functor, Foldable, Traversable)

toUserList :: Foldable f => Local x -> f (Qualified a) -> UserList a
toUserList loc = uncurry (flip UserList) . partitionRemoteOrLocalIds' (lDomain loc)

ulAddLocal :: a -> UserList a -> UserList a
ulAddLocal x ul = ul {ulLocals = x : ulLocals ul}

ulAll :: Local x -> UserList a -> [Qualified a]
ulAll loc ul = map (unTagged . qualifyAs loc) (ulLocals ul) <> map unTagged (ulRemotes ul)
