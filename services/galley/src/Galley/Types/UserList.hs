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

module Galley.Types.UserList
  ( UserList (..),
    toUserList,
    ulAddLocal,
    ulAll,
    ulFromLocals,
    ulFromRemotes,
    ulDiff,
  )
where

import Data.Qualified
import Imports

-- | A list of users, partitioned into locals and remotes
data UserList a = UserList
  { ulLocals :: [a],
    ulRemotes :: [Remote a]
  }
  deriving (Show, Functor, Foldable, Traversable)

instance Semigroup (UserList a) where
  UserList locals1 remotes1 <> UserList locals2 remotes2 =
    UserList (locals1 <> locals2) (remotes1 <> remotes2)

instance Monoid (UserList a) where
  mempty = UserList mempty mempty

toUserList :: (Foldable f) => Local x -> f (Qualified a) -> UserList a
toUserList loc = uncurry UserList . partitionQualified loc

ulAddLocal :: a -> UserList a -> UserList a
ulAddLocal x ul = ul {ulLocals = x : ulLocals ul}

ulAll :: Local x -> UserList a -> [Qualified a]
ulAll loc ul = map (tUntagged . qualifyAs loc) (ulLocals ul) <> map tUntagged (ulRemotes ul)

ulFromLocals :: [a] -> UserList a
ulFromLocals = flip UserList []

ulFromRemotes :: [Remote a] -> UserList a
ulFromRemotes = UserList []

-- | Remove from the first list all the users that are in the second list.
ulDiff :: (Eq a) => UserList a -> UserList a -> UserList a
ulDiff (UserList lA rA) (UserList lB rB) =
  UserList
    (filter (`notElem` lB) lA)
    (filter (`notElem` rB) rA)
