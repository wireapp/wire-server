module Wire.UserList where

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

ulNull :: UserList a -> Bool
ulNull ul = null ul.ulLocals && null ul.ulRemotes
