{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Galley.Types.Clients
  ( Clients,
    userIds,
    clientIds,
    toList,
    fromList,
    fromUserClients,
    toMap,
    fromMap,
    singleton,
    insert,
    diff,
    filter,
    contains,
    Galley.Types.Clients.null,
    Galley.Types.Clients.nil,
    rmClient,
  )
where

import Data.Id
import Data.Map.Strict qualified as Map
import Data.Range
import Data.Set qualified as Set
import Imports hiding (filter, toList)
import Wire.API.User.Client

newtype Clients = Clients
  { clients :: UserClients
  }
  deriving (Eq, Show, Semigroup, Monoid)

instance Bounds Clients where
  within c x y =
    let n = Map.size ((userClients . clients) c)
     in n >= fromIntegral x && n <= fromIntegral y

null :: Clients -> Bool
null = Map.null . (userClients . clients)

nil :: Clients
nil = Clients $ UserClients Map.empty

userIds :: Clients -> [UserId]
userIds = Map.keys . (userClients . clients)

clientIds :: UserId -> Clients -> [ClientId]
clientIds u c = Set.toList $ fromMaybe Set.empty (Map.lookup u ((userClients . clients) c))

toList :: Clients -> [(UserId, [ClientId])]
toList = Map.foldrWithKey' fn [] . (userClients . clients)
  where
    fn u c a = (u, Set.toList c) : a

fromList :: [(UserId, [ClientId])] -> Clients
fromList = Clients . UserClients . foldr fn Map.empty
  where
    fn (u, c) = Map.insert u (Set.fromList c)

fromUserClients :: UserClients -> Clients
fromUserClients = Clients

fromMap :: Map UserId (Set ClientId) -> Clients
fromMap = Clients . UserClients

toMap :: Clients -> Map UserId (Set ClientId)
toMap = userClients . clients

singleton :: UserId -> [ClientId] -> Clients
singleton u c =
  Clients . UserClients $ Map.singleton u (Set.fromList c)

filter :: (UserId -> Bool) -> Clients -> Clients
filter p =
  Clients
    . UserClients
    . Map.filterWithKey (\u _ -> p u)
    . (userClients . clients)

contains :: UserId -> ClientId -> Clients -> Bool
contains u c =
  maybe False (Set.member c) . Map.lookup u . (userClients . clients)

insert :: UserId -> ClientId -> Clients -> Clients
insert u c =
  Clients
    . UserClients
    . Map.insertWith Set.union u (Set.singleton c)
    . (userClients . clients)

diff :: Clients -> Clients -> Clients
diff (Clients (UserClients ca)) (Clients (UserClients cb)) =
  Clients . UserClients $ Map.differenceWith fn ca cb
  where
    fn a b =
      let d = a `Set.difference` b
       in if Set.null d then Nothing else Just d

rmClient :: UserId -> ClientId -> Clients -> Clients
rmClient u c (Clients (UserClients m)) =
  Clients . UserClients $ Map.update f u m
  where
    f x = let s = Set.delete c x in if Set.null s then Nothing else Just s
