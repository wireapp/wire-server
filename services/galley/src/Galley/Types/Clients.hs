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
    clientIds,
    toList,
    fromList,
    fromUserClients,
    toMap,
    contains,
  )
where

import Data.Id (ClientId, UserId)
import Data.Map.Strict qualified as Map
import Data.Range (Bounds (..))
import Data.Set qualified as Set
import Imports hiding (toList)
import Wire.API.User.Client (UserClients (..))

newtype Clients = Clients
  { clients :: UserClients
  }
  deriving (Eq, Show, Semigroup, Monoid)

instance Bounds Clients where
  within c x y =
    let n = Map.size ((userClients . clients) c)
     in n >= fromIntegral x && n <= fromIntegral y

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

toMap :: Clients -> Map UserId (Set ClientId)
toMap = userClients . clients

contains :: UserId -> ClientId -> Clients -> Bool
contains u c =
  maybe False (Set.member c) . Map.lookup u . (userClients . clients)
