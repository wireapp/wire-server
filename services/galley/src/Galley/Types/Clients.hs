{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Galley.Types.Clients
    ( Clients
    , userIds
    , clientIds
    , toList
    , fromList
    , toMap
    , fromMap
    , singleton
    , insert
    , diff
    , filter
    , contains
    , Galley.Types.Clients.null
    , Galley.Types.Clients.nil
    , rmClient
    ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Id
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Range
import Prelude hiding (filter)

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set

newtype Clients = Clients
    { clients :: Map UserId (Set ClientId)
    } deriving (Eq, Show, Monoid)

instance Bounds Clients where
    within c x y =
        let n = Map.size (clients c) in
        n >= fromIntegral x && n <= fromIntegral y

null :: Clients -> Bool
null = Map.null . clients

nil :: Clients
nil = Clients Map.empty

userIds :: Clients -> [UserId]
userIds = Map.keys . clients

clientIds :: UserId -> Clients -> [ClientId]
clientIds u c = Set.toList $ fromMaybe Set.empty (Map.lookup u (clients c))

toList :: Clients -> [(UserId, [ClientId])]
toList = Map.foldrWithKey' fn [] . clients
  where
    fn u c a = (u, Set.toList c) : a

fromList :: [(UserId, [ClientId])] -> Clients
fromList = Clients . foldr fn Map.empty
  where
    fn (u, c) = Map.insert u (Set.fromList c)

fromMap :: Map UserId (Set ClientId) -> Clients
fromMap = Clients

toMap :: Clients -> Map UserId (Set ClientId)
toMap = clients

singleton :: UserId -> [ClientId] -> Clients
singleton u c = Clients $ Map.singleton u (Set.fromList c)

filter :: (UserId -> Bool) -> Clients -> Clients
filter p = Clients . Map.filterWithKey (\u _ -> p u) . clients

contains :: UserId -> ClientId -> Clients -> Bool
contains u c = maybe False (Set.member c) . Map.lookup u . clients

insert :: UserId -> ClientId -> Clients -> Clients
insert u c = Clients . Map.insertWith Set.union u (Set.singleton c) . clients

diff :: Clients -> Clients -> Clients
diff (Clients ca) (Clients cb) = Clients $ Map.differenceWith fn ca cb
  where
    fn a b =
        let d = a `Set.difference` b in
        if Set.null d then Nothing else Just d

rmClient :: UserId -> ClientId -> Clients -> Clients
rmClient u c (Clients m) = Clients $ Map.update f u m
  where
    f x = let s = Set.delete c x in if Set.null s then Nothing else Just s
