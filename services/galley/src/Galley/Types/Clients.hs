{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import qualified Data.Map.Strict as Map
import Data.Range
import qualified Data.Set as Set
import Galley.Types (UserClients (..))
import Imports hiding (filter, toList)

newtype Clients
  = Clients
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
fromUserClients ucs = Clients ucs

fromMap :: Map UserId (Set ClientId) -> Clients
fromMap = Clients . UserClients

toMap :: Clients -> Map UserId (Set ClientId)
toMap = userClients . clients

singleton :: UserId -> [ClientId] -> Clients
singleton u c =
  Clients . UserClients $ Map.singleton u (Set.fromList c)

filter :: (UserId -> Bool) -> Clients -> Clients
filter p =
  Clients . UserClients
    . Map.filterWithKey (\u _ -> p u)
    . (userClients . clients)

contains :: UserId -> ClientId -> Clients -> Bool
contains u c =
  maybe False (Set.member c) . Map.lookup u . (userClients . clients)

insert :: UserId -> ClientId -> Clients -> Clients
insert u c =
  Clients . UserClients
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
