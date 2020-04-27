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

module Network.Wire.Bot.Clients
  ( Clients,
    Network.Wire.Bot.Clients.empty,
    addSession,
    addMembers,
    foldSessions,
    lookupSession,
  )
where

import Data.Id
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Imports
import System.CryptoBox (Session)

data Clients = Clients
  { members :: TVar (Map ConvId (Set UserId)),
    sessions :: TVar Sessions
  }

newtype Sessions = Sessions
  {clients :: Map UserId (Map ClientId Session)}

empty :: IO Clients
empty = Clients <$> newTVarIO Map.empty <*> newTVarIO (Sessions Map.empty)

addSession :: MonadIO m => Clients -> UserId -> Map ClientId Session -> m ()
addSession self u d =
  liftIO . atomically $ modifyTVar' (sessions self) (Sessions . Map.alter f u . clients)
  where
    f Nothing = Just d
    f (Just m) = Just $ m `Map.union` d

addMembers :: MonadIO m => Clients -> ConvId -> [UserId] -> m ()
addMembers self c uu =
  liftIO . atomically $
    modifyTVar' (members self) (Map.insertWith Set.union c (Set.fromList uu))

-- TODO: Move / inline to Network.Wire.Bot.Crypto and remove this module
foldSessions :: MonadIO m => Clients -> ConvId -> a -> (UserId -> ClientId -> Session -> a -> m a) -> m a
foldSessions self c a f =
  foldrM fun a =<< Map.findWithDefault Set.empty c <$> liftIO (readTVarIO (members self))
  where
    fun u acc1 = do
      cm <- Map.findWithDefault Map.empty u . clients <$> liftIO (readTVarIO (sessions self))
      foldrM (\(d, s) acc2 -> f u d s acc2) acc1 (Map.toList cm)

lookupSession :: MonadIO m => Clients -> UserId -> ClientId -> m (Maybe Session)
lookupSession self u d = do
  s <- liftIO $ readTVarIO (sessions self)
  return $ Map.lookup u (clients s) >>= Map.lookup d
