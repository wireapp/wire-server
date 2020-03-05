module Brig.API.Util where

import Brig.API.Handler
import qualified Brig.Data.User as Data
import Brig.Types
import Control.Monad
import Data.Id as Id
import Data.IdMapping (MappedOrLocalId (Local))
import Data.Maybe
import Imports

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> Handler [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
  selfTeam <- lift $ Data.lookupUserTeam self
  return $ case selfTeam of
    Just team -> filter (\x -> profileTeam x == Just team) us
    Nothing -> us

-- | this exists as a shim to find and mark places where we need to handle 'OpaqueUserId's.
resolveOpaqueUserId :: Monad m => OpaqueUserId -> m (MappedOrLocalId Id.U)
resolveOpaqueUserId (Id opaque) =
  -- FUTUREWORK(federation): implement database lookup
  pure . Local $ Id opaque
