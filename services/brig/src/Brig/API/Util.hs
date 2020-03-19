module Brig.API.Util where

import Brig.API.Handler
import Brig.App (Env, settings)
import qualified Brig.Data.User as Data
import Brig.Options (defEnableFederation, enableFederation)
import Brig.Types
import Control.Lens (view)
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

-- FUTUREWORK(federation): implement function to resolve IDs in batch

-- | this exists as a shim to find and mark places where we need to handle 'OpaqueUserId's.
resolveOpaqueUserId :: MonadReader Env m => OpaqueUserId -> m (MappedOrLocalId Id.U)
resolveOpaqueUserId (Id opaque) = do
  mEnabled <- view (settings . enableFederation)
  case fromMaybe defEnableFederation mEnabled of
    False ->
      -- don't check the ID mapping, just assume it's local
      pure . Local $ Id opaque
    True ->
      -- FUTUREWORK(federation): implement database lookup
      pure . Local $ Id opaque
