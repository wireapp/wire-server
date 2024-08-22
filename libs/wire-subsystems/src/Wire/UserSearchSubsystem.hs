{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSearchSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus)
import Wire.API.Team.Feature
import Wire.API.User.Search
import Wire.UserSearch.Types

data UserSearchSubsystem m a where
  SyncUser :: UserId -> UserSearchSubsystem m ()
  UpdateTeamSearchVisibilityInbound :: TeamStatus SearchVisibilityInboundConfig -> UserSearchSubsystem m ()
  SearchUsers :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> UserSearchSubsystem m (SearchResult Contact)
  BrowseTeam ::
    UserId ->
    BrowseTeamFilters ->
    Maybe (Range 1 500 Int) ->
    Maybe PagingState ->
    UserSearchSubsystem m (SearchResult TeamContact)

makeSem ''UserSearchSubsystem

-- | Bulk operations, must not be used from any web handler
data UserSearchSubsystemBulk m a where
  -- | Only changes data if it is not updated since last update, use when users
  -- need to be synced because of an outage, or migrating to a new ES instance.
  SyncAllUsers :: UserSearchSubsystemBulk m ()
  -- | Overwrite all users in the ES index, use it when trying to fix some
  -- inconsistency or while introducing a new field in the mapping.
  ForceSyncAllUsers :: UserSearchSubsystemBulk m ()
  MigrateData :: UserSearchSubsystemBulk m ()

makeSem ''UserSearchSubsystemBulk
