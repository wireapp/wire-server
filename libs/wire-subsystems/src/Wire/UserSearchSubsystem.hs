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

data BrowseTeamFilters = BrowseTeamFilters
  { teamId :: TeamId,
    mQuery :: Maybe Text,
    mRoleFilter :: Maybe RoleFilter,
    mSortBy :: Maybe TeamUserSearchSortBy,
    mSortOrder :: Maybe TeamUserSearchSortOrder
  }

data UserSearchSubsystem m a where
  SyncUser :: UserId -> UserSearchSubsystem m ()
  UpdateTeamSearchVisibilityInbound :: TeamStatus SearchVisibilityInboundConfig -> UserSearchSubsystem m ()
  SearchUsers :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> UserSearchSubsystem m (SearchResult Contact)
  BrowseTeam :: UserId -> BrowseTeamFilters -> Maybe (Range 1 500 Int32) -> Maybe PagingState -> UserSearchSubsystem m [TeamContact]

makeSem ''UserSearchSubsystem

-- | This function exists because there are a lot query params and they cannot all become 'BrowseTeamFilters' automatically
browseTeamHandler ::
  (Member UserSearchSubsystem r) =>
  UserId ->
  TeamId ->
  Maybe Text ->
  Maybe RoleFilter ->
  Maybe TeamUserSearchSortBy ->
  Maybe TeamUserSearchSortOrder ->
  Maybe (Range 1 500 Int32) ->
  Maybe PagingState ->
  Sem r [TeamContact]
browseTeamHandler uid tid mQuery mRoleFilter mTeamUserSearchSortBy mTeamUserSearchSortOrder mMaxResults mPagingState = do
  let browseTeamFilters = BrowseTeamFilters tid mQuery mRoleFilter mTeamUserSearchSortBy mTeamUserSearchSortOrder
  browseTeam uid browseTeamFilters mMaxResults mPagingState

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
