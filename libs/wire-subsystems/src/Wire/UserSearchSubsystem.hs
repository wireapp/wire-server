{-# LANGUAGE TemplateHaskell #-}

module Wire.UserSearchSubsystem where

import Data.Domain
import Data.Id
import Data.Qualified
import Data.Range
import Imports
import Polysemy
import Wire.API.User.Search

data BrowseTeamFilters = BrowseTeamFilters
  { teamId :: TeamId,
    mQuery :: Maybe Text,
    mRoleFilter :: Maybe RoleFilter,
    mSortBy :: Maybe TeamUserSearchSortBy,
    mSortOrder :: Maybe TeamUserSearchSortOrder
  }

data UserSearchSubsystem m a where
  UpsertUser :: UserId -> UserSearchSubsystem m ()
  SearchUser :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> UserSearchSubsystem m [Contact]
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
