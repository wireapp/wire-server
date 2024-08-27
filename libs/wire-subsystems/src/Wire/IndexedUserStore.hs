{-# LANGUAGE TemplateHaskell #-}

module Wire.IndexedUserStore where

import Data.Id
import Database.Bloodhound.Types hiding (SearchResult)
import Imports
import Polysemy
import Wire.API.User.Search
import Wire.UserSearch.Types

data IndexedUserStore m a where
  Upsert :: DocId -> UserDoc -> VersionControl -> IndexedUserStore m ()
  UpdateTeamSearchVisibilityInbound ::
    TeamId ->
    SearchVisibilityInbound ->
    IndexedUserStore m ()
  -- | Will only be applied to main ES index and not the additional one
  BulkUpsert :: [(DocId, UserDoc, VersionControl)] -> IndexedUserStore m ()
  DoesIndexExist :: IndexedUserStore m Bool
  SearchUsers ::
    UserId ->
    Maybe TeamId ->
    TeamSearchInfo ->
    Text ->
    Int ->
    IndexedUserStore m (SearchResult UserDoc)
  PaginateTeamMembers ::
    BrowseTeamFilters ->
    Int ->
    Maybe PagingState ->
    IndexedUserStore m (SearchResult UserDoc)

makeSem ''IndexedUserStore
