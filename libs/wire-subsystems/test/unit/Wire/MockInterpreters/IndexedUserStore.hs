-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.IndexedUserStore where

import Data.Handle
import Data.Id
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Tuple.Extra
import Database.Bloodhound.Internal.Client qualified as ES
import Database.Bloodhound.Types qualified as ES
import Imports
import Polysemy
import Polysemy.State
import Wire.API.Team.Size
import Wire.API.User.Search
import Wire.IndexedUserStore
import Wire.MockInterpreters.UserStore (storedUserToIndexUser)
import Wire.StoredUser
import Wire.UserSearch.Types
import Wire.UserStore.IndexUser

newtype OrdDocId = OrdDocId Text
  deriving (Show, Eq, Ord)

data UserIndex = UserIndex
  { nextVersion :: ES.DocVersion,
    docs :: Map OrdDocId (UserDoc, ES.DocVersion)
  }
  deriving (Show, Eq)

fromDocId :: ES.DocId -> OrdDocId
fromDocId (ES.DocId docId) = OrdDocId docId

emptyIndex :: UserIndex
emptyIndex =
  UserIndex
    { nextVersion = (ES.DocVersion 0),
      docs = mempty
    }

storedUserToDoc :: StoredUser -> UserDoc
storedUserToDoc user =
  let indexUser = storedUserToIndexUser user
   in indexUserToDoc defaultSearchVisibilityInbound Nothing indexUser

indexFromStoredUsers :: [StoredUser] -> UserIndex
indexFromStoredUsers storedUsers = do
  run . execState emptyIndex . inMemoryIndexedUserStoreInterpreter $ do
    for_ storedUsers $ \storedUser ->
      upsert (userIdToDocId storedUser.id) (storedUserToDoc storedUser) ES.NoVersionControl

runInMemoryIndexedUserStoreIntepreter :: InterpreterFor IndexedUserStore r
runInMemoryIndexedUserStoreIntepreter =
  evalState emptyIndex
    . inMemoryIndexedUserStoreInterpreter
    . raiseUnder

inMemoryIndexedUserStoreInterpreter :: (Member (State UserIndex) r) => InterpreterFor IndexedUserStore r
inMemoryIndexedUserStoreInterpreter =
  interpret $ \case
    Upsert docId userDoc versionControl ->
      upsertImpl docId userDoc versionControl
    UpdateTeamSearchVisibilityInbound tid visibility ->
      modify $ \index ->
        index
          { docs =
              Map.map
                ( first
                    ( \doc ->
                        if doc.udTeam == Just tid
                          then doc {udSearchVisibilityInbound = Just visibility}
                          else doc
                    )
                )
                index.docs
          }
    BulkUpsert upserts -> mapM_ (uncurry3 upsertImpl) upserts
    DoesIndexExist -> pure True
    SearchUsers searcher mTeam teamSearchInfo query maxResults ->
      searchImpl searcher mTeam teamSearchInfo query maxResults
    PaginateTeamMembers {} ->
      error "IndexedUserStore: unimplemented in memory interpreter"
    GetTeamSize tid ->
      gets $ \index ->
        TeamSize
          . fromIntegral
          . length
          $ Map.filter (\(doc, _) -> doc.udTeam == Just tid) index.docs

upsertImpl :: (Member (State UserIndex) r) => ES.DocId -> UserDoc -> ES.VersionControl -> Sem r ()
upsertImpl docId userDoc versionControl =
  modify $ \index ->
    let mOldDoc = Map.lookup (fromDocId docId) index.docs
        insertedDocs ver = Map.insert (fromDocId docId) (userDoc, ver) index.docs
        insertWithVersionCheck newVer comp =
          case mOldDoc of
            (Just (_, oldVer))
              | newVer `comp` oldVer ->
                  index {docs = insertedDocs newVer}
            _ -> index
     in case (versionControl) of
          (ES.NoVersionControl) ->
            index
              { nextVersion = succ index.nextVersion,
                docs = insertedDocs index.nextVersion
              }
          (ES.InternalVersion newVer) ->
            insertWithVersionCheck newVer (>)
          (ES.ExternalGT (ES.ExternalDocVersion newVer)) ->
            insertWithVersionCheck newVer (>)
          (ES.ExternalGTE (ES.ExternalDocVersion newVer)) ->
            insertWithVersionCheck newVer (>=)
          (ES.ForceVersion (ES.ExternalDocVersion ver)) ->
            index {docs = insertedDocs ver}

data MatchType = Reject | NonTeamMember | TeamMate | NameMatch | HandleMatch
  deriving (Show)

matchScore :: MatchType -> Int
matchScore = \case
  Reject -> 0
  NonTeamMember -> 1
  TeamMate -> 2
  NameMatch -> 3
  HandleMatch -> 4

searchImpl :: (Member (State UserIndex) r) => UserId -> Maybe TeamId -> TeamSearchInfo -> Text -> Int -> Sem r (SearchResult UserDoc)
searchImpl searcher mTeam teamSearchInfo query maxResults = do
  let teamFilter (doc :: UserDoc) = case (mTeam, teamSearchInfo) of
        (Nothing, _) -> maybe NonTeamMember (const Reject) doc.udTeam
        (Just _, NoTeam) -> maybe NonTeamMember (const Reject) doc.udTeam
        (Just searcherTeam, AllUsers) ->
          if Just searcherTeam == doc.udTeam then TeamMate else NonTeamMember
        (Just searcherTeam, TeamOnly team) ->
          if (searcherTeam == team && Just searcherTeam == doc.udTeam)
            then TeamMate
            else Reject
      searcherFilter (doc :: UserDoc) = listToMaybe [Reject | doc.udId == searcher]
      tokens = Text.splitOn " " query
      nameFilter (doc :: UserDoc) =
        case doc.udNormalized of
          Nothing -> Reject
          Just normalizedName ->
            let isMatch = flip all tokens $ \token -> any (token `Text.isPrefixOf`) $ Text.splitOn " " normalizedName
             in if isMatch then NameMatch else Reject
      handleFilter (doc :: UserDoc) =
        case doc.udHandle of
          Nothing -> Reject
          Just handle ->
            if (query `Text.isPrefixOf` fromHandle handle)
              then HandleMatch
              else Reject
      totalScore (doc :: UserDoc) =
        matchScore (teamFilter doc)
          * maybe 1 matchScore (searcherFilter doc)
          * ( matchScore (nameFilter doc)
                + matchScore (handleFilter doc)
            )
  allDocs <- map fst . Map.elems <$> gets (.docs)
  pure
    . mkResult maxResults
    . map fst
    . sortOn snd
    . filter (\(_, score) -> score /= 0)
    . map (\doc -> (doc, totalScore doc))
    $ filter (\u -> fromMaybe True u.udSearchable) allDocs

mkResult :: Int -> [a] -> SearchResult a
mkResult maxResults results =
  SearchResult
    { searchTook = 0,
      searchReturned = min maxResults (length results),
      searchResults = take maxResults results,
      searchPagingState = Nothing,
      searchHasMore = Just False,
      searchFound = length results,
      searchPolicy = FullSearch
    }
