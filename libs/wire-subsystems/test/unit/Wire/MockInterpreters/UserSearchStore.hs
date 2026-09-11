-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | In-memory interpreter for 'UserSearchStore', used by 'MiniBackend' unit
-- tests.  Ports the search semantics of the former
-- 'Wire.MockInterpreters.IndexedUserStore'.
--
-- Note: like the former mock, 'SearchUsers' does not apply the
-- 'SearchVisibilityInbound' restriction of the searched user's team; the
-- setting is recorded per team (and applied by the Postgres interpreter in
-- production).
module Wire.MockInterpreters.UserSearchStore where

import Data.Aeson qualified as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.ByteString.Lazy qualified as LBS
import Data.Domain (Domain)
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map.Strict qualified as Map
import Data.Qualified (Local, Qualified (..), tDomain, tUnqualified)
import Data.Text qualified as Text
import Data.Text.Ascii (decodeBase64Url, encodeBase64Url)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.State
import Wire.API.Team.Role (Role, roleName)
import Wire.API.Team.Size (TeamSize (..))
import Wire.API.User
import Wire.API.User.Search
import Wire.UserSearch.Normalize (normalized)
import Wire.UserSearchStore

-- | A user as seen by the mock search store.  Roughly the document that
-- used to be indexed into ElasticSearch, plus the fields needed to apply
-- the former @shouldIndex@ candidate filter.
data SearchUserDoc = SearchUserDoc
  { sdId :: UserId,
    sdType :: UserType,
    sdTeam :: Maybe TeamId,
    sdName :: Text,
    sdAccentId :: Maybe Int,
    sdHandle :: Maybe Text,
    sdEmail :: Maybe EmailAddress,
    sdEmailUnvalidated :: Maybe EmailAddress,
    sdNormalized :: Maybe Text,
    sdSearchable :: Maybe Bool,
    sdRole :: Maybe Role,
    sdCreatedAt :: Maybe UTCTime,
    sdManagedBy :: Maybe ManagedBy,
    sdSAMLIdp :: Maybe Text,
    sdScimExternalId :: Maybe Text,
    sdSso :: Maybe Sso,
    sdAccountStatus :: Maybe AccountStatus,
    sdActivated :: Bool,
    sdService :: Maybe ServiceId
  }
  deriving (Show, Eq)

data UserSearchIndex = UserSearchIndex
  { docs :: Map UserId SearchUserDoc,
    teamVisibility :: Map TeamId SearchVisibilityInbound
  }
  deriving (Show, Eq)

emptyUserSearchIndex :: UserSearchIndex
emptyUserSearchIndex = UserSearchIndex {docs = mempty, teamVisibility = mempty}

runInMemoryUserSearchStoreInterpreter ::
  (Member (Input (Local ())) r) =>
  InterpreterFor UserSearchStore r
runInMemoryUserSearchStoreInterpreter =
  evalState emptyUserSearchIndex
    . inMemoryUserSearchStoreInterpreter
    . raiseUnder

inMemoryUserSearchStoreInterpreter ::
  ( Member (State UserSearchIndex) r,
    Member (Input (Local ())) r
  ) =>
  InterpreterFor UserSearchStore r
inMemoryUserSearchStoreInterpreter =
  interpret $ \case
    SearchUsers lSearcher mTeam teamSearchInfo query maxResults mTypes ->
      searchImpl lSearcher mTeam teamSearchInfo query maxResults mTypes
    PaginateTeamMembers filters maxResults mPagingState ->
      paginateTeamMembersImpl filters maxResults mPagingState
    SearchUsersFederated mOnlyInTeams query maxResults mTypes ->
      federatedSearchImpl mOnlyInTeams query maxResults mTypes
    GetTeamSize tid ->
      gets $ \index ->
        let isCounted doc =
              doc.sdTeam == Just tid
                && doc.sdActivated
                && maybe True (`elem` [Active, Suspended]) doc.sdAccountStatus
                && isNothing doc.sdService
            countTyped ty =
              fromIntegral . length $
                filter (\doc -> isCounted doc && mType doc == Just ty) (Map.elems index.docs)
            mType doc = case doc.sdType of
              UserTypeRegular -> Just UserTypeRegular
              UserTypeApp -> Just UserTypeApp
              UserTypeBot -> Nothing
         in TeamSize {regulars = countTyped UserTypeRegular, apps = countTyped UserTypeApp}
    SetTeamSearchVisibilityInbound tid visibility ->
      modify $ \index ->
        index {teamVisibility = Map.insert tid visibility index.teamVisibility}

emailText :: EmailAddress -> Text
emailText = TE.decodeUtf8 . toByteString'

toContact :: Domain -> SearchUserDoc -> Contact
toContact dom doc =
  Contact
    { contactQualifiedId = Qualified doc.sdId dom,
      contactName = doc.sdName,
      contactColorId = doc.sdAccentId,
      contactHandle = doc.sdHandle,
      contactTeam = doc.sdTeam,
      contactType = doc.sdType
    }

data MatchType = Reject | NonTeamMember | TeamMate | NameMatch | HandleMatch
  deriving (Show)

matchScore :: MatchType -> Int
matchScore = \case
  Reject -> 0
  NonTeamMember -> 1
  TeamMate -> 2
  NameMatch -> 3
  HandleMatch -> 4

searchImpl ::
  forall r.
  (Member (State UserSearchIndex) r) =>
  Local UserId ->
  Maybe TeamId ->
  TeamSearchInfo ->
  Text ->
  Int ->
  Maybe [UserTypeFilter] ->
  Sem r (SearchResult Contact)
searchImpl lSearcher mTeam teamSearchInfo query maxResults mTypes = do
  allDocs <- gets (Map.elems . (.docs))
  pure
    . mkContactResult maxResults
    . map (toContact (tDomain lSearcher))
    . map fst
    . sortOn snd
    . filter (\(_, score) -> score /= 0)
    . map (\doc -> (doc, totalScore doc))
    . filter candidateDoc
    . filter (\u -> fromMaybe True u.sdSearchable)
    . filter (typeMatches mTypes)
    $ allDocs
  where
    typeMatches = \case
      Nothing -> const True
      Just [] -> const True
      Just uts -> \doc -> userTypeToFilter doc.sdType `elem` uts

    userTypeToFilter UserTypeRegular = UserTypeFilterRegular
    userTypeToFilter UserTypeApp = UserTypeFilterApp
    userTypeToFilter UserTypeBot = UserTypeFilterRegular

    candidateDoc doc =
      doc.sdActivated
        && maybe True (`elem` [Active, Suspended]) doc.sdAccountStatus
        && isNothing doc.sdService
        && doc.sdId /= tUnqualified lSearcher

    teamFilter (doc :: SearchUserDoc) = case (mTeam, teamSearchInfo) of
      (Nothing, _) -> maybe NonTeamMember (const Reject) doc.sdTeam
      (Just _, NoTeam) -> maybe NonTeamMember (const Reject) doc.sdTeam
      (Just searcherTeam, AllUsers) ->
        if Just searcherTeam == doc.sdTeam then TeamMate else NonTeamMember
      (Just searcherTeam, TeamOnly team) ->
        if searcherTeam == team && Just searcherTeam == doc.sdTeam
          then TeamMate
          else Reject
    tokens = Text.splitOn " " (normalized query)
    nameFilter (doc :: SearchUserDoc) =
      case doc.sdNormalized of
        Nothing -> Reject
        Just normalizedName ->
          let isMatch = all (\token -> any (token `Text.isPrefixOf`) $ Text.splitOn " " normalizedName) tokens
           in if isMatch then NameMatch else Reject
    handleFilter (doc :: SearchUserDoc) =
      case doc.sdHandle of
        Nothing -> Reject
        Just handle ->
          if normalized query `Text.isPrefixOf` handle
            then HandleMatch
            else Reject
    totalScore (doc :: SearchUserDoc) =
      matchScore (teamFilter doc)
        * (matchScore (nameFilter doc) + matchScore (handleFilter doc))

paginateTeamMembersImpl ::
  (Member (State UserSearchIndex) r) =>
  BrowseTeamFilters ->
  Int ->
  Maybe PagingState ->
  Sem r (SearchResult TeamContact)
paginateTeamMembersImpl filters maxResults mPagingState = do
  allDocs <- gets (Map.elems . (.docs))
  let offset = fromMaybe 0 (mPagingState >>= decodeOffset)
      teamDocs =
        filter candidateDoc
          . filter (filterTeam filters.teamId)
          . filter (filterSearchable filters.mSearchable)
          . filter (filterEmail filters.mEmailVerificationFilter)
          . filter (filterRole filters.mRoleFilter)
          . filter (filterQuery filters.mQuery)
          $ allDocs
      sorted = sortDocs teamDocs
      page = take maxResults (drop offset sorted)
      hasMore = length sorted > offset + length page
  pure (mkResult maxResults (map toTeamContact page) hasMore (offset + length page))
  where
    candidateDoc doc =
      doc.sdActivated
        && maybe True (`elem` [Active, Suspended]) doc.sdAccountStatus
        && isNothing doc.sdService

    filterTeam tid doc = doc.sdTeam == Just tid

    filterSearchable = \case
      Nothing -> const True
      Just False -> \doc -> doc.sdSearchable == Just False
      Just True -> \doc -> fromMaybe True doc.sdSearchable

    filterRole = \case
      Nothing -> const True
      Just (RoleFilter rs) -> \doc -> maybe False (`elem` rs) doc.sdRole

    filterEmail = \case
      Nothing -> const True
      Just EmailVerified -> \doc -> isJust doc.sdEmail && isNothing doc.sdEmailUnvalidated
      Just EmailUnverified -> \doc -> isJust doc.sdEmailUnvalidated

    filterQuery mQuery doc = case normalized <$> mQuery of
      Nothing -> True
      Just q | Text.null q -> True
      Just q -> all (`tokenMatches` doc) (Text.splitOn " " q)

    tokenMatches token doc =
      maybe False (any (token `Text.isPrefixOf`) . Text.splitOn " ") doc.sdNormalized
        || maybe False (token `Text.isPrefixOf`) doc.sdHandle
        || maybe False ((token `Text.isPrefixOf`) . emailText) doc.sdEmail

    -- Without an explicit sort, browse is ordered by creation date, newest
    -- first (as in the former ES query and the PG interpreter).  With one,
    -- missing keys sort last when ascending and first when descending, and
    -- the user id is the deterministic tie breaker.
    sortDocs :: [SearchUserDoc] -> [SearchUserDoc]
    sortDocs docs = case filters.mSortBy of
      Nothing -> sortOn (\doc -> (Down doc.sdCreatedAt, doc.sdId)) docs
      Just _ -> arrange (fromMaybe SortOrderAsc filters.mSortOrder) (sortOn ascKey docs)
      where
        ascKey doc = (isNothing (keyOf doc), keyOf doc, doc.sdId)

    keyOf :: SearchUserDoc -> Maybe Text
    keyOf doc = case filters.mSortBy of
      Just SortByName -> Just doc.sdName
      Just SortByHandle -> doc.sdHandle
      Just SortByEmail -> emailText <$> doc.sdEmail
      Just SortBySAMLIdp -> doc.sdSAMLIdp
      Just SortByRole -> roleName @Text <$> doc.sdRole
      Just SortByManagedBy -> Text.pack . show <$> doc.sdManagedBy
      Just SortByCreatedAt -> Text.pack . show <$> doc.sdCreatedAt
      Nothing -> Text.pack . show <$> doc.sdCreatedAt

    arrange = \case
      SortOrderAsc -> id
      SortOrderDesc -> reverse

    toTeamContact :: SearchUserDoc -> TeamContact
    toTeamContact doc =
      TeamContact
        { teamContactUserId = doc.sdId,
          teamContactUserType = doc.sdType,
          teamContactName = doc.sdName,
          teamContactColorId = doc.sdAccentId,
          teamContactHandle = doc.sdHandle,
          teamContactTeam = doc.sdTeam,
          teamContactEmail = doc.sdEmail,
          teamContactCreatedAt = toUTCTimeMillis <$> doc.sdCreatedAt,
          teamContactManagedBy = doc.sdManagedBy,
          teamContactSAMLIdp = doc.sdSAMLIdp,
          teamContactRole = doc.sdRole,
          teamContactScimExternalId = doc.sdScimExternalId,
          teamContactSso = doc.sdSso,
          teamContactEmailUnvalidated = doc.sdEmailUnvalidated,
          teamContactUserGroups = [],
          teamContactSearchable = fromMaybe True doc.sdSearchable
        }

federatedSearchImpl ::
  (Member (State UserSearchIndex) r, Member (Input (Local ())) r) =>
  Maybe [TeamId] ->
  Text ->
  Int ->
  Maybe [UserTypeFilter] ->
  Sem r (SearchResult Contact)
federatedSearchImpl mOnlyInTeams query maxResults mTypes = do
  loc <- input
  index <- get
  let allDocs = Map.elems index.docs
  pure
    . mkContactResult maxResults
    . map (toContact (tDomain loc))
    . filter candidateDoc
    . filter (\u -> fromMaybe True u.sdSearchable)
    . filter (typeMatches mTypes)
    . filter (visibility index)
    . filter matchesQuery
    $ allDocs
  where
    term = normalized query
    tokens = Text.splitOn " " term

    typeMatches = \case
      Nothing -> const True
      Just [] -> const True
      Just uts -> \doc -> userTypeToFilter doc.sdType `elem` uts

    userTypeToFilter UserTypeRegular = UserTypeFilterRegular
    userTypeToFilter UserTypeApp = UserTypeFilterApp
    userTypeToFilter UserTypeBot = UserTypeFilterRegular

    candidateDoc doc =
      doc.sdActivated
        && maybe True (`elem` [Active, Suspended]) doc.sdAccountStatus
        && isNothing doc.sdService

    visOf idx tid = Map.findWithDefault SearchableByOwnTeam tid idx.teamVisibility

    visibility idx doc = case mOnlyInTeams of
      Nothing -> case doc.sdTeam of
        Nothing -> True
        Just tid -> visOf idx tid == SearchableByAllTeams
      Just [] -> False
      Just teams ->
        maybe False (\tid -> tid `elem` teams && visOf idx tid == SearchableByAllTeams) doc.sdTeam

    matchesQuery doc =
      not (Text.null term)
        && all
          ( \token ->
              any (token `Text.isPrefixOf`) (maybe [] (Text.splitOn " ") doc.sdNormalized)
                || maybe False (token `Text.isPrefixOf`) doc.sdHandle
          )
          tokens

decodeOffset :: PagingState -> Maybe Int
decodeOffset (PagingState ps) = do
  bs <- decodeBase64Url ps
  case Aeson.eitherDecode (LBS.fromStrict bs) of
    Right n -> Just n
    Left _ -> Nothing

mkResult :: Int -> [a] -> Bool -> Int -> SearchResult a
mkResult maxResults results hasMore nextOffset =
  SearchResult
    { searchTook = 0,
      searchReturned = min maxResults (length results),
      searchResults = take maxResults results,
      searchPagingState =
        if hasMore
          then Just . PagingState . encodeBase64Url . LBS.toStrict . Aeson.encode $ nextOffset
          else Nothing,
      searchHasMore = Just hasMore,
      searchFound = length results,
      searchPolicy = FullSearch
    }

-- | Result builder for searches without paging.
mkContactResult :: Int -> [a] -> SearchResult a
mkContactResult maxResults results =
  SearchResult
    { searchTook = 0,
      searchReturned = min maxResults (length results),
      searchResults = take maxResults results,
      searchPagingState = Nothing,
      searchHasMore = Nothing,
      searchFound = length results,
      searchPolicy = FullSearch
    }
