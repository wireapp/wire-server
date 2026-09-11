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

-- | Postgres interpreter for 'UserSearchStore'.
--
-- Queries brig's @wire_user@ table directly, replacing the former
-- ElasticSearch projection ('Wire.IndexedUserStore.ElasticSearch' is the
-- semantic spec for the predicates below):
--
--   * candidate filter: activated, status active/suspended, no service
--     accounts (mirrors the former @shouldIndex@ write filter);
--
--   * 'searchUsers': prefix matching on @name_normalized@ (ICU-folded) and
--     handle, rank-ordered like the documented swagger ordering (exact
--     handle, exact name, handle prefix, name prefix), visibility via the
--     @team_search_visibility@ table (left join; a missing row defaults to
--     own-team-only);
--
--   * 'paginateTeamMembers': keyset pagination on @(sort_value, id)@ for
--     SQL-side sorts; role filter and role/SAML-idp sorts are resolved via
--     the galley role map and paginated by offset in Haskell (team
--     membership is bounded by @hardTruncationLimit@);
--
--   * 'getTeamSize': count of activated, active/suspended team members.
module Wire.UserSearchStore.Postgres
  ( interpretUserSearchStorePostgres,
  )
where

import Control.Lens ((^.))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Conversion qualified as BSC
import Data.ByteString.Lazy qualified as LBS
import Data.Domain (Domain)
import Data.Functor.Contravariant ((>$<))
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Map.Strict qualified as Map
import Data.Qualified (Local, Qualified (..), tDomain, tUnqualified)
import Data.Text qualified as Text
import Data.Text.Ascii (decodeBase64Url, encodeBase64Url)
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime)
import Hasql.Decoders qualified as Dec
import Hasql.Encoders qualified as Enc
import Hasql.Statement (Statement, refineResult)
import Hasql.TH
import Imports
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as TinyLog
import System.Logger.Class qualified as Log
import Wire.API.PostgresMarshall
import Wire.API.Team.HardTruncationLimit (hardTruncationLimit, hardTruncationLimitRange)
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Member.Info (TeamMemberInfo (..), TeamMemberInfoList (members))
import Wire.API.Team.Role (Role, permissionsToRole, roleName)
import Wire.API.Team.Size (TeamSize (..))
import Wire.API.User
import Wire.API.User.Search
import Wire.GalleyAPIAccess (GalleyAPIAccess)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.Postgres
import Wire.StoredUser (inferUserType)
import Wire.UserSearch.Normalize (normalized)
import Wire.UserSearchStore

-- | Only users with these account statuses were indexed into ElasticSearch
-- (see the former @shouldIndex@); encoded as in
-- @instance PostgresMarshall Int32 AccountStatus@.
accountStatusIndexed :: [AccountStatus]
accountStatusIndexed = [Active, Suspended]

interpretUserSearchStorePostgres ::
  ( PGConstraints r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r
  ) =>
  InterpreterFor UserSearchStore r
interpretUserSearchStorePostgres =
  interpret $ \case
    SearchUsers lSearcher mSearcherTeam teamSearchInfo term maxResults mTypes ->
      searchUsersImpl lSearcher mSearcherTeam teamSearchInfo term maxResults mTypes
    PaginateTeamMembers filters maxResults mPagingState ->
      paginateTeamMembersImpl filters maxResults mPagingState
    SearchUsersFederated mOnlyInTeams term maxResults mTypes ->
      searchUsersFederatedImpl mOnlyInTeams term maxResults mTypes
    GetTeamSize tid -> getTeamSizeImpl tid
    SetTeamSearchVisibilityInbound tid vis ->
      runStatement (tid, searchVisibilityInboundToInt vis) upsertTeamSearchVisibilityStatement

--------------------------------------------------------------------------------
-- SearchUsers

searchUsersImpl ::
  (PGConstraints r) =>
  Local UserId ->
  Maybe TeamId ->
  TeamSearchInfo ->
  Text ->
  Int ->
  Maybe [UserTypeFilter] ->
  Sem r (SearchResult Contact)
searchUsersImpl lSearcher mSearcherTeam teamSearchInfo searchTerm maxResults mTypes =
  case (Text.words term, visibilityCondition mSearcherTeam teamSearchInfo) of
    -- The former ES query could not match the empty term either.
    ([], _) -> pure emptyResult
    (_, VisImpossible) -> pure emptyResult
    (tokens, VisCondition vis) ->
      runContactSearch
        localDomain
        (contactSearchQuery (Just searcher) mSearcherTeam tokens term vis maxResults mTypes)
        rankedOrder
  where
    searcher = tUnqualified lSearcher
    localDomain = tDomain lSearcher
    term = Text.unwords (analyzeTerm searchTerm)

    rankedOrder = literal "order by" <> rankFragment term <> literal "asc, wu.id asc"

    emptyResult = SearchResult 0 0 0 [] FullSearch Nothing Nothing

-- | Inbound federated search: the searcher is unknown, so no self
-- exclusion applies and results are not relevance-ordered (the former ES
-- implementation scored all matches equally here).
searchUsersFederatedImpl ::
  (PGConstraints r, Member (Input (Local ())) r) =>
  Maybe [TeamId] ->
  Text ->
  Int ->
  Maybe [UserTypeFilter] ->
  Sem r (SearchResult Contact)
searchUsersFederatedImpl mOnlyInTeams searchTerm maxResults mTypes =
  case Text.words term of
    [] -> pure emptyResult
    tokens -> case federatedVisibility mOnlyInTeams of
      VisImpossible -> pure emptyResult
      VisCondition vis -> do
        loc <- input
        runContactSearch (tDomain loc) (contactSearchQuery Nothing Nothing tokens term vis maxResults mTypes) (literal "order by wu.id asc")
  where
    term = normalized searchTerm

    emptyResult = SearchResult 0 0 0 [] FullSearch Nothing Nothing

    federatedVisibility = \case
      Nothing ->
        VisCondition $
          literal "(wu.team is null or tsv.search_visibility_inbound ="
            <> intParam searchVisibilityInboundAllTeams
            <> literal ")"
      Just [] ->
        -- Impossible to fulfill (safety net, handled earlier by the caller).
        VisImpossible
      Just teams ->
        VisCondition $
          andList
            [ paramLiteral
                (const (map toUUID teams) >$< Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid))))
                (\i -> "wu.team = any(" <> argPattern "uuid[]" i <> ")"),
              literal "tsv.search_visibility_inbound =" <> intParam searchVisibilityInboundAllTeams
            ]

runContactSearch ::
  (PGConstraints r) =>
  Domain ->
  QueryFragment ->
  QueryFragment ->
  Sem r (SearchResult Contact)
runContactSearch localDomain query orderFragment = do
  rows <-
    runStatement
      ()
      ( refineResult
          (traverse (\(total, raw) -> (,) (fromIntegral (total :: Int64)) <$> rawToContact localDomain raw))
          (buildStatement (query <> orderFragment) (Dec.rowList contactRow))
      )
  pure
    SearchResult
      { searchFound = maybe 0 fst (listToMaybe rows),
        searchReturned = length rows,
        searchTook = 0,
        searchResults = map snd rows,
        searchPolicy = FullSearch,
        searchPagingState = Nothing,
        searchHasMore = Nothing
      }

-- | Shared filter and matching logic of 'SearchUsers' and
-- 'SearchUsersFederated' (the former @defaultUserQuery@ / @mkUserQuery@).
-- The ordering clause is supplied by the caller.
contactSearchQuery ::
  Maybe UserId ->
  Maybe TeamId ->
  [Text] ->
  Text ->
  QueryFragment ->
  Int ->
  Maybe [UserTypeFilter] ->
  QueryFragment
contactSearchQuery mSearcher mSearcherTeam tokens term vis maxResults mTypes =
  literal "select (count(*) over ()) :: bigint, wu.id :: uuid, wu.name :: text, wu.accent_id :: int, wu.handle :: text, wu.team :: uuid, wu.user_type :: int"
    <> literal "from wire_user wu left join team_search_visibility tsv on tsv.team = wu.team"
    <> literal "where"
    <> andList conditions
    <> limitParam maxResults
  where
    conditions =
      candidateCondition
        ++ [ -- Exact handle matches are fetched by the caller (user store
             -- lookup for local search, exact-handle search for federation).
             -- Handle-less users pass (they are still findable by name).
             literal "(wu.handle is null or lower(wu.handle) <>"
               <> textParam term
               <> literal ")",
             literal "(wu.searchable is null or wu.searchable)",
             appExclusionCondition mSearcherTeam,
             vis
           ]
        ++ catMaybes [selfExclusion mSearcher, userTypeCondition mTypes]
        ++ map (tokenMatchCondition False) tokens

    selfExclusion = \case
      Nothing -> Nothing
      Just searcher -> Just (clause1 "wu.id" "<>" searcher)

-- | Rank ordering documented in the swagger docs of @/users/search@:
-- exact name, handle prefix, name prefix.  (Exact handle matches are
-- excluded from this query entirely - the swagger's first tier is
-- resolved by the caller's exact-handle lookup.)
rankFragment :: Text -> QueryFragment
rankFragment term =
  literal "case when wu.name_normalized ="
    <> textParam term
    <> literal "then 0 when lower(wu.handle) like"
    <> textParam (escapeLike term <> "%")
    <> literal "then 1 when wu.name_normalized like"
    <> textParam (escapeLike term <> "%")
    <> literal "then 2 else 3 end"

contactRow :: Dec.Row (Int64, (UserId, Text, Int32, Maybe Text, Maybe TeamId, Int32))
contactRow =
  (,)
    <$> Dec.column (Dec.nonNullable Dec.int8)
    <*> ( (,,,,,)
            <$> (Id <$> Dec.column (Dec.nonNullable Dec.uuid))
            <*> Dec.column (Dec.nonNullable Dec.text)
            <*> Dec.column (Dec.nonNullable Dec.int4)
            <*> Dec.column (Dec.nullable Dec.text)
            <*> ((Id <$>) <$> Dec.column (Dec.nullable Dec.uuid))
            <*> Dec.column (Dec.nonNullable Dec.int4)
        )

rawToContact :: Domain -> (UserId, Text, Int32, Maybe Text, Maybe TeamId, Int32) -> Either Text Contact
rawToContact dom (uid, name, accent, mHandle, mTeam, utype) = do
  ty <- postgresUnmarshall utype
  pure
    Contact
      { contactQualifiedId = Qualified uid dom,
        contactName = name,
        contactColorId = Just (fromIntegral accent),
        contactHandle = mHandle,
        contactTeam = mTeam,
        contactType = inferUserType Nothing (Just ty)
      }

--------------------------------------------------------------------------------
-- PaginateTeamMembers

data BrowseCursor
  = -- | Offset into the Haskell-sorted result set (role / SAML idp sorts).
    OffsetCursor Int
  | -- | Keyset: null-flag of the sort value, the value itself, and the id
    -- of the last returned row.
    KeysetCursor Bool Aeson.Value UserId

-- | Raw @wire_user@ row served to the browse endpoint.
data TeamRow = TeamRow
  { trFound :: Int64,
    trId :: UserId,
    trType :: Int32,
    trName :: Text,
    trAccent :: Int32,
    trHandle :: Maybe Text,
    trTeam :: Maybe TeamId,
    trEmail :: Maybe Text,
    trEmailUnvalidated :: Maybe Text,
    trCreatedAt :: UTCTime,
    trManagedBy :: Maybe Int32,
    trSsoId :: Maybe UserSSOId,
    trSearchable :: Maybe Bool
  }

teamRow :: Dec.Row TeamRow
teamRow =
  TeamRow
    <$> Dec.column (Dec.nonNullable Dec.int8)
    <*> (Id <$> Dec.column (Dec.nonNullable Dec.uuid))
    <*> Dec.column (Dec.nonNullable Dec.int4)
    <*> Dec.column (Dec.nonNullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.int4)
    <*> Dec.column (Dec.nullable Dec.text)
    <*> ((Id <$>) <$> Dec.column (Dec.nullable Dec.uuid))
    <*> Dec.column (Dec.nullable Dec.text)
    <*> Dec.column (Dec.nullable Dec.text)
    <*> Dec.column (Dec.nonNullable Dec.timestamptz)
    <*> Dec.column (Dec.nullable Dec.int4)
    <*> Dec.column (Dec.nullable (Dec.jsonbBytes decodeJsonStrict))
    <*> Dec.column (Dec.nullable Dec.bool)

decodeJsonStrict :: (Aeson.FromJSON a) => ByteString -> Either Text a
decodeJsonStrict = first Text.pack . Aeson.eitherDecodeStrict'

teamRowToTeamContact :: TeamRow -> Either Text TeamContact
teamRowToTeamContact row = do
  ty <- postgresUnmarshall row.trType
  managedBy <- traverse postgresUnmarshall row.trManagedBy
  email <- traverse decodeEmail row.trEmail
  emailUnvalidated <- traverse decodeEmail row.trEmailUnvalidated
  pure
    TeamContact
      { teamContactUserId = row.trId,
        teamContactUserType = ty,
        teamContactName = row.trName,
        teamContactColorId = Just (fromIntegral row.trAccent),
        teamContactHandle = row.trHandle,
        teamContactTeam = row.trTeam,
        teamContactEmail = email,
        teamContactCreatedAt = Just (toUTCTimeMillis row.trCreatedAt),
        teamContactManagedBy = managedBy,
        teamContactSAMLIdp = fst <$> (ssoIssuerAndNameId =<< row.trSsoId),
        teamContactRole = Nothing,
        teamContactScimExternalId = join (scimExternalId <$> managedBy <*> row.trSsoId),
        teamContactSso = fmap (uncurry Sso) (row.trSsoId >>= ssoIssuerAndNameId),
        teamContactEmailUnvalidated = emailUnvalidated,
        teamContactUserGroups = [],
        teamContactSearchable = fromMaybe True row.trSearchable
      }
  where
    decodeEmail = first Text.pack . BSC.runParser BSC.parser . TE.encodeUtf8

-- | @refineResult@ step keeping the raw row alongside the contact, so that
-- keyset cursors are built from the exact DB values.
withRaw :: TeamRow -> Either Text (TeamContact, TeamRow)
withRaw row = (,row) <$> teamRowToTeamContact row

paginateTeamMembersImpl ::
  forall r.
  (PGConstraints r, Member GalleyAPIAccess r, Member TinyLog r) =>
  BrowseTeamFilters ->
  Int ->
  Maybe PagingState ->
  Sem r (SearchResult TeamContact)
paginateTeamMembersImpl filters maxResults mPagingState = do
  -- The role filter is resolved via the galley role map; sorts on role and
  -- SAML idp are not SQL-expressible on wire_user.
  let needRoleMap = isJust filters.mRoleFilter || filters.mSortBy `elem` [Just SortByRole, Just SortBySAMLIdp]
  mRoleMap <-
    if needRoleMap
      then Just <$> fetchRoleMap filters.teamId
      else pure Nothing
  let mCursor = mPagingState >>= decodeCursor
  case filters.mSortBy of
    Just SortByRole -> haskellSortPage (Just SortByRole) mRoleMap mCursor
    Just SortBySAMLIdp -> haskellSortPage (Just SortBySAMLIdp) mRoleMap mCursor
    _ -> sqlKeysetPage mRoleMap mCursor
  where
    -- Full result set, filtered/sorted/paged in Haskell.  Only used for
    -- sorts that depend on the galley role map; team membership is bounded
    -- by `hardTruncationLimit` (see `fetchRoleMap`).
    haskellSortPage ::
      Maybe TeamUserSearchSortBy ->
      Maybe (Map UserId Role) ->
      Maybe BrowseCursor ->
      Sem r (SearchResult TeamContact)
    haskellSortPage mSortBy' mRoleMap mCursor = do
      let startOffset = case mCursor of
            Just (OffsetCursor n) -> n
            _ -> 0
      rows <-
        runStatement
          ()
          (refineResult (traverse withRaw) (buildStatement (browseQuery filters mRoleMap Nothing) (Dec.rowList teamRow)))
      let total = maybe 0 (trFound . snd) (listToMaybe rows)
          dir = fromMaybe SortOrderAsc filters.mSortOrder
          sorted = sortTeamContacts mSortBy' dir (map fst rows)
          page = take maxResults (drop startOffset sorted)
          hasMore = length sorted > startOffset + length page
      applyRoleFill mRoleMap (mkSearchResult (fromIntegral total) page (Just (OffsetCursor (startOffset + length page))) hasMore)

    -- Keyset-paginated SQL path for all sorts directly expressible on
    -- wire_user.
    sqlKeysetPage ::
      Maybe (Map UserId Role) ->
      Maybe BrowseCursor ->
      Sem r (SearchResult TeamContact)
    sqlKeysetPage mRoleMap mCursor = do
      let sortSpec@(_, dir) = effectiveSort filters
          keyset = mCursor >>= keysetPredicate (Just (fst sortSpec)) dir
      rows <-
        runStatement
          ()
          ( refineResult
              (traverse withRaw)
              ( buildStatement
                  ( browseQuery filters mRoleMap keyset
                      <> browseOrderBy sortSpec
                      <> limitParam (maxResults + 1)
                  )
                  (Dec.rowList teamRow)
              )
          )
      let total = maybe 0 (trFound . snd) (listToMaybe rows)
          pageRows = take maxResults rows
          hasMore = length rows > maxResults
          nextCursor = keysetCursor sortSpec . snd <$> listToMaybe (reverse pageRows)
      applyRoleFill mRoleMap (mkSearchResult (fromIntegral total) (map fst pageRows) nextCursor hasMore)

    -- Fill roles from the role map when one was fetched (role filter/sort),
    -- otherwise resolve the page roles via the galley batch RPC.
    applyRoleFill ::
      Maybe (Map UserId Role) ->
      SearchResult TeamContact ->
      Sem r (SearchResult TeamContact)
    applyRoleFill (Just roleMap) result =
      pure result {searchResults = map setRole (searchResults result)}
      where
        setRole tc = tc {teamContactRole = Map.lookup tc.teamContactUserId roleMap}
    applyRoleFill Nothing result = do
      results <- fillRolesFromGalley filters.teamId (searchResults result)
      pure result {searchResults = results}

-- | Browse query on @wire_user wu@; @keyset@ (if given) is the pagination
-- continuation predicate, @mRoleMap@ provides the optional role filter.
browseQuery ::
  BrowseTeamFilters ->
  Maybe (Map UserId Role) ->
  Maybe QueryFragment ->
  QueryFragment
browseQuery filters mRoleMap keyset =
  literal "select (count(*) over ()) :: bigint, wu.id :: uuid, wu.user_type :: int, wu.name :: text, wu.accent_id :: int, wu.handle :: text, wu.team :: uuid, wu.email :: text, wu.email_unvalidated :: text, wu.created_at :: timestamptz, wu.managed_by :: int, wu.sso_id :: jsonb, wu.searchable :: bool"
    <> literal "from wire_user wu"
    <> literal "where"
    <> andList
      ( candidateCondition
          ++ [ clause1 "wu.team" "=" filters.teamId,
               searchableCondition filters.mSearchable,
               emailVerificationCondition filters.mEmailVerificationFilter
             ]
          ++ catMaybes [roleFilterCondition mRoleMap filters.mRoleFilter, keyset]
          ++ map (tokenMatchCondition True) (maybe [] analyzeTerm filters.mQuery)
      )

-- | Keyset continuation cursor for the last returned row.
keysetCursor :: (TeamUserSearchSortBy, TeamUserSearchSortOrder) -> TeamRow -> BrowseCursor
keysetCursor (mSortBy', _) row =
  KeysetCursor (isNothing val) (maybe (Aeson.toJSON ()) sortValToAeson val) row.trId
  where
    val = rowSortVal (Just mSortBy') row

rowSortVal :: Maybe TeamUserSearchSortBy -> TeamRow -> Maybe SortVal
rowSortVal mSortBy' row = case mSortBy' of
  Just SortByName -> Just (SortText row.trName)
  Just SortByHandle -> SortText <$> row.trHandle
  Just SortByEmail -> SortText <$> row.trEmail
  Just SortByManagedBy -> SortInt32 <$> row.trManagedBy
  Just SortByCreatedAt -> Just (SortTime row.trCreatedAt)
  _ -> Nothing

-- | The effective SQL sort: explicit @sort-by@ wins (default direction
-- ascending); without it, browse is ordered by creation date, newest first
-- (same as the former ES query).
effectiveSort :: BrowseTeamFilters -> (TeamUserSearchSortBy, TeamUserSearchSortOrder)
effectiveSort filters = case filters.mSortBy of
  Just sb -> (sb, fromMaybe SortOrderAsc filters.mSortOrder)
  Nothing -> (SortByCreatedAt, SortOrderDesc)

-- | Ordering clause for the SQL keyset path.  Nulls sort last when
-- ascending and first when descending (the former ES implementation sorted
-- missing values last in BOTH directions - a small, documented divergence);
-- the user id is the deterministic tie breaker.
browseOrderBy :: (TeamUserSearchSortBy, TeamUserSearchSortOrder) -> QueryFragment
browseOrderBy (mSortBy', dir) =
  literal "order by"
    <> literal ("(" <> col <> " is null)")
    <> literal dirSql
    <> literal ","
    <> literal col
    <> literal dirSql
    <> literal ","
    <> literal ("wu.id " <> dirSql)
  where
    col = fromMaybe "wu.created_at" (sortColumnExpr (Just mSortBy'))
    dirSql = case dir of SortOrderAsc -> "asc"; SortOrderDesc -> "desc"

-- | SQL expressions of the SQL-side sort columns.
sortColumnExpr :: Maybe TeamUserSearchSortBy -> Maybe Text
sortColumnExpr = \case
  Just SortByName -> Just "wu.name"
  Just SortByHandle -> Just "wu.handle"
  Just SortByEmail -> Just "wu.email"
  Just SortByManagedBy -> Just "wu.managed_by"
  Just SortByCreatedAt -> Just "wu.created_at"
  _ -> Nothing

-- | Keyset continuation predicate for @(null-flag, sort value, id)@ tuples.
-- @Nothing@ when the cursor does not match the requested sort (the page
-- restarts).
keysetPredicate ::
  Maybe TeamUserSearchSortBy ->
  TeamUserSearchSortOrder ->
  BrowseCursor ->
  Maybe QueryFragment
keysetPredicate mSortBy dir = \case
  OffsetCursor _ -> Nothing
  KeysetCursor flag val uid -> do
    col <- sortColumnExpr mSortBy
    typed <- sortValFromAeson col val
    let (op, idOp, dirSql) = case dir of
          SortOrderAsc -> (">", ">", "asc")
          SortOrderDesc -> ("<", "<", "desc")
    pure $
      literal "((("
        <> literal ("(" <> col <> " is null)")
        <> literal dirSql
        <> literal ","
        <> literal col
        <> literal dirSql
        <> literal ","
        <> literal ("wu.id " <> dirSql)
        <> literal (") " <> op <> " (")
        <> boolParam flag
        <> literal ","
        <> sortValParam typed
        <> literal ","
        <> uidParam uid
        <> literal "))"
        <> literal "or ("
        <> literal col
        <> literal "is null and"
        <> boolParam flag
        <> literal ("and wu.id " <> idOp)
        <> uidParam uid
        <> literal ")"

-- | Sort value of a row, used for keyset pagination cursors.
data SortVal = SortText Text | SortInt32 Int32 | SortTime UTCTime

sortValToAeson :: SortVal -> Aeson.Value
sortValToAeson = \case
  SortText t -> Aeson.toJSON t
  SortInt32 n -> Aeson.toJSON n
  SortTime t -> Aeson.toJSON t

-- | Decode a cursor sort value according to the SQL type of the column.
sortValFromAeson :: Text -> Aeson.Value -> Maybe SortVal
sortValFromAeson col v = case col of
  "wu.managed_by" -> tryVal SortInt32
  "wu.created_at" -> tryVal SortTime
  _ -> tryVal SortText
  where
    tryVal :: forall a. (Aeson.FromJSON a) => (a -> SortVal) -> Maybe SortVal
    tryVal mk = case Aeson.fromJSON v of
      Aeson.Success x -> Just (mk x)
      Aeson.Error _ -> Nothing

sortValParam :: SortVal -> QueryFragment
sortValParam = \case
  SortText t -> textParam t
  SortInt32 n -> intParam n
  SortTime t -> timeParam t

sortTeamContacts :: Maybe TeamUserSearchSortBy -> TeamUserSearchSortOrder -> [TeamContact] -> [TeamContact]
sortTeamContacts mSortBy' dir = arrange dir . sortOn key
  where
    key tc = case mSortBy' of
      Just SortByRole -> roleName @Text <$> tc.teamContactRole
      Just SortBySAMLIdp -> tc.teamContactSAMLIdp
      _ -> Nothing
    arrange = \case
      SortOrderAsc -> id
      SortOrderDesc -> reverse

-- | Role filter as an @id = any(...)@ condition over the members with a
-- matching role in the galley role map.
roleFilterCondition :: Maybe (Map UserId Role) -> Maybe RoleFilter -> Maybe QueryFragment
roleFilterCondition mRoleMap mRoleFilter = do
  roleMap <- mRoleMap
  RoleFilter roles <- mRoleFilter
  -- The former ES query treated an empty role list as "no filter".
  if null roles
    then pure (literal "true")
    else
      let roleNames = map (roleName @Text) roles
          matching =
            [toUUID uid | (uid, role) <- Map.toList roleMap, roleName @Text role `elem` roleNames]
       in pure $
            paramLiteral
              (const matching >$< Enc.param (Enc.nonNullable (Enc.foldableArray (Enc.nonNullable Enc.uuid))))
              (\i -> "wu.id = any(" <> argPattern "uuid[]" i <> ")")

searchableCondition :: Maybe Bool -> QueryFragment
searchableCondition = \case
  Nothing -> literal "true"
  -- Former `searchableFilter`: false = exactly false; true = not false
  -- (i.e. true or unset).
  Just False -> literal "wu.searchable is false"
  Just True -> literal "not (wu.searchable is false)"

-- | Former @emailFilter@: verified = has a verified email and no unvalidated
-- one; unverified = has an unvalidated email.
emailVerificationCondition :: Maybe EmailVerificationFilter -> QueryFragment
emailVerificationCondition = \case
  Nothing -> literal "true"
  Just EmailVerified -> literal "(wu.email is not null and wu.email_unvalidated is null)"
  Just EmailUnverified -> literal "wu.email_unvalidated is not null"

-- | Fetches the team member role map from galley.  Like the other
-- team-member endpoints, the list is truncated at @hardTruncationLimit@;
-- the truncation is logged and the (possibly incomplete) map is used.
fetchRoleMap :: (Member GalleyAPIAccess r, Member TinyLog r) => TeamId -> Sem r (Map UserId Role)
fetchRoleMap tid = do
  teamMemberList <- GalleyAPIAccess.getTeamMembersWithLimit tid (Just (hardTruncationLimitRange @Int32))
  let teamMembers = teamMemberList ^. Team.teamMembers
  when (length teamMembers >= hardTruncationLimit) $
    TinyLog.warn $
      Log.msg (Log.val "UserSearchStore: team member list truncated, browse role filter/sort may be incomplete")
        . Log.field "team" (idToText tid)
  pure $
    Map.fromList
      [ (m ^. Team.userId, role)
      | m <- teamMembers,
        Just role <- [permissionsToRole (m ^. Team.permissions)]
      ]

-- | Fills 'TeamContact.teamContactRole' for the returned page via the
-- galley batch RPC (only needed for pages produced without a role map).
fillRolesFromGalley :: (Member GalleyAPIAccess r) => TeamId -> [TeamContact] -> Sem r [TeamContact]
fillRolesFromGalley tid contacts = do
  infos <- members <$> GalleyAPIAccess.selectTeamMemberInfos tid (map (.teamContactUserId) contacts)
  let roleOf = Map.fromList [(i.userId, permissionsToRole i.permissions) | i <- infos]
  pure [tc {teamContactRole = join (Map.lookup tc.teamContactUserId roleOf)} | tc <- contacts]

--------------------------------------------------------------------------------
-- GetTeamSize

-- | Counts activated team members with an active/suspended status, split by
-- regulars and apps (the former ES implementation aggregated over index
-- documents, which never contained deactivated/deleted/service users).
--
-- The user type literals mirror @instance PostgresMarshall Int32 UserType@
-- (regular = 0, app = 2); the status literals mirror
-- @instance PostgresMarshall Int32 AccountStatus@ (active = 0, suspended = 1).
getTeamSizeImpl :: (PGConstraints r) => TeamId -> Sem r TeamSize
getTeamSizeImpl tid = do
  (regulars, apps) <-
    runStatement tid select
  pure TeamSize {regulars = fromIntegral regulars, apps = fromIntegral apps}
  where
    select :: Statement TeamId (Int64, Int64)
    select =
      dimapPG
        [singletonStatement|
          select
            count(*) filter (where wu.user_type = 0) :: bigint,
            count(*) filter (where wu.user_type = 2) :: bigint
          from wire_user wu
          where wu.team = ($1 :: uuid)
            and wu.activated
            and (wu.account_status is null or wu.account_status in (0, 1))
            and wu.service is null
        |]

--------------------------------------------------------------------------------
-- SetTeamSearchVisibilityInbound

-- | Integral encoding shared with @instance C.Cql SearchVisibilityInbound@.
searchVisibilityInboundToInt :: SearchVisibilityInbound -> Int32
searchVisibilityInboundToInt = \case
  SearchableByOwnTeam -> 0
  SearchableByAllTeams -> 1

upsertTeamSearchVisibilityStatement :: Statement (TeamId, Int32) ()
upsertTeamSearchVisibilityStatement =
  dimapPG
    [resultlessStatement|
    insert into team_search_visibility (team, search_visibility_inbound)
    values ($1 :: uuid, $2 :: int)
      on conflict (team) do update set search_visibility_inbound = excluded.search_visibility_inbound
    |]

--------------------------------------------------------------------------------
-- Shared query fragments

-- | Normalizes a search term and splits it into tokens, dropping the
-- leading '@' of each token (the former ES analyzer did this implicitly;
-- the swagger documents that '@' does nothing special).
analyzeTerm :: Text -> [Text]
analyzeTerm = map (Text.dropWhile (== '@')) . Text.words . normalized

-- | Prefix match on a single (whitespace-split) token of the normalized
-- search term.  A name token must start at the beginning of the name or be
-- preceded by whitespace; handle starts with the token.  Email is matched
-- only for team browse: matching email in contact search would enable
-- email-based user enumeration on those broader surfaces, which the
-- former ES contact-search queries did not do either.
tokenMatchCondition :: Bool -> Text -> QueryFragment
tokenMatchCondition matchEmail tok =
  literal "("
    <> likeParam "wu.name_normalized" prefix
    <> literal "or"
    <> likeParam "wu.name_normalized" midWord
    <> literal "or"
    <> likeParam "lower(wu.handle)" prefix
    <> (if matchEmail then literal "or" <> likeParam "lower(wu.email)" prefix else literal "true")
    <> literal ")"
  where
    prefix = escapeLike tok <> "%"
    midWord = "% " <> escapeLike tok <> "%"

-- | Mirrors the former @shouldIndex@ write filter: only activated users with
-- an active/suspended status and no service account are searchable.
candidateCondition :: [QueryFragment]
candidateCondition =
  [ literal "wu.activated",
    literal "(wu.account_status is null or wu.account_status in"
      <> commaList (intParam . postgresMarshall @Int32 <$> accountStatusIndexed)
      <> literal "))",
    literal "wu.service is null"
  ]

-- | The user_type filter semantics of the former ES implementation: no
-- filter for @Nothing@ and @Just []@.
userTypeCondition :: Maybe [UserTypeFilter] -> Maybe QueryFragment
userTypeCondition = \case
  Nothing -> Nothing
  Just [] -> Nothing
  Just uts ->
    Just $
      literal "("
        <> foldr1
          (\a b -> a <> literal "or" <> b)
          [clause1 "wu.user_type" "=" (postgresMarshall @Int32 (userTypeFilterToUserType ut)) | ut <- uts]
        <> literal ")"

-- | Apps are only searchable within their own team (former
-- @matchAppsFromOtherTeams@, expressed as a negated condition).
appExclusionCondition :: Maybe TeamId -> QueryFragment
appExclusionCondition = \case
  Nothing ->
    literal "not (wu.user_type ="
      <> intParam (postgresMarshall @Int32 UserTypeApp)
      <> literal "and wu.team is not null)"
  Just st ->
    literal "not (wu.user_type ="
      <> intParam (postgresMarshall @Int32 UserTypeApp)
      <> literal "and (wu.team is null or wu.team <>"
      <> uidParam st
      <> literal "))"

-- | Inbound search visibility (former @restrictSearchSpaceByTeam@).
data Visibility = VisImpossible | VisCondition QueryFragment

visibilityCondition :: Maybe TeamId -> TeamSearchInfo -> Visibility
visibilityCondition mSearcherTeam teamSearchInfo = case (mSearcherTeam, teamSearchInfo) of
  (Nothing, _) -> VisCondition (literal "wu.team is null")
  (Just _, NoTeam) -> VisCondition (literal "wu.team is null")
  (Just searcherTeam, TeamOnly t)
    | searcherTeam == t -> VisCondition (clause1 "wu.team" "=" t)
    | otherwise -> VisImpossible
  (Just searcherTeam, AllUsers) ->
    -- Team members of other teams are only visible if their team set
    -- search_visibility_inbound = searchable-by-all-teams.  The left join
    -- yields null (own-team-only default) when the team has no row.
    VisCondition $
      literal "(wu.team is null or"
        <> clause1 "wu.team" "=" searcherTeam
        <> literal "or tsv.search_visibility_inbound ="
        <> intParam searchVisibilityInboundAllTeams
        <> literal ")"

searchVisibilityInboundAllTeams :: Int32
searchVisibilityInboundAllTeams = 1

andList :: [QueryFragment] -> QueryFragment
andList = foldr1 (\a b -> a <> literal "and" <> b)

commaList :: [QueryFragment] -> QueryFragment
commaList = foldr1 (\a b -> a <> literal "," <> b)

-- | Single (non-null) parameter fragments.
intParam :: Int32 -> QueryFragment
intParam n = paramLiteral (const n >$< Enc.param (Enc.nonNullable Enc.int4)) (argPattern "int")

textParam :: Text -> QueryFragment
textParam t = paramLiteral (const t >$< Enc.param (Enc.nonNullable Enc.text)) (argPattern "text")

timeParam :: UTCTime -> QueryFragment
timeParam t = paramLiteral (const t >$< Enc.param (Enc.nonNullable Enc.timestamptz)) (argPattern "timestamptz")

boolParam :: Bool -> QueryFragment
boolParam b = paramLiteral (const b >$< Enc.param (Enc.nonNullable Enc.bool)) (argPattern "bool")

uidParam :: Id a -> QueryFragment
uidParam u = paramLiteral (const (toUUID u) >$< Enc.param (Enc.nonNullable Enc.uuid)) (argPattern "uuid")

limitParam :: Int -> QueryFragment
limitParam n = paramLiteral (const (fromIntegral n) >$< Enc.param (Enc.nonNullable Enc.int4)) (\i -> "limit " <> argPattern "int" i)

-- | @<field> like $n@ with the given (already escaped) pattern.
likeParam :: Text -> Text -> QueryFragment
likeParam field pat = paramLiteral (const pat >$< Enc.param (Enc.nonNullable Enc.text)) (\i -> field <> " like " <> argPattern "text" i)

escapeLike :: Text -> Text
escapeLike = Text.replace "_" "\\_" . Text.replace "%" "\\%" . Text.replace "\\" "\\\\"

mkSearchResult :: Int -> [a] -> Maybe BrowseCursor -> Bool -> SearchResult a
mkSearchResult found results cursor hasMore =
  SearchResult
    { searchFound = found,
      searchReturned = length results,
      searchTook = 0,
      searchResults = results,
      searchPolicy = FullSearch,
      searchPagingState = encodeCursor <$> cursor,
      searchHasMore = Just hasMore
    }

encodeCursor :: BrowseCursor -> PagingState
encodeCursor c =
  PagingState
    . encodeBase64Url
    . LBS.toStrict
    . Aeson.encode
    $ case c of
      OffsetCursor n -> Aeson.toJSON n
      KeysetCursor flag val uid -> Aeson.toJSON (flag, val, uid)

decodeCursor :: PagingState -> Maybe BrowseCursor
decodeCursor (PagingState ps) = do
  bs <- decodeBase64Url ps
  v <- either (const Nothing) Just (Aeson.eitherDecode (LBS.fromStrict bs))
  case Aeson.fromJSON @Int v of
    Aeson.Success n -> Just (OffsetCursor n)
    Aeson.Error _ -> case Aeson.fromJSON @(Bool, Aeson.Value, UserId) v of
      Aeson.Success (flag, val, uid) -> Just (KeysetCursor flag val uid)
      Aeson.Error _ -> Nothing
