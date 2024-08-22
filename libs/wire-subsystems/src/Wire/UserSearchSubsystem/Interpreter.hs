{-# LANGUAGE RecordWildCards #-}

module Wire.UserSearchSubsystem.Interpreter where

import Cassandra.Exec (paginateWithStateC)
import Conduit (ConduitT, runConduit, (.|))
import Control.Error (MaybeT (..))
import Data.Conduit.Combinators qualified as Conduit
import Data.Domain
import Data.Handle qualified as Handle
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import Servant.Client.Core (RunClient)
import System.Logger.Message qualified as Log
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig qualified as FedBrig
import Wire.API.Federation.Error
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member
import Wire.API.Team.Permission qualified as Permission
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User.Search
import Wire.FederationAPIAccess
import Wire.FederationConfigStore
import Wire.GalleyAPIAccess
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserMigrationStore, IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe), unsafePooledForConcurrentlyN)
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics
import Wire.StoredUser
import Wire.UserSearch.Metrics
import Wire.UserSearch.Migration
import Wire.UserSearch.Types
import Wire.UserSearchSubsystem
import Wire.UserStore
import Wire.UserStore qualified as UserStore
import Wire.UserStore.IndexUser
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.Interpreter

interpretUserSearchSubsystem ::
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r,
    Member Metrics r,
    Member (Error UserSubsystemError) r,
    Member FederationConfigStore r,
    RunClient (fedM 'Brig),
    Member (FederationAPIAccess fedM) r,
    FederationMonad fedM,
    Typeable fedM,
    Member TinyLog r,
    Member (Error FederationError) r
  ) =>
  UserSubsystemConfig ->
  InterpreterFor UserSearchSubsystem r
interpretUserSearchSubsystem config = interpret \case
  SyncUser uid ->
    syncUserImpl uid
  UpdateTeamSearchVisibilityInbound status ->
    updateTeamSearchVisibilityInboundImpl status
  SearchUsers luid query mDomain mMaxResults ->
    searchUsersImpl config luid query mDomain mMaxResults
  BrowseTeam uid browseTeamFilters mMaxResults mPagingState ->
    browseTeamImpl uid browseTeamFilters mMaxResults mPagingState

interpretUserSearchSubsystemBulk ::
  ( Member TinyLog r,
    Member UserStore r,
    Member (Concurrency Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r,
    Member (Error MigrationException) r,
    Member IndexedUserMigrationStore r
  ) =>
  InterpreterFor UserSearchSubsystemBulk r
interpretUserSearchSubsystemBulk = interpret \case
  SyncAllUsers -> syncAllUsersImpl
  ForceSyncAllUsers -> forceSyncAllUsersImpl
  MigrateData -> migrateDataImpl

syncUserImpl ::
  forall r.
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r,
    Member Metrics r
  ) =>
  UserId ->
  Sem r ()
syncUserImpl uid =
  getIndexUser uid
    >>= maybe delete upsert
  where
    delete :: Sem r ()
    delete = do
      Metrics.incCounter indexDeleteCounter
      IndexedUserStore.upsert (docId uid) (emptyUserDoc uid) ES.NoVersionControl

    upsert :: IndexUser -> Sem r ()
    upsert indexUser = do
      vis <-
        maybe
          (pure defaultSearchVisibilityInbound)
          teamSearchVisibilityInbound
          indexUser.teamId
      let userDoc = indexUserToDoc vis indexUser
          version = ES.ExternalGT . ES.ExternalDocVersion . docVersion $ indexUserToVersion indexUser
      Metrics.incCounter indexUpdateCounter
      IndexedUserStore.upsert (docId uid) userDoc version

syncAllUsersImpl ::
  forall r.
  ( Member UserStore r,
    Member TinyLog r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  Sem r ()
syncAllUsersImpl = syncAllUsersWithVersion ES.ExternalGT

forceSyncAllUsersImpl ::
  forall r.
  ( Member UserStore r,
    Member TinyLog r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  Sem r ()
forceSyncAllUsersImpl = syncAllUsersWithVersion ES.ExternalGTE

syncAllUsersWithVersion ::
  forall r.
  ( Member UserStore r,
    Member TinyLog r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r
  ) =>
  (ES.ExternalDocVersion -> ES.VersionControl) ->
  Sem r ()
syncAllUsersWithVersion mkVersion =
  runConduit $
    paginateWithStateC getIndexUsersPaginated
      .| logPage
      .| mkUserDocs
      .| Conduit.mapM_ IndexedUserStore.bulkUpsert
  where
    logPage :: ConduitT [IndexUser] [IndexUser] (Sem r) ()
    logPage = Conduit.iterM $ \page -> do
      info $
        Log.field "size" (length page)
          . Log.msg (Log.val "Reindex: processing C* page")

    mkUserDocs :: ConduitT [IndexUser] [(ES.DocId, UserDoc, ES.VersionControl)] (Sem r) ()
    mkUserDocs = Conduit.mapM $ \page -> do
      visMap <- fmap Map.fromList . unsafePooledForConcurrentlyN 16 (Set.fromList $ mapMaybe (.teamId) page) $ \t ->
        (t,) <$> teamSearchVisibilityInbound t
      let vis indexUser = fromMaybe defaultSearchVisibilityInbound $ flip Map.lookup visMap =<< indexUser.teamId
          mkUserDoc indexUser = indexUserToDoc (vis indexUser) indexUser
          mkDocVersion = mkVersion . ES.ExternalDocVersion . docVersion . indexUserToVersion
      pure $ map (\u -> (docId u.userId, mkUserDoc u, mkDocVersion u)) page

updateTeamSearchVisibilityInboundImpl :: (Member IndexedUserStore r) => TeamStatus SearchVisibilityInboundConfig -> Sem r ()
updateTeamSearchVisibilityInboundImpl teamStatus =
  IndexedUserStore.updateTeamSearchVisibilityInbound teamStatus.team $
    searchVisibilityInboundFromFeatureStatus teamStatus.status

searchUsersImpl ::
  forall r fedM.
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r,
    Member IndexedUserStore r,
    Member FederationConfigStore r,
    RunClient (fedM 'Brig),
    Member (FederationAPIAccess fedM) r,
    FederationMonad fedM,
    Typeable fedM,
    Member TinyLog r,
    Member (Error FederationError) r
  ) =>
  UserSubsystemConfig ->
  Local UserId ->
  Text ->
  Maybe Domain ->
  Maybe (Range 1 500 Int32) ->
  Sem r (SearchResult Contact)
searchUsersImpl config searcherId searchTerm maybeDomain maybeMaxResults = do
  storedSearcher <- fromMaybe (error "TODO: searcher is not real") <$> UserStore.getUser (tUnqualified searcherId)
  for_ storedSearcher.teamId $ \tid -> ensurePermissions (tUnqualified searcherId) tid [SearchContacts]
  let localDomain = tDomain searcherId
  let queryDomain = fromMaybe localDomain maybeDomain
  if queryDomain == localDomain
    then searchLocally config (qualifyAs searcherId storedSearcher) searchTerm maybeMaxResults
    else searchRemotely queryDomain storedSearcher.teamId searchTerm

searchLocally ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member UserStore r,
    Member IndexedUserStore r
  ) =>
  UserSubsystemConfig ->
  Local StoredUser ->
  Text ->
  Maybe (Range 1 500 Int32) ->
  Sem r (SearchResult Contact)
searchLocally config searcher searchTerm maybeMaxResults = do
  let maxResults = maybe 15 (fromIntegral . fromRange) maybeMaxResults
  let searcherTeamId = (tUnqualified searcher).teamId
      searcherId = (tUnqualified searcher).id
  teamSearchInfo <- mkTeamSearchInfo searcherTeamId

  maybeExactHandleMatch <- exactHandleSearch teamSearchInfo

  let exactHandleMatchCount = length maybeExactHandleMatch
      esMaxResults = maxResults - exactHandleMatchCount

  esResult <-
    if esMaxResults > 0
      then IndexedUserStore.searchUsers searcherId searcherTeamId teamSearchInfo searchTerm esMaxResults
      else pure $ SearchResult 0 0 0 [] FullSearch Nothing Nothing

  -- Prepend results matching exact handle and results from ES.
  pure $
    esResult
      { searchResults = maybeToList maybeExactHandleMatch <> map userDocToContact (searchResults esResult),
        searchFound = exactHandleMatchCount + searchFound esResult,
        searchReturned = exactHandleMatchCount + searchReturned esResult
      }
  where
    handleTeamVisibility :: TeamId -> TeamSearchVisibility -> TeamSearchInfo
    handleTeamVisibility _ SearchVisibilityStandard = AllUsers
    handleTeamVisibility t SearchVisibilityNoNameOutsideTeam = TeamOnly t

    userDocToContact :: UserDoc -> Contact
    userDocToContact userDoc =
      Contact
        { contactQualifiedId = tUntagged $ qualifyAs searcher userDoc.udId,
          contactName = maybe "" fromName userDoc.udName,
          contactColorId = fromIntegral . fromColourId <$> userDoc.udColourId,
          contactHandle = Handle.fromHandle <$> userDoc.udHandle,
          contactTeam = userDoc.udTeam
        }

    mkTeamSearchInfo :: Maybe TeamId -> Sem r TeamSearchInfo
    mkTeamSearchInfo searcherTeamId = do
      case searcherTeamId of
        Nothing -> pure NoTeam
        Just t ->
          -- This flag in brig overrules any flag on galley - it is system wide
          if config.searchSameTeamOnly
            then pure (TeamOnly t)
            else do
              -- For team users, we need to check the visibility flag
              handleTeamVisibility t <$> GalleyAPIAccess.getTeamSearchVisibility t

    exactHandleSearch :: TeamSearchInfo -> Sem r (Maybe Contact)
    exactHandleSearch _teamSerachInfo = runMaybeT $ do
      handle <- MaybeT . pure $ Handle.parseHandle searchTerm
      owner <- MaybeT $ UserStore.lookupHandle handle
      storedUser <- MaybeT $ UserStore.getUser owner
      let contact = contactFromStoredUser (tDomain searcher) storedUser
          isContactVisible =
            (config.searchSameTeamOnly && (tUnqualified searcher).teamId == storedUser.teamId)
              || (not config.searchSameTeamOnly)
      -- case teamSerachInfo of
      -- AllUsers -> True
      -- NoTeam -> isNothing (storedUser.teamId)
      -- TeamOnly tid -> storedUser.teamId == Just tid
      if isContactVisible
        then pure contact
        else MaybeT $ pure Nothing

    contactFromStoredUser :: Domain -> StoredUser -> Contact
    contactFromStoredUser domain storedUser =
      Contact
        { contactQualifiedId = Qualified storedUser.id domain,
          contactName = fromName storedUser.name,
          contactHandle = Handle.fromHandle <$> storedUser.handle,
          contactColorId = Just . fromIntegral . fromColourId $ storedUser.accentId,
          contactTeam = storedUser.teamId
        }

searchRemotely ::
  ( Member FederationConfigStore r,
    RunClient (fedM 'Brig),
    Member (FederationAPIAccess fedM) r,
    FederationMonad fedM,
    Typeable fedM,
    Member TinyLog r,
    Member (Error FederationError) r
  ) =>
  Domain ->
  Maybe TeamId ->
  Text ->
  Sem r (SearchResult Contact)
searchRemotely domain mTid searchTerm = do
  Log.info $
    Log.msg (Log.val "searchRemotely")
      . Log.field "domain" (show domain)
      . Log.field "searchTerm" searchTerm
  mFedCnf <- getFederationConfig domain
  let onlyInTeams = case restriction <$> mFedCnf of
        Just FederationRestrictionAllowAll -> Nothing
        Just (FederationRestrictionByTeam teams) -> Just teams
        -- if we are not federating at all, we also do not allow to search any remote teams
        Nothing -> Just []

  searchResponse <-
    runFederated (toRemoteUnsafe domain ()) $
      fedClient @'Brig @"search-users" (FedBrig.SearchRequest searchTerm mTid onlyInTeams)
  let contacts = searchResponse.contacts
  let count = length contacts
  pure
    SearchResult
      { searchResults = contacts,
        searchFound = count,
        searchReturned = count,
        searchTook = 0,
        searchPolicy = searchResponse.searchPolicy,
        searchPagingState = Nothing,
        searchHasMore = Nothing
      }

browseTeamImpl ::
  ( Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r,
    Member IndexedUserStore r
  ) =>
  UserId ->
  BrowseTeamFilters ->
  Maybe (Range 1 500 Int) ->
  Maybe PagingState ->
  Sem r (SearchResult TeamContact)
browseTeamImpl uid filters mMaxResults mPagingState = do
  -- limit this to team admins to reduce risk of involuntary DOS attacks. (also,
  -- this way we don't need to worry about revealing confidential user data to
  -- other team members.)
  ensurePermissions uid filters.teamId [Permission.AddTeamMember]

  let maxResults = maybe 15 fromRange mMaxResults
  userDocToTeamContact <$$> IndexedUserStore.paginateTeamMembers filters maxResults mPagingState

migrateDataImpl ::
  ( Member IndexedUserStore r,
    Member (Error MigrationException) r,
    Member IndexedUserMigrationStore r,
    Member UserStore r,
    Member (Concurrency Unsafe) r,
    Member GalleyAPIAccess r,
    Member TinyLog r
  ) =>
  Sem r ()
migrateDataImpl = do
  unlessM IndexedUserStore.doesIndexExist $
    throw TargetIndexAbsent
  IndexedUserStore.ensureMigrationIndex
  foundVersion <- IndexedUserStore.getLatestMigrationVersion
  if expectedMigrationVersion > foundVersion
    then do
      Log.info $
        Log.msg (Log.val "Migration necessary.")
          . Log.field "expectedVersion" expectedMigrationVersion
          . Log.field "foundVersion" foundVersion
      forceSyncAllUsersImpl
      IndexedUserStore.persistMigrationVersion expectedMigrationVersion
    else do
      Log.info $
        Log.msg (Log.val "No migration necessary.")
          . Log.field "expectedVersion" expectedMigrationVersion
          . Log.field "foundVersion" foundVersion

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 6

docId :: UserId -> ES.DocId
docId uid = ES.DocId (idToText uid)

teamSearchVisibilityInbound :: (Member GalleyAPIAccess r) => TeamId -> Sem r SearchVisibilityInbound
teamSearchVisibilityInbound tid =
  searchVisibilityInboundFromFeatureStatus . (.status)
    <$> getFeatureConfigForTeam @_ @SearchVisibilityInboundConfig tid

ensurePermissions ::
  ( IsPerm perm,
    Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  [perm] ->
  Sem r ()
ensurePermissions u t perms = do
  m <- GalleyAPIAccess.getTeamMember u t
  unless (check m) $
    throw UserSubsystemInsufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just m) = all (hasPermission m) perms
    check Nothing = False
