module Wire.UserSearchSubsystem.Interpreter where

import Cassandra.Exec (paginateWithStateC)
import Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.Domain
import Data.Id
import Data.Map qualified as Map
import Data.Qualified
import Data.Range
import Data.Set qualified as Set
import Database.Bloodhound.Types qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti (TeamStatus (..))
import Wire.API.Team.Feature
import Wire.API.User.Search
import Wire.GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserMigrationStore, IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe), unsafePooledForConcurrentlyN)
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics
import Wire.UserSearch.Metrics
import Wire.UserSearch.Migration
import Wire.UserSearch.Types
import Wire.UserSearchSubsystem
import Wire.UserStore
import Wire.UserStore.IndexUser

interpretUserSearchSubsystem ::
  ( Member UserStore r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r,
    Member Metrics r
  ) =>
  InterpreterFor UserSearchSubsystem r
interpretUserSearchSubsystem = interpret \case
  SyncUser uid -> syncUserImpl uid
  UpdateTeamSearchVisibilityInbound status -> updateTeamSearchVisibilityInboundImpl status
  SearchUser luid query mDomain mMaxResults -> searchUserImpl luid query mDomain mMaxResults
  BrowseTeam uid browseTeamFilters mMaxResults mPagingState -> do
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

searchUserImpl :: Local UserId -> Text -> Maybe Domain -> Maybe (Range 1 500 Int32) -> Sem r [Contact]
searchUserImpl = undefined

browseTeamImpl :: UserId -> BrowseTeamFilters -> Maybe (Range 1 500 Int32) -> Maybe PagingState -> Sem r [TeamContact]
browseTeamImpl = undefined

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
