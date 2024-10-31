module Wire.IndexedUserStore.Bulk.ElasticSearch where

import Cassandra.Exec (paginateWithStateC)
import Conduit (ConduitT, runConduit, (.|))
import Data.Conduit.Combinators qualified as Conduit
import Data.Id
import Data.Map qualified as Map
import Data.Set qualified as Set
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Team.Feature
import Wire.GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.IndexedUserStore.Bulk
import Wire.IndexedUserStore.MigrationStore
import Wire.IndexedUserStore.MigrationStore qualified as MigrationStore
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe), unsafePooledForConcurrentlyN)
import Wire.UserSearch.Migration
import Wire.UserSearch.Types
import Wire.UserStore
import Wire.UserStore.IndexUser

interpretIndexedUserStoreBulk ::
  ( Member TinyLog r,
    Member UserStore r,
    Member (Concurrency Unsafe) r,
    Member GalleyAPIAccess r,
    Member IndexedUserStore r,
    Member (Error MigrationException) r,
    Member IndexedUserMigrationStore r
  ) =>
  InterpreterFor IndexedUserStoreBulk r
interpretIndexedUserStoreBulk = interpret \case
  SyncAllUsers -> syncAllUsersImpl
  ForceSyncAllUsers -> forceSyncAllUsersImpl
  MigrateData -> migrateDataImpl

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
    paginateWithStateC (getIndexUsersPaginated 1000)
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
      let teamIds =
            Set.fromList $
              mapMaybe (fmap value . ((.teamId))) page
      visMap <- fmap Map.fromList . unsafePooledForConcurrentlyN 16 teamIds $ \t ->
        (t,) <$> teamSearchVisibilityInbound t
      let vis indexUser = fromMaybe defaultSearchVisibilityInbound $ (flip Map.lookup visMap . value =<< indexUser.teamId)
          mkUserDoc indexUser = indexUserToDoc (vis indexUser) indexUser
          mkDocVersion = mkVersion . ES.ExternalDocVersion . docVersion . indexUserToVersion
      pure $ map (\u -> (userIdToDocId u.userId, mkUserDoc u, mkDocVersion u)) page

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
  MigrationStore.ensureMigrationIndex
  foundVersion <- MigrationStore.getLatestMigrationVersion
  if expectedMigrationVersion > foundVersion
    then do
      Log.info $
        Log.msg (Log.val "Migration necessary.")
          . Log.field "expectedVersion" expectedMigrationVersion
          . Log.field "foundVersion" foundVersion
      forceSyncAllUsersImpl
      MigrationStore.persistMigrationVersion expectedMigrationVersion
    else do
      Log.info $
        Log.msg (Log.val "No migration necessary.")
          . Log.field "expectedVersion" expectedMigrationVersion
          . Log.field "foundVersion" foundVersion

teamSearchVisibilityInbound :: (Member GalleyAPIAccess r) => TeamId -> Sem r SearchVisibilityInbound
teamSearchVisibilityInbound tid =
  searchVisibilityInboundFromFeatureStatus . (.status)
    <$> getFeatureConfigForTeam @_ @SearchVisibilityInboundConfig tid
