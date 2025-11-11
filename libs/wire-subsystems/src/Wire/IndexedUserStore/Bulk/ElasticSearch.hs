module Wire.IndexedUserStore.Bulk.ElasticSearch where

import Cassandra.Exec (paginateWithStateC)
import Cassandra.Util (Writetime (Writetime))
import Conduit (ConduitT, runConduit, (.|))
import Control.Error (headMay)
import Control.Exception (try)
import Control.Monad.Extra (mapMaybeM)
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as CL
import Data.Id
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import Data.Map qualified as Map
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError)
import Wire.API.Team.Feature
import Wire.API.Team.Member.Info
import Wire.API.Team.Role
import Wire.BlockListStore (BlockListStore)
import Wire.FederationAPIAccess (FederationAPIAccess)
import Wire.FederationConfigStore (FederationConfigStore)
import Wire.GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserStore, IndexedUserStoreError)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.IndexedUserStore.MigrationStore
import Wire.IndexedUserStore.MigrationStore qualified as MigrationStore
import Wire.ParseException (ParseException)
import Wire.Rpc (Rpc)
import Wire.Sem.Concurrency (Concurrency, ConcurrencySafety (Unsafe))
import Wire.Sem.Metrics (Metrics)
import Wire.UserKeyStore (UserKeyStore)
import Wire.UserSearch.Migration
import Wire.UserSearch.Types
import Wire.UserStore
import Wire.UserStore.IndexUser
import Wire.UserSubsystem.Error (UserSubsystemError)

type BulkEffectStack =
  [ UserKeyStore,
    BlockListStore,
    Error UserSubsystemError,
    FederationAPIAccess FederatorClient,
    Error FederationError,
    UserStore,
    IndexedUserStore,
    Error IndexedUserStoreError,
    IndexedUserMigrationStore,
    Error MigrationException,
    FederationConfigStore,
    GalleyAPIAccess,
    Error ParseException,
    Rpc,
    Metrics,
    TinyLog,
    Concurrency 'Unsafe,
    Embed IO,
    Final IO
  ]

type BulkEffectStackInterpreter = forall a. Sem BulkEffectStack a -> IO a

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 6

syncAllUsers :: BulkEffectStackInterpreter -> IO ()
syncAllUsers interpreter = syncAllUsersWithVersion interpreter ES.ExternalGT

forceSyncAllUsers :: BulkEffectStackInterpreter -> IO ()
forceSyncAllUsers interpreter = syncAllUsersWithVersion interpreter ES.ExternalGTE

syncAllUsersWithVersion :: BulkEffectStackInterpreter -> (ES.ExternalDocVersion -> ES.VersionControl) -> IO ()
syncAllUsersWithVersion interpreter mkVersion =
  runConduit $
    zipSources (CL.sourceList [1 ..]) (paginateWithStateC (interpreter . getIndexUsersPaginated pageSize))
      .| logPage
      .| mkUserDocs
      .| Conduit.mapM_ (interpreter . IndexedUserStore.bulkUpsert)
  where
    pageSize = 10000

    logPage :: ConduitT (Int32, [IndexUser]) [IndexUser] IO ()
    logPage = Conduit.mapM $ \(pageNumber, page) -> do
      interpreter $
        info $
          Log.field "estimatedUserSoFar" (length page + fromIntegral (pageSize * pageNumber))
            . Log.msg (Log.val "Received user page")
            . Log.field "firstUser" (maybe "N/A" (idToText . (.userId)) (headMay page))
      pure page

    mkUserDocs :: ConduitT [IndexUser] [(ES.DocId, UserDoc, ES.VersionControl)] IO ()
    mkUserDocs = Conduit.mapM $ \page -> do
      let teams :: Map TeamId [IndexUser] = Map.fromListWith (<>) $ mapMaybe (\u -> (,[u]) . value <$> u.teamId) page
          teamIds = Map.keys teams

      visMap <- fmap Map.fromList . pooledForConcurrentlyN 16 teamIds $ \t -> do
        x <- try $ interpreter $ teamSearchVisibilityInbound t
        pure (t, x)

      roles :: Map UserId (Either SomeException (WithWritetime Role)) <-
        fmap (Map.fromList . concat) . pooledForConcurrentlyN 16 (Map.toList teams) $ \(t, us) -> do
          eithMembers <- try $ interpreter $ (.members) <$> selectTeamMemberInfos t (fmap (.userId) us)
          case eithMembers of
            Left e -> pure $ map (\iu -> (iu.userId, Left e)) us
            Right tms -> pure $ mapMaybe (fmap rightSecond . mkRoleWithWriteTime) tms

      let vis :: IndexUser -> Either SomeException SearchVisibilityInbound
          vis indexUser = do
            let mTeam = value <$> indexUser.teamId
             in fromMaybe (Right defaultSearchVisibilityInbound) $ flip Map.lookup visMap =<< mTeam

          mkUserDoc :: IndexUser -> Either SomeException UserDoc
          mkUserDoc indexUser = do
            currentVis <- vis indexUser
            currentRole <- sequence $ Map.lookup indexUser.userId roles
            pure $ indexUserToDoc currentVis ((.value) <$> currentRole) indexUser

          mkDocVersion :: IndexUser -> Either SomeException ES.VersionControl
          mkDocVersion u = do
            roleWithTime <- sequence (Map.lookup u.userId roles)
            pure . mkVersion . ES.ExternalDocVersion . docVersion $ indexUserToVersion roleWithTime u

      let docsWithErrors = map (\u -> (userIdToDocId u.userId, mkUserDoc u, mkDocVersion u)) page
      interpreter . flip mapMaybeM docsWithErrors $ logAndHush

    rightSecond :: (a, b) -> (a, Either c b)
    rightSecond (a, b) = (a, Right b)

    logAndHush :: (Member TinyLog r) => (ES.DocId, Either SomeException UserDoc, Either SomeException ES.VersionControl) -> Sem r (Maybe (ES.DocId, UserDoc, ES.VersionControl))
    logAndHush (docId@(ES.DocId idText), eithUserDoc, eithVersion) =
      case (,) <$> eithUserDoc <*> eithVersion of
        Left e -> do
          Log.err $
            Log.msg (Log.val "Error ocurred while indexing user")
              . Log.field "userId" idText
              . Log.field "error" (show e)
          pure Nothing
        Right (userDoc, version) -> pure $ Just (docId, userDoc, version)

    mkRoleWithWriteTime :: TeamMemberInfo -> Maybe (UserId, WithWritetime Role)
    mkRoleWithWriteTime tmi =
      ( \role ->
          ( tmi.userId,
            WithWriteTime
              { value = role,
                writetime = Writetime $ fromUTCTimeMillis tmi.permissionsWriteTime
              }
          )
      )
        <$> permissionsToRole tmi.permissions

migrateData ::
  BulkEffectStackInterpreter ->
  IO ()
migrateData interpreter = interpreter $ do
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
      embed $ forceSyncAllUsers interpreter
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
