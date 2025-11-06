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
import Hasql.Pool (Pool, UsageError)
import Imports
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.Input (Input)
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Federation.Client (FederatorClient)
import Wire.API.Federation.Error (FederationError)
import Wire.API.Team.Feature
import Wire.API.Team.Member.Info
import Wire.API.Team.Role
import Wire.AppStore (AppStore)
import Wire.BlockListStore (BlockListStore)
import Wire.ClientSubsystem.Error (ClientError)
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
    AppStore,
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
    Input Pool,
    Error UsageError,
    Error ClientError,
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
      -- FUTUREWORK: extract team visibilities, roles and user type
      -- more efficiently sending one query per page

      -- FUTUREWORK: introduce type ExtendedUser (or something), which
      -- contains User, Maybe Role, UserType, ..., and pass around
      -- ExtendedUser.  this should make the code less convoluted.

      let teams :: Map TeamId [IndexUser] = Map.fromListWith (<>) $ mapMaybe (\u -> (,[u]) <$> u.teamId) page
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
          vis indexUser =
            fromMaybe (Right defaultSearchVisibilityInbound) $ flip Map.lookup visMap =<< indexUser.teamId

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
          Log.info $
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
