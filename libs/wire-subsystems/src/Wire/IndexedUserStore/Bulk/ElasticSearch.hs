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
import Data.Aeson (encode)
import Data.Conduit.Combinators qualified as Conduit
import Data.Id
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import Data.Map qualified as Map
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import Wire.API.Team.Feature
import Wire.API.Team.Member.Info
import Wire.API.Team.Role
import Wire.API.User
import Wire.AppStore
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
    Member AppStore r,
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
    Member AppStore r,
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
    Member AppStore r,
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
    Member AppStore r,
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
      -- FUTUREWORK: extract team visibilities, roles and user type
      -- more efficiently sending one query per page

      -- FUTUREWORK: introduce type ExtendedUser (or something), which
      -- contains User, Maybe Role, UserType, ..., and pass around
      -- ExtendedUser.  this should make the code less convoluted.

      let teams :: Map TeamId [IndexUser] = Map.fromListWith (<>) $ mapMaybe (\u -> (,[u]) . value <$> u.teamId) page
          teamIds = Map.keys teams
      visMap <- fmap Map.fromList . unsafePooledForConcurrentlyN 16 teamIds $ \t ->
        (t,) <$> teamSearchVisibilityInbound t
      userTypes :: Map UserId UserType <- fmap Map.fromList . unsafePooledForConcurrentlyN 16 page $ \iu ->
        (iu.userId,) <$> getUserType iu
      warnIfMissingUserTypes page userTypes
      roles :: Map UserId (WithWritetime Role) <- fmap (Map.fromList . concat) . unsafePooledForConcurrentlyN 16 (Map.toList teams) $ \(t, us) -> do
        tms <- (.members) <$> selectTeamMemberInfos t (fmap (.userId) us)
        pure $ mapMaybe mkRoleWithWriteTime tms
      let vis indexUser = fromMaybe defaultSearchVisibilityInbound $ (flip Map.lookup visMap . value =<< indexUser.teamId)
          mkUserDoc indexUser =
            indexUserToDoc
              (vis indexUser)
              (Map.lookup indexUser.userId userTypes)
              ((.value) <$> Map.lookup indexUser.userId roles)
              indexUser
          mkDocVersion u = mkVersion . ES.ExternalDocVersion . docVersion $ indexUserToVersion (Map.lookup u.userId roles) u
      pure $ map (\u -> (userIdToDocId u.userId, mkUserDoc u, mkDocVersion u)) page

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

    -- `page` and `userTypes` *should* overlap perfectly, but we're
    -- using `unsafePooledForConcurrentlyN` to make concurrent db
    -- calls and that swallows any errors that might occur.
    --
    -- FUTUREWORK: we need to get rid of `Wire.Sem.Concurrency`, it's
    -- unidiomatic and dangerous!
    warnIfMissingUserTypes :: [IndexUser] -> Map UserId ignored -> Sem r ()
    warnIfMissingUserTypes page userTypes = do
      let missing = us \\ ts
          us, ts :: [UserId]
          us = (.userId) <$> page
          ts = Map.keys userTypes
      unless (null missing) do
        warn $
          Log.field "missing" (encode missing)
            . Log.msg (Log.val "Reindex: could not lookup all user types!")

migrateDataImpl ::
  ( Member IndexedUserStore r,
    Member (Error MigrationException) r,
    Member IndexedUserMigrationStore r,
    Member UserStore r,
    Member AppStore r,
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

-- | FUTUREWORK: this is duplicated code from UserSubsystem, we should
-- probably expose it as an action there.
getUserType ::
  forall r.
  (Member AppStore r) =>
  IndexUser ->
  Sem r UserType
getUserType iu = case iu.serviceId of
  Just _ -> pure UserTypeBot
  Nothing -> do
    mmApp <- mapM (getApp iu.userId) (iu.teamId <&> (.value))
    case join mmApp of
      Just _ -> pure UserTypeApp
      Nothing -> pure UserTypeRegular
