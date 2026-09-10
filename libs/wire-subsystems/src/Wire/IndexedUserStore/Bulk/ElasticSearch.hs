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
import Control.Error (headMay, hush)
import Control.Exception (try)
import Data.Conduit.Combinators qualified as Conduit
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as CL
import Data.Id
import Data.Json.Util (UTCTimeMillis (fromUTCTimeMillis))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Database.Bloodhound qualified as ES
import Imports
import Polysemy
import Polysemy.Error hiding (try)
import Polysemy.TinyLog
import Polysemy.TinyLog qualified as Log
import System.Logger.Message qualified as Log
import UnliftIO (pooledForConcurrentlyN)
import Wire.API.Team.Collaborator (gTeam, gUser)
import Wire.API.Team.Feature
import Wire.API.Team.Member.Info
import Wire.API.Team.Role
import Wire.GalleyAPIAccess
import Wire.IndexedUserStore (IndexedUserStore)
import Wire.IndexedUserStore qualified as IndexedUserStore
import Wire.IndexedUserStore.MigrationStore
import Wire.IndexedUserStore.MigrationStore qualified as MigrationStore
import Wire.TeamCollaboratorsStore (TeamCollaboratorsStore, getTeamCollaborationsForUsers)
import Wire.UserSearch.Migration
import Wire.UserSearch.Types
import Wire.UserStore
import Wire.UserStore.IndexUser

type IOInterpreter r = forall a. Sem r a -> IO a

-- | Increase this number any time you want to force reindexing.
expectedMigrationVersion :: MigrationVersion
expectedMigrationVersion = MigrationVersion 7

syncAllUsers :: (Member UserStore r, Member IndexedUserStore r, Member TinyLog r, Member GalleyAPIAccess r, Member TeamCollaboratorsStore r) => IOInterpreter r -> Int32 -> IO (Int, [String])
syncAllUsers interpreter pageSize = syncAllUsersWithVersion interpreter pageSize ES.ExternalGT

forceSyncAllUsers :: (Member UserStore r, Member IndexedUserStore r, Member TinyLog r, Member GalleyAPIAccess r, Member TeamCollaboratorsStore r) => IOInterpreter r -> Int32 -> IO (Int, [String])
forceSyncAllUsers interpreter pageSize = syncAllUsersWithVersion interpreter pageSize ES.ExternalGTE

-- | Returns the number of users that could not be indexed because some of the
-- data needed to build their document was unavailable, together with a list
-- of error messages describing the failures (one per skipped user).  Those
-- users have been logged individually by 'logFailures'.
syncAllUsersWithVersion :: (Member UserStore r, Member IndexedUserStore r, Member TinyLog r, Member GalleyAPIAccess r, Member TeamCollaboratorsStore r) => IOInterpreter r -> Int32 -> (ES.ExternalDocVersion -> ES.VersionControl) -> IO (Int, [String])
syncAllUsersWithVersion interpreter pageSize mkVersion =
  fmap (\(Sum skipped, errors) -> (skipped, errors)) . runConduit $
    zipSources (CL.sourceList [1 ..]) (paginateWithStateC (interpreter . getIndexUsersPaginated pageSize))
      .| logPage
      .| mkUserDocs
      .| Conduit.foldMapM upsertPage
  where
    upsertPage :: (Int, [String], [(ES.DocId, UserDoc, ES.VersionControl)]) -> IO (Sum Int, [String])
    upsertPage (skipped, errors, docs) = (Sum skipped, errors) <$ interpreter (IndexedUserStore.bulkUpsert docs)

    logPage :: ConduitT (Int32, [IndexUser]) [IndexUser] IO ()
    logPage = Conduit.mapM $ \(pageNumber, page) -> do
      interpreter $
        info $
          Log.field "estimatedUserSoFar" (length page + fromIntegral (pageSize * pageNumber))
            . Log.msg (Log.val "Received user page")
            . Log.field "firstUser" (maybe "N/A" (idToText . (.userId)) (headMay page))
      pure page

    -- Emits the documents to be indexed together with the number of users of
    -- this page that had to be skipped and the error messages for each of
    -- those skipped users.
    mkUserDocs :: ConduitT [IndexUser] (Int, [String], [(ES.DocId, UserDoc, ES.VersionControl)]) IO ()
    mkUserDocs = Conduit.mapM $ \page -> do
      -- FUTUREWORK: extract team visibilities, roles and user type
      -- more efficiently sending one query per page

      -- FUTUREWORK: introduce type ExtendedUser (or something), which
      -- contains User, Maybe Role, UserType, ..., and pass around
      -- ExtendedUser.  this should make the code less convoluted.

      let teams :: Map TeamId [IndexUser]
          teams = Map.fromListWith (<>) $ mapMaybe (\u -> (,[u]) <$> u.teamId) page

      roles :: Map UserId (WithWritetime Role) <- do
        let -- Accounts that have no team, can have no role.  If a role
            -- can't be found on brig for an account, that account does
            -- not have role info in their index or document any more.
            -- This is fine because it only affects accounts that are
            -- already inconsistent accross cassandras (user entry with
            -- team ref, but no team member entry).
            getRoles :: TeamId -> [UserId] -> IO (Map UserId (WithWritetime Role))
            getRoles tid uids = do
              eithMembers <- try @SomeException $ interpreter $ (.members) <$> selectTeamMemberInfos tid uids
              pure case eithMembers of
                Left _ -> Map.empty
                Right tms -> Map.fromList $ mapMaybe mkRoleWithWriteTime tms

        fmap Map.unions . pooledForConcurrentlyN 16 (Map.toList teams) $ \(t, us) ->
          getRoles t (fmap (.userId) us)

      vis :: IndexUser -> SearchVisibilityInbound <- do
        visMap <- fmap Map.fromList . pooledForConcurrentlyN 16 (Map.keys teams) $ \t -> do
          x <- try @SomeException $ interpreter $ teamSearchVisibilityInbound t
          pure (t, x)
        pure $ \vis indexUser ->
          fromMaybe SearchableByOwnTeam $ hush =<< flip Map.lookup visMap =<< indexUser.teamId

      mkUserDoc :: IndexUser -> Either SomeException UserDoc <- do
        -- One query for the whole page.  A failure here fails every document of the
        -- page, which 'logFailures' then logs and skips.
        eithCollabTeams :: Either SomeException (Map UserId [TeamId]) <-
          try . fmap (Map.fromListWith (<>) . map (\tc -> (gUser tc, [gTeam tc]))) . interpreter $
            getTeamCollaborationsForUsers (Set.fromList (map (.userId) page))

        pure \indexUser -> do
          let currentVis = vis indexUser
              currentRole = ((.value)) <$> Map.lookup indexUser.userId roles
          currentCollabTeams <- Map.findWithDefault [] indexUser.userId <$> eithCollabTeams
          pure $ indexUserToDoc currentVis currentRole currentCollabTeams indexUser

      let mkDocVersion :: IndexUser -> Either SomeException ES.VersionControl
          mkDocVersion u =
            let roleWithTime = Map.lookup u.userId roles
             in pure . mkVersion . ES.ExternalDocVersion . docVersion $ indexUserToVersion roleWithTime u

          docsWithErrors :: (e ~ Either SomeException) => [(ES.DocId, e UserDoc, e ES.VersionControl)]
          docsWithErrors = map (\u -> (userIdToDocId u.userId, mkUserDoc u, mkDocVersion u)) page

      results <- interpreter $ traverse logFailures docsWithErrors
      let (errors, docs) = partitionEithers results
          skipped = length errors
          cappedErrors = take 1000 errors
      pure (skipped, cappedErrors, docs)

    logFailures :: (Member TinyLog r) => (ES.DocId, Either SomeException UserDoc, Either SomeException ES.VersionControl) -> Sem r (Either String (ES.DocId, UserDoc, ES.VersionControl))
    logFailures (docId@(ES.DocId idText), eithUserDoc, eithVersion) =
      case (,) <$> eithUserDoc <*> eithVersion of
        Left e -> do
          Log.err $
            Log.msg (Log.val "Error ocurred while indexing user")
              . Log.field "userId" idText
              . Log.field "error" (show e)
          pure $ Left $ show idText <> ": " <> show e
        Right (userDoc, version) -> pure $ Right (docId, userDoc, version)

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
  (Member (Embed IO) r, Member IndexedUserStore r, Member (Error MigrationException) r, Member IndexedUserMigrationStore r, Member TinyLog r, Member UserStore r, Member GalleyAPIAccess r, Member TeamCollaboratorsStore r) =>
  IOInterpreter r ->
  Int32 ->
  IO ()
migrateData interpreter pageSize = interpreter $ do
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
      (skipped, errors) <- embed $ forceSyncAllUsers interpreter pageSize
      if skipped == 0
        then MigrationStore.persistMigrationVersion expectedMigrationVersion
        else do
          Log.err $
            Log.msg (Log.val "Migration incomplete, not persisting migration version.")
              . Log.field "expectedVersion" expectedMigrationVersion
              . Log.field "skippedUsers" skipped
              . Log.field "errors" (show errors)
          throw $ SyncIncomplete
    else do
      Log.info $
        Log.msg (Log.val "No migration necessary.")
          . Log.field "expectedVersion" expectedMigrationVersion
          . Log.field "foundVersion" foundVersion

teamSearchVisibilityInbound :: (Member GalleyAPIAccess r) => TeamId -> Sem r SearchVisibilityInbound
teamSearchVisibilityInbound tid =
  searchVisibilityInboundFromFeatureStatus . (.status)
    <$> getFeatureConfigForTeam @_ @SearchVisibilityInboundConfig tid
