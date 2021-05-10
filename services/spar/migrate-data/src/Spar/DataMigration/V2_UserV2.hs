-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
module Spar.DataMigration.V2_UserV2 where

import Cassandra
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as CL
import Data.Id
import qualified Data.Map.Strict as Map
import Data.String.Conversions (cs)
import Imports
import qualified SAML2.WebSSO as SAML
import Spar.Data.Instances ()
import Spar.DataMigration.Types (logger)
import Spar.DataMigration.Types hiding (logger)
import Spar.Types (NormalizedUNameID, normalizeQualifiedNameId, unNormalizedUNameID)
import qualified System.Logger as Log

migration :: Migration
migration =
  Migration
    { version = MigrationVersion 2,
      text = "Migrate to spar.user_v2",
      action = migrationAction
    }

type DebugMap = Map (SAML.Issuer, NormalizedUNameID) [(SAML.NameID, UserId)]

data OwnEnv = OwnEnv
  { migEnv :: Env,
    debugMap :: IORef DebugMap
  }

type EnvIO = ReaderT OwnEnv IO

logInfo :: String -> EnvIO ()
logInfo msg = do
  logger_ <- asks (logger . migEnv)
  Log.info logger_ (Log.msg msg)

runSpar :: Client a -> EnvIO a
runSpar cl = do
  cass <- asks (sparCassandra . migEnv)
  runClient cass cl

migrationAction :: Env -> IO ()
migrationAction env = do
  let action = case dryRun env of
        DryRun -> runFindCollisions
        NoDryRun -> performMigration
  dbgInput <- newIORef Map.empty
  runReaderT action (OwnEnv env dbgInput)

performMigration :: EnvIO ()
performMigration = do
  runConduit $
    zipSources
      (CL.sourceList [(1 :: Int32) ..])
      readLegacyEntries
      .| migratePage

migratePage :: ConduitM (Int32, [(SAML.Issuer, SAML.NameID, UserId)]) Void EnvIO ()
migratePage = go
  where
    go :: ConduitM (Int32, [(SAML.Issuer, SAML.NameID, UserId)]) Void EnvIO ()
    go = do
      mbPage <- await
      for_ mbPage $ \(page, rows) -> lift $ do
        logInfo $ "page " <> show page
        for_ rows $ \tpl ->
          runSpar $ write insert (params Quorum (migrate tpl))

    insert :: PrepQuery W (SAML.Issuer, NormalizedUNameID, SAML.NameID, UserId) ()
    insert = "INSERT INTO user_v2 (issuer, normalized_uname_id, sso_id, uid) VALUES (?, ?, ?, ?)"

migrate :: (SAML.Issuer, SAML.NameID, UserId) -> (SAML.Issuer, NormalizedUNameID, SAML.NameID, UserId)
migrate (issuer, nid, uid) = (issuer, normalizeQualifiedNameId nid, nid, uid)

readLegacyEntries :: ConduitM () [(SAML.Issuer, SAML.NameID, UserId)] EnvIO ()
readLegacyEntries = do
  pSize <- lift $ asks (pageSize . migEnv)
  transPipe runSpar $
    paginateC select (paramsP Quorum () pSize) x5
  where
    select :: PrepQuery R () (SAML.Issuer, SAML.NameID, UserId)
    select = "SELECT issuer, sso_id, uid FROM user"

runFindCollisions :: EnvIO ()
runFindCollisions = do
  runConduit $
    zipSources
      (CL.sourceList [(1 :: Int32) ..])
      readLegacyEntries
      .| collectDebugMap
  findCollisions =<< (readIORef =<< asks debugMap)

collectDebugMap :: ConduitM (Int32, [(SAML.Issuer, SAML.NameID, UserId)]) Void EnvIO ()
collectDebugMap = go
  where
    go :: ConduitM (Int32, [(SAML.Issuer, SAML.NameID, UserId)]) Void EnvIO ()
    go = do
      dbgMap_ <- asks debugMap
      mbPage <- await
      for_ mbPage $ \(page, rows) -> lift $ do
        logInfo $ "page " <> show page
        for_ rows $ \tpl -> do
          let (issuer, normNid, nid, uid) = migrate tpl
          modifyIORef dbgMap_ (Map.insertWith (<>) (issuer, normNid) [(nid, uid)])

findCollisions :: DebugMap -> EnvIO ()
findCollisions debugMap = do
  logInfo "Looking for collisions..."
  for_ (Map.toList debugMap) $ \((issuer, normId), matches) ->
    when (length matches > 1) $ do
      let matches' = nubBy (\(_nid1, uid1) (_nid2, uid2) -> uid1 == uid2) matches
      when (length matches' > 1) $ do
        logInfo (unlines ["Collisions found for", show issuer, cs . unNormalizedUNameID $ normId])
        for_ matches' $ \(nid, uid) ->
          logInfo $ unwords [show uid, show nid]
  logInfo "...done"
