{-# OPTIONS -Wno-ambiguous-fields -Wno-orphans #-}

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

module Work where

import Cassandra
import Conduit
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Conduit.List qualified as C
import Data.Id
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as V
import Imports
import Selector
import System.Logger qualified as Log
import Wire.API.Team.Feature

data Opts = Opts
  { granularity :: Int,
    logger :: Log.Logger,
    clientState :: ClientState,
    feature :: Text,
    selector :: Maybe Selector,
    update :: Maybe UpdateOperation,
    dryRun :: Bool
  }

accumulateConfigs :: (MonadIO m, Ord cfg) => IORef (Map cfg [tid]) -> cfg -> tid -> m ()
accumulateConfigs configsRef cfg tid = do
  modifyIORef configsRef $
    Map.insertWith (<>) cfg [tid]

runCommand :: Opts -> IO ()
runCommand opts =
  selectConfigs opts
    >>= updateConfigs opts

selectConfigs :: Opts -> IO (Map PersistedConfig [TeamId])
selectConfigs opts = do
  uniqConfigs :: IORef (Map (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) [TeamId]) <- newIORef mempty
  teamCount :: IORef Int <- newIORef 0
  let updateTeamCount = do
        modifyIORef teamCount (+ 1)
        count <- readIORef teamCount
        when (count `mod` opts.granularity == 0) $
          Log.info opts.logger $
            Log.msg (Log.val "team count update")
              . Log.field "team_count" count
  runClient opts.clientState $
    runConduit $
      getConfigs
        .| C.map (filter $ \(_, feature, _, _, _) -> feature == opts.feature)
        .| C.concat
        .| C.mapM (<$ updateTeamCount)
        .| C.filter (applySelector opts.selector)
        .| C.mapM_ (\(tid, _, status, lock, cfg) -> accumulateConfigs uniqConfigs (status, lock, cfg) tid)
  finalCount <- readIORef teamCount
  configs <- readIORef uniqConfigs
  Log.info opts.logger $
    Log.msg (Log.val "Finished analysing team feature")
      . Log.field "team_feature" opts.feature
      . Log.field "team_count" finalCount
      . Log.field "unique_configs" (Map.size configs)
  printMostPopularConfigs opts.logger configs
  pure configs

updateConfigs :: Opts -> Map PersistedConfig [TeamId] -> IO ()
updateConfigs Opts {update = Nothing, logger} _ =
  Log.info logger $ Log.msg (Log.val "No updates")
updateConfigs opts@Opts {update = Just upd} oldConfigs = do
  let computedUpdate = computeUpdate upd
  newUniqConfigs :: IORef (Map (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig) [TeamId]) <- newIORef mempty
  runClient opts.clientState . runConduit $
    C.sourceList (Map.toList oldConfigs)
      .| C.mapFoldable distributeTuple
      .| C.mapM
        ( \(cfg, tid) -> do
            newCfg <- applyUpdate opts.feature opts.logger opts.dryRun computedUpdate cfg tid
            pure (newCfg, tid)
        )
      .| C.mapM_ (uncurry $ accumulateConfigs newUniqConfigs)
  newConfigs <- readIORef newUniqConfigs
  Log.info opts.logger $
    Log.msg (Log.val "Finished updating configs")
      . Log.field "dryRun" opts.dryRun
      . Log.field "unique_configs" (Map.size newConfigs)
  printMostPopularConfigs opts.logger newConfigs

distributeTuple :: (a, [b]) -> [(a, b)]
distributeTuple (a, bs) = (a,) <$> bs

printMostPopularConfigs :: (MonadIO m) => Log.Logger -> Map PersistedConfig [TeamId] -> m ()
printMostPopularConfigs lgr configs = do
  when (Map.size configs > 20) $
    Log.warn lgr $
      Log.msg (Log.val "Too many different configs found, printing only 20 most popular configs.")
        . Log.field "config_count" (Map.size configs)
  let mostPopularConfigs =
        take 20
          . sortOn ((0 -) . length . snd)
          $ Map.toList configs
  for_ mostPopularConfigs $ \((status, lockStatus, cfg), teams) ->
    Log.info lgr $
      Log.msg (Log.val "feature config")
        . Log.field "status" (show status)
        . Log.field "lockStatus" (show lockStatus)
        . Log.field "config" (Aeson.encode $ (.unDbConfig) <$> cfg)
        . Log.field "team_count" (length teams)

type PersistedConfig = (Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)

applyUpdate :: Text -> Log.Logger -> Bool -> ComputedUpdate -> PersistedConfig -> TeamId -> Client PersistedConfig
applyUpdate feature lgr dryRun upd (mOldStatus, mOldLock, mOldConfig) tid = do
  let mNewStatus = upd.status <|> mOldStatus
      mNewLock = upd.lockStatus <|> mOldLock
      mNewConfig = maybe id (Just .) upd.dbConfig $ mOldConfig
  Log.debug lgr $
    Log.msg (Log.val "Feature config update")
      . Log.field "status" (show mNewStatus)
      . Log.field "lockStatus" (show mNewLock)
      . Log.field "config" (show mNewConfig)
      . Log.field "team" (idToText tid)
  unless dryRun $ do
    let mStatusUpdate = if mOldStatus /= mNewStatus then mNewStatus else Nothing
        mLockUpdate = if mOldLock /= mNewLock then mNewLock else Nothing
        mConfigUpdate = if mOldConfig /= mNewConfig then mNewConfig else Nothing
    updateConfig feature tid (mStatusUpdate, mLockUpdate, mConfigUpdate)
  pure (mNewStatus, mNewLock, mNewConfig)

data ComputedUpdate = ComputedUpdate
  { status :: Maybe FeatureStatus,
    lockStatus :: Maybe LockStatus,
    dbConfig :: Maybe (Maybe DbConfig -> DbConfig)
  }

computeUpdate :: UpdateOperation -> ComputedUpdate
computeUpdate = \case
  UpdateStatus newStatus -> ComputedUpdate (Just newStatus) Nothing Nothing
  UpdateLockStatus newLock -> ComputedUpdate Nothing (Just newLock) Nothing
  UpdateDbConfig path newVal ->
    ComputedUpdate Nothing Nothing . Just $ \mDbConfig ->
      DbConfig
        . dbConfigUpdate path newVal
        $ maybe Aeson.Null (.unDbConfig) mDbConfig
  UpdateMultiple upd1 upd2 -> mergeUpdates (computeUpdate upd1) (computeUpdate upd2)

mergeUpdates :: ComputedUpdate -> ComputedUpdate -> ComputedUpdate
mergeUpdates upd1 upd2 =
  ComputedUpdate
    { status = upd2.status <|> upd1.status,
      lockStatus = upd2.lockStatus <|> upd1.lockStatus,
      dbConfig = case (upd1.dbConfig, upd2.dbConfig) of
        (Just u1, Just u2) -> Just $ u2 . Just . u1
        (Just u1, Nothing) -> Just u1
        (_, u2) -> u2
    }

dbConfigUpdate :: [Text] -> Val -> Aeson.Value -> Aeson.Value
dbConfigUpdate [] _ cfg = cfg
dbConfigUpdate (seg : remaining) val cfg =
  let valJson = case val of
        ValStr s -> Aeson.String s
        ValNum n -> Aeson.Number n
      newVal previousVal =
        case remaining of
          [] -> valJson
          _ -> dbConfigUpdate remaining val previousVal
   in case (readMaybe @Int (Text.unpack seg), cfg) of
        (Just n, Aeson.Array arr) ->
          if
            | n < 0 -> cfg
            | n < V.length arr ->
                let previousVal = arr V.! n
                 in Aeson.Array $ arr V.// [(n, newVal previousVal)]
            | n == V.length arr -> Aeson.Array $ V.snoc arr (newVal Aeson.Null)
            | otherwise -> cfg
        (Just 0, Aeson.Null) ->
          Aeson.Array . V.singleton $ newVal Aeson.Null
        (Just _, Aeson.Null) ->
          error "invalid update"
        (_, Aeson.Object obj) ->
          let key = Key.fromText seg
              previousVal = fromMaybe Aeson.Null $ KM.lookup key obj
           in Aeson.Object $ KM.insert key (newVal previousVal) obj
        (Nothing, Aeson.Null) ->
          let key = Key.fromText seg
           in Aeson.Object $ KM.singleton key (newVal Aeson.Null)
        _ -> cfg

applySelector :: Maybe Selector -> FeatureRow -> Bool
applySelector Nothing _ = True
applySelector (Just sel) row@(_, _, status, lockStatus, config) =
  case sel of
    SelectorFeatureStatusEq expectedStatus ->
      Just expectedStatus == status
    SelectorLockStatusEq expectedLockStatus ->
      Just expectedLockStatus == lockStatus
    SelectorDbConfigCompare path op compVal ->
      compareDbConfig path op compVal config
    SelectorAnd sel1 sel2 ->
      applySelector (Just sel1) row && applySelector (Just sel2) row
    SelectorOr sel1 sel2 ->
      applySelector (Just sel1) row || applySelector (Just sel2) row

compareDbConfig :: [Text] -> Ordering -> Val -> Maybe DbConfig -> Bool
compareDbConfig _ _ _ Nothing = False
compareDbConfig path expectedOrder compVal (Just config) =
  let mActualVal = retrieveVal path config.unDbConfig
   in case mActualVal of
        Nothing -> False
        Just actualVal ->
          let actualOrder = compVal `compare` actualVal
           in expectedOrder == actualOrder

retrieveVal :: [Text] -> Aeson.Value -> Maybe Val
retrieveVal [] config =
  case config of
    Aeson.String str -> Just $ ValStr str
    Aeson.Number n -> Just $ ValNum n
    _ -> Nothing
retrieveVal (segment : remaining) config =
  case (readMaybe @Int (Text.unpack segment), config) of
    (Just n, Aeson.Array arr) ->
      retrieveVal remaining =<< (arr V.!? n)
    (_, Aeson.Object obj) ->
      retrieveVal remaining =<< KM.lookup (Key.fromText segment) obj
    _ -> Nothing

pageSize :: Int32
pageSize = 10000

type FeatureRow = (TeamId, Text, Maybe FeatureStatus, Maybe LockStatus, Maybe DbConfig)

-- | sadly this needs to go over all configs because finding a specific feature
-- for all teams requires filtering.
getConfigs :: (MonadClient m) => ConduitM () [FeatureRow] m ()
getConfigs =
  paginateC q (paramsP LocalQuorum () pageSize) x5
  where
    q :: PrepQuery R () FeatureRow
    q = "select team, feature, status, lock_status, config from team_features_dyn"

updateConfig :: Text -> TeamId -> PersistedConfig -> Client ()
updateConfig feature tid (mUpdateStatus, mUpdateLock, mUpdateConfig) = do
  batch $ do
    setConsistency LocalQuorum
    for_ mUpdateStatus $ \status ->
      addPrepQuery qStatus (status, tid, feature)
    for_ mUpdateLock $ \lock ->
      addPrepQuery qLock (lock, tid, feature)
    for_ mUpdateConfig $ \cfg ->
      addPrepQuery qConfig (cfg, tid, feature)
  where
    qStatus :: PrepQuery W (FeatureStatus, TeamId, Text) ()
    qStatus = "update team_features_dyn set status = ? where team = ? and feature = ?"
    qLock :: PrepQuery W (LockStatus, TeamId, Text) ()
    qLock = "update team_features_dyn set lock_status = ? where team = ? and feature = ?"
    qConfig :: PrepQuery W (DbConfig, TeamId, Text) ()
    qConfig = "update team_features_dyn set config = ? where team = ? and feature = ?"
