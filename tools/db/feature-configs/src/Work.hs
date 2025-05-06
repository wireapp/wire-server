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
import Data.Conduit.List qualified as C
import Data.Id
import Data.Map qualified as Map
import Imports
import System.Logger qualified as Log
import Wire.API.Team.Feature

data Opts = Opts
  { granularity :: Int,
    logger :: Log.Logger,
    clientState :: ClientState,
    feature :: Text
  }

runCommand :: Opts -> IO ()
runCommand opts = do
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
        .| C.mapM_
          ( \(tid, _, status, lockStatus, cfg) -> do
              modifyIORef uniqConfigs $
                Map.insertWith (<>) (status, lockStatus, cfg) [tid]
          )
  finalCount <- readIORef teamCount
  configs <- readIORef uniqConfigs
  Log.info opts.logger $
    Log.msg (Log.val "Finished analysing team feature")
      . Log.field "team_feature" opts.feature
      . Log.field "team_count" finalCount
      . Log.field "unique_configs" (Map.size configs)
  when (Map.size configs > 20) $
    Log.warn opts.logger $
      Log.msg (Log.val "Too many different configs found, printing only 20 most popular configs.")
        . Log.field "config_count" (Map.size configs)
  let mostPopularConfigs =
        take 20
          . sortOn (length . snd)
          $ Map.toList configs
  for_ mostPopularConfigs $ \((status, lockStatus, cfg), teams) ->
    Log.info opts.logger $
      Log.msg (Log.val "feature config")
        . Log.field "status" (show status)
        . Log.field "lockStatus" (show lockStatus)
        . Log.field "config" (Aeson.encode $ (.unDbConfig) <$> cfg)
        . Log.field "team_count" (length teams)

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
