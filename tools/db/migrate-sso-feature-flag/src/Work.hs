{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

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

module Work where

import Brig.Types hiding (Client)
import Cassandra
import Data.Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.Misc
import Galley.Data.Instances ()
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import Wire.API.Team.Feature

deriving instance Cql Name

runCommand :: Logger -> ClientState -> ClientState -> IO ()
runCommand l spar galley = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient spar) getSsoTeams)
      .| C.mapM
        ( \(i, tids) -> do
            Log.info l (Log.field "number of idps processed: " (show (i * pageSize)))
            pure (runIdentity <$> tids)
        )
      .| C.mapM_ (\tids -> runClient galley (writeSsoFlags tids))

pageSize :: Int32
pageSize = 1000

getSsoTeams :: ConduitM () [Identity TeamId] Client ()
getSsoTeams = paginateC cql (paramsP Quorum () pageSize) x5
  where
    cql :: PrepQuery R () (Identity TeamId)
    cql = "select team from idp"

writeSsoFlags :: [TeamId] -> Client ()
writeSsoFlags = mapM_ (`setSSOTeamConfig` TeamFeatureEnabled)
  where
    setSSOTeamConfig :: MonadClient m => TeamId -> TeamFeatureStatus -> m ()
    setSSOTeamConfig tid ssoTeamConfigStatus = do
      retry x5 $ write updateSSOTeamConfig (params Quorum (ssoTeamConfigStatus, tid))
    updateSSOTeamConfig :: PrepQuery W (TeamFeatureStatus, TeamId) ()
    updateSSOTeamConfig = "update team_features set sso_status = ? where team_id = ?"
