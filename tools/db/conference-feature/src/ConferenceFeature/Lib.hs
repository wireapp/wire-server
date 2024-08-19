{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2024 Wire Swiss GmbH <opensource@wire.com>
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

module ConferenceFeature.Lib where

import Cassandra as C
import ConferenceFeature.Types
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Set as Set
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as Log
import System.Logger.Message ((.=))
import Wire.API.Team.Feature
import Wire.API.User (AccountStatus (Active))

pageSize :: Int32
pageSize = 1000

readUsers :: forall m. (MonadClient m, MonadLogger m) => ConduitM () [UserRow] m ()
readUsers =
  paginateC cql (paramsP One () pageSize) x5
    .| Conduit.map (fmap CQL.asRecord)
  where
    cql :: C.PrepQuery C.R () (CQL.TupleType UserRow)
    cql =
      "SELECT id, activated, status, team, feature_conference_calling FROM user"

scanUsers :: forall m. (MonadClient m, MonadLogger m) => ConduitT () Result m ()
scanUsers = do
  readUsers
    .| Conduit.concat
    .| Conduit.map userToResult
    .| Conduit.scanl (<>) mempty
    .| Conduit.iterM (logEvery 100000 . usersTotal)
  where
    logEvery :: Int -> Sum Int -> m ()
    logEvery i s =
      when (getSum s `mod` i == 0) $ Log.info $ "intermediate" .= show s

userToResult :: UserRow -> Result
userToResult user = case user.conferenceCallingFeatureStatus of
  Just FeatureStatusEnabled ->
    if user.activated && user.status == Just Active
      then case user.team of
        Just tid -> Result (Sum 1) mempty (Sum 1) mempty (Set.singleton tid)
        Nothing -> Result (Sum 1) (Sum 1) mempty mempty mempty
      else Result (Sum 1) mempty mempty (Sum 1) mempty
  Just FeatureStatusDisabled -> Result (Sum 1) mempty mempty mempty mempty
  Nothing -> Result (Sum 1) mempty mempty mempty mempty

run :: forall m. (MonadClient m, MonadLogger m) => m ()
run = do
  result <- runConduit $ scanUsers .| Conduit.lastDef mempty
  Log.info $ "result" .= show result

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  env <- mkEnv opts
  runReaderT (unAppT run) env
  where
    desc = header "conference-feature" <> progDesc "find users with conference feature" <> fullDesc
