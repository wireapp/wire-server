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

module PhoneDataMigration.Lib where

import Cassandra as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as ConduitList
import Imports
import Options.Applicative
import PhoneDataMigration.Types
import qualified System.Logger.Class as Log
import System.Logger.Message ((.=))

pageSize :: Int32
pageSize = 1000

getKeys :: (MonadClient m) => ConduitM () Phone m ()
getKeys =
  paginateC cql (paramsP LocalQuorum () pageSize) x5
    .| Conduit.concatMap (mapMaybe (parsePhone . runIdentity))
  where
    cql :: PrepQuery R () (Identity Text)
    cql = "SELECT key FROM user_keys"

deleteKey :: (MonadClient m) => Phone -> m IntSum
deleteKey p = do
  retry x5 $ write cql (params LocalQuorum (Identity $ fromPhone p))
  pure 1
  where
    cql :: PrepQuery W (Identity Text) ()
    cql = "DELETE FROM user_keys WHERE key = ?"

deleteKeys :: [Phone] -> ConduitT a IntSum (AppT IO) ()
deleteKeys keys = do
  ConduitList.sourceList keys
    .| Conduit.mapM deleteKey
    .| Conduit.scanl (<>) mempty
    .| Conduit.iterM (logEvery 100000)
  where
    logEvery :: Int -> IntSum -> AppT IO ()
    logEvery i s =
      when (unIntSum s `mod` i == 0) $ Log.info $ "deleted_keys" .= show s

run :: AppT IO ()
run = do
  phoneKeys <- runConduit $ getKeys .| Conduit.sinkList
  result <- runConduit $ deleteKeys phoneKeys .| Conduit.lastDef mempty
  Log.info $ "final_deleted_keys" .= show result

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  env <- mkEnv opts
  runReaderT (unAppT run) env
  where
    desc = header "phone-data-migration" <> progDesc "Remove phone data from wire-server" <> fullDesc
