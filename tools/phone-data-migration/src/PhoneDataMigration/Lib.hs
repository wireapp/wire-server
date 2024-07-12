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
import PhoneDataMigration.Phone
import PhoneDataMigration.Types
import System.Logger.Class (MonadLogger)
import qualified System.Logger.Class as Log
import System.Logger.Message ((.=))

pageSize :: Int32
pageSize = 1000

getKeys :: forall m. (MonadClient m, MonadLogger m) => IORef Int -> ConduitM () Phone m ()
getKeys counter =
  paginateC cql (paramsP LocalQuorum () pageSize) x5
    .| Conduit.concatMap (mapMaybe (parsePhone . runIdentity))
    .| Conduit.iterM (const $ logEvery 100000 counter)
  where
    cql :: PrepQuery R () (Identity Text)
    cql = "SELECT key FROM user_keys"

    logEvery :: Int -> IORef Int -> m ()
    logEvery i ctr = do
      c <- liftIO $ atomicModifyIORef' ctr (\x -> (x + 1, x + 1))
      when (c `mod` i == 0) $ Log.info $ "found_phone_keys" .= show c

deleteKey :: (MonadClient m) => Phone -> m IntSum
deleteKey p = do
  retry x5 $ write cql (params LocalQuorum (Identity $ toText p))
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
  ctr <- liftIO $ newIORef 0
  -- deleting from user_keys while paginating through it is probably not a good idea
  -- therefore we read all keys first and then delete them
  phoneKeys <- runConduit $ getKeys ctr .| Conduit.sinkList
  result <- runConduit $ deleteKeys phoneKeys .| Conduit.lastDef mempty
  Log.info $ "final_deleted_phone_keys" .= show result

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  env <- mkEnv opts
  runReaderT (unAppT run) env
  where
    desc = header "phone-data-migration" <> progDesc "Remove phone data from wire-server" <> fullDesc
