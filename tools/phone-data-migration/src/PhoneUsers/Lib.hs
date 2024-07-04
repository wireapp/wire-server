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
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as CL
import Data.Id (TeamId, UserId)
import Data.Time
import qualified Database.CQL.Protocol as CQL
import Imports
import Options.Applicative
import PhoneDataMigration.Types
import qualified System.Logger as Log
import System.Logger.Message ((.=), (~~))
import Wire.API.Team.Feature (FeatureStatus (FeatureStatusDisabled, FeatureStatusEnabled))
import Wire.API.User (AccountStatus (Active))

let pageSize = 1000

process :: Log.Logger -> Maybe Int -> ClientState -> IO Result
process logger limit brigClient =
  runConduit
    $ undefined brigClient
    .| Conduit.concat
    .| (maybe (Conduit.filter (const True)) Conduit.take limit)
    .| Conduit.mapM (getUserInfo logger brigClient galleyClient)
    .| forever (CL.isolate pageSize .| (Conduit.foldMap infoToResult >>= yield))
    .| Conduit.takeWhile ((> 0) . usersSearched)
    .| CL.scan (<>) mempty
      `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "intermediate_result" .= show r)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  res <- process logger opts.limit brigClient
  Log.info logger $ "result" .= show res
  where
    initLogger =
      Log.new
        . Log.setLogLevel Log.Info
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "phone-data-migration" <> progDesc "Remove phone data from wire-server" <> fullDesc
