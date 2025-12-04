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

module MlsUsers.Lib where

import Cassandra as C hiding (All)
import Cassandra.Settings as C hiding (All)
import Conduit
import Control.Monad.Extra
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as ConduitL
import Data.Id
import qualified Data.Set as Set
import Data.Time.Clock
import qualified Database.CQL.Protocol as CQL
import Imports
import MlsUsers.Types
import Options.Applicative
import qualified System.Logger as Log
import System.Logger.Message ((.=), (~~))
import Wire.API.User

getUserResult :: Log.Logger -> ClientState -> UserRow -> IO Result
getUserResult logger brigClient ur = do
  included <-
    andM
      [ pure ur.activated,
        pure $ ur.status == Just Active,
        pure $ Set.member BaseProtocolMLSTag ur.supportedProtocols,
        -- check that the user has at least one active client
        do
          now <- getCurrentTime
          tms <- catMaybes <$> lookupClientsLastActiveTimestamps brigClient ur.userId
          let active = any (\tm -> diffUTCTime now tm < 90 * nominalDay) tms
          when active $
            Log.info logger $
              "user_record" .= show ur
                ~~ "last_active_timestamps" .= show tms
                ~~ Log.msg (Log.val "active user found")
          pure active
      ]

  pure
    Result
      { totalUsers = 1,
        activeNoMLS = if included then 1 else 0
      }

process :: Log.Logger -> Maybe Int -> ClientState -> IO Result
process logger limit brigClient =
  runConduit $
    readUsers brigClient
      .| Conduit.concat
      .| (maybe (mapC id) takeC limit)
      -- process users in chunks, yield a Result for each chunk
      .| forever
        ( ConduitL.isolate 10000
            .| (foldMapMC (getUserResult logger brigClient) >>= yield)
        )
      .| Conduit.takeWhile ((> 0) . totalUsers)
      -- join all results and log
      .| ConduitL.scan (<>) mempty
      `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "intermediate_result" .= show r)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  result <- process logger opts.limit brigClient
  Log.info logger $ "result" .= show result
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
    desc = header "mls-users" <> progDesc "This program scans brig's users table and finds active users that don't support MLS" <> fullDesc

--------------------------------------------------------------------------------
-- queries

lookupClientsLastActiveTimestamps :: ClientState -> UserId -> IO [Maybe UTCTime]
lookupClientsLastActiveTimestamps client u = do
  runClient client $ runIdentity <$$> retry x1 (query selectClients (params One (Identity u)))
  where
    selectClients :: PrepQuery R (Identity UserId) (Identity (Maybe UTCTime))
    selectClients = "SELECT last_active from clients where user = ?"

readUsers :: ClientState -> ConduitM () [UserRow] IO ()
readUsers client =
  transPipe (runClient client) (paginateC selectUsersAll (paramsP One () 1000) x5)
    .| Conduit.map (fmap CQL.asRecord)
  where
    selectUsersAll :: C.PrepQuery C.R () (CQL.TupleType UserRow)
    selectUsersAll =
      "SELECT id, activated, status, supported_protocols FROM user"
