{-# LANGUAGE RecordWildCards #-}

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

module PhoneUsers.Lib where

import Cassandra as C
import Cassandra.Settings as C
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Id (UserId)
import Data.Time
import Imports
import Options.Applicative
import PhoneUsers.Types
import qualified System.IO as SIO
import qualified System.Logger as Log

lookupClients :: ClientState -> UserId -> IO [Maybe UTCTime]
lookupClients client u = do
  runClient client $ runIdentity <$$> retry x1 (query selectClients (params LocalQuorum (Identity u)))
  where
    selectClients :: PrepQuery R (Identity UserId) (Identity (Maybe UTCTime))
    selectClients = "SELECT last_active from clients where user = ?"

readUsers :: ClientState -> ConduitM () [UserRow] IO ()
readUsers client =
  transPipe (runClient client) $
    paginateC selectUsersAll (paramsP LocalQuorum () 200) x5
  where
    selectUsersAll :: C.PrepQuery C.R () UserRow
    selectUsersAll =
      "SELECT id,  email, phone, sso_id, activated, status, handle, team, managed_by FROM user"

process :: Maybe Int -> ClientState -> IO Result
process limit client =
  runConduit $
    readUsers client
      .| Conduit.mapM (\chunk -> SIO.hPutStr stderr "." $> chunk)
      .| Conduit.concat
      .| (maybe (Conduit.filter (const True)) Conduit.take limit)
      .| Conduit.mapM (getUserInfo client)
      .| Conduit.foldMap infoToResult

-- activity:
--   inactive: they have no client or client's last_active is greater than 90 days ago
--   active: otherwise
-- team user: if active, check if they are a team user
-- paid team: if active and team user, check the team's billing info from ibis
getUserInfo :: ClientState -> UserRow -> IO UserInfo
getUserInfo client ur = do
  if not $ isCandidate ur
    then pure NoPhoneUser
    else do
      let (uid, _, _, _, _, _, _, mTid, _) = ur
      -- should we give C* a little break here and add a small threadDelay?
      threadDelay 100
      lastActiveTimeStamps <- lookupClients client uid
      -- SIO.hPrint stderr lastActiveTimeStamps
      now <- getCurrentTime
      let activeLast90Days = any (clientWasActiveLast90Days now) lastActiveTimeStamps
      let userInfo =
            if activeLast90Days
              then do
                -- check if they are a paid team user
                maybe ActivePersonalUser (const $ ActiveTeamUser Paid) mTid
              else InactiveLast90Days
      -- SIO.hPrint stderr ur >> SIO.hPrint stderr userInfo
      pure $ PhoneUser userInfo
  where
    -- drop users with any status other than `Active`
    -- only phone/sms login: drop if they have a verified email
    -- also drop if no phone
    isCandidate :: UserRow -> Bool
    isCandidate (_, Nothing, Just _, _, True, _, _, _, _) = True
    isCandidate _ = False

clientWasActiveLast90Days :: UTCTime -> Maybe UTCTime -> Bool
clientWasActiveLast90Days now = \case
  Just t -> diffUTCTime now t < 90 * nominalDay
  -- if last_active is not set, we consider the client active
  Nothing -> True

infoToResult :: UserInfo -> Result
infoToResult = \case
  NoPhoneUser -> mempty {usersSearched = 1}
  PhoneUser InactiveLast90Days -> mempty {usersSearched = 1, phoneUsersTotal = 1, inactivePhoneUsers = 1}
  PhoneUser ActivePersonalUser -> mempty {usersSearched = 1, phoneUsersTotal = 1, activePhoneUsers = 1}
  PhoneUser (ActiveTeamUser Free) ->
    Result
      { usersSearched = 1,
        phoneUsersTotal = 1,
        inactivePhoneUsers = 0,
        activePhoneUsers = 1,
        activeTeamPhoneUsers = 1,
        activeFreeTeamPhoneUsers = 1,
        activePaidTeamPhoneUsers = 0
      }
  PhoneUser (ActiveTeamUser Paid) ->
    Result
      { usersSearched = 1,
        phoneUsersTotal = 1,
        inactivePhoneUsers = 0,
        activePhoneUsers = 1,
        activeTeamPhoneUsers = 1,
        activeFreeTeamPhoneUsers = 0,
        activePaidTeamPhoneUsers = 1
      }

main :: IO ()
main = do
  opts <- execParser (info (helper <*> sampleParser) desc)
  logger <- initLogger
  client <- initCas opts logger
  putStrLn "scanning users table..."
  res <- process opts.limit client
  putStrLn "\n"
  print res
  where
    initLogger = Log.new . Log.setOutput Log.StdOut . Log.setFormat Nothing . Log.setBufSize 0 $ Log.defSettings
    initCas Opts {..} l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts cHost []
        . C.setPortNumber (fromIntegral cPort)
        . C.setKeyspace cKeyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "phone-users" <> progDesc "This program scans brig's users table and determines the number of users that can only login by phone/sms" <> fullDesc
