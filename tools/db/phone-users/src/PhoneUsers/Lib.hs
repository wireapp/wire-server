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

module PhoneUsers.Lib where

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
import PhoneUsers.Types
-- import qualified System.IO as SIO
import qualified System.Logger as Log
import System.Logger.Message ((.=), (~~))
import Wire.API.Team.Feature (FeatureStatus (FeatureStatusDisabled, FeatureStatusEnabled))
import Wire.API.User (AccountStatus (Active))

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
      "SELECT id, email, phone, activated, status, team FROM user"

getConferenceCalling :: ClientState -> TeamId -> IO (Maybe FeatureStatus)
getConferenceCalling client tid = do
  runClient client $ runIdentity <$$> retry x1 (query1 select (params One (Identity tid)))
  where
    select :: PrepQuery R (Identity TeamId) (Identity FeatureStatus)
    select =
      "select conference_calling from team_features where team_id = ?"

process :: Log.Logger -> Maybe Int -> ClientState -> ClientState -> IO Result
process logger limit brigClient galleyClient =
  runConduit
    $ readUsers brigClient
    -- .| Conduit.mapM (\chunk -> SIO.hPutStr stderr "." $> chunk)
    .| Conduit.concat
    .| (maybe (Conduit.filter (const True)) Conduit.take limit)
    .| Conduit.mapM (getUserInfo logger brigClient galleyClient)
    .| forever (CL.isolate 10000 .| (Conduit.foldMap infoToResult >>= yield))
    .| Conduit.takeWhile ((> 0) . usersSearched)
    .| CL.scan (<>) mempty
      `fuseUpstream` Conduit.mapM_ (\r -> Log.info logger $ "intermediate_result" .= show r)

getUserInfo :: Log.Logger -> ClientState -> ClientState -> UserRow -> IO UserInfo
getUserInfo logger brigClient galleyClient ur = do
  if not $ isCandidate
    then pure NoPhoneUser
    else do
      -- should we give C* a little break here and add a small threadDelay?
      -- threadDelay 200
      lastActiveTimeStamps <- lookupClientsLastActiveTimestamps brigClient ur.id
      now <- getCurrentTime
      -- activity:
      --   inactive: they have no client or client's last_active is greater than 90 days ago
      --   active: otherwise
      -- last_active is null on client creation, but it will be set once notifications are fetched
      -- therefore we can consider empty last_active as inactive
      let activeLast90Days = any (clientWasActiveLast90Days now) $ catMaybes lastActiveTimeStamps
      userInfo <-
        if activeLast90Days
          then do
            apu <- case ur.team of
              Nothing -> pure ActivePersonalUser
              Just tid -> do
                isPaying <- isPayingTeam galleyClient tid
                pure
                  $ if isPaying
                    then ActiveTeamUser Free
                    else ActiveTeamUser Paid
            Log.info logger
              $ "active_phone_user"
              .= show apu
              ~~ "user_record" .= show ur
              ~~ "last_active_timestamps" .= show lastActiveTimeStamps
              ~~ Log.msg (Log.val "active phone user found")
            pure apu
          else pure InactiveLast90Days
      pure $ PhoneUser userInfo
  where
    -- to qualify as an active phone user candidate, their account must be active and they must have a phone number but no verified email
    isCandidate :: Bool
    isCandidate =
      ur.activated && ur.status == Just Active && isJust ur.phone && isNothing ur.email

    clientWasActiveLast90Days :: UTCTime -> UTCTime -> Bool
    clientWasActiveLast90Days now lastActive = diffUTCTime now lastActive < 90 * nominalDay

    -- if conference_calling is enabled for the team, then it's a paying team
    isPayingTeam :: ClientState -> TeamId -> IO Bool
    isPayingTeam client tid = do
      status <- getConferenceCalling client tid
      pure $ case status of
        Just FeatureStatusEnabled -> True
        Just FeatureStatusDisabled -> False
        Nothing -> False

infoToResult :: UserInfo -> Result
infoToResult = \case
  NoPhoneUser -> mempty {usersSearched = 1}
  PhoneUser InactiveLast90Days -> mempty {usersSearched = 1, phoneUsersTotal = 1, inactivePhoneUsers = 1}
  PhoneUser ActivePersonalUser -> mempty {usersSearched = 1, phoneUsersTotal = 1, activePersonalPhoneUsers = 1}
  PhoneUser (ActiveTeamUser Free) ->
    Result
      { usersSearched = 1,
        phoneUsersTotal = 1,
        inactivePhoneUsers = 0,
        activePersonalPhoneUsers = 0,
        activeFreeTeamPhoneUsers = 1,
        activePaidTeamPhoneUsers = 0
      }
  PhoneUser (ActiveTeamUser Paid) ->
    Result
      { usersSearched = 1,
        phoneUsersTotal = 1,
        inactivePhoneUsers = 0,
        activePersonalPhoneUsers = 0,
        activeFreeTeamPhoneUsers = 0,
        activePaidTeamPhoneUsers = 1
      }

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  galleyClient <- initCas opts.galleyDb logger
  putStrLn "scanning users table..."
  res <- process logger opts.limit brigClient galleyClient
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
    desc = header "phone-users" <> progDesc "This program scans brig's users table and determines the number of users that can only login by phone/sms" <> fullDesc
