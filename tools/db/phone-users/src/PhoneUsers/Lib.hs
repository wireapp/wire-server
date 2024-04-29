{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Aeson as A
import Data.ByteString.Conversion
import Data.Conduit
import qualified Data.Conduit.Combinators as Conduit
import Data.Id (TeamId, UserId)
import Data.Time
import Imports
import Network.HTTP.Client
import Network.HTTP.Types.Status
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

process :: Manager -> Maybe Int -> ClientState -> String -> Int -> IO Result
process mgr limit client ibisHost ibisPort =
  runConduit $
    readUsers client
      .| Conduit.mapM (\chunk -> SIO.hPutStr stderr "." $> chunk)
      .| Conduit.concat
      .| (maybe (Conduit.filter (const True)) Conduit.take limit)
      .| Conduit.mapM (getUserInfo mgr client ibisHost ibisPort)
      .| Conduit.foldMap infoToResult

getUserInfo :: Manager -> ClientState -> String -> Int -> UserRow -> IO UserInfo
getUserInfo mgr client ibisHost ibisPort ur = do
  if not $ isCandidate ur
    then pure NoPhoneUser
    else do
      let (uid, _, _, _, _, _, _, mTid, _) = ur
      -- should we give C* a little break here and add a small threadDelay?
      threadDelay 200
      lastActiveTimeStamps <- lookupClients client uid
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
            case mTid of
              Nothing -> pure ActivePersonalUser
              Just tid -> do
                mBillingInfo <- getTeamBillingInfo mgr ibisHost ibisPort tid
                pure $ case mBillingInfo of
                  Nothing -> ActiveTeamUser Free
                  Just _ -> ActiveTeamUser Paid
          else pure InactiveLast90Days
      SIO.hPrint stderr ur >> SIO.hPrint stderr lastActiveTimeStamps >> SIO.hPrint stderr userInfo
      pure $ PhoneUser userInfo
  where
    -- to qualify as an active phone user candidate, their account must be active and they must have a phone number but no verified email
    isCandidate :: UserRow -> Bool
    isCandidate (_, mEmail, mPhone, _, active, _, _, _, _) =
      active && isJust mPhone && isNothing mEmail

    clientWasActiveLast90Days :: UTCTime -> UTCTime -> Bool
    clientWasActiveLast90Days now lastActive = diffUTCTime now lastActive < 90 * nominalDay

getTeamBillingInfo :: Manager -> String -> Int -> TeamId -> IO (Maybe TeamBillingInfo)
getTeamBillingInfo mgr ibisHost ibisPort tid = do
  initRequest <- parseRequest ibisHost
  let req = initRequest {port = ibisPort, path = "/i/team/" <> toByteString' tid <> "/billing"}
  response <- httpLbs req mgr
  let status = statusCode $ responseStatus response
  case status of
    200 -> do
      case A.eitherDecode $ responseBody response of
        Right bi -> pure $ Just bi
        Left e -> do
          SIO.hPutStrLn stderr $ "Failed to decode response body: " <> e
          pure Nothing
    404 -> pure Nothing
    _ -> do
      SIO.hPutStrLn stderr $ "Unexpected status code: " <> show status
      pure Nothing

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
  opts <- execParser (info (helper <*> sampleParser) desc)
  logger <- initLogger
  client <- initCas opts logger
  mgr <- newManager defaultManagerSettings
  putStrLn "scanning users table..."
  res <- process mgr opts.limit client opts.ibisHost opts.ibisPort
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
