{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

import Brig.Data.UserKey
import Cassandra
import Cassandra.Util
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Data.Time
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import Wire.API.User.Identity

runCommand :: Logger -> ClientState -> ClientState -> ClientState -> IO ()
runCommand l brig galley spar = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient spar) getSCIMTeams)
      .| C.mapM
        ( \(i, tids) -> do
            Log.info l (Log.field "teams" (show ((i - 1) * pageSize + fromIntegral (length tids))))
            pure $ map runIdentity tids
        )
      .| C.mapM_ (mapM_ (findMismatchesForTeam l brig galley spar))

findMismatchesForTeam :: Logger -> ClientState -> ClientState -> ClientState -> TeamId -> IO ()
findMismatchesForTeam l brig galley spar tid = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) (getTeamMembers tid))
      .| C.mapM
        ( \(i, p) -> do
            Log.info l $
              Log.field "team members" (show ((i - 1) * pageSize + fromIntegral (length p)))
                . Log.field "team" (idToText tid)
            pure p
        )
      .| C.mapM_ (mapM_ (checkUser l brig spar tid))

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: TeamId -> ConduitM () [(UserId, Maybe UserId, Maybe UTCTime)] Client ()
getTeamMembers tid = paginateC cql (paramsP LocalQuorum (Identity tid) pageSize) x5
  where
    cql :: PrepQuery R (Identity TeamId) (UserId, Maybe UserId, Maybe UTCTime)
    cql = "SELECT user, invited_by, invited_at FROM team_member where team = ?"

getEmailAndExternalId :: UserId -> Client (Maybe (Maybe Email, Maybe (Writetime Email), Maybe Email, Maybe UserSSOId))
getEmailAndExternalId uid =
  retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Maybe Email, Maybe (Writetime Email), Maybe Email, Maybe UserSSOId)
    cql = "SELECT email, writetime(email), email_unvalidated, sso_id from user where id = ?"

getSCIMTeams :: ConduitM () [Identity TeamId] Client ()
getSCIMTeams = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () (Identity TeamId)
    cql = "SELECT distinct team from scim_external"

checkUser :: Logger -> ClientState -> ClientState -> TeamId -> (UserId, Maybe UserId, Maybe UTCTime) -> IO ()
checkUser l brig spar tid (uid, mInvitedBy, mInvitedAt) = do
  maybeEmails <- runClient brig $ getEmailAndExternalId uid
  case maybeEmails of
    Nothing ->
      Log.warn l (Log.msg (Log.val "No email information found") . Log.field "user" (idToText uid))
    Just (mEmail, mEmailWritetime, unvalidatedEmail, ssoId) -> do
      case mEmail of
        Nothing ->
          Log.warn l $
            Log.msg (Log.val "No email found")
              . Log.field "user" (idToText uid)
              . Log.field "team" (idToText tid)
        Just email -> do
          let emailWritetime = writeTimeToUTC <$> mEmailWritetime
          case unvalidatedEmail of
            Nothing -> pure ()
            Just ue ->
              Log.warn l $
                Log.msg (Log.val "Unvalidated email found")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "unvalidatedEmail" (show ue)
                  . Log.field "team" (idToText tid)
          case ssoId of
            Nothing ->
              Log.warn l $
                Log.msg (Log.val "No SSO Id Found")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "invited_by" (showMaybe mInvitedBy)
                  . Log.field "invited_at" (showMaybe mInvitedAt)
                  . Log.field "team" (idToText tid)
            Just (UserSSOId ref) ->
              Log.warn l $
                Log.msg (Log.val "User SSO Id is not SCIM External ID")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "userSSOIdRef" (show ref)
                  . Log.field "team" (idToText tid)
            Just (UserScimExternalId extId) -> do
              when (extId /= fromEmail email) $
                Log.warn l $
                  Log.msg (Log.val "External Id is not in sync with email")
                    . Log.field "user" (idToText uid)
                    . Log.field "email" (fromEmail email)
                    . Log.field "external id" extId
                    . Log.field "team" (idToText tid)
              mUidFromEmail <- runClient brig $ lookupKey (userEmailKey email)
              case mUidFromEmail of
                Nothing -> do
                  mTimes <- runClient spar $ readScimUserTimes uid
                  Log.warn l $
                    Log.msg (Log.val "Email missing from user_keys")
                      . Log.field "user" (idToText uid)
                      . Log.field "email" (fromEmail email)
                      . Log.field "email_write_time" (showMaybe emailWritetime)
                      . Log.field "scim_created_at" (showMaybe $ fst <$> mTimes)
                      . Log.field "scim_updated_at" (showMaybe $ snd <$> mTimes)
                      . Log.field "invited_by" (showMaybe mInvitedBy)
                      . Log.field "invited_at" (showMaybe mInvitedAt)
                      . Log.field "team" (idToText tid)
                Just uidFromEmail ->
                  when (uidFromEmail /= uid) $
                    Log.warn l $
                      Log.msg (Log.val "Wrong user found when looking up by email")
                        . Log.field "expected_user" (idToText uid)
                        . Log.field "found_user" (idToText uidFromEmail)
                        . Log.field "email" (fromEmail email)
                        . Log.field "team" (idToText tid)

-- | Read creation and last-update time from database for a given user id.
readScimUserTimes :: (HasCallStack, MonadClient m) => UserId -> m (Maybe (UTCTime, UTCTime))
readScimUserTimes uid = do
  retry x1 . query1 sel $ params LocalQuorum (Identity uid)
  where
    sel :: PrepQuery R (Identity UserId) (UTCTime, UTCTime)
    sel = "SELECT created_at, last_updated_at FROM scim_user_times WHERE uid = ?"

showMaybe :: Show a => Maybe a -> String
showMaybe (Just a) = show a
showMaybe Nothing = ""
