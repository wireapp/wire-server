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
import Conduit
import Data.Conduit.Internal (zipSources)
import qualified Data.Conduit.List as C
import Data.Id
import Imports
import System.Logger (Logger)
import qualified System.Logger as Log
import Wire.API.User.Identity

runCommand :: Logger -> ClientState -> ClientState -> TeamId -> IO ()
runCommand l brig galley tid = do
  runConduit $
    zipSources
      (C.sourceList [(1 :: Int32) ..])
      (transPipe (runClient galley) (getTeamMembers tid))
      .| C.mapM
        ( \(i, p) ->
            Log.info l (Log.field "team members" (show (i * pageSize)))
              >> pure (map runIdentity p)
        )
      .| transPipe (runClient brig) (C.mapM_ (mapM_ (checkUser l)))

pageSize :: Int32
pageSize = 1000

----------------------------------------------------------------------------
-- Queries

-- | Get team members from Galley
getTeamMembers :: TeamId -> ConduitM () [Identity UserId] Client ()
getTeamMembers tid = paginateC cql (paramsP LocalQuorum (Identity tid) pageSize) x5
  where
    cql :: PrepQuery R (Identity TeamId) (Identity UserId)
    cql = "SELECT user FROM team_member where team = ?"

getEmailAndExternalId :: UserId -> Client (Maybe (Maybe Email, Maybe Email, Maybe UserSSOId))
getEmailAndExternalId uid =
  retry x1 $ query1 cql (params LocalQuorum (Identity uid))
  where
    cql :: PrepQuery R (Identity UserId) (Maybe Email, Maybe Email, Maybe UserSSOId)
    cql = "SELECT email, email_unvalidated, sso_id from user where id = ?"

checkUser :: Logger -> UserId -> Client ()
checkUser l uid = do
  maybeEmails <- getEmailAndExternalId uid
  case maybeEmails of
    Nothing ->
      Log.warn l (Log.msg (Log.val "No email information found") . Log.field "user" (idToText uid))
    Just (mEmail, unvalidatedEmail, ssoId) -> do
      case mEmail of
        Nothing ->
          Log.warn l $
            Log.msg (Log.val "No email found")
              . Log.field "user" (idToText uid)
        Just email -> do
          case unvalidatedEmail of
            Nothing -> pure ()
            Just ue ->
              Log.warn l $
                Log.msg (Log.val "Unvalidated email found")
                  . Log.field "user" (idToText uid)
                  . Log.field "email" (show email)
                  . Log.field "unvalidatedEmail" (show ue)
          case ssoId of
            Nothing ->
              Log.warn l $
                Log.msg (Log.val "No SSO Id Found")
                  . Log.field "user" (idToText uid)
            Just (UserSSOId ref) ->
              Log.warn l $
                Log.msg (Log.val "User SSO Id is not SCIM External ID")
                  . Log.field "user" (idToText uid)
                  . Log.field "userSSOIdRef" (show ref)
            Just (UserScimExternalId extId) -> do
              when (extId /= fromEmail email) $
                Log.warn l $
                  Log.msg (Log.val "External Id is not in sync with email")
                    . Log.field "user" (idToText uid)
                    . Log.field "email" (fromEmail email)
                    . Log.field "external id" extId
              mUidFromEmail <- lookupKey (userEmailKey email)
              case mUidFromEmail of
                Nothing ->
                  Log.warn l $ Log.msg (Log.val "Email missing from user_keys") . Log.field "user" (idToText uid) . Log.field "email" (fromEmail email)
                Just uidFromEmail ->
                  when (uidFromEmail /= uid) $
                    Log.warn l $
                      Log.msg (Log.val "Wrong user found when looking up by email")
                        . Log.field "expected_user" (idToText uid)
                        . Log.field "found_user" (idToText uidFromEmail)
                        . Log.field "email" (fromEmail email)
