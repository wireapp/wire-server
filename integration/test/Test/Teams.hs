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

module Test.Teams where

import API.Brig
import API.BrigInternal (createUser, getInvitationCode, refreshIndex)
import API.Galley (getTeamMembers)
import API.GalleyInternal (setTeamFeatureStatus)
import Control.Monad.Codensity (Codensity (runCodensity))
import Control.Monad.Reader (asks)
import Notifications (isUserUpdatedNotif)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude
import Testlib.ResourcePool (acquireResources)

testInvitePersonalUserToTeam :: (HasCallStack) => App ()
testInvitePersonalUserToTeam = do
  resourcePool <- asks (.resourcePool)
  runCodensity (acquireResources 1 resourcePool) $ \[testBackend] -> do
    let domain = testBackend.berDomain
    (owner, tid, tm) <- runCodensity (startDynamicBackend testBackend def) $ \_ -> do
      (owner, tid, tm : _) <- createTeam domain 2
      pure (owner, tid, tm)

    runCodensity
      ( startDynamicBackend
          testBackend
          (def {galleyCfg = setField "settings.exposeInvitationURLsTeamAllowlist" [tid]})
      )
      $ \_ -> do
        ownerId <- owner %. "id" & asString
        setTeamFeatureStatus domain tid "exposeInvitationURLsToTeamAdmin" "enabled" >>= assertSuccess
        user <- createUser domain def >>= getJSON 201
        uid <- user %. "id" >>= asString
        email <- user %. "email" >>= asString
        inv <- postInvitation owner (PostInvitation $ Just email) >>= getJSON 201
        code <- getInvitationCode owner inv >>= getJSON 200 >>= (%. "code") & asString
        queryParam <- inv %. "url" & asString <&> getQueryParam "team_code"
        queryParam `shouldMatch` Just (Just code)
        void $ withWebSockets [user] $ \wss -> do
          acceptTeamInvitation user code >>= assertSuccess
          for wss $ \ws -> do
            n <- awaitMatch isUserUpdatedNotif ws
            n %. "payload.0.user.team" `shouldMatch` tid
        bindResponse (getSelf user) $ \resp -> do
          resp.status `shouldMatchInt` 200
          resp.json %. "team" `shouldMatch` tid
        -- a team member can now find the former personal user in the team
        bindResponse (getTeamMembers tm tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          members <- resp.json %. "members" >>= asList
          ids <- for members ((%. "user") >=> asString)
          ids `shouldContain` [uid]
        -- the former personal user can now see other team members
        bindResponse (getTeamMembers user tid) $ \resp -> do
          resp.status `shouldMatchInt` 200
          members <- resp.json %. "members" >>= asList
          ids <- for members ((%. "user") >=> asString)
          tmId <- tm %. "id" & asString
          ids `shouldContain` [ownerId]
          ids `shouldContain` [tmId]
        -- the former personal user can now search for the owner
        bindResponse (searchContacts user (owner %. "name") domain) $ \resp -> do
          resp.status `shouldMatchInt` 200
          documents <- resp.json %. "documents" >>= asList
          ids <- for documents ((%. "id") >=> asString)
          ids `shouldContain` [ownerId]
        refreshIndex domain
        -- a team member can now search for the former personal user
        bindResponse (searchContacts tm (user %. "name") domain) $ \resp -> do
          resp.status `shouldMatchInt` 200
          document <- resp.json %. "documents" >>= asList >>= assertOne
          document %. "id" `shouldMatch` uid
          document %. "team" `shouldMatch` tid
