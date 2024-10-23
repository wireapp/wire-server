{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.TeamSettings where

import API.Galley
import SetupHelpers
import Testlib.Prelude

testTeamSettingsUpdate :: (HasCallStack) => App ()
testTeamSettingsUpdate = do
  (ownerA, tidA, [mem]) <- createTeam OwnDomain 2
  partner <- createTeamMember ownerA def {role = "partner"}

  bindResponse (putAppLockSettings tidA ownerA def) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (putAppLockSettings tidA mem def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"
  bindResponse (putAppLockSettings tidA partner def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

  (ownerB, _tidB, []) <- createTeam OwnDomain 1
  bindResponse (putAppLockSettings tidA ownerB def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"

testTeamPropertiesUpdate :: (HasCallStack) => App ()
testTeamPropertiesUpdate = do
  (ownerA, tidA, [mem]) <- createTeam OwnDomain 2
  partner <- createTeamMember ownerA def {role = "partner"}

  bindResponse (putTeamProperties tidA ownerA def) $ \resp -> do
    resp.status `shouldMatchInt` 200
  bindResponse (putTeamProperties tidA mem def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"
  bindResponse (putTeamProperties tidA partner def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "operation-denied"

  (ownerB, _tidB, []) <- createTeam OwnDomain 1
  bindResponse (putTeamProperties tidA ownerB def) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "no-team-member"
