{-# OPTIONS -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Apps where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testCreateApp :: (HasCallStack) => App ()
testCreateApp = do
  domain <- make OwnDomain
  (owner, tid, [regularMember]) <- createTeam domain 2
  let new =
        def
          { name = "chappie",
            description = "some description of this app",
            category = "ai"
          } ::
          NewApp

  -- Regular team member can't create apps
  bindResponse (createApp regularMember tid new) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  -- Owner can create an app
  (appId, cookie) <- bindResponse (createApp owner tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

  -- app user should have type "app"
  let appIdObject = object ["domain" .= domain, "id" .= appId]
  bindResponse (getUser owner appIdObject) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "app"

  -- creator should have type "regular"
  bindResponse (getUser owner owner) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "regular"

  void $ bindResponse (renewToken domain cookie) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user" `shouldMatch` appId
    resp.json %. "token_type" `shouldMatch` "Bearer"
    resp.json %. "access_token" & asString

  -- get app for the app created above succeeds
  void $ getApp regularMember tid appId `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    (resp.json %. "name") `shouldMatch` "chappie"
    (resp.json %. "description") `shouldMatch` "some description of this app"
    (resp.json %. "category") `shouldMatch` "ai"

  -- A teamless user can't get the app
  outsideUser <- randomUser OwnDomain def
  bindResponse (getApp outsideUser tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  -- Another team's owner nor member can't get the app
  (owner2, tid2, [regularMember2]) <- createTeam domain 2
  bindResponse (getApp owner2 tid appId) $ \resp -> resp.status `shouldMatchInt` 403
  bindResponse (getApp owner2 tid2 appId) $ \resp -> resp.status `shouldMatchInt` 404
  bindResponse (getApp regularMember2 tid appId) $ \resp -> resp.status `shouldMatchInt` 403

  -- category must be any of the values for the Category enum
  void $ bindResponse (createApp owner tid new {category = "notinenum"}) $ \resp -> do
    resp.status `shouldMatchInt` 400

testRefreshAppCookie :: (HasCallStack) => App ()
testRefreshAppCookie = do
  (alice, tid, [bob]) <- createTeam OwnDomain 2
  charlie <- randomUser OwnDomain def

  let new = def {name = "flexo"} :: NewApp

  (appId, cookie) <- bindResponse (createApp alice tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

  bindResponse (refreshAppCookie bob tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  bindResponse (refreshAppCookie charlie tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "app-no-permission"

  cookie' <- bindResponse (refreshAppCookie alice tid appId) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "cookie" & asString

  for_ [cookie, cookie'] $ \c ->
    void $ bindResponse (renewToken OwnDomain c) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "user" `shouldMatch` appId
      resp.json %. "token_type" `shouldMatch` "Bearer"
      resp.json %. "access_token" & asString
