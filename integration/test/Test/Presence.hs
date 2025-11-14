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

module Test.Presence where

import API.Common
import API.Gundeck
import API.GundeckInternal
import SetupHelpers
import Testlib.Prelude

ensurePresent :: (HasCallStack, MakesValue u) => u -> Int -> App ()
ensurePresent u n = retryT $ do
  ps <- getPresence u >>= getJSON 200 >>= asList
  length ps `shouldMatchInt` n

registerUser :: (HasCallStack) => App (Value, String)
registerUser = do
  alice <- randomUserId OwnDomain
  c <- randomClientId
  withWebSocket (alice, "conn", c) $ \_ ->
    ensurePresent alice 1
  pure (alice, c)

testAddUser :: (HasCallStack) => App ()
testAddUser = void registerUser

testRemoveUser :: (HasCallStack) => App ()
testRemoveUser = do
  -- register alice and add a push token
  (alice, c) <- registerUser
  void $ generateAndPostPushToken alice c def >>= getJSON 201
  do
    t <- getPushTokens alice >>= getJSON 200
    tokens <- t %. "tokens" & asList
    length tokens `shouldMatchInt` 1

  -- push something to alice
  do
    r <- recipient alice
    let push =
          object
            [ "recipients" .= [r],
              "payload" .= [object ["foo" .= "bar"]]
            ]
    void $ postPush alice [push] >>= getBody 200

  -- unregister alice
  void $ unregisterUser alice >>= getBody 200

  -- check that the token is deleted
  do
    t <- getPushTokens alice >>= getJSON 200
    t %. "tokens" `shouldMatch` ([] :: [Value])

  -- check that notifications are deleted
  do
    ns <- getNotifications alice def {client = Just c} >>= getJSON 200
    ns %. "notifications" `shouldMatch` ([] :: [Value])
    ns %. "has_more" `shouldMatch` False
