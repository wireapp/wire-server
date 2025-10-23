{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

module Test.ChannelSearch where

import API.Galley
import qualified API.GalleyInternal as I
import MLS.Util
import SetupHelpers
import Testlib.Prelude
import Text.Printf

testChannelSearch :: App ()
testChannelSearch = do
  (alice, tid, [bob, charlie]) <- createTeam OwnDomain 3
  [alice1, bob1, charlie1] <- traverse (createMLSClient def) [alice, bob, charlie]
  dee <- randomUser OwnDomain def
  traverse_ (uploadNewKeyPackage def) [bob1, charlie1]
  I.setTeamFeatureLockStatus alice tid "channels" "unlocked"
  void
    $ I.setTeamFeatureConfig
      alice
      tid
      "channels"
      ( object
          [ "status" .= "enabled",
            "config"
              .= object
                [ "allowed_to_create_channels" .= "everyone",
                  "allowed_to_open_channels" .= "everyone"
                ]
          ]
      )

  -- unnamed channel
  unnamed <-
    postConversation
      alice1
      defMLS
        { groupConvType = Just "channel",
          team = Just tid
        }
      >>= getJSON 201
  void $ do
    convId <- objConvId unnamed
    createGroup def alice1 convId
    let update = ["access" .= (["link"] :: [String]), "access_role" .= ["team_member"]]
    void
      $ createAddCommit alice1 convId [bob, charlie]
      >>= sendAndConsumeCommitBundle
    void $ updateAccess alice convId update >>= getJSON 200

  -- named channels
  named <- for [0 :: Int .. 20] $ \i ->
    postConversation
      alice1
      defMLS
        { groupConvType = Just "channel",
          team = Just tid,
          name = Just ("foo" <> printf "%02d" i)
        }
      >>= getJSON 201

  -- regular group conversations (should be ignored)
  void $ postConversation alice1 defProteus {team = Just tid} >>= getJSON 201

  -- search channels, default parameters
  bindResponse (searchChannels alice tid def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    results <- resp.json %. "page" & asList
    length results `shouldMatchInt` 15

    results %. "0.id" `shouldMatch` (unnamed %. "qualified_id.id")
    results %. "0.member_count" `shouldMatchInt` 3
    results %. "0.admin_count" `shouldMatchInt` 1
    results %. "0.access" `shouldMatch` ["link"]
    lookupField results "0.name" `shouldMatch` (Nothing :: Maybe Value)

    results %. "1.id" `shouldMatch` (last named %. "qualified_id.id")
    results %. "1.name" `shouldMatch` "foo20"
    results %. "1.member_count" `shouldMatchInt` 1
    results %. "1.admin_count" `shouldMatchInt` 1
    results %. "1.access" `shouldMatch` ["invite"]

  -- smaller page, ascending
  (lastName, lastId) <- bindResponse
    ( searchChannels
        alice
        tid
        def
          { sortOrder = Just "asc",
            pageSize = Just 5
          }
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      results <- resp.json %. "page" & asList
      length results `shouldMatchInt` 5
      for_ (zip results named) $ \(actual, expected) ->
        actual %. "id" `shouldMatch` (expected %. "qualified_id.id")
      lastName <- last results %. "name" & asString
      lastId <- last results %. "id" & asString
      pure (lastName, lastId)

  -- next page
  bindResponse
    ( searchChannels
        alice
        tid
        def
          { sortOrder = Just "asc",
            pageSize = Just 5,
            lastName = Just lastName,
            lastId = Just lastId
          }
    )
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      results <- resp.json %. "page" & asList
      length results `shouldMatchInt` 5
      for_ (zip results (drop 5 named)) $ \(actual, expected) ->
        actual %. "id" `shouldMatch` (expected %. "qualified_id.id")

  -- public channels
  bindResponse (searchChannels dee tid def {discoverable = True})
    $ \resp -> do
      resp.status `shouldMatchInt` 200
      results <- resp.json %. "page" & asList
      length results `shouldMatchInt` 1
      head results %. "id" `shouldMatch` (unnamed %. "qualified_id.id")
  bindResponse (searchChannels dee tid def {discoverable = False}) $ \resp -> do
    resp.status `shouldMatchInt` 403
