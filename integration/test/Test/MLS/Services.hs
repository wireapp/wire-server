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

module Test.MLS.Services where

import API.Brig
import API.GalleyInternal (patchTeamFeatureConfig)
import SetupHelpers
import Testlib.JSON
import Testlib.Prelude

testWhitelistUpdatePermissions :: (HasCallStack) => App ()
testWhitelistUpdatePermissions = do
  -- Create a team
  (owner, tid, []) <- createTeam OwnDomain 1

  -- Create a team admin
  admin <- createTeamMember owner def {role = "admin"}

  -- Create a service
  provider <- make <$> setupProvider owner def
  providerId <- provider %. "id" & asString
  service <- make <$> newService OwnDomain providerId def

  do
    -- Check that a random user can't add the service to the whitelist
    uid <- randomUser OwnDomain def
    serviceId <- service %. "id" & asString
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    bindResponse (postServiceWhitelist uid tid np) $ \resp -> do
      resp.status `shouldMatchInt` 403
      (resp.json %. "label") `shouldMatch` Just "insufficient-permissions"

  do
    -- Check that an admin can add the service to the whitelist
    serviceId <- service %. "id" & asString
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    postServiceWhitelist admin tid np >>= assertStatus 200

  -- set team's defaultProtocol to MLS
  mlsConfig <-
    make
      $ object
        [ "config"
            .= object
              [ "allowedCipherSuites" .= [1 :: Int],
                "defaultCipherSuite" .= (1 :: Int),
                "defaultProtocol" .= "mls",
                "protocolToggleUsers" .= ([] :: [String]),
                "supportedProtocols" .= ["mls", "proteus"]
              ],
          "status" .= "enabled",
          "ttl" .= "unlimited"
        ]
  patchTeamFeatureConfig OwnDomain tid "mls" mlsConfig >>= assertStatus 200

  do
    -- Check that a random user can't add the service to the whitelist
    uid <- randomUser OwnDomain def
    serviceId <- service %. "id" & asString
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    bindResponse (postServiceWhitelist uid tid np) $ \resp -> do
      resp.status `shouldMatchInt` 409
      (resp.json %. "label") `shouldMatch` Just "mls-services-not-allowed"

  do
    -- Check that an admin can't add the service to the whitelist
    serviceId <- service %. "id" & asString
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    postServiceWhitelist admin tid np >>= \resp -> do
      resp.status `shouldMatchInt` 409
      (resp.json %. "label") `shouldMatch` Just "mls-services-not-allowed"

-- | Removing a service from an MLS team should succeed even though adding is blocked.
testRemoveServiceFromMLSTeam :: (HasCallStack) => App ()
testRemoveServiceFromMLSTeam = do
  -- Create a team (default protocol is proteus)
  (owner, tid, []) <- createTeam OwnDomain 1

  -- Create a service
  provider <- make <$> setupProvider owner def
  providerId <- provider %. "id" & asString
  service <- make <$> newService OwnDomain providerId def
  serviceId <- service %. "id" & asString

  -- Whitelist the service while the team is still on proteus
  do
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    postServiceWhitelist owner tid np >>= assertStatus 200

  -- Upgrade the team to MLS
  mlsConfig <-
    make
      $ object
        [ "config"
            .= object
              [ "allowedCipherSuites" .= [1 :: Int],
                "defaultCipherSuite" .= (1 :: Int),
                "defaultProtocol" .= "mls",
                "protocolToggleUsers" .= ([] :: [String]),
                "supportedProtocols" .= ["mls", "proteus"]
              ],
          "status" .= "enabled",
          "ttl" .= "unlimited"
        ]
  patchTeamFeatureConfig OwnDomain tid "mls" mlsConfig >>= assertStatus 200

  -- Adding a NEW service on an MLS team should be blocked
  do
    -- Service is already whitelisted, so we need a fresh service to test the add path
    service2 <- make <$> newService OwnDomain providerId def
    serviceId2 <- service2 %. "id" & asString
    np2 <-
      make
        $ object
          [ "id" .= serviceId2,
            "provider" .= providerId,
            "whitelisted" .= True
          ]
    postServiceWhitelist owner tid np2 >>= \resp -> do
      resp.status `shouldMatchInt` 409
      (resp.json %. "label") `shouldMatch` Just "mls-services-not-allowed"

  -- Removing the already-whitelisted service from the MLS team should succeed
  do
    np <-
      make
        $ object
          [ "id" .= serviceId,
            "provider" .= providerId,
            "whitelisted" .= False
          ]
    postServiceWhitelist owner tid np >>= assertStatus 200
