{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.MLS.Services where

import API.Brig
import API.Common
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
  email <- randomEmail
  provider <- make <$> setupProvider owner def {newProviderEmail = email}
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
      (resp.jsonBody %. "label") `shouldMatch` Just "insufficient-permissions"

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
      (resp.jsonBody %. "label") `shouldMatch` Just "mls-services-not-allowed"

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
      (resp.jsonBody %. "label") `shouldMatch` Just "mls-services-not-allowed"
