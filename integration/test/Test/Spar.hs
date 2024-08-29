{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import API.Common (randomEmail, randomExternalId)
import API.Spar
import Control.Concurrent (threadDelay)
import SetupHelpers
import Testlib.Prelude

testSparUserCreationInvitationTimeout :: (HasCallStack) => App ()
testSparUserCreationInvitationTimeout = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

  -- Trying to create the same user again right away should fail
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 409

  -- However, if we wait until the invitation timeout has passed
  -- (assuming it is configured to 10s locally and in CI)...
  liftIO $ threadDelay (11_000_000)

  -- ...we should be able to create the user again
  retryT $ bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201

testSparExternalIdDifferentFromEmail :: (HasCallStack) => App ()
testSparExternalIdDifferentFromEmail = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  email <- randomEmail
  extId <- randomExternalId
  scimUser <- randomScimUserWith extId email
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201
