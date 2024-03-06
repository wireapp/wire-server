{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Test.Spar where

import API.Spar
import SetupHelpers
import Testlib.Prelude

testSparHappyUserCreation :: HasCallStack => App ()
testSparHappyUserCreation = do
  (owner, _tid, _) <- createTeam OwnDomain 1
  tok <- createScimToken owner >>= \resp -> resp.json %. "token" >>= asString
  scimUser <- randomScimUser
  bindResponse (createScimUser OwnDomain tok scimUser) $ \res -> do
    res.status `shouldMatchInt` 201
