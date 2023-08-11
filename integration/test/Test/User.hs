module Test.User where

import API.Brig
import API.BrigInternal
import SetupHelpers
import Testlib.Prelude

testSupportedProtocols :: HasCallStack => Domain -> App ()
testSupportedProtocols bobDomain = do
  alice <- randomUser OwnDomain def
  alice %. "supported_protocols" `shouldMatchSet` ["proteus"]

  bob <- randomUser bobDomain def

  do
    -- bob sees default supported protocols for alice
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatch` ["proteus"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus"]

  -- alice updates her supported protocols
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls"]) $ \resp ->
    resp.status `shouldMatchInt` 200

  do
    -- bob sees the updated list
    u <- getUser bob alice >>= getJSON 200
    u %. "supported_protocols" `shouldMatchSet` ["proteus", "mls"]

    p <- getUserSupportedProtocols bob alice >>= getJSON 200
    p `shouldMatch` ["proteus", "mls"]

  -- invalid protocol name in update
  bindResponse (putUserSupportedProtocols alice ["proteus", "mls", "mixed"]) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"

testCreateUserSupportedProtocols :: HasCallStack => App ()
testCreateUserSupportedProtocols = do
  alice <- randomUser OwnDomain def {supportedProtocols = Just ["proteus", "mls"]}
  bindResponse (getUserSupportedProtocols alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` ["proteus", "mls"]

  bindResponse (createUser OwnDomain def {supportedProtocols = Just ["proteus", "mixed"]}) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "bad-request"
