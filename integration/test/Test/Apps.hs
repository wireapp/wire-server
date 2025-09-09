{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Apps where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testCreateApp :: (HasCallStack) => App ()
testCreateApp = do
  domain <- make OwnDomain
  (alice, tid, [bob]) <- createTeam domain 2
  let new = def {name = "chappie"} :: NewApp

  bindResponse (createApp bob tid new) $ \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "create-app-no-permission"

  (appId, cookie) <- bindResponse (createApp alice tid new) $ \resp -> do
    resp.status `shouldMatchInt` 200
    appId <- resp.json %. "user.id" & asString
    cookie <- resp.json %. "cookie" & asString
    pure (appId, cookie)

  -- app user should have type "app"
  let appIdObject = object ["domain" .= domain, "id" .= appId]
  bindResponse (getUser alice appIdObject) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "app"

  -- creator should have type "regular"
  bindResponse (getUser alice alice) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "type" `shouldMatch` "regular"

  void $ bindResponse (renewToken domain cookie) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "user" `shouldMatch` appId
    resp.json %. "token_type" `shouldMatch` "Bearer"
    resp.json %. "access_token" & asString
