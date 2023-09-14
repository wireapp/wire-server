module Test.Defederation where

import API.BrigInternal
import GHC.Stack
import SetupHelpers
import Testlib.Prelude

testDefederationRemoteNotifications :: HasCallStack => App ()
testDefederationRemoteNotifications = do
  let remoteDomain = "example.example.com"
  -- Setup federation between OtherDomain and the remote domain
  bindResponse (createFedConn OtherDomain $ object ["domain" .= remoteDomain, "search_policy" .= "full_search"]) $ \resp ->
    resp.status `shouldMatchInt` 200

  -- Setup a remote user we can get notifications for.
  user <- randomUser OtherDomain def

  withWebSocket user $ \ws -> do
    -- Defederate from a domain that doesn't exist. This won't do anything to the databases
    -- But it will send out notifications that we can wait on.
    -- Begin the whole process at Brig, the same as an operator would.
    void $ deleteFedConn OwnDomain remoteDomain
    void $ awaitNMatches 2 3 (\n -> nPayload n %. "type" `isEqual` "federation.connectionRemoved") ws
