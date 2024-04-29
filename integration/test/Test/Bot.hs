{-# OPTIONS_GHC -Wwarn #-}

module Test.Bot where

import API.Brig
import API.Common
import API.Galley
import SetupHelpers
import Testlib.MockIntegrationService
import Testlib.Prelude

testBot :: App ()
testBot = do
  alice <- randomUser OwnDomain def
  assertSuccess =<< addClient alice def
  let mkBotService _ = undefined
  withMockServer (mkBotService) $ \(host, port) _chan -> do
    email <- randomEmail
    provider <- setupProvider alice def {newProviderEmail = email}
    providerId <- provider %. "id" & asString
    service <-
      newService OwnDomain providerId $
        def {newServiceUrl = "https://" <> host <> ":" <> show port}
    serviceId <- asString $ service %. "id"
    convId <-
      postConversation alice defProteus `bindResponse` \res -> do
        res.status `shouldMatchInt` 201
        asString $ res.json %. "id"
    assertSuccess =<< addBot alice providerId serviceId convId
