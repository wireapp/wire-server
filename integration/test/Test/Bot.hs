{-# OPTIONS_GHC -Wwarn #-}

module Test.Bot where

import API.Brig
import API.Common
import API.Galley
import Data.String.Conversions (cs)
import Network.HTTP.Types (status200, status201)
import Network.Wai (responseLBS)
import qualified Network.Wai as Wai
import qualified Network.Wai.Route as Wai
import SetupHelpers
import Testlib.MockIntegrationService
import Testlib.Prelude
import UnliftIO

testBot :: App ()
testBot = do
  alice <- randomUser OwnDomain def
  assertSuccess =<< addClient alice def
  withMockServer mkBotService \(host, port) _chan -> do
    email <- randomEmail
    provider <- setupProvider alice def {newProviderEmail = email, newProviderPassword = Just defPassword}
    providerId <- provider %. "id" & asString
    service <-
      newService OwnDomain providerId $
        def {newServiceUrl = "https://" <> host <> ":" <> show port}
    serviceId <- asString $ service %. "id"
    convId <-
      postConversation alice defProteus `bindResponse` \res -> do
        res.status `shouldMatchInt` 201
        asString $ res.json %. "id"
    assertStatus 200 =<< updateServiceConn providerId serviceId do
      object ["enabled" .= True, "password" .= defPassword]
    assertSuccess =<< addBot alice providerId serviceId convId

data BotEvent
  = BotCreated
  | BotMessage String
  deriving stock (Eq, Ord, Show)

mkBotService :: Chan BotEvent -> LiftedApplication
mkBotService _chan =
  Wai.route
    [ (cs "/bots", onBotCreate),
      (cs "/bots/:bot/messages", onBotMessage),
      (cs "/alive", onBotAlive)
    ]

onBotCreate,
  onBotMessage,
  onBotAlive ::
    [(ByteString, ByteString)] ->
    Wai.Request ->
    (Wai.Response -> App Wai.ResponseReceived) ->
    App Wai.ResponseReceived
onBotCreate _ req k = do
  print req
  k (responseLBS status201 mempty mempty)
onBotMessage _ req k = do
  print req
  k (responseLBS status200 mempty mempty)
onBotAlive _ _req k = do
  k (responseLBS status200 mempty (cs "success"))
