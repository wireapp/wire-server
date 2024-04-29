{-# OPTIONS_GHC -Wwarn #-}

module Test.Bot where

import API.Brig
import API.Common
import API.Galley
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.ProtoLens as Proto
import Data.String.Conversions (cs)
import Network.HTTP.Types (status200, status201)
import Network.Wai (responseLBS)
import qualified Network.Wai as Wai
import qualified Network.Wai.Route as Wai
import Numeric.Lens (hex)
import qualified Proto.Otr as Proto
import qualified Proto.Otr_Fields as Proto
import SetupHelpers
import Testlib.MockIntegrationService
import Testlib.Prelude
import UnliftIO

testBot :: App ()
testBot = do
  alice <- randomUser OwnDomain def
  alicec <- getJSON 201 =<< addClient alice def
  withMockServer mkBotService \(host, port) _chan -> do
    email <- randomEmail
    provider <- setupProvider alice def {newProviderEmail = email, newProviderPassword = Just defPassword}
    providerId <- provider %. "id" & asString
    service <-
      newService OwnDomain providerId $
        def {newServiceUrl = "https://" <> host <> ":" <> show port}
    serviceId <- asString $ service %. "id"
    conv <- getJSON 201 =<< postConversation alice defProteus
    convId <- conv %. "id" & asString
    assertStatus 200 =<< updateServiceConn providerId serviceId do
      object ["enabled" .= True, "password" .= defPassword]
    resp <- getJSON 201 =<< addBot alice providerId serviceId convId

    botid <- resp %. "id"
    botcid <- resp %. "client"
    aliceid :: String <- objId alicec
    msg <-
      mkProteusRecipients
        alice
        [(botid, [botcid])]
        "hi bot"
    let aliceBotMessage =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (aliceid ^?! hex)
            & #recipients .~ [msg]
            & #reportAll .~ Proto.defMessage
    assertStatus 201
      =<< postProteusMessage alice conv aliceBotMessage

data BotEvent
  = BotCreated
  | BotMessage String
  deriving stock (Eq, Ord, Show)

mkBotService :: Chan BotEvent -> LiftedApplication
mkBotService chan =
  Wai.route
    [ (cs "/bots", onBotCreate chan),
      (cs "/bots/:bot/messages", onBotMessage chan),
      (cs "/alive", onBotAlive chan)
    ]

onBotCreate,
  onBotMessage,
  onBotAlive ::
    Chan BotEvent ->
    [(ByteString, ByteString)] ->
    Wai.Request ->
    (Wai.Response -> App Wai.ResponseReceived) ->
    App Wai.ResponseReceived
onBotCreate chan _headers _req k = do
  ((: []) -> pks) <- getPrekey
  writeChan chan BotCreated
  lpk <- getLastPrekey
  k $ responseLBS status201 mempty do
    Aeson.encode $
      object
        [ "prekeys" .= pks,
          "last_prekey" .= lpk
        ]
onBotMessage chan _headers req k = do
  body <- liftIO $ Wai.strictRequestBody req
  writeChan chan (BotMessage (cs body))
  liftIO $ putStrLn $ cs body
  k (responseLBS status200 mempty mempty)
onBotAlive _chan _headers _req k = do
  k (responseLBS status200 mempty (cs "success"))
