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
import Testlib.Certs
import Testlib.MockIntegrationService
import Testlib.Prelude
import UnliftIO

testBotRootCA :: App ()
testBotRootCA = do
  (cert, privkey, pubkey) <- bundleToTriple <$> createRootCA def {caName = "Example"}
  let settings = MkMockServerSettings cert privkey pubkey
  withBotWithSettings settings \resp' -> withResponse resp' \resp -> do
    resp.status `shouldMatchInt` 502
    resp.json %. "label" `shouldMatch` "bad-gateway"
    resp.json %. "message" `shouldMatch` "The upstream service returned an invalid response: PinFingerprintMismatch"

testBotIntermCA :: App ()
testBotIntermCA = do
  (cert, privkey, pubkey) <-
    bundleToTriple <$> do
      createRootCA def {caName = "Where'sDecrypt"}
        >>= intermediateCA def {caName = "Hoogle"}
  let settings = MkMockServerSettings cert privkey pubkey
  withBotWithSettings settings \resp' -> withResponse resp' \resp -> do
    resp.status `shouldMatchInt` 502
    resp.json %. "label" `shouldMatch` "bad-gateway"
    resp.json %. "message" `shouldMatch` "The upstream service returned an invalid response: PinFingerprintMismatch"

testBotLeafCert :: App ()
testBotLeafCert = do
  (cert, privkey, pubkey) <-
    bundleToTriple <$> do
      createRootCA def {caName = "Where'sDecrypt"}
        >>= intermediateCA def {caName = "Hoogle"}
        >>= leafCert "Kabel"

  let settings = MkMockServerSettings cert privkey pubkey
  withBotWithSettings settings \resp' -> withResponse resp' \resp -> do
    resp.status `shouldMatchInt` 502
    resp.json %. "label" `shouldMatch` "bad-gateway"
    resp.json %. "message" `shouldMatch` "The upstream service returned an invalid response: PinFingerprintMismatch"

testBotSelfSigned :: App ()
testBotSelfSigned = do
  withBotWithSettings def \resp' -> do
    resp <- withResponse resp' \resp -> do
      resp.status `shouldMatchInt` 201
      pure resp

    -- If self signed, we should be able to exchange messages
    -- with the bot conversation.
    botClient <- resp.json %. "client"
    botId <- resp.json %. "id"
    aliceQid <- resp.json %. "event.qualified_from"
    conv <- resp.json %. "event.qualified_conversation"

    aliceC <- getJSON 201 =<< addClient aliceQid def
    aliceCid <- objId aliceC

    msg <-
      mkProteusRecipients
        aliceQid
        [(botId, [botClient])]
        "hi bot"
    let aliceBotMessage =
          Proto.defMessage @Proto.QualifiedNewOtrMessage
            & #sender . Proto.client .~ (aliceCid ^?! hex)
            & #recipients .~ [msg]
            & #reportAll .~ Proto.defMessage
    assertStatus 201
      =<< postProteusMessage aliceQid conv aliceBotMessage

withBotWithSettings ::
  MockServerSettings ->
  (Response -> App ()) ->
  App ()
withBotWithSettings settings k = do
  alice <- randomUser OwnDomain def

  withMockServer settings mkBotService \(host, port) _chan -> do
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
    addBot alice providerId serviceId convId >>= k

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
