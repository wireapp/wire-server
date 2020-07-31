{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Network.Wire.Simulations.SmokeTest
  ( mainBotNet,
  )
where

import qualified Codec.MIME.Type as MIME
import qualified Data.ByteString.Lazy as LBS
import Data.Id (ConvId, makeIdOpaque)
import Data.List1
import Imports
import Network.Wire.Bot
import Network.Wire.Bot.Assert
import qualified Network.Wire.Bot.Clients as Clients
import Network.Wire.Bot.Crypto
import Network.Wire.Client.API.Asset
import Network.Wire.Client.API.Client (ClientType (PermanentClientType))
import Network.Wire.Client.API.Conversation
import Network.Wire.Client.API.Push
import Network.Wire.Client.API.Search
import Network.Wire.Client.API.User
import Network.Wire.Simulations
import System.Logger.Class
import UnliftIO (mapConcurrently)

default (ByteString)

mainBotNet ::
  -- | How many participants to create
  Int ->
  BotNet ()
mainBotNet n | n < 3 = error "mainBotNet: need at least 3 participants"
mainBotNet n = do
  info $ msg $ "Starting Smoke Test with " <> show n <> " bots"
  -- Create some participants: Ally, Bill, Carl, and a bunch of goons
  [ally, bill, carl] <- mapM newBot ["Ally", "Bill", "Carl"]
  goons <- for [1 .. n -3] $ \i ->
    newBot (fromString ("Goon" <> show i))
  -- Set up a connection from Ally to someone
  let allyConnectTo :: Bot -> BotSession ConvId
      allyConnectTo user = do
        conn <-
          connectTo
            ConnectionRequest
              { crUser = makeIdOpaque (botId user),
                crName = fromMaybe "" (botEmail ally),
                crMessage = Message "Hi there!"
              }
        assertConnectRequested ally user
        requireMaybe (ucConvId conn) "conv_id not set after connection request"
  info $ msg "Setting up connections between Ally and the rest of the gang"
  (a2b, a2c, a2goons) <- runBotSession ally $ do
    a2b <- allyConnectTo bill
    a2c <- allyConnectTo carl
    -- NB. this might break if the backend implements a limit on the
    -- number of un-accepted connections a user can have; in this case
    -- the test would have to be rewritten slightly
    a2goons <- mapM allyConnectTo goons
    return (a2b, a2c, a2goons)
  -- Accept a connection request from Ally
  let allyAccept :: Bot -> BotNet ()
      allyAccept user = runBotSession user $ do
        void $ updateConnection (botId ally) (ConnectionUpdate Accepted)
        assertConnectAccepted user ally
  mapM_ allyAccept (bill : carl : goons)
  mapM_ awaitAssertions (ally : bill : carl : goons)
  info $ msg "Creating a group conversation ('Meetup') with everyone"
  meetup <- runBotSession ally $ do
    let others = bill : carl : goons
    conv <- cnvId <$> createConv (map botId others) (Just "Meetup")
    assertConvCreated conv ally others
    return conv
  info $ msg "Bill updates his member state"
  runBotSession bill $ do
    let update =
          MemberUpdateData
            { misTarget = Just $ botId bill,
              misOtrMuted = Nothing,
              misOtrMutedStatus = Nothing,
              misOtrMutedRef = Nothing,
              misOtrArchived = Just True,
              misOtrArchivedRef = Nothing,
              misHidden = Nothing,
              misHiddenRef = Nothing,
              misConvRoleName = Nothing
            }
    memberUpdate meetup update
    c <- getConv meetup
    assertEqual
      (Just True)
      ((memOtrArchived . cmSelf . cnvMembers) <$> c)
      "Archived update failed"
  info $ msg "Bill kicks and then re-adds Ally"
  runBotSession bill $ do
    removeMember meetup (botId ally) >>= assertMembersLeft (ally : carl : goons)
    addMembers meetup (singleton (botId ally)) >>= assertMembersJoined (ally : carl : goons)
  mapM_ awaitAssertions (ally : bill : carl : goons)
  info $ msg "Basic search reachability"
  _ <- runBotSession ally $ search (SearchParams "whatever" 3 10 True)
  info $ msg "Registering clients for everyone"
  allyPhone <- addBotClient ally PermanentClientType (Just "iPhone")
  billPC <- addBotClient bill PermanentClientType (Just "Linux PC")
  carlTablet <- addBotClient carl PermanentClientType (Just "Android tablet")
  goonClients <- for goons $ \goon ->
    addBotClient goon PermanentClientType (Just "Calculator")
  let allyWithPhone = (ally, allyPhone)
  let billWithPC = (bill, billPC)
  let carlWithTablet = (carl, carlTablet)
  let people :: [(Bot, ConvId, BotClient)] -- everyone except for Ally
      people =
        (bill, a2b, billPC) :
        (carl, a2c, carlTablet) :
        zip3 goons a2goons goonClients
  info $ msg (val "OTR 1-1 greetings")
  -- Ally greets everyone in 1-1
  runBotSession ally $
    for_ people $ \(user, conv, _client) -> do
      botInitSession (botId user)
      Clients.addMembers (botClientSessions allyPhone) conv [botId user]
      let message = "Hey " <> unTag (botTag user) <> ", Everything secure?"
      postOtrTextMsg allyPhone conv message >>= assertNoClientMismatch
  -- Everyone answers
  for_ people $ \(user, conv, client) -> runBotSession user $ do
    pkm <- awaitOtrMsg conv allyWithPhone (user, client)
    plain <- initSessionFromMsg client pkm >>= requireTextMsg
    assertEqual
      plain
      ("Hey " <> unTag (botTag user) <> ", Everything secure?")
      (unTag (botTag user) <> ": Plaintext /= CipherText")
    Clients.addMembers (botClientSessions client) conv [botId ally]
    postOtrTextMsg client conv "Thanks Ally, All good." >>= assertNoClientMismatch
  -- Ally confirms the answers
  runBotSession ally $
    for_ people $ \(user, conv, client) -> do
      message <- awaitOtrMsg conv (user, client) allyWithPhone
      plain <- decryptTextMsg allyPhone message
      assertEqual
        plain
        "Thanks Ally, All good."
        ("Ally (from " <> unTag (botTag user) <> "): Plaintext /= CipherText")
      postOtrTextMsg allyPhone conv "Glad to hear that." >>= assertNoClientMismatch
  -- Everyone checks Ally's response
  for_ people $ \(user, conv, client) -> runBotSession user $ do
    message <- awaitOtrMsg conv allyWithPhone (user, client)
    plain <- decryptTextMsg client message
    assertEqual
      plain
      "Glad to hear that."
      (unTag (botTag user) <> " (from Ally): Plaintext /= CipherText")
  info $ msg "OTR group conversation"
  -- Ally posts an asset in the group conversation
  runBotSession ally $ do
    Clients.addMembers
      (botClientSessions allyPhone)
      meetup
      (map botId (bill : carl : goons))
    keys <- randomSymmetricKeys allyPhone
    assetData <- encryptSymmetric allyPhone keys "secret data"
    let mimeType = MIME.Type (MIME.Application "octet-stream") []
    asset <- postAsset mimeType defAssetSettings (LBS.fromStrict assetData)
    let assetMsg = encode (mkAssetMsg asset keys)
    postOtrMsg allyPhone meetup assetMsg >>= assertNoClientMismatch
  -- Everyone receives the asset
  for_ people $ \(user, _conv, client) -> runBotSession user $ do
    message <- awaitOtrMsg meetup allyWithPhone (user, client)
    asInfo <- decryptMessage client message >>= requireAssetMsg
    assetData <- getAsset (assetInfoKey asInfo) (assetInfoToken asInfo)
    plainData <- for assetData (decryptSymmetric client (assetInfoKeys asInfo) . LBS.toStrict)
    assertEqual plainData (Just "secret data") "OTR asset data mismatch"
  -- Bill writes a reply
  runBotSession bill $ do
    -- We already have a session with Ally, but not with the rest of the gang
    mapM_ (botInitSession . botId) (carl : goons)
    Clients.addMembers (botClientSessions billPC) meetup (map botId (ally : carl : goons))
    postOtrTextMsg billPC meetup "Wow!" >>= assertNoClientMismatch
  -- Carl receives Bill's reply (first message => initialise session)
  runBotSession carl $ do
    message <- awaitOtrMsg meetup billWithPC carlWithTablet
    plain <- initSessionFromMsg carlTablet message >>= requireTextMsg
    assertEqual plain "Wow!" "Carl: Plaintext /= CipherText"
  -- Ally receives Bill's reply
  runBotSession ally $ do
    message <- awaitOtrMsg meetup billWithPC allyWithPhone
    plain <- decryptTextMsg allyPhone message
    assertEqual plain "Wow!" "Ally: Plaintext /= CipherText"
  mapM_ awaitAssertions (ally : bill : carl : goons)
  info $ msg "Bill gets a new phone"
  billPhone <- addBotClient bill PermanentClientType (Just "Linux phone")
  let billWithPhone = (bill, billPhone)
  -- Bill writes Ally from his new phone
  runBotSession bill $ do
    clientInitSession billPhone (botId bill)
    Clients.addMembers (botClientSessions billPhone) a2b [botId ally, botId bill]
    postOtrTextMsg billPhone a2b "Hey Ally, I've got a new phone!" >>= assertClientMissing (botId ally) allyPhone
    clientInitSession billPhone (botId ally)
    postOtrTextMsg billPhone a2b "Hey Ally, I've got a new phone!" >>= assertNoClientMismatch
  -- TODO: Check that it arrives on Bill's first client as well.
  --       Right now that will not happen because we use a single websocket
  --       per user and sending and receiving happens with the same auth token
  --       (and thus the same connection ID).
  -- cipher <- awaitOtrMsg a2b billWithPhone billWithPC
  -- plain  <- decryptMessage billPC cipher
  -- assertEqual plain "Hey Ally, I've got a new phone!" "BillPC: Plaintext /= CipherText"

  -- Ally answers
  runBotSession ally $ do
    message <- awaitOtrMsg a2b billWithPhone allyWithPhone
    sess <- Clients.lookupSession (botClientSessions allyPhone) (convEvtFrom message) (otrSender $ convEvtData message)
    assertTrue (isNothing sess) "Surprisingly, Ally knows Bill's new phone already"
    plain <- initSessionFromMsg allyPhone message >>= requireTextMsg
    assertEqual plain "Hey Ally, I've got a new phone!" "Ally: Plaintext /= CipherText"
    postOtrTextMsg allyPhone a2b "That's nice, Bill" >>= assertNoClientMismatch
  -- Bill receives Ally's answer on both clients
  runBotSession bill $ do
    plain1 <- awaitOtrMsg a2b allyWithPhone billWithPhone >>= decryptTextMsg billPhone
    plain2 <- awaitOtrMsg a2b allyWithPhone billWithPC >>= decryptTextMsg billPC
    assertEqual plain1 "That's nice, Bill" "Bill: Plaintext /= CipherText"
    assertEqual plain2 "That's nice, Bill" "Bill: Plaintext /= CipherText"
  info $ msg "Waiting for event & assertion timeouts (if any)"
  void $ mapConcurrently drainBot (ally : bill : carl : goons)

postOtrTextMsg :: BotClient -> ConvId -> Text -> BotSession ClientMismatch
postOtrTextMsg cl cnv m = postOtrMsg cl cnv (encode (BotTextMessage m))

postOtrMsg :: BotClient -> ConvId -> ByteString -> BotSession ClientMismatch
postOtrMsg cl cnv m = encryptMessage cl cnv m >>= postOtrMessage cnv

awaitOtrMsg :: ConvId -> (Bot, BotClient) -> (Bot, BotClient) -> BotSession (ConvEvent OtrMessage)
awaitOtrMsg cnv from to = do
  m <- awaitOtrMessage cnv from to
  requireMaybe m "Missing OTR message"

decryptTextMsg :: BotClient -> ConvEvent OtrMessage -> BotSession Text
decryptTextMsg cl bs = decryptMessage cl bs >>= requireTextMsg
