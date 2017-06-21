{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Network.Wire.Simulations.SmokeTest (mainBotNet) where

import Control.Concurrent.Async.Lifted
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Id
import Data.List1
import Data.Maybe (isNothing, fromMaybe)
import Data.Text (Text)
import Data.Traversable (for)
import Network.Wire.Bot
import Network.Wire.Bot.Assert
import Network.Wire.Bot.Crypto
import Network.Wire.Simulations
import Network.Wire.Client.API.Asset
import Network.Wire.Client.API.Conversation
import Network.Wire.Client.API.Push
import Network.Wire.Client.API.Search
import Network.Wire.Client.API.User
import System.Logger.Class

import qualified Codec.MIME.Type          as MIME
import qualified Data.ByteString.Lazy     as LBS
import qualified Network.Wire.Bot.Clients as Clients

default (ByteString)

mainBotNet :: BotNet ()
mainBotNet = do
    info $ msg "Starting Smoke Test"
    [ally, bill, carl] <- mapM newBot ["Ally", "Bill", "Carl"]

    info $ msg "Setting up connections"
    (a2b, a2c) <- runBotSession ally $ do
        c1 <- connectTo $ ConnectionRequest (botId bill) (fromMaybe "" (botEmail ally)) (Message "Hi there!")
        assertConnectRequested ally bill
        c2 <- connectTo $ ConnectionRequest (botId carl) (fromMaybe "" (botEmail ally)) (Message "Hi there!")
        assertConnectRequested ally carl
        a2b <- requireMaybe (ucConvId c1) "conv_id not set after connection request"
        a2c <- requireMaybe (ucConvId c2) "conv_id not set after connection request"
        return (a2b, a2c)

    runBotSession bill $ do
        void $ updateConnection (botId ally) (ConnectionUpdate Accepted)
        assertConnectAccepted bill ally

    runBotSession carl $ do
        void $ updateConnection (botId ally) (ConnectionUpdate Accepted)
        assertConnectAccepted carl ally

    mapM_ awaitAssertions [ally, bill, carl]

    info $ msg "Creating conversations"

    abc <- runBotSession ally $ do
        abc <- cnvId <$> createConv (botId bill) (singleton $ botId carl) (Just "Meetup")
        assertConvCreated abc ally [bill, carl]
        return abc

    info $ msg "Member state updates"

    runBotSession bill $ do
        let update = MemberUpdateData
                   { misOtrMuted       = Nothing
                   , misOtrMutedRef    = Nothing
                   , misOtrArchived    = Just True
                   , misOtrArchivedRef = Nothing
                   , misHidden         = Nothing
                   , misHiddenRef      = Nothing
                   }
        memberUpdate abc update
        c <- getConv abc
        assertEqual (Just True)
                    ((memOtrArchived . cmSelf . cnvMembers) <$> c)
                    "Archived update failed"

    info $ msg "Members join & leave"

    runBotSession bill $ do
        removeMember abc (botId ally) >>= assertMembersLeft [ally, carl]
        addMembers   abc (singleton (botId ally)) >>= assertMembersJoined [ally, carl]

    mapM_ awaitAssertions [ally, bill, carl]

    info $ msg "Basic search reachability"

    _ <- runBotSession ally $ search (SearchParams "whatever" 3 10 True)

    info $ msg "Registering Clients"

    allyPhone  <- addBotClient ally PermanentClient (Just "iPhone")
    billPC     <- addBotClient bill PermanentClient (Just "Linux PC")
    carlTablet <- addBotClient carl PermanentClient (Just "Android tablet")

    let allyWithPhone  = (ally, allyPhone)
    let billWithPC     = (bill, billPC)
    let carlWithTablet = (carl, carlTablet)

    info $ msg (val "OTR 1-1 greetings")

    -- Ally greets Bill and Carl in 1-1
    runBotSession ally $ do
        botInitSession (botId bill)
        botInitSession (botId carl)
        Clients.addMembers (botClientSessions allyPhone) a2b [botId bill]
        Clients.addMembers (botClientSessions allyPhone) a2c [botId carl]
        Clients.addMembers (botClientSessions allyPhone) abc [botId bill, botId carl]
        postOtrTextMsg allyPhone a2b "Hey Bill, Everything secure?" >>= assertNoClientMismatch
        postOtrTextMsg allyPhone a2c "Hey Carl, Everything secure?" >>= assertNoClientMismatch

    -- Bill answers
    runBotSession bill $ do
        pkm   <- awaitOtrMsg a2b allyWithPhone billWithPC
        plain <- initSessionFromMsg billPC pkm >>= requireTextMsg
        assertEqual plain "Hey Bill, Everything secure?" "Bill: Plaintext <> CipherText"
        Clients.addMembers (botClientSessions billPC) a2b [botId ally]
        postOtrTextMsg billPC a2b "Thanks Ally, All good." >>= assertNoClientMismatch

    -- Carl answers
    runBotSession carl $ do
        pkm   <- awaitOtrMsg a2c allyWithPhone carlWithTablet
        plain <- initSessionFromMsg carlTablet pkm >>= requireTextMsg
        assertEqual plain "Hey Carl, Everything secure?" "Carl: Plaintext <> CipherText"
        Clients.addMembers (botClientSessions carlTablet) a2c [botId ally]
        postOtrTextMsg carlTablet a2c "Thanks Ally, All good." >>= assertNoClientMismatch

    -- Ally confirms both answers
    runBotSession ally $ do
        msg1 <- awaitOtrMsg a2b billWithPC allyWithPhone
        msg2 <- awaitOtrMsg a2c carlWithTablet allyWithPhone
        plain1 <- decryptTextMsg allyPhone msg1
        plain2 <- decryptTextMsg allyPhone msg2
        assertEqual plain1 "Thanks Ally, All good." "Ally: Plaintext <> CipherText"
        assertEqual plain2 "Thanks Ally, All good." "Ally: Plaintext <> CipherText"
        postOtrTextMsg allyPhone a2b "Glad to hear that." >>= assertNoClientMismatch
        postOtrTextMsg allyPhone a2c "Glad to hear that." >>= assertNoClientMismatch

    runBotSession bill $ do
        message <- awaitOtrMsg a2b allyWithPhone billWithPC
        plain   <- decryptTextMsg billPC message
        assertEqual plain "Glad to hear that." "Bill: Plaintext <> CipherText"

    runBotSession carl $ do
        message <- awaitOtrMsg a2c allyWithPhone carlWithTablet
        plain   <- decryptTextMsg carlTablet message
        assertEqual plain "Glad to hear that." "Carl: Plaintext <> CipherText"

    info $ msg "OTR group conversation"

    -- Ally posts an asset in the group conversation
    runBotSession ally $ do
        keys <- randomSymmetricKeys allyPhone
        assetData <- encryptSymmetric allyPhone keys "secret data"
        let mimeType = MIME.Type (MIME.Application "octet-stream") []
        asset <- postAsset mimeType defAssetSettings (LBS.fromStrict assetData)
        let assetMsg = encode (mkAssetMsg asset keys)
        postOtrMsg allyPhone abc assetMsg >>= assertNoClientMismatch

    -- Bill receives the asset and writes a reply
    runBotSession bill $ do
        message   <- awaitOtrMsg abc allyWithPhone billWithPC
        asInfo    <- decryptMessage billPC message >>= requireAssetMsg
        assetData <- getAsset (assetInfoKey asInfo) (assetInfoToken asInfo)
        plainData <- for assetData (decryptSymmetric billPC (assetInfoKeys asInfo) . LBS.toStrict)
        assertEqual plainData (Just "secret data") "OTR asset data mismatch"
        botInitSession (botId carl)
        Clients.addMembers (botClientSessions billPC) abc [botId ally, botId carl]
        postOtrTextMsg billPC abc "Wow!" >>= assertNoClientMismatch

    -- Carl receives the asset and the reply from Bill
    runBotSession carl $ do
        -- Asset from Ally
        message   <- awaitOtrMsg abc allyWithPhone carlWithTablet
        asInfo    <- decryptMessage carlTablet message >>= requireAssetMsg
        assetData <- getAsset (assetInfoKey asInfo) (assetInfoToken asInfo)
        plainData <- for assetData (decryptSymmetric carlTablet (assetInfoKeys asInfo) . LBS.toStrict)
        assertEqual plainData (Just "secret data") "OTR asset data mismatch"
        -- Reply from Bill (first message => initialise session)
        message2 <- awaitOtrMsg abc billWithPC carlWithTablet
        plain2 <- initSessionFromMsg carlTablet message2 >>= requireTextMsg
        assertEqual plain2 "Wow!" "Carl: Plaintext <> CipherText"

    runBotSession ally $ do
        message <- awaitOtrMsg abc billWithPC allyWithPhone
        plain   <- decryptTextMsg allyPhone message
        assertEqual plain "Wow!" "Ally: Plaintext <> CipherText"

    mapM_ awaitAssertions [ally, bill, carl]

    info $ msg "Bill gets a new phone"

    billPhone <- addBotClient bill PermanentClient (Just "Linux phone")
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
        -- assertEqual plain "Hey Ally, I've got a new phone!" "BillPC: Plaintext <> CipherText"

    -- Ally answers
    runBotSession ally $ do
        message <- awaitOtrMsg a2b billWithPhone allyWithPhone
        sess    <- Clients.lookupSession (botClientSessions allyPhone) (convEvtFrom message) (otrSender $ convEvtData message)
        assertTrue (isNothing sess) "Surprisingly, Ally knows Bill's new phone already"
        plain   <- initSessionFromMsg allyPhone message >>= requireTextMsg
        assertEqual plain "Hey Ally, I've got a new phone!" "Ally: Plaintext <> CipherText"
        postOtrTextMsg allyPhone a2b "That's nice, Bill" >>= assertNoClientMismatch

    -- Bill receives Ally's answer on both clients
    runBotSession bill $ do
        plain1 <- awaitOtrMsg a2b allyWithPhone billWithPhone >>= decryptTextMsg billPhone
        plain2 <- awaitOtrMsg a2b allyWithPhone billWithPC >>= decryptTextMsg billPC
        assertEqual plain1 "That's nice, Bill" "Bill: Plaintext <> CipherText"
        assertEqual plain2 "That's nice, Bill" "Bill: Plaintext <> CipherText"

    info $ msg "Waiting for event & assertion timeouts (if any)"
    void $ mapConcurrently drainBot [ally, bill, carl]

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

