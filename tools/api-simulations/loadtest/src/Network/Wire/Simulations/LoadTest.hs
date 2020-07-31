{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

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

module Network.Wire.Simulations.LoadTest where

import qualified Codec.MIME.Type as MIME
import qualified Control.Monad.Catch as Ex
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB
import Data.Id (ConvId)
import qualified Data.Metrics as Metrics
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Imports hiding (log)
import Network.Wire.Bot
import Network.Wire.Bot.Assert
import qualified Network.Wire.Bot.Clients as Clients
import Network.Wire.Bot.Crypto
import Network.Wire.Client.API.Asset
import Network.Wire.Client.API.Client (ClientType (PermanentClientType))
import Network.Wire.Client.API.Conversation
import Network.Wire.Simulations
import System.Logger.Class
import qualified System.Random.MWC as MWC
import UnliftIO.Async

runLoadTest :: LoadTestSettings -> BotNet ()
runLoadTest s =
  replicateM (conversationsTotal s) mkConv
    >>= mapM startConv
    >>= mapM_ wait
  where
    startConv bots = do
      liftIO . threadDelay $ calcDelay (conversationRamp s)
      (async . runConv s) bots
    calcDelay :: Maybe RampType -> Int
    calcDelay ramp = case ramp of
      Just (RampStep t) -> t
      Just (RampTotal t) -> t `div` conversationsTotal s
      Nothing -> 0
    mkConv :: BotNet ([BotNet Bot], [BotNet Bot])
    mkConv = do
      active <-
        between
          (conversationMinActiveMembers s)
          (conversationMaxActiveMembers s)
      passive <-
        between
          (conversationMinPassiveMembers s)
          (conversationMaxPassiveMembers s)
      -- since we use 'cachedBot' here, all returned accounts will be distinct
      return
        ( [cachedBot (fromString ("bot" <> show i)) | i <- [1 .. active]],
          [cachedBot (fromString ("passiveBot" <> show i)) | i <- [1 .. passive]]
        )

runConv :: LoadTestSettings -> ([BotNet Bot], [BotNet Bot]) -> BotNet ()
runConv s g = do
  active <- pooledMapConcurrentlyN (parallelRequests s) id (fst g)
  passive <- pooledMapConcurrentlyN (parallelRequests s) id (snd g)
  let bots = active ++ passive
  -- Clear existing clients ----------
  log Info $ msg $ val "Clearing existing clients"
  pooledForConcurrentlyN_ (parallelRequests s) bots $ \b ->
    resetBotClients b
  -- Create conv ---------------------
  log Info $ msg $ val "Creating conversation"
  conv <- prepareConv bots
  Metrics.counterIncr conversationsEstablished =<< getMetrics
  -- Prepare -------------------------
  log Info $ msg $ val "Preparing"
  let botsMarked = map (True,) active ++ map (False,) passive
  states <- pooledForConcurrentlyN (parallelRequests s) botsMarked $ \(isActive, b) -> do
    nmsg <- if isActive then between (messagesMin s) (messagesMax s) else pure 0
    nast <- if isActive then between (assetsMin s) (assetsMax s) else pure 0
    nClients <- between (clientsMin s) (clientsMax s)
    runBotSession b $ do
      log Info $ msg $ val "Creating clients"
      uniq <- liftIO UUID.nextRandom
      let mainLabel = "main-client-" <> UUID.toText uniq
      mainClient <- addBotClient b PermanentClientType (Just mainLabel)
      otherClients <- for [1 .. nClients - 1] $ \i -> do
        let label = "client-" <> Text.pack (show i) <> "-" <> UUID.toText uniq
        addBotClient b PermanentClientType (Just label)
      return $! BotState mainClient otherClients conv bots nmsg nast
  -- Run -----------------------------
  log Info $ msg $ val "Running"
  pooledForConcurrentlyN_ (parallelRequests s) (zip bots states) $ \(b, st) ->
    runBotSession b $ do
      log Info $ msg $ val "Initializing sessions"
      let allClients = botClient st : botOtherClients st
      forM_ allClients $ \client -> do
        forM_ bots $ clientInitSession client . botId
        Clients.addMembers (botClientSessions client) conv (map botId bots)
  let removeClients (b, st) =
        mapM_ (removeBotClient b) (botClient st : botOtherClients st)
  void . flip mapConcurrently (zip bots states) $ \(b, st) ->
    runBotSession b $ do
      log Info $ msg $ val "Starting bot"
      runBot s st `Ex.onException` removeClients (b, st)
  -- Drain ---------------------------
  log Info $ msg $ val "Draining"
  pooledForConcurrentlyN_ (parallelRequests s) (zip bots states) $ \(b, st) -> do
    removeClients (b, st)
    drainBot b

runBot :: LoadTestSettings -> BotState -> BotSession ()
runBot _ BotState {..} | done = return ()
  where
    done = messagesLeft <= 0 && assetsLeft <= 0
runBot ls s@BotState {..} = do
  liftIO . threadDelay $ stepDelay ls
  runBot ls
    =<< if messagesLeft >= assetsLeft
      then postMsg >> return s {messagesLeft = messagesLeft - 1}
      else postAst >> return s {assetsLeft = assetsLeft - 1}
  where
    postMsg = do
      self <- getBot
      void . try $ do
        m <- mkMsg
        let len = Text.length m
        timed postMessageTime $ do
          cipher <- encryptMessage botClient botConv (encode (BotTextMessage m))
          postOtrMessage botConv cipher >>= assertNoClientMismatch
        Metrics.counterIncr messagesSent =<< getMetrics
        Metrics.counterAdd (fromIntegral len) messagesSize =<< getMetrics
        forM_ (filter (/= self) botConvMembers) $ \b -> do
          bcs <- getBotClients b
          forM_ bcs $ \bc -> do
            evt <- awaitOtrMessage botConv (self, botClient) (b, bc)
            for_ evt $ \e -> do
              m' <- decryptMessage bc e >>= requireTextMsg
              assertEqual m' m "OTR plaintext mismatch"
    postAst = do
      self <- getBot
      void . try $ do
        plainData <- mkAst
        let len = BS.length plainData
        keys <- randomSymmetricKeys botClient
        cipherData <- encryptSymmetric botClient keys plainData
        let mimeType = MIME.Type (MIME.Application "octet-stream") []
        asset <- timed postAssetTime $ postAsset mimeType defAssetSettings (LB.fromStrict cipherData)
        let plainMsg = encode (mkAssetMsg asset keys)
        cipherMsg <- encryptMessage botClient botConv plainMsg
        postOtrMessage botConv cipherMsg >>= assertNoClientMismatch
        Metrics.counterIncr assetsSent =<< getMetrics
        Metrics.counterAdd (fromIntegral len) assetsSize =<< getMetrics
        forM_ (filter (/= self) botConvMembers) $ \b -> do
          bcs <- getBotClients b
          forM_ bcs $ \bc -> do
            evt <- awaitOtrMessage botConv (self, botClient) (b, bc)
            for_ evt $ \e -> do
              i <- decryptMessage bc e >>= requireAssetMsg
              cipherData' <- timed getAssetTime $ getAsset (assetInfoKey i) (assetInfoToken i)
              plainData' <- for (LB.toStrict <$> cipherData') $ \cd -> do
                assertEqual cd cipherData "OTR asset ciphertext mismatch"
                decryptSymmetric bc (assetInfoKeys i) cd
              assertEqual plainData' (Just plainData) "OTR asset plaintext mismatch"
    mkMsg = do
      l <- between (messageMinLength ls) (messageMaxLength ls)
      return $ Text.replicate l "A"
    mkAst = do
      l <- between (assetMinSize ls) (assetMaxSize ls)
      return $ BS.replicate (fromIntegral l) 42

data BotState = BotState
  { botClient :: !BotClient, -- "main" client (sends messages, etc)
    botOtherClients :: ![BotClient], -- other clients (just sit around)
    botConv :: !ConvId,
    botConvMembers :: [Bot],
    messagesLeft :: !Int,
    assetsLeft :: !Int
  }
  deriving (Eq)

-------------------------------------------------------------------------------
-- Settings

data RampType = RampStep Int | RampTotal Int deriving (Eq, Show)

data LoadTestSettings = LoadTestSettings
  { ltsBotNetSettings :: !BotNetSettings,
    conversationRamp :: !(Maybe RampType),
    conversationsTotal :: !Int,
    conversationMinActiveMembers :: !Int,
    conversationMaxActiveMembers :: !Int,
    conversationMinPassiveMembers :: !Int,
    conversationMaxPassiveMembers :: !Int,
    clientsMin :: !Int,
    clientsMax :: !Int,
    messagesMin :: !Int,
    messagesMax :: !Int,
    messageMinLength :: !Int,
    messageMaxLength :: !Int,
    assetsMin :: !Int,
    assetsMax :: !Int,
    assetMinSize :: !Int,
    assetMaxSize :: !Int,
    stepDelay :: !Int,
    parallelRequests :: !Int
  }
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Metrics

conversationsEstablished :: Metrics.Path
conversationsEstablished = Metrics.path "conversations.created"

messagesSent :: Metrics.Path
messagesSent = Metrics.path "messages.sent"

messagesSize :: Metrics.Path
messagesSize = Metrics.path "messages.size"

assetsSent :: Metrics.Path
assetsSent = Metrics.path "assets.sent"

assetsSize :: Metrics.Path
assetsSize = Metrics.path "assets.size"

postMessageTime :: Metrics.Path
postMessageTime = Metrics.path "post-message.time"

getAssetTime :: Metrics.Path
getAssetTime = Metrics.path "get-asset.time"

postAssetTime :: Metrics.Path
postAssetTime = Metrics.path "post-asset.time"

--------------------------------------------------------------------------------
-- Utilities

between :: MonadBotNet m => Int -> Int -> m Int
between x y = getGen >>= liftIO . MWC.uniformR (max 0 x, max x y)
