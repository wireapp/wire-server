{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}

module Main where

import Control.Monad (unless, void)
import Data.Foldable (asum)
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wire.Client.API.Push (EventType (..))
import Network.Wire.Bot
import Network.Wire.Bot.Report
import Network.Wire.Simulations.LoadTest
import Options.Applicative.Extended

import qualified System.Logger as Log

main :: IO ()
main = do
    o <- parseOptions
    unless (isJust (setBotNetUsersFile (ltsBotNetSettings o))) $
        error "--users-file was not specified; the loadtest can't work without \
              \the list of user accounts provided to it"
    m <- newManager tlsManagerSettings
    l <- Log.new Log.defSettings
    e <- newBotNetEnv m l (ltsBotNetSettings o)
    void . runBotNet e $ do
        runLoadTest o
        Log.flush l >> Log.close l
        report "LoadTest - Conversations" sections
  where
    sections = section "Summary"
                [ Counter "Conversations Established" conversationsEstablished
                , Counter "Messages Sent" messagesSent
                , Counter "Message Bytes Total" messagesSize
                , Counter "Assets Sent" assetsSent
                , Counter "Asset Bytes Total" assetsSize
                ]
            <> botsSection
            <> assertionsSection
            <> exceptionsSection
            <> eventsTotalSection
            <> eventTypeSection TConvCreate
            <> eventTypeSection TConvMemberStateUpdate
            <> eventTypeSection TConvOtrMessageAdd
            <> section "Timings"
                [ Buckets "Post Message" postMessageTime
                , Buckets "Post Asset" postAssetTime
                , Buckets "Get Asset" getAssetTime
                ]

parseOptions :: IO LoadTestSettings
parseOptions = execParser (info (helper <*> ltsSettingsParser) desc)
  where
    desc = header "Wire API Load Test" <> fullDesc

ltsSettingsParser :: Parser LoadTestSettings
ltsSettingsParser = do
    ltsBotNetSettings <- botNetSettingsParser
    conversationRamp <- optional $ asum
        [ fmap RampStep $ option auto $
              long "ramp-step"
              <> metavar "INT"
              <> help "delay in microseconds between conversations start"
        , fmap RampTotal $ option auto $
              long "ramp-total"
              <> metavar "INT"
              <> help "time in microseconds until full load"
        ]
    conversationsTotal <- option auto $
        long "conversations-total"
        <> metavar "INT"
        <> help "total number of conversations"
    conversationActiveMembers <- option autoRange $
        long "conversation-bots"
        <> metavar "INT|INT..INT"
        <> help "number of bots in a conversation"
        <> value (2, 5)
        <> showDefault
    conversationPassiveMembers <- option autoRange $
        long "conversation-passive-bots"
        <> metavar "INT|INT..INT"
        <> help "number of passive bots (that don't send anything) \
                \to be added along with usual bots"
        <> value (0, 0)
        <> showDefault
    messages <- option autoRange $
        long "bot-messages"
        <> metavar "INT|INT..INT"
        <> help "number of text messages to post per bot"
        <> value (0, 0)
        <> showDefault
    messageLength <- option autoRange $
        long "message-length"
        <> metavar "INT|INT..INT"
        <> help "length of text messages posted"
        <> value (1, 100)
        <> showDefault
    assets <- option autoRange $
        long "bot-assets"
        <> metavar "INT|INT..INT"
        <> help "number of assets to post per bot"
        <> value (0, 0)
        <> showDefault
    assetSize <- option autoRange $
        long "asset-size"
        <> metavar "INT|INT..INT"
        <> help "size (bytes) of assets posted"
        <> value (10, 1000)
        <> showDefault
    stepDelay <- option auto $
        long "step-delay"
        <> metavar "INT"
        <> help "delay in microseconds between actions taken by a single bot"
        <> value 1000000
        <> showDefault
    parallelRequests <- option auto $
        long "parallel-requests"
        <> metavar "INT"
        <> help "maximum number of parallel requests (for bots in a single \
                \conversation); applies to everything except sending or \
                \receiving messages"
        <> value 10
        <> showDefault
    pure $
        let (messagesMin, messagesMax) = messages
            (messageMinLength, messageMaxLength) = messageLength
            (assetsMin, assetsMax) = assets
            (assetMinSize, assetMaxSize) = assetSize
            (conversationMinActiveMembers, conversationMaxActiveMembers)
                = conversationActiveMembers
            (conversationMinPassiveMembers, conversationMaxPassiveMembers)
                = conversationPassiveMembers
        in LoadTestSettings{..}
