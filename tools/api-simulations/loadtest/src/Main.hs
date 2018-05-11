{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless, void)
import Data.Maybe
import Data.Monoid
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wire.Client.API.Push (EventType (..))
import Network.Wire.Bot
import Network.Wire.Bot.Report
import Network.Wire.Simulations.LoadTest
import Options.Applicative

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
        simulation o
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
ltsSettingsParser = LoadTestSettings
    <$> botNetSettingsParser
    <*> optional
        (
            RampStep <$> option auto
            (  long "ramp-step"
            <> metavar "INT"
            <> help "delay in microseconds between conversations start")
         <|>
            RampTotal <$> option auto
            (  long "ramp-total"
            <> metavar "INT"
            <> help "time in microseconds until full load")
        )
    <*> option auto
        (  long "conversations-total"
        <> metavar "INT"
        <> help "total number of conversations"
        )
    <*> option auto
        (  long "conversation-bots-min"
        <> metavar "INT"
        <> help "min number of bots in a conversation"
        <> value 2
        <> showDefault
        )
    <*> option auto
        (  long "conversation-bots-max"
        <> metavar "INT"
        <> help "max number of bots in a conversation"
        <> value 5
        <> showDefault
        )
    <*> option auto
        (  long "bot-messages-min"
        <> metavar "INT"
        <> help "min number of text messages to post per bot"
        <> value 0
        <> showDefault
        )
    <*> option auto
        (  long "bot-messages-max"
        <> metavar "INT"
        <> help "max number of text messages to post per bot"
        <> value 0
        <> showDefault
        )
    <*> option auto
        (  long "message-length-min"
        <> metavar "INT"
        <> help "min length of text messages posted"
        <> value 1
        <> showDefault
        )
    <*> option auto
        (  long "message-length-max"
        <> metavar "INT"
        <> help "max length of text messages posted"
        <> value 100
        <> showDefault
        )
    <*> option auto
        (  long "bot-assets-min"
        <> metavar "INT"
        <> help "min number of assets to post per bot"
        <> value 0
        <> showDefault
        )
    <*> option auto
        (  long "bot-assets-max"
        <> metavar "INT"
        <> help "max number of assets to post per bot"
        <> value 0
        <> showDefault
        )
    <*> option auto
        (  long "asset-size-min"
        <> metavar "INT"
        <> help "min size (bytes) of assets posted"
        <> value 10
        <> showDefault
        )
    <*> option auto
        (  long "asset-size-max"
        <> metavar "INT"
        <> help "max size (bytes) of assets posted"
        <> value 1000
        <> showDefault
        )
    <*> option auto
        (  long "step-delay"
        <> metavar "INT"
        <> help "delay in microseconds between actions taken by a single bot"
        <> value 1000000
        <> showDefault
        )
