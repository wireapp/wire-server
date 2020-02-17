{-# LANGUAGE OverloadedStrings #-}

module Main where

import Imports
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wire.Bot
import Network.Wire.Bot.Metrics (assertionsFailed)
import Network.Wire.Bot.Report
import Network.Wire.Simulations.SmokeTest
import Options.Applicative
import System.Exit
import qualified System.Logger as Log

main :: IO ()
main = do
  o <- parseOptions
  m <- newManager tlsManagerSettings
  l <- Log.new Log.defSettings -- TODO: use mkLogger'?
  e <- newBotNetEnv m l o
  r <- runBotNet e $ do
    mainBotNet 5
    Log.flush l >> Log.close l
    report "Smoke Test" defaultSections
  unless
    (reportCounter r assertionsFailed == 0)
    exitFailure

parseOptions :: IO BotNetSettings
parseOptions = execParser (info (helper <*> botNetSettingsParser) desc)
  where
    desc = header "Wire API Smoke Test" <> fullDesc
