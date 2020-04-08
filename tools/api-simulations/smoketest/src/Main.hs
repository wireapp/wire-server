{-# LANGUAGE OverloadedStrings #-}

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
