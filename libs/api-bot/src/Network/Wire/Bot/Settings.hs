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

module Network.Wire.Bot.Settings
  ( BotNetSettings (..),
    botNetSettingsParser,
    BotSettings,
    defBotSettings,
    botSettingsParser,
    botMaxEvents,
    botMaxAsserts,
    botEventTimeout,
    botAssertTimeout,
    setMaxEvents,
    setMaxAsserts,
    setEventTimeout,
    setAssertTimeout,
  )
where

import Data.ByteString.Char8 (pack)
import qualified Data.Text as Text
import Data.Time.Clock (NominalDiffTime)
import Imports
import Network.Wire.Client.API.User (Email (..), parseEmail)
import Options.Applicative

-------------------------------------------------------------------------------
-- BotNetSettings

data BotNetSettings = BotNetSettings
  { setBotNetApiHost :: !ByteString,
    setBotNetApiPort :: !Word16,
    setBotNetApiWsHost :: !(Maybe ByteString),
    setBotNetApiWsPort :: !(Maybe Word16),
    setBotNetApiSSL :: !Bool,
    setBotNetAssert :: !Bool,
    setBotNetMailboxConfig :: !(Maybe FilePath),
    setBotNetSender :: !Email,
    setBotNetUsersFile :: !(Maybe FilePath),
    setBotNetReportDir :: !(Maybe FilePath),
    setBotNetBotSettings :: !BotSettings,
    setBotNetMailboxFolders :: ![String]
  }
  deriving (Eq, Show)

botNetSettingsParser :: Parser BotNetSettings
botNetSettingsParser =
  BotNetSettings
    <$> apiHostOption
    <*> apiPortOption
    <*> apiWsHostOption
    <*> apiWsPortOption
    <*> apiSSLOption
    <*> enableAssertsOption
    <*> mailboxConfigOption
    <*> mailSenderOption
    <*> usersFileOption
    <*> reportDirOption
    <*> botSettingsParser
    <*> mailboxFoldersOption

apiHostOption :: Parser ByteString
apiHostOption =
  bsOption $
    long "api-host"
      <> metavar "HOSTNAME"
      <> help "HTTP(S) API hostname or address to connect to"

apiPortOption :: Parser Word16
apiPortOption =
  option auto $
    long "api-port"
      <> metavar "PORT"
      <> help "HTTP(S) API port to connect to"

apiWsHostOption :: Parser (Maybe ByteString)
apiWsHostOption =
  optional . bsOption $
    long "api-websocket-host"
      <> metavar "HOSTNAME"
      <> help "Websocket API hostname or address to connect to"

apiWsPortOption :: Parser (Maybe Word16)
apiWsPortOption =
  optional . option auto $
    long "api-websocket-port"
      <> metavar "PORT"
      <> help "Websocket API port to connect to"

apiSSLOption :: Parser Bool
apiSSLOption =
  switch $
    long "api-ssl"
      <> help "Use TLS (HTTPS)"

enableAssertsOption :: Parser Bool
enableAssertsOption =
  switch $
    long "enable-asserts"
      <> help "Enable assertions"

mailboxConfigOption :: Parser (Maybe FilePath)
mailboxConfigOption =
  optional . strOption $
    long "mailbox-config"
      <> metavar "FILE"
      <> help "Path to mailbox config"

mailSenderOption :: Parser Email
mailSenderOption =
  emailOption $
    long "sender-email"
      <> metavar "EMAIL"
      <> help "Expected sender email address (FROM)"
      <> value (Email "accounts" "wire.com")
      <> showDefault

usersFileOption :: Parser (Maybe FilePath)
usersFileOption =
  optional . strOption $
    long "users-file"
      <> metavar "FILE"
      <> help
        "Path to users file; which is a headerless csv \
        \ containing a list of ALREADY EXISTING users with the columns: \
        \ User-Id,Email,Password"

reportDirOption :: Parser (Maybe FilePath)
reportDirOption =
  optional . strOption $
    long "report-dir"
      <> metavar "DIR"
      <> help "Output directory for reports"

mailboxFoldersOption :: Parser [String]
mailboxFoldersOption =
  ( some . strOption $
      long "mailbox-folder"
        <> help
          "In which mailbox folder to search for emails. \
          \ Defaults to 'INBOX' if not specified. \
          \ Can be specified multiple times for multiple folders."
  )
    <|> pure ["INBOX"]

-------------------------------------------------------------------------------
-- BotSettings

data BotSettings = BotSettings
  { _botMaxEvents :: Word16,
    _botEventTimeout :: NominalDiffTime,
    _botMaxAsserts :: Word16,
    _botAssertTimeout :: NominalDiffTime
  }
  deriving (Eq, Show)

defBotSettings :: BotSettings
defBotSettings =
  BotSettings
    { _botMaxEvents = 100,
      _botEventTimeout = 30,
      _botMaxAsserts = 100,
      _botAssertTimeout = 30
    }

botMaxEvents :: BotSettings -> Word16
botMaxEvents = _botMaxEvents

botMaxAsserts :: BotSettings -> Word16
botMaxAsserts = _botMaxAsserts

botEventTimeout :: BotSettings -> NominalDiffTime
botEventTimeout = _botEventTimeout

botAssertTimeout :: BotSettings -> NominalDiffTime
botAssertTimeout = _botAssertTimeout

setMaxEvents :: Word16 -> BotSettings -> BotSettings
setMaxEvents n bs =
  if n == 0
    then error "max events must be > 0"
    else bs {_botMaxEvents = n}

setMaxAsserts :: Word16 -> BotSettings -> BotSettings
setMaxAsserts n bs =
  if n == 0
    then error "max asserts must be > 0"
    else bs {_botMaxAsserts = n}

setEventTimeout :: NominalDiffTime -> BotSettings -> BotSettings
setEventTimeout n bs =
  if n == 0
    then error "event timeout must be > 0"
    else bs {_botEventTimeout = n}

setAssertTimeout :: NominalDiffTime -> BotSettings -> BotSettings
setAssertTimeout n bs =
  if n == 0
    then error "assert timeout must be > 0"
    else bs {_botAssertTimeout = n}

botSettingsParser :: Parser BotSettings
botSettingsParser =
  BotSettings
    <$> maxEventsOption
    <*> eventTimeoutOption
    <*> maxAssertsOption
    <*> assertTimeoutOption

maxEventsOption :: Parser Word16
maxEventsOption =
  option (auto >>= greater 0) $
    long "max-events"
      <> metavar "NUM"
      <> value 100
      <> help "Max. event inbox size per bot"

eventTimeoutOption :: Parser NominalDiffTime
eventTimeoutOption =
  timeoutOption $
    long "event-timeout"
      <> metavar "SECONDS"
      <> value 30
      <> help "Timeout for unmatched events when assertions are enabled"

maxAssertsOption :: Parser Word16
maxAssertsOption =
  option (auto >>= greater 0) $
    long "max-asserts"
      <> metavar "NUM"
      <> value 100
      <> help "Max. assert queue size per bot"

assertTimeoutOption :: Parser NominalDiffTime
assertTimeoutOption =
  timeoutOption $
    long "assert-timeout"
      <> metavar "SECONDS"
      <> value 30
      <> help "Timeout for assertions"

-------------------------------------------------------------------------------
-- Utilities

greater :: (Integral a, Show a) => a -> a -> ReadM a
greater n a
  | a <= n = readerError ("must be > " ++ show n)
  | otherwise = return a

bsOption :: Mod OptionFields String -> Parser ByteString
bsOption = fmap pack . strOption

timeoutOption :: Mod OptionFields Integer -> Parser NominalDiffTime
timeoutOption = fmap fromInteger . option auto

emailOption :: Mod OptionFields Email -> Parser Email
emailOption =
  option . eitherReader $
    maybe (Left "Invalid email") Right
      . parseEmail
      . Text.pack
