{-# LANGUAGE OverloadedStrings #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module ClientInfo.Lib where

import Cassandra as C
import Cassandra.Settings as C
import ClientInfo.Types
import Conduit
import qualified Data.Conduit.Combinators as Conduit
import qualified Data.Conduit.List as ConduitL
import Data.Id (UserId, idToText, parseIdFromText)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format (defaultTimeLocale, formatTime)
import Imports
import Options.Applicative
import System.IO (hPutStrLn)
import qualified System.Logger as Log
import Util.Timeout (UTCTime)

main :: IO ()
main = do
  opts <- execParser (info (helper <*> optsParser) desc)
  logger <- initLogger
  brigClient <- initCas opts.brigDb logger
  userIds <- readUserIds opts.inputFile
  let terms = normalizeSearchTerms opts.searchTerms
  process brigClient terms userIds
  where
    initLogger =
      Log.new
        . Log.setLogLevel Log.Error
        . Log.setOutput Log.StdOut
        . Log.setFormat Nothing
        . Log.setBufSize 0
        $ Log.defSettings
    initCas settings l =
      C.init
        . C.setLogger (C.mkLogger l)
        . C.setContacts settings.host []
        . C.setPortNumber (fromIntegral settings.port)
        . C.setKeyspace settings.keyspace
        . C.setProtocolVersion C.V4
        $ C.defSettings
    desc = header "client-info" <> progDesc "get client information for user ids" <> fullDesc

process :: ClientState -> Maybe [Text] -> [UserId] -> IO ()
process brigClient terms userIds = do
  rows <-
    runConduit $
      ConduitL.sourceList userIds
        .| Conduit.concatMapM (readClients brigClient)
        .| ConduitL.consume
  for_ rows $ \row -> when (matchesTerms terms row) $ Text.putStrLn (renderClientRow row)

readUserIds :: InputFile -> IO [UserId]
readUserIds inputFile = do
  contents <- Text.readFile inputFile.unInputFile
  let linesWithNumbers = zip [(1 :: Int) ..] (Text.lines contents)
  fmap catMaybes $ forM linesWithNumbers $ \(lineNo, line) -> do
    let trimmed = Text.strip line
    if Text.null trimmed
      then pure Nothing
      else case parseIdFromText trimmed of
        Left err -> do
          hPutStrLn stderr $ "Skipping invalid user id on line " <> show lineNo <> ": " <> show err
          pure Nothing
        Right uid -> pure (Just uid)

renderClientRow :: ClientRow -> Text
renderClientRow row =
  Text.intercalate
    ","
    [ csvEscape (idToText row.userId),
      csvEscape row.clientId.unClientId,
      csvEscape (fromMaybe "" row.clientModel),
      csvEscape (fromMaybe "" row.clientLabel),
      csvEscape (maybe "" tsToDateText row.lastActive)
    ]
  where
    tsToDateText = T.pack . formatTime defaultTimeLocale "%d-%m-%Y"

csvEscape :: Text -> Text
csvEscape v
  | Text.any (\c -> c == ',' || c == '"' || c == '\n' || c == '\r') v =
      "\"" <> Text.replace "\"" "\"\"" v <> "\""
  | otherwise = v

normalizeSearchTerms :: Maybe Text -> Maybe [Text]
normalizeSearchTerms = \case
  Nothing -> Nothing
  Just raw ->
    let trimmed = Text.strip raw
        terms =
          filter (not . Text.null) $
            map (Text.toCaseFold . Text.strip) $
              Text.splitOn "," trimmed
     in if Text.null trimmed || null terms then Nothing else Just terms

matchesTerms :: Maybe [Text] -> ClientRow -> Bool
matchesTerms Nothing _ = True
matchesTerms (Just terms) row =
  let haystacks =
        map Text.toCaseFold $
          catMaybes
            [ row.clientModel,
              row.clientLabel
            ]
   in any (\term -> any (Text.isInfixOf term) haystacks) terms

readClients :: ClientState -> UserId -> IO [ClientRow]
readClients client uid =
  runConduit $
    transPipe
      (runClient client)
      ( paginateC selectClients (paramsP One (Identity uid) clientPageSize) x5
          .| Conduit.map (map (\(cid, model, label, lastActive) -> ClientRow uid (ClientId cid) model label lastActive))
      )
      .| Conduit.concat
      .| ConduitL.consume
  where
    selectClients :: PrepQuery R (Identity UserId) (Text, Maybe Text, Maybe Text, Maybe UTCTime)
    selectClients = "SELECT client, model, label, last_active from clients where user = ?"

clientPageSize :: Int32
clientPageSize = 1000
