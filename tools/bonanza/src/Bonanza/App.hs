{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

module Bonanza.App
  ( runBonanza,
  )
where

import Bonanza.Anon
import Bonanza.Geo
import Bonanza.Metrics
import qualified Bonanza.Streaming.Kibana as Kibana
import qualified Bonanza.Streaming.Parser as Parser
import qualified Bonanza.Streaming.Snappy as Snappy
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Conduit (ConduitM, runConduit, (.|))
import Data.Conduit.Binary (sinkHandle, sourceHandle)
import qualified Data.Conduit.List as Conduit
import qualified Data.Conduit.Zlib as Conduit
import qualified Data.Text as T
import Data.Time
import Data.Version (showVersion)
import Imports
import Options.Applicative
import Paths_bonanza (version)
import System.CPUTime

data Opts = Opts CommonOpts Command
  deriving (Show)

data Compression = GZip | Snappy

instance Read Compression where
  readsPrec _ ('g' : 'z' : 'i' : 'p' : xs) = [(GZip, xs)]
  readsPrec _ ('s' : 'n' : 'a' : 'p' : 'p' : 'y' : xs) = [(Snappy, xs)]
  readsPrec _ _ = []

instance Show Compression where
  show GZip = "gzip"
  show Snappy = "snappy"

data CommonOpts = CommonOpts
  { parser :: !String,
    geo :: [String],
    geodat :: !FilePath,
    anon :: [String],
    quiet :: !Bool,
    debug :: !Bool,
    decomp :: !(Maybe Compression),
    comp :: !(Maybe Compression)
  }
  deriving (Show)

data KibanaOpts = KibanaOpts
  { print0 :: !Bool,
    idxPrefix :: !Text
  }
  deriving (Show)

data Command
  = Kibana KibanaOpts
  | JSON
  deriving (Show)

kibanaOpts :: Parser KibanaOpts
kibanaOpts =
  KibanaOpts
    <$> switch
      ( short '0'
          <> long "null"
          <> help "Produce output suitable for 'xargs -0'."
      )
    <*> option
      (eitherReader (Right . T.pack))
      ( long "index-prefix"
          <> value "logs"
          <> help "Kibana index prefix. Will be expanded to PREFIX-yyyy-mm-dd based on the event timestamp"
          <> showDefault
      )

kibana :: Parser Command
kibana = Kibana <$> kibanaOpts

opts :: Parser Opts
opts =
  Opts
    <$> ( CommonOpts
            <$> strOption
              ( short 'p'
                  <> long "parser"
                  <> metavar "STR"
                  <> help "Input parser."
              )
            <*> many
              ( strOption
                  ( short 'g'
                      <> long "geolocate"
                      <> metavar "STR"
                      <> help "Tags to retrieve geo information for. The value must be a valid IPv4 address."
                  )
              )
            <*> strOption
              ( long "geoip-dat"
                  <> metavar "PATH"
                  <> value "/etc/bonanza/GeoLiteCity.dat"
                  <> help "Path to the GeoIP City database."
                  <> showDefault
              )
            <*> many
              ( strOption
                  ( short 'a'
                      <> long "anonymise"
                      <> metavar "STR"
                      <> help "Tags to anonymise (ie. strip from output)."
                  )
              )
            <*> switch
              ( short 'q'
                  <> long "quiet"
                  <> help "Silence informational output on stderr."
              )
            <*> switch
              ( short 'd'
                  <> long "debug"
                  <> help "Output debugging info."
              )
            <*> optional
              ( option
                  auto
                  ( long "decompress"
                      <> help "Decompress input."
                  )
              )
            <*> optional
              ( option
                  auto
                  ( long "compress"
                      <> help "Compress output."
                  )
              )
        )
    <*> subparser
      ( command
          "kibana"
          ( info
              kibana
              (progDesc "Produce Elasticsearch (Kibana) Bulk API JSON.")
          )
          <> command
            "json"
            ( info
                (pure JSON)
                (progDesc "Produce plain JSON.")
            )
      )

optInfo :: ParserInfo Opts
optInfo =
  info
    (helper <*> opts)
    ( fullDesc
        <> header ("bonanza v" ++ showVersion version)
        <> progDesc "Parse and encode log formats."
    )

runBonanza :: IO ()
runBonanza =
  execParser optInfo >>= \(Opts CommonOpts {..} cmd) -> do
    started <- getCurrentTime
    (bytes_in, bytes_out, events_in) <-
      (,,)
        <$> newIORef 0
        <*> newIORef 0
        <*> newIORef 0
    geoDB <- mkGeo geodat
    runConduit $
      sourceHandle stdin
        .| runDecompress decomp
        .| Conduit.mapM
          ( \bs ->
              modifyIORef' bytes_in (+ fromIntegral (BS.length bs))
                *> pure bs
          )
        .| readWith parser
        .| Conduit.mapM
          ( \evt ->
              modifyIORef' events_in (+ 1)
                *> pure evt
          )
        .| runGeo geo geoDB
        .| runAnonymise anon
        .| runCmd cmd
        .| runCompress comp
        .| Conduit.mapM
          ( \bs ->
              modifyIORef' bytes_out (+ fromIntegral (BS.length bs))
                *> pure bs
          )
        .| sinkHandle stdout
    completed <- getCurrentTime
    stats <-
      Stats
        <$> readIORef bytes_in
        <*> readIORef bytes_out
        <*> (picosecondsToDiffTime <$> getCPUTime)
        <*> pure (completed `diffUTCTime` started)
        <*> readIORef events_in
    unless quiet $ do
      dumpStderr stats
  where
    runGeo [] _ = Conduit.map id
    runGeo tags db =
      Conduit.mapM
        (\e -> foldM (\e' t -> geolocate db (T.pack t) e') e tags)
    runAnonymise = Conduit.map . anonymise . map T.pack
    runCmd (Kibana KibanaOpts {..}) =
      Conduit.mapM Kibana.fromLogEvent
        .| Conduit.map
          ( (: [if print0 then "\0" else mempty])
              . Kibana.jsonEncode idxPrefix
          )
        .| Conduit.concat
        .| Conduit.map BL.toChunks
        .| Conduit.concat
    runCmd JSON =
      Conduit.map ((: ["\n"]) . Aeson.encode)
        .| Conduit.concat
        .| Conduit.map BL.toChunks
        .| Conduit.concat
    runDecompress :: Maybe Compression -> ConduitM ByteString ByteString IO ()
    runDecompress Nothing = Conduit.map id
    runDecompress (Just GZip) = Conduit.ungzip
    runDecompress (Just Snappy) = Snappy.decode .| Snappy.bytes
    runCompress :: Maybe Compression -> ConduitM ByteString ByteString IO ()
    runCompress Nothing = Conduit.map id
    runCompress (Just GZip) = Conduit.gzip
    runCompress (Just Snappy) = Snappy.encode .| Snappy.bytes
    readWith p = Parser.stream (Parser.byName p)
