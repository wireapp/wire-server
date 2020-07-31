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

module Main
  ( main,
  )
where

import Control.Concurrent.Async
import Control.Concurrent.STM (retry)
import Control.Error (runExceptT, syncIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Version (showVersion)
import Imports
import Network.HTTP.Client
import Network.HTTP.Client.Conduit (requestBodySourceChunked)
import Network.HTTP.Client.TLS
import Options.Applicative
import Paths_bonanza (version)

data Opts = Opts
  { url :: String,
    maxBulkSize :: !Int,
    maxBufferSize :: !Int,
    concurrency :: !Int
  }
  deriving (Show)

parseOpts :: Parser Opts
parseOpts =
  Opts
    <$> strOption
      ( short 'u'
          <> long "url"
          <> help "(Base) URL of the elasticsearch server."
          <> value "http://localhost:9200"
          <> showDefault
      )
    <*> option
      auto
      ( short 'b'
          <> long "max-bulk-size"
          <> metavar "INT"
          <> value 1000
          <> help "Max. records per Elasticsearch bulk request."
          <> showDefault
      )
    <*> option
      auto
      ( short 'q'
          <> long "max-buffer-size"
          <> metavar "INT"
          <> value 1000000
          <> help "Max. input buffer size."
          <> showDefault
      )
    <*> option
      auto
      ( short 'c'
          <> long "concurrency"
          <> metavar "INT"
          <> value 100
          <> help "Number of concurrent consumers"
          <> showDefault
      )

optInfo :: ParserInfo Opts
optInfo =
  info
    (helper <*> parseOpts)
    ( fullDesc
        <> header ("kibanana v" ++ showVersion version)
        <> progDesc "Read null-delimited bulk API input from stdin and shove it into elasticsearch"
    )

data Signal = Stop | Go

main :: IO ()
main =
  execParser optInfo >>= \Opts {..} -> do
    mgr <- newManager tlsManagerSettings
    req <- baseReq url
    buffer <- newTVarIO Seq.empty
    signal <- newTVarIO Go
    -- Start consumers
    cs <-
      replicateM concurrency $
        async $
          consume buffer req signal mgr maxBulkSize
    -- Setup producer pipeline
    runConduit $
      CB.sourceHandle stdin
        .| breakByte 0
        .| CL.mapM_ (produce buffer maxBufferSize)
    -- Graceful stop
    drain buffer >> atomically (writeTVar signal Stop) >> mapM_ wait cs
  where
    baseReq url =
      (\req -> req {path = "/_bulk", method = "POST"})
        <$> parseUrlThrow url
    produce b s x = atomically $ do
      xs <- readTVar b
      if Seq.length xs + 1 > s
        then retry
        else writeTVar b (xs |> x)
    consume b r s m i = do
      chunk <-
        atomically $
          readTVar s >>= \case
            Stop -> return Seq.empty
            Go -> do
              (now, later) <- Seq.splitAt i <$> readTVar b
              if Seq.null now
                then retry
                else writeTVar b later >> return now
      unless (Seq.null chunk) $ do
        let body = requestBodySourceChunked (mapM_ yield chunk)
        let req = r {requestBody = body}
        res <- runExceptT . syncIO $ httpLbs req m
        either print (BLC.putStrLn . responseBody) res
        consume b r s m i
    drain b = do
      done <- Seq.null <$> readTVarIO b
      unless done $ do
        threadDelay 1000000
        drain b

-- | Like 'Data.Conduit.Binary.lines', but split on an arbitrary delimiter
breakByte :: Monad m => Word8 -> ConduitT ByteString ByteString m ()
breakByte delim = loop id
  where
    loop front = await >>= maybe (finish front) (go front)
    finish front =
      let final = front BS.empty
       in unless (BS.null final) (yield final)
    go sofar more =
      case BS.uncons second of
        Just (_, second') -> yield (sofar first) >> go id second'
        Nothing ->
          let rest = sofar more
           in loop $ BS.append rest
      where
        (first, second) = BS.break (== delim) more
