{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use shutdown" #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Main (main) where

import Control.Concurrent.Async
import Control.Monad.Catch
import Control.Monad.Trans
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import Data.Bifunctor
import Data.Bitraversable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import Data.Foldable
import Data.Functor (($>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IP
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import Data.Traversable
import Data.Word
import Network.DNS hiding (header)
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Applicative
import System.Clock
import qualified System.Logger as Log
import System.Logger.Message (msg, val)
import System.Metrics.Prometheus.Concurrent.RegistryT -- this library sucks
import System.Metrics.Prometheus.Encode.Text
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import System.Metrics.Prometheus.Metric.Gauge (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge as Gauge
import qualified System.Metrics.Prometheus.Metric.Histogram as Histo
import System.Metrics.Prometheus.MetricId
import System.Metrics.Prometheus.Registry (RegistrySample)
import System.Timeout (timeout)

data Opts = Opts
  { optExposePort :: !Word16,
    optRestundUDPStatusPort :: !Word16,
    optRestundUDPListenPort :: !Word16,
    optTier :: !Text,
    optZone :: !ByteString
  }
  deriving (Show)

parseOpts :: ParserInfo Opts
parseOpts = info (helper <*> parser) desc
  where
    desc = header "restund Metrics Exporter" <> fullDesc

    parser =
      Opts
        <$> option
          auto
          ( short 'p'
              <> long "port"
              <> metavar "PORT"
              <> help "Expose metrics on this port"
              <> value 9200
              <> showDefault
          )
        <*> option
          auto
          ( long "restund-udp-status-port"
              <> metavar "PORT"
              <> help "UDP Status Port"
              <> value 33000
              <> showDefault
          )
        <*> option
          auto
          ( long "restund-udp-listen-port"
              <> metavar "PORT"
              <> help "UDP Listen Port (aka STUN port aka Management port)"
              <> value 3478
              <> showDefault
          )
        <*> option
          txt
          ( long "tier"
              <> help "Deployment Tier"
          )
        <*> option
          bs
          ( long "zone"
              <> value "wire.com"
              <> help "Deployment Zone"
              <> showDefault
          )

    txt = Text.pack <$> str
    bs = ByteString.pack <$> str

main :: IO ()
main = withSocketsDo $ do
  opts <- execParser parseOpts
  lgr <- Log.new Log.defSettings
  rlv <- makeResolvSeed defaultResolvConf

  let labels =
        fromList
          [ ("tier", optTier opts),
            ("app", "restund"),
            ("srv", "rex")
          ]
      dns =
        mconcat
          [ "_turn._tcp.",
            encodeUtf8 (optTier opts),
            ".",
            optZone opts,
            "."
          ]

  runRegistryT $ do
    known <- knownStats labels
    unknown <- registerCounter "UNKNOWN" labels
    rxq <- registerGauge "recv_queue" labels
    txq <- registerGauge "send_queue" labels
    drp <- registerGauge "packet_drops" labels
    peers <- registerGauge "known_peers" labels
    rpeers <- registerGauge "reachable_peers" labels

    timing <-
      registerHistogram
        "scrape_timing_ns"
        labels
        [ 500000, -- .5 ms
          1000000, -- 1 ms
          5000000 -- 5 ms
        ]

    sampleIO <- sample

    liftIO . serveIO opts $ do
      Log.info lgr $ msg (val "Scraping ...")
      start <- getTime Monotonic

      (!_, !_, !_) <-
        runConcurrently $
          (,,)
            <$> Concurrently
              ( do
                  sockStats <- getSocketStats (optRestundUDPListenPort opts)
                  Log.trace lgr $ msg (show sockStats)
                  for_ sockStats $ \SocketStats {..} -> do
                    Gauge.set (fromIntegral rxQueue) rxq
                    Gauge.set (fromIntegral txQueue) txq
                    Gauge.set (fromIntegral drops) drp
              )
            <*> Concurrently
              ( withSocket $ \ssock -> do
                  appStats <- getAppStats lgr (statusAddr opts) ssock
                  Log.trace lgr $ msg (show appStats)
                  for_ appStats $ \(k, v) ->
                    maybe
                      (Counter.inc unknown)
                      (Gauge.set v)
                      (HashMap.lookup k known)
              )
            <*> Concurrently
              ( do
                  peerStats <- getPeerConnectivityStats lgr rlv dns
                  Log.trace lgr $ msg (show peerStats)
                  Gauge.set (fromIntegral (peersDiscovered peerStats)) peers
                  Gauge.set (fromIntegral (peersReachable peerStats)) rpeers
              )

      !took <- toNanoSecs . (`diffTimeSpec` start) <$> getTime Monotonic
      Log.info lgr $ msg ("Done scaping in " <> show took <> "ns")
      Histo.observe (fromIntegral took) timing

      sampleIO
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)

    statusAddr Opts {optRestundUDPStatusPort = p} =
      SockAddrInet (fromIntegral p) localhost

    withSocket =
      bracket
        (socket AF_INET Datagram defaultProtocol)
        close

data SocketStats = SocketStats
  { lPort :: !Word16,
    rxQueue :: Word64,
    txQueue :: Word64,
    drops :: Word64
  }
  deriving (Show)

-- nb. that this requires restund and rex to run in the same network namespace
-- (ie. either both run with --net=host, or both run in the same pod)
getSocketStats :: Word16 -> IO (Maybe SocketStats)
getSocketStats port = do
  pnu <- Text.readFile "/proc/net/udp"
  pure
    . find ((== port) . lPort)
    . map (mk . Text.words)
    . drop 1
    $ Text.lines pnu
  where
    -- sl local_address rem_address st tx_queue:rx_queue tr:tm->when retrnsmt uid timeout inode ref pointer drops
    mk [_, la, _, _, qs, _, _, _, _, _, _, _, ds] =
      let p = hex . snd . Text.breakOnEnd ":" $ la
          (rx, tx) = bimap hex hex . Text.breakOn ":" $ qs
          d = either (const 0) fst . Text.decimal $ ds
       in SocketStats p rx tx d

    hex :: (Integral a, Bounded a) => Text -> a
    hex = either (const minBound) fst . Text.hexadecimal

getAppStats :: Log.Logger -> SockAddr -> Socket -> IO [(Text, Double)]
getAppStats lgr addr sock = fmap mconcat . for cmds $ \cmd -> do
  sendAllTo sock cmd addr
  (reply, _) <- recvFrom sock 1024
  Log.trace lgr $ msg (ByteString.intercalate "\n" (ByteString.lines reply))
  pure $
    parseAppStats reply
  where
    cmds = ["stat", "turnstats", "turnreply", "tcpstats", "authstats"]

parseAppStats :: ByteString -> [(Text, Double)]
parseAppStats =
  mapMaybe
    ( bitraverse Just id
        . bimap
          decodeUtf8
          ( either (const Nothing) Just
              . Parser.parseOnly
                (Parser.skipWhile Parser.isSpace *> Parser.double)
          )
        . ByteString.break (== ' ')
    )
    . ByteString.lines

knownStats :: Labels -> RegistryT IO (HashMap Text Gauge)
knownStats def =
  HashMap.fromList
    <$> traverse
      (mk def)
      [ -- stat
        "binding_req",
        "allocate_req",
        "refresh_req",
        "createperm_req",
        "chanbind_req",
        "unknown_req",
        -- turnstats
        "allocs_cur",
        "allocs_tot",
        "bytes_tx",
        "bytes_rx",
        "bytes_tot",
        -- turnreply
        "scode_400",
        "scode_420",
        "scode_437",
        "scode_440",
        "scode_441",
        "scode_442",
        "scode_443",
        "scode_500",
        "scode_508",
        -- tcpstats
        "tcp_connections",
        "tls_connections",
        -- authstats
        "auth_req_mi",
        "auth_req_no_mi"
      ]
  where
    mk :: Labels -> Name -> RegistryT IO (Text, Gauge)
    mk labels name = do
      g <- registerGauge name labels
      pure (unName name, g)

data PeerConnectivityStats = PeerConnectivityStats
  { peersDiscovered :: Int,
    peersReachable :: Int
  }
  deriving (Show)

getPeerConnectivityStats ::
  Log.Logger ->
  ResolvSeed ->
  Domain ->
  IO PeerConnectivityStats
getPeerConnectivityStats lgr seed dom = do
  addrs <- disco
  reach <- length . catMaybes <$> mapConcurrently shakehands addrs
  pure
    PeerConnectivityStats
      { peersDiscovered = length addrs,
        peersReachable = reach
      }
  where
    disco = withResolver seed $ \rlv ->
      lookupSRV rlv dom
        >>= either
          (const $ pure [])
          ( \xs ->
              concatMap mkAddr . zip xs
                <$> traverse (lookupA rlv . _4) xs
          )

    shakehands (addr, port) =
      handleIOError (\e -> logUnreachable addr port e $> Nothing)
        . timeout (5 * 1000000)
        $ bracket
          (socket AF_INET Stream defaultProtocol)
          close
          (`connect` SockAddrInet (fromIntegral port) (toHostAddress addr))

    mkAddr (_, Left _) = mempty
    mkAddr (rr, Right ips) = (,_3 rr) <$> ips

    _4 (_, _, _, x) = x
    _3 (_, _, x, _) = x

    logUnreachable addr port e =
      Log.warn lgr . msg $
        "Peer " <> show addr <> ":" <> show port <> " unreachable: " <> show e

serveIO :: (MonadIO m) => Opts -> IO RegistrySample -> m ()
serveIO opts runSample =
  liftIO $
    runSettings
      ( setPort (fromIntegral (optExposePort opts))
          . setHost "127.0.0.1"
          $ defaultSettings
      )
      (app runSample)

app :: IO RegistrySample -> Application
app runSample req respond = case pathInfo req of
  ("metrics" : _) -> runSample >>= respond . r200
  _ -> respond r404
  where
    r200 = responseBuilder status200 hdrs . encodeMetrics
    r404 = responseLBS status404 hdrs mempty
    hdrs = [(hContentType, "text/plain")]
