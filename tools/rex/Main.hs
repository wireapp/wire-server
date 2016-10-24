{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import           Control.Concurrent.Async
import           Control.Monad                    (void)
import           Control.Monad.Catch
import           Control.Monad.Trans
import qualified Data.Attoparsec.ByteString.Char8 as Parser
import           Data.Bifunctor
import           Data.Bitraversable
import qualified Data.ByteString                  as ByteString
import           Data.Char
import           Data.Foldable
import           Data.HashMap.Strict              (HashMap)
import qualified Data.HashMap.Strict              as HashMap
import           Data.Maybe
import           Data.Monoid
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Data.Text.Encoding               (decodeUtf8)
import qualified Data.Text.IO                     as Text
import qualified Data.Text.Read                   as Text
import           Data.Traversable
import           Data.Word
import           Network.BSD                      (getHostName)
import           Network.HTTP.Types
import           Network.Socket                   hiding (recvFrom)
import           Network.Socket.ByteString
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Options.Applicative
import           System.Clock
import qualified System.Logger                    as Log
import           System.Logger.Message            (msg, val)

-- this library sucks
import           System.Metrics.Prometheus.Concurrent.RegistryT
import           System.Metrics.Prometheus.Encode
import qualified System.Metrics.Prometheus.Metric.Counter       as Counter
import           System.Metrics.Prometheus.Metric.Gauge         (Gauge)
import qualified System.Metrics.Prometheus.Metric.Gauge         as Gauge
import qualified System.Metrics.Prometheus.Metric.Histogram     as Histo
import           System.Metrics.Prometheus.MetricId
import           System.Metrics.Prometheus.Registry             (RegistrySample)


data Opts = Opts
    { optExposePort           :: !Word16
    , optRestundUDPStatusPort :: !Word16
    , optRestundUDPListenPort :: !Word16
    , optTier                 :: !Text
    } deriving Show

parseOpts :: ParserInfo Opts
parseOpts = info (helper <*> parser) desc
  where
    desc = header "restund Metrics Exporter" <> fullDesc

    parser = Opts
        <$> option auto
                ( short   'p'
               <> long    "port"
               <> metavar "PORT"
               <> help    "Expose metrics on this port"
               <> value   9200
               <> showDefault
                )
        <*> option auto
                ( long    "restund-udp-status-port"
               <> metavar "PORT"
               <> help    "UDP Status Port"
               <> value   33000
               <> showDefault
                )
        <*> option auto
                ( long    "restund-udp-listen-port"
               <> metavar "PORT"
               <> help    "UDP Listen Port (aka STUN port aka Management port)"
               <> value   3478
               <> showDefault
                )
        <*> option txt
                ( long  "tier"
               <> help  "Deployment Tier"
                )

    txt = Text.pack <$> str

main :: IO ()
main = withSocketsDo $ do
    opts <- execParser parseOpts
    host <- getHostName
    lgr  <- Log.new Log.defSettings

    let labels = fromList [ ("host", Text.pack host)
                          , ("tier", optTier opts)
                          , ("app", "restund")
                          , ("srv", "rex")
                          ]

    runRegistryT $ do
        known    <- knownStats labels
        unknown  <- registerCounter "UNKNOWN"      labels
        rxq      <- registerGauge   "recv_queue"   labels
        txq      <- registerGauge   "send_queue"   labels
        drp      <- registerGauge   "packet_drops" labels

        timing   <- registerHistogram "scrape_timing_ns"
                                      labels
                                      [ 500000 -- .5 ms
                                      , 1000000 -- 1 ms
                                      , 5000000 -- 5 ms
                                      ]
        errors   <- registerCounter   "scrape_errors" labels

        sampleIO <- sample

        liftIO . serveIO opts
               . handleAll (\e -> logError lgr errors e >> throwM e) $ do
            start <- getTime Monotonic

            void $ concurrently
                (handleIOError (logError lgr errors) $ do
                    Log.info lgr $ msg (val "Scraping Socket Stats...")
                    sockStats <- getSocketStats (optRestundUDPListenPort opts)
                    for_ sockStats $ \SocketStats{..} -> do
                        Gauge.set (fromIntegral rxQueue) rxq
                        Gauge.set (fromIntegral txQueue) txq
                        Gauge.set (fromIntegral drops  ) drp
                )
                (handleIOError (logError lgr errors) $ do
                    Log.info lgr $ msg (val "Scraping App Stats...")
                    withSocket $ \ssock -> do
                        appStats <- getAppStats (statusAddr opts) ssock
                        for_ appStats $ \(k,v) ->
                            maybe (Counter.inc unknown)
                                  (Gauge.set v)
                                  (HashMap.lookup k known)
                )

            took <- toNanoSecs . (`diffTimeSpec` start) <$> getTime Monotonic
            Log.debug lgr $ msg ("Scraping took: " <> show took)
            Histo.observe (fromIntegral took) timing

            sampleIO
  where
    localhost = tupleToHostAddress (127, 0, 0, 1)

    statusAddr Opts{optRestundUDPStatusPort=p}
        = SockAddrInet (fromIntegral p) localhost

    logError lgr cnt e = do
        Log.err lgr $ msg (show e)
        Counter.inc cnt

    withSocket = bracket
        (socket AF_INET Datagram defaultProtocol)
        close


data SocketStats = SocketStats
    { lPort   :: !Word16
    , rxQueue :: Word64
    , txQueue :: Word64
    , drops   :: Word64
    }

-- nb. that this requires restund and rex to run in the same network namespace
-- (ie. either both run with --net=host, or both run in the same pod)
getSocketStats :: Word16 -> IO (Maybe SocketStats)
getSocketStats port = do
    pnu <- Text.readFile "/proc/net/udp"
    return
        . listToMaybe
        . filter ((== port) . lPort)
        . map (mk . Text.words)
        . drop 1
        $ Text.lines pnu
  where
    -- sl local_address rem_address st tx_queue:rx_queue tr:tm->when retrnsmt uid timeout inode ref pointer drops
    mk [_, la, _, _, qs, _, _, _, _, _, _, _, ds] =
        let p       = hex . snd . Text.breakOnEnd ":" $ la
            (rx,tx) = bimap hex hex . Text.breakOn ":" $ qs
            d       = either (const 0) fst . Text.decimal $ ds
         in SocketStats p rx tx d

    hex :: (Integral a, Bounded a) => Text -> a
    hex = either (const minBound) fst . Text.hexadecimal

getAppStats :: SockAddr -> Socket -> IO [(Text, Double)]
getAppStats addr sock = fmap mconcat . for cmds $ \cmd -> do
    sendAllTo sock cmd addr
    (reply,_) <- recvFrom sock 1024
    return
        . mapMaybe ( bitraverse Just id
                   . bimap decodeUtf8
                           ( either (const Nothing) Just
                           . Parser.parseOnly Parser.double
                           )
                   . ByteString.break (== spc)
                   )
        . ByteString.split nl
        $ reply
  where
    spc = fromIntegral (ord ' ')
    nl  = fromIntegral (ord '\n')

    cmds = [ "stat", "turnstats", "turnreply", "tcpstats", "authstats" ]

knownStats :: Labels -> RegistryT IO (HashMap Text Gauge)
knownStats def = HashMap.fromList <$> traverse (mk def)
    [ -- stat
      "binding_req"
    , "allocate_req"
    , "refresh_req"
    , "createperm_req"
    , "chanbind_req"
    , "unknown_req"
      -- turnstats
    , "allocs_cur"
    , "allocs_tot"
    , "bytes_tx"
    , "bytes_rx"
    , "bytes_tot"
      -- turnreply
    , "scode_400"
    , "scode_420"
    , "scode_437"
    , "scode_440"
    , "scode_441"
    , "scode_442"
    , "scode_443"
    , "scode_500"
    , "scode_508"
      -- tcpstats
    , "tcp_connections"
    , "tls_connections"
      -- authstats
    , "auth_req_mi"
    , "auth_req_no_mi"
    ]
  where
    mk :: Labels -> Name -> RegistryT IO (Text, Gauge)
    mk labels name = do
        g <- registerGauge name labels
        pure (unName name, g)



serveIO :: MonadIO m => Opts -> IO RegistrySample -> m ()
serveIO opts runSample = liftIO $
    runSettings ( setPort (fromIntegral (optExposePort opts))
                . setHost "127.0.0.1"
                $ defaultSettings
                )
                (app runSample)

app :: IO RegistrySample -> Application
app runSample req respond = case pathInfo req of
    ("metrics":_) -> runSample >>= respond . r200
    _             -> respond r404
  where
    r200 = responseBuilder status200 hdrs . encodeMetrics
    r404 = responseLBS status404 hdrs mempty
    hdrs = [(hContentType, "text/plain")]
