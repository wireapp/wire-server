{-# LANGUAGE NumericUnderscores #-}

module Util where

import qualified Bilge
import Control.Concurrent.Async (race_)
import qualified Control.Concurrent.Async as Async
import Control.Exception (throw)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Codensity
import qualified Data.ByteString as S
import Data.Metrics.Middleware (metrics)
import Data.Streaming.Network (bindRandomPortTCP)
import qualified Data.Text as Text
import Gundeck.Env (createEnv)
import Gundeck.Options
import Gundeck.Run (mkApp)
import Imports
import Network.Run.TCP (runTCPServer)
import Network.Socket hiding (listen)
import Network.Socket.ByteString (recv, sendAll)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified System.Timeout as System
import TestSetup

withMockServer :: Wai.Application -> Codensity IO Word16
withMockServer app = Codensity $ \k ->
  bracket
    (liftIO $ startMockServer Nothing app)
    (liftIO . fst)
    (k . fromIntegral . snd)

withSettingsOverrides :: (Opts -> Opts) -> TestM a -> TestM a
withSettingsOverrides f action = do
  ts <- ask
  let opts = f (view tsOpts ts)
  m <- metrics
  env <- liftIO $ createEnv m opts
  liftIO . lowerCodensity $ do
    let app = mkApp env
    p <- withMockServer app
    liftIO $ Bilge.runHttpT (ts ^. tsManager) $ runReaderT (runTestM action) $ ts & tsGundeck .~ GundeckR (mkRequest p)
  where
    mkRequest p = Bilge.host "127.0.0.1" . Bilge.port p

-- TODO: Copy the comment from federator, perhaps create a new package or put it in one of the shared ones?
startMockServer :: Maybe Warp.TLSSettings -> Wai.Application -> IO (IO (), Warp.Port)
startMockServer mtlsSettings app = do
  (port, sock) <- bindRandomPortTCP "*6"
  serverStarted <- newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0 -- Defaults to 2 seconds, causes server stop to take very long
          & Warp.setBeforeMainLoop (putMVar serverStarted ())

  serverThread <- Async.async $ case mtlsSettings of
    Just tlsSettings -> Warp.runTLSSocket tlsSettings wsettings sock app
    Nothing -> Warp.runSettingsSocket wsettings sock app
  serverStartedSignal <- System.timeout 10_000_000 (readMVar serverStarted)
  let closeMock = do
        me <- Async.poll serverThread
        case me of
          Nothing -> Async.cancel serverThread
          Just (Left e) -> throw e
          Just (Right a) -> pure a
  case serverStartedSignal of
    Nothing -> do
      Async.cancel serverThread
      throw (MockTimeout port)
    Just _ -> pure (closeMock, port)

-- | Thrown in IO by mock federator if the server could not be started after 10
-- seconds.
newtype MockTimeout = MockTimeout Warp.Port
  deriving (Eq, Show, Typeable)

instance Exception MockTimeout

--- TCP Proxy

-- data ProxySetting = ProxySetting {locPort :: PortNumber, remHost :: String, remPort :: String}

-- proxySetting :: ProxySetting
-- proxySetting = ProxySetting 9900 "ftp.free.fr" "80"

runRedisProxy :: Text -> Word16 -> Word16 -> IO ()
runRedisProxy redisHost redisPort proxyPort = do
  (servAddr : _) <- getAddrInfo Nothing (Just $ Text.unpack redisHost) (Just $ show redisPort)
  runTCPServer (Just "localhost") (show proxyPort) $ \listener ->
    forever $
      accept listener >>= \(client, _) ->
        void $
          forkIO $ do
            server <- getServerSocket servAddr
            client <~~> server
  where
    -- installHandler sigPIPE Ignore Nothing >> do
    --   (servAddr : _) <- getAddrInfo Nothing (Just $ remHost proxySetting) (Just $ remPort proxySetting)
    --   withSocketsDo $ do
    --     listener <- open serverAddr
    --     forever $
    --       accept listener >>= \(client, _) ->
    --         void $
    --           forkIO $ do
    --             server <- getServerSocket servAddr
    --             client <~~> server

    getServerSocket servAddr = do
      server <- socket (addrFamily servAddr) Stream defaultProtocol
      connect server (addrAddress servAddr) >> return server
    p1 <~~> p2 = finally (race_ (p1 `mapData` p2) (p2 `mapData` p1)) (close p1 >> close p2)
    mapData f t = do
      content <- recv f 4096
      unless (S.null content) $ sendAll t content >> mapData f t

--   open addr = do
--     sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
--     setSocketOption sock ReuseAddr 1
--     withFdSocket sock $ setCloseOnExecIfNeeded
--     bind sock $ addrAddress addr
--     listen sock 1024
--     return sock
