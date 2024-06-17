module Util where

import Bilge qualified
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race_)
import Control.Exception qualified as E
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Codensity
import Data.ByteString qualified as S
import Data.Text qualified as Text
import Gundeck.Env (createEnv)
import Gundeck.Options
import Gundeck.Run (mkApp)
import Imports
import Network.Socket hiding (openSocket)
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai.Utilities.MockServer (withMockServer)
import TestSetup

withSettingsOverrides :: (Opts -> Opts) -> TestM a -> TestM a
withSettingsOverrides f action = do
  ts <- ask
  let opts = f (view tsOpts ts)
  (_rThreads, env) <- liftIO $ createEnv opts
  liftIO . lowerCodensity $ do
    let app = mkApp env
    p <- withMockServer app
    liftIO $ Bilge.runHttpT (ts ^. tsManager) $ runReaderT (runTestM action) $ ts & tsGundeck .~ GundeckR (mkRequest p)
  where
    mkRequest p = Bilge.host "127.0.0.1" . Bilge.port p

withEnvOverrides :: forall m a. (MonadIO m, MonadMask m) => [(String, String)] -> m a -> m a
withEnvOverrides envOverrides action = do
  bracket (setEnvVars envOverrides) (resetEnvVars) $ const action
  where
    setEnvVars :: [(String, String)] -> m [(String, Maybe String)]
    setEnvVars newVars = liftIO $ do
      oldVars <- mapM (\(k, _) -> (k,) <$> lookupEnv k) newVars
      mapM_ (uncurry setEnv) newVars
      pure oldVars

    resetEnvVars :: [(String, Maybe String)] -> m ()
    resetEnvVars =
      mapM_ (\(k, mV) -> maybe (unsetEnv k) (setEnv k) mV)

runRedisProxy :: Text -> Word16 -> Word16 -> IO ()
runRedisProxy redisHost redisPort proxyPort = do
  (servAddr : _) <- getAddrInfo Nothing (Just $ Text.unpack redisHost) (Just $ show redisPort)
  runTCPServer Nothing (show proxyPort) $ \client -> do
    server <- getServerSocket servAddr
    client <~~> server
  where
    getServerSocket servAddr = do
      server <- socket (addrFamily servAddr) Stream defaultProtocol
      connect server (addrAddress servAddr) >> pure server
    p1 <~~> p2 = finally (race_ (p1 `mapData` p2) (p2 `mapData` p1)) (close p1 >> close p2)
    mapData f t = do
      content <- recv f 4096
      unless (S.null content) $ sendAll t content >> mapData f t

-- Forked from network-run, added logic to cleanup clients when server is closed

-- | Running a TCP server with an accepted socket and its peer name.
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO b
runTCPServer mhost port' server = withSocketsDo $ do
  addr <- resolve Stream mhost port' True
  clientThreads <- newTVarIO []
  E.bracket (open addr) (cleanupClients clientThreads) (loop clientThreads)
  where
    open addr = E.bracketOnError (openServerSocket addr) close $ \sock -> do
      listen sock 1024
      pure sock
    loop clientThreads sock = forever $ do
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) -> do
          thread <- forkFinally (server conn) (const $ gracefulClose conn 5000)
          atomically $ modifyTVar clientThreads (thread :)
    cleanupClients :: TVar [ThreadId] -> Socket -> IO ()
    cleanupClients clientThreads sock = do
      close sock
      mapM_ killThread =<< readTVarIO clientThreads

resolve :: SocketType -> Maybe HostName -> ServiceName -> Bool -> IO AddrInfo
resolve socketType mhost port' passive =
  head <$> getAddrInfo (Just hints) mhost (Just port')
  where
    hints =
      defaultHints
        { addrSocketType = socketType,
          addrFlags = [AI_PASSIVE | passive]
        }

openServerSocket :: AddrInfo -> IO Socket
openServerSocket addr = E.bracketOnError (openSocket addr) close $ \sock -> do
  setSocketOption sock ReuseAddr 1
  withFdSocket sock $ setCloseOnExecIfNeeded
  bind sock $ addrAddress addr
  pure sock

openSocket :: AddrInfo -> IO Socket
openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
