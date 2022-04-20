module Util where

import qualified Bilge
import Control.Concurrent.Async (race_)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Codensity
import qualified Data.ByteString as S
import Data.Metrics.Middleware (metrics)
import qualified Data.Text as Text
import Gundeck.Env (createEnv)
import Gundeck.Options
import Gundeck.Run (mkApp)
import Imports
import Network.Run.TCP (runTCPServer)
import Network.Socket hiding (listen)
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai.Utilities.MockServer (withMockServer)
import TestSetup

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
    getServerSocket servAddr = do
      server <- socket (addrFamily servAddr) Stream defaultProtocol
      connect server (addrAddress servAddr) >> return server
    p1 <~~> p2 = finally (race_ (p1 `mapData` p2) (p2 `mapData` p1)) (close p1 >> close p2)
    mapData f t = do
      content <- recv f 4096
      unless (S.null content) $ sendAll t content >> mapData f t
