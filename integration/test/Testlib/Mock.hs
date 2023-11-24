module Testlib.Mock where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Codensity
import Data.Streaming.Network
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Testlib.Prelude

codensityApp :: (Wai.Request -> Codensity IO Wai.Response) -> Wai.Application
codensityApp f req = runCodensity (f req)

data MockServerConfig = MockServerConfig
  { port :: Maybe Warp.Port,
    tls :: Bool
  }

instance Default MockServerConfig where
  def = MockServerConfig {port = Nothing, tls = False}

startMockServer :: MockServerConfig -> Wai.Application -> Codensity App Warp.Port
startMockServer config app = do
  (port, sock) <- liftIO $ case config.port of
    Nothing -> bindRandomPortTCP (fromString "*6")
    Just n -> (n,) <$> bindPortTCP n (fromString "*6")
  serverStarted <- liftIO newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0
          & Warp.setBeforeMainLoop (putMVar serverStarted ())

  -- Start server in a separate thread. Here it's fine to fire and forget,
  -- because the server is linked to the socket, and closing the socket will
  -- shutdown the server.
  void . liftIO . async $
    if config.tls
      then
        Warp.runTLSSocket
          ( Warp.tlsSettings
              "services/federator/test/resources/integration-leaf.pem"
              "services/federator/test/resources/integration-leaf-key.pem"
          )
          wsettings
          sock
          app
      else Warp.runSettingsSocket wsettings sock app

  Codensity $ \k -> do
    action <- appToIO (k ())
    liftIO
      $ bracket
        (readMVar serverStarted)
        ( \_ ->
            catch (Socket.close sock) (\(_ :: SomeException) -> pure ())
        )
      $ const action

  pure port
