module Testlib.Mock where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Reader
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
  let closeSocket sock = catch (Socket.close sock) (\(_ :: SomeException) -> pure ())
  (port, sock) <- Codensity $ \k -> do
    action <- appToIOKleisli k
    liftIO $
      bracket
        ( case config.port of
            Nothing -> bindRandomPortTCP (fromString "*6")
            Just n -> (n,) <$> bindPortTCP n (fromString "*6")
        )
        (\(_, sock) -> closeSocket sock)
        action
  serverStarted <- liftIO newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0
          & Warp.setBeforeMainLoop (putMVar serverStarted Nothing)

  -- Action to start server in a separate thread.
  base <- asks (fromMaybe "." . ((.servicesCwdBase)))
  let startServer = do
        if config.tls
          then
            Warp.runTLSSocket
              ( Warp.tlsSettings
                  (base <> "/federator/test/resources/integration-leaf.pem")
                  (base <> "/federator/test/resources/integration-leaf-key.pem")
              )
              wsettings
              sock
              app
          else Warp.runSettingsSocket wsettings sock app
  let startServerAsync = do
        a <- async $ do
          catch startServer $ \(e :: SomeException) ->
            void $ tryPutMVar serverStarted (Just e)
        mException <- readMVar serverStarted
        traverse_ throw mException
        pure a

  Codensity $ \k -> do
    action <- appToIO (k ())
    liftIO
      $ bracket
        startServerAsync
        ( \serverAsync -> do
            closeSocket sock
            -- kill the thread running the server
            cancel serverAsync
        )
      $ const action

  pure port
