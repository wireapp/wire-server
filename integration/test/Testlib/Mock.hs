-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Testlib.Mock (startMockServer, MockServerConfig (..), codensityApp) where

import Control.Arrow ((>>>))
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad.Codensity
import Control.Monad.Reader
import Data.Streaming.Network
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Testlib.Prelude

codensityApp :: (Wai.Request -> Codensity IO Wai.Response) -> Wai.Application
codensityApp f req = runCodensity (f req)

data MockServerConfig = MockServerConfig
  { port :: Maybe Warp.Port,
    tls :: Bool
  }

instance Default MockServerConfig where
  def = MockServerConfig {port = Nothing, tls = False}

spawnServer :: Warp.Settings -> Socket.Socket -> Wai.Application -> App ()
spawnServer wsettings sock app = liftIO $ Warp.runSettingsSocket wsettings sock app

spawnTLSServer :: Warp.Settings -> Socket.Socket -> Wai.Application -> App ()
spawnTLSServer wsettings sock app = do
  (cert, key) <-
    asks do
      servicesCwdBase >>> \case
        Nothing ->
          ( "/etc/wire/federator/secrets/tls.crt",
            "/etc/wire/federator/secrets/tls.key"
          )
        Just base ->
          ( base <> "/federator/test/resources/integration-leaf.pem",
            base <> "/federator/test/resources/integration-leaf-key.pem"
          )
  liftIO $ Warp.runTLSSocket (Warp.tlsSettings cert key) wsettings sock app

startMockServer :: MockServerConfig -> Wai.Application -> Codensity App Warp.Port
startMockServer config app = do
  let closeSocket sock = catch (Socket.close sock) (\(_ :: SomeException) -> pure ())
  (port, sock) <-
    hoistCodensity
      $ Codensity
      $ bracket
        ( case config.port of
            Nothing -> bindRandomPortTCP (fromString "*6")
            Just n -> (n,) <$> bindPortTCP n (fromString "*6")
        )
        (\(_, sock) -> closeSocket sock)
  serverStarted <- liftIO newEmptyMVar
  let wsettings =
        Warp.defaultSettings
          & Warp.setPort port
          & Warp.setGracefulCloseTimeout2 0
          & Warp.setBeforeMainLoop (putMVar serverStarted Nothing)

  -- Action to start server in a separate thread.
  startServer <- lift . appToIO $ (if config.tls then spawnTLSServer else spawnServer) wsettings sock app
  let startServerAsync = do
        a <- async $ do
          catch startServer $ \(e :: SomeException) ->
            void $ tryPutMVar serverStarted (Just e)
        mException <- readMVar serverStarted
        traverse_ throw mException
        pure a

  void
    $ hoistCodensity
    $ Codensity
    $ bracket
      startServerAsync
      ( \serverAsync -> do
          closeSocket sock
          -- kill the thread running the server
          cancel serverAsync
      )

  pure port
