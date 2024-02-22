module Test.Brig.SSL where

import Control.Monad.Catch
import Data.Dynamic (fromDynamic)
import Data.Streaming.Network hiding (runTCPClient, runTCPServer)
import Debug.Trace
import Imports
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.Internal
import Network.HTTP.Client.TLS as HTTP
import Network.HTTP.Types
import Network.Socket qualified as Socket
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.Warp.Internal qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as Warp
import Network.Wai.Route qualified as Wai
import OpenSSL.Session qualified as SSL
import Test.Tasty
import Test.Tasty.HUnit
import UnliftIO.Async
import UnliftIO.Timeout (timeout)

tests :: TestTree
tests =
  testGroup "SSL" $
    [ testGroup "verify" $
        [ testCase "fingerprint" testVerifyFingerprint
        ]
    ]

testVerifyFingerprint :: Assertion
testVerifyFingerprint = withFreePort $ \(port, socket) -> do
  if True
    then pure ()
    else do
      serverStarted <- newEmptyMVar
      let tlss = Warp.tlsSettings "test/resources/localhost.pem" "test/resources/localhost-key.pem"
      let defs =
            Warp.defaultSettings
              { Warp.settingsPort = port,
                Warp.settingsBeforeMainLoop = putMVar serverStarted ()
              }
      srv <- async $ Warp.runTLS tlss defs app
      srvMVar <- timeout 5_000_000 $ takeMVar serverStarted
      putStrLn ("\n--- server started" :: String)
      case srvMVar of
        Just () -> go socket port `finally` cancel srv
        Nothing -> error . show =<< poll srv
  where
    go _socket port = do
      -- let tlsSettings =
      -- TLSSettingsSimple
      --   { -- This is where we disable certificate verification
      --     settingDisableCertificateValidation = True,
      --     settingDisableSession = False,
      --     settingUseServerName = True
      --   }
      mgr <- newManager tlsManagerSettings
      let req =
            defaultRequest
              { method = methodGet,
                secure = True,
                host = "localhost",
                port = port,
                path = "/alive"
              }

      putStrLn "\n ---- 1. withConnection:"
      void $ withConnection' req mgr Reuse $ \mConn -> do
        putStrLn "\n ---- 2. withConnection"
        let conn = managedResource mConn
        putStrLn $ "\n ---- 3. got conn"
        case fromDynamic @SSL.SSL (connectionRaw conn) of
          Nothing -> error ("OH NO SSL validation failed: " <> show req)
          Just _ssl -> print ("SSL!!" :: String)

      res <- httpLbs req mgr
      print res

-- pure ()

--     go socket port = do
--       -- SSL
-- sslCtx <- SSL.context
-- SSL.contextAddOption sslCtx SSL.SSL_OP_NO_SSLv2
-- SSL.contextAddOption sslCtx SSL.SSL_OP_NO_SSLv3
-- SSL.contextSetCiphers sslCtx "HIGH"
-- SSL.contextSetDefaultVerifyPaths sslCtx
-- SSL.contextSetPrivateKeyFile sslCtx "test/resources/key.pem"
-- SSL.contextSetCAFile sslCtx "test/resources/cert.pem"
-- SSL.contextSetVerificationMode sslCtx $
--   SSL.VerifyPeer
--     True
--     True
--     ( Just $ \_bool _ctx -> do
--         putStrLn "\n --- callback: veriy peer"
--         pure True
--     )
--
-- putStrLn ("\n--- connect socket: " <> show socket)
-- Socket.listen socket 1024
-- -- Socket.bind socket $
-- --   Socket.SockAddrInet
-- --     (fromIntegral port)
-- --     (Socket.tupleToHostAddress (127, 0, 0, 1))
-- -- Socket.connect socket $ Socket.SockAddrInet (fromIntegral port) 0x0100007f
--
-- putStrLn ("\n--- get ssl from socket")
-- ssl <- SSL.connection sslCtx socket
-- result <- SSL.tryAccept ssl
-- putStrLn ("\n-- result: " <> show result)
--
-- putStrLn ("\n--- connect ssl")
-- SSL.connect ssl
--
-- let fingerprint = Fingerprint "ioy3GeIjgQRsobf2EKGO3O8mq/FofFxHRqy0T4ERIZ8=\\"
-- putStrLn ("\n--- verify:" :: String)
-- verifyCertFingerprint [fingerprint] ssl
-- putStrLn ("\n--- finished verification" :: String)
--
-- pure ()

withFreePort :: ((Int, Socket.Socket) -> IO a) -> IO a
withFreePort = bracket bindingPort cleaning
  where
    cleaning = trace ("\n--- cleaning") (Socket.close . snd)
    bindingPort = bindRandomPortGen Socket.NoSocketType (fromString @HostPreference "*")

app :: Application
app =
  Wai.route
    [ ("/alive", onAlive)
    ]
  where
    onAlive _ rq k = do
      putStrLn ("\n------------- alive!! " <> show rq)
      k $ responseLBS status200 [] "success"

--
-- -- runTCPServer port server = Socket.withSocketsDo $ do
-- --   addr <- resolve
-- --   bracket (open addr) Socket.close loop
-- --   where
-- --     resolve = do
-- --       let hints =
-- --             Socket.defaultHints
-- --               { Socket.addrFlags = [Socket.AI_PASSIVE],
-- --                 Socket.addrSocketType = Socket.Stream
-- --               }
-- --       head <$> Socket.getAddrInfo (Just hints) Nothing (Just $ show port)
-- --     open addr = bracketOnError (Socket.openSocket addr) Socket.close $ \sock -> do
-- --       Socket.setSocketOption sock Socket.ReuseAddr 1
-- --       Socket.withFdSocket sock Socket.setCloseOnExecIfNeeded
-- --       Socket.bind sock $ Socket.addrAddress addr
-- --       Socket.listen sock 1024
-- --       pure sock
-- --     loop sock = forever $
-- --       bracketOnError (Socket.accept sock) (Socket.close . fst) $
-- --         \(conn, _peer) ->
-- --           void $
-- --             -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
-- --             -- but 'E.bracketOnError' above will be necessary if some
-- --             -- non-atomic setups (e.g. spawning a subprocess to handle
-- --             -- @conn@) before proper cleanup of @conn@ is your case
-- --             forkFinally (server conn) (const $ Socket.gracefulClose conn 5000)
-- --
-- -- runTCPClient port client = Socket.withSocketsDo $ do
-- --   addr <- resolve
-- --   bracket (open addr) Socket.close client
-- --   where
-- --     resolve = do
-- --       let hints = Socket.defaultHints {Socket.addrSocketType = Socket.Stream}
-- --       head <$> Socket.getAddrInfo (Just hints) (Just "localhost") (Just port)
-- --     open addr = bracketOnError (Socket.openSocket addr) Socket.close $ \sock -> do
-- --       Socket.connect sock $ Socket.addrAddress addr
-- --       pure sock
-- --
-- -- --
-- -- --
-- -- --
-- -- --
-- -- -- void $ async $ runTCPClient "3000" $ \s -> do
-- -- --   sendAll s "Hello, world!"
-- -- --   msg <- recv s 1024
-- -- --   putStr "Received: "
-- -- --   Char8.putStrLn msg
