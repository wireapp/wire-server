module Test.NginxZAuthModule where

import API.Brig
import API.Common
import Control.Monad.Codensity
import Control.Monad.Reader
import qualified Data.ByteString as BS
import Data.List.Extra
import Data.Streaming.Network
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import Network.Socket (Socket)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import SetupHelpers
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp
import System.Posix
import System.Process (getPid)
import Testlib.Prelude
import Text.RawString.QQ
import UnliftIO (bracket)
import UnliftIO.Async (async, waitBoth)
import qualified UnliftIO.Async as Async
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Timeout (timeout)

-- Happy flow: login yields a valid zauth token.
--
-- This test uses `withTestNginz` which responds with the user id and time stamp from the
-- token instead of proxying anywhere.  See also: 'testBearerToken2'
testBearerToken :: (HasCallStack) => App ()
testBearerToken = do
  runCodensity withTestNginz $ \port -> do
    alice <- randomUser OwnDomain def
    email <- asString $ alice %. "email"
    loginResp <- login alice email defPassword >>= getJSON 200
    token <- asString $ loginResp %. "access_token"

    req0 <- HTTP.parseRequest "http://localhost"
    let req =
          req0
            { HTTP.port = port,
              HTTP.requestHeaders = [(hAuthorization, fromString $ "Bearer " <> token)]
            }
    submit "GET" req `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "user" `shouldMatch` (alice %. "qualified_id.id")
      resp.json %. "timestamp" `shouldNotMatch` ""

-- Happy flow (zauth token encoded in AWS4_HMAC_SHA256)
--
-- This test uses `withTestNginz` which responds with the user id and time stamp from the
-- token instead of proxying anywhere.  See also: 'testAWS4_HMAC_SHA256_token2'
testAWS4_HMAC_SHA256_token :: (HasCallStack) => App ()
testAWS4_HMAC_SHA256_token = do
  runCodensity withTestNginz $ \port -> do
    alice <- randomUser OwnDomain def
    email <- asString $ alice %. "email"
    loginResp <- login alice email defPassword >>= getJSON 200
    token <- asString $ loginResp %. "access_token"

    req0 <- HTTP.parseRequest "http://localhost"

    let mkReq authHeader =
          req0
            { HTTP.port = port,
              HTTP.requestHeaders = [(hAuthorization, authHeader)]
            }
        testCases =
          [ (True, fromString $ "AWS4-HMAC-SHA256 Credential=" <> token <> ", foo=bar"),
            (True, fromString $ "AWS4-HMAC-SHA256 Credential=" <> token),
            (True, fromString $ "AWS4-HMAC-SHA256 foo=bar, Credential=" <> token),
            (True, fromString $ "AWS4-HMAC-SHA256 foo=bar, Credential=" <> token <> ", baz=qux"),
            (True, fromString $ "AWS4-HMAC-SHA256 foo=bar,Credential=" <> token <> ",baz=qux"),
            (False, fromString $ "AWS4-HMAC-SHA256 Credential=bad")
          ]
    for_ testCases $ \(good, header) -> do
      submit "GET" (mkReq header) `bindResponse` \resp -> do
        if good
          then do
            resp.status `shouldMatchInt` 200
            resp.json %. "user" `shouldMatch` (alice %. "qualified_id.id")
            resp.json %. "timestamp" `shouldNotMatch` ""
          else do
            resp.status `shouldMatchInt` 200
            resp.json %. "user" `shouldMatch` ""
            resp.json %. "timestamp" `shouldMatch` ""

withTestNginz :: Codensity App Int
withTestNginz = do
  tmpDir <- Codensity $ withSystemTempDirectory "integration-NginxZauthModule"
  env <- ask
  -- Create config
  let (keystorePath, oauthPubKey) = case env.servicesCwdBase of
        Nothing ->
          ( "/etc/wire/nginz/secrets/zauth.conf",
            "/etc/wire/nginz/secrets/oauth_ed25519_pub.jwk"
          )
        Just basedir ->
          ( basedir </> "nginz/integration-test/resources/zauth/pubkeys.txt",
            basedir </> "nginz/integration-test/resources/oauth/ed25519_public.jwk"
          )
      unixSocketPath = tmpDir </> "sock"
      config =
        nginxTestConfigTemplate
          -- Listen on unix socket because its too complicated to make nginx run
          -- on a random available port.
          & replace "{socket_path}" unixSocketPath
          & replace "{pid_file}" (tmpDir </> "pid")

      configPath = tmpDir </> "nginx.conf"

  copyFile keystorePath (tmpDir </> "keystore")
  copyFile oauthPubKey (tmpDir </> "oauth-pub-key")
  liftIO $ writeFile (tmpDir </> "acl") ""
  liftIO $ writeFile configPath config

  let startNginx = do
        (_, Just stdoutHdl, Just stderrHdl, processHandle) <-
          createProcess
            (proc "nginx" ["-c", configPath, "-g", "daemon off;", "-e", "/dev/stdout"])
              { cwd = Just tmpDir,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        -- Enable this when debugging
        -- liftIO $ void $ forkIO $ logToConsole id "nginx-zauth-module" stdoutHdl
        -- liftIO $ void $ forkIO $ logToConsole id "nginx-zauth-module" stderrHdl
        pure (stdoutHdl, stderrHdl, processHandle)

      stopNginx (_, _, processHandle) = do
        mPid <- liftIO $ getPid processHandle
        liftIO $ for_ mPid (signalProcess keyboardSignal)
        timeout 50000 (waitForProcess processHandle) >>= \case
          Just _ -> pure ()
          Nothing -> do
            liftIO $ for_ mPid (signalProcess killProcess)
            void $ waitForProcess processHandle
  _ <- Codensity $ bracket startNginx stopNginx

  -- The http-client package doesn't support connecting to servers running on a
  -- unix domain socket. So, we bind to a random TCP port and forward the
  -- requests and responses to and from the unix domain socket of nginx.
  (port, sock) <- Codensity $ bracket (liftIO $ bindRandomPortTCP (fromString "*6")) (liftIO . NS.close . snd)
  _ <- Codensity $ bracket (async $ forwardToUnixDomain sock unixSocketPath) Async.cancel
  pure port

forwardToUnixDomain :: (MonadIO m) => Socket -> FilePath -> m ()
forwardToUnixDomain tcpSock unixSockAddr = liftIO . forever $ do
  (conn, _) <- NS.accept tcpSock
  void $ async $ do
    unixSock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
    NS.connect unixSock (NS.SockAddrUnix unixSockAddr)

    tcpToUnix <- async $ forward conn unixSock
    unixToTCP <- async $ forward unixSock conn

    void $ waitBoth tcpToUnix unixToTCP

forward :: Socket -> Socket -> IO ()
forward src dst = do
  let loop = do
        bs <- NSB.recv src 4096
        if BS.null bs
          then pure ()
          else NSB.sendAll dst bs >> loop
  loop

nginxTestConfigTemplate :: String
nginxTestConfigTemplate =
  [r|
    events {
       worker_connections 128;
    }

    error_log /dev/stderr info;
    pid {pid_file};

    http {
       server {
          listen unix:{socket_path};
          zauth_keystore "./keystore";
          zauth_acl "./acl";
          oauth_pub_key "./oauth-pub-key";

          access_log /dev/stdout combined;

          location / {
            default_type application/json;
            return 200 '{"user":"$zauth_user", "timestamp": "$zauth_timestamp"}';
          }
       }
    }
 |]
