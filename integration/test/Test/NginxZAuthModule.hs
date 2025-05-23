module Test.NginxZAuthModule where

import API.Brig
import API.Common
import Control.Monad.Reader
import Data.List.Extra
import Data.Streaming.Network
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
import qualified Network.Socket as Socket
import SetupHelpers
import System.FilePath ((</>))
import System.IO (writeFile)
import System.IO.Temp
import System.Posix
import System.Process (getPid)
import Testlib.Prelude
import Text.RawString.QQ
import UnliftIO (bracket)
import UnliftIO.Concurrent (forkIO)
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Timeout (timeout)

testBearerToken :: (HasCallStack) => App ()
testBearerToken = do
  withSystemTempDirectory "integration-testBearerToken-xxx" $ \tmpDir -> do
    env <- ask
    -- Find a port to run our test nginx
    (port, sock) <- liftIO $ bindRandomPortTCP (fromString "*6")

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
        config =
          nginxTestConfigTemplate
            & replace "{port}" (show port)
            & replace "{pid_file}" (tmpDir </> "pid")

        configPath = tmpDir </> "nginx.conf"

    putStrLn config
    copyFile keystorePath (tmpDir </> "keystore")
    copyFile oauthPubKey (tmpDir </> "oauth-pub-key")
    liftIO $ writeFile (tmpDir </> "acl") ""
    liftIO $ writeFile configPath config

    -- Stop listening to our port and start nginx, and hope noone takes that
    -- port in the meantime.
    liftIO $ Socket.close sock
    runNginx tmpDir configPath $ do
      -- Get a token
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
          listen *:{port};
          zauth_keystore "./keystore";
          zauth_acl "./acl";
          oauth_pub_key "./oauth-pub-key";

          access_log /dev/stdout combined;

          location / {
            default_type application/json;
            return 200 '{"user":"$zauth_user"}';
          }
       }
    }
 |]

runNginx :: FilePath -> FilePath -> App () -> App ()
runNginx cwd configPath test = do
  let startNginx = do
        (_, Just stdoutHdl, Just stderrHdl, processHandle) <-
          createProcess
            (proc "nginx" ["-c", configPath, "-g", "daemon off;", "-e", "/dev/stdout"])
              { cwd = Just cwd,
                std_out = CreatePipe,
                std_err = CreatePipe
              }
        liftIO $ void $ forkIO $ logToConsole id "nginx-zauth-module" stdoutHdl
        liftIO $ void $ forkIO $ logToConsole id "nginx-zauth-module" stderrHdl
        pure (stdoutHdl, stderrHdl, processHandle)

      stopNginx (_, _, processHandle) = do
        mPid <- liftIO $ getPid processHandle
        liftIO $ for_ mPid (signalProcess keyboardSignal)
        timeout 50000 (waitForProcess processHandle) >>= \case
          Just _ -> pure ()
          Nothing -> do
            liftIO $ for_ mPid (signalProcess killProcess)
            void $ waitForProcess processHandle
  bracket startNginx stopNginx (const test)
