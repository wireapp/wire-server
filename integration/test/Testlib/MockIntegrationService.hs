module Testlib.MockIntegrationService (withMockServer, lhMockApp, mkLegalHoldSettings) where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.String.Conversions (cs)
import Network.HTTP.Types
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import SetupHelpers (withFreePortAnyAddr)
import Testlib.Prekeys
import Testlib.Prelude
import UnliftIO.Async
import UnliftIO.Chan
import UnliftIO.MVar
import UnliftIO.Timeout (timeout)

withMockServer ::
  HasCallStack =>
  -- | the mock server
  (Chan e -> Application) ->
  -- | the test
  (Warp.Port -> Chan e -> App a) ->
  App a
withMockServer mkApp go = withFreePortAnyAddr $ \(sPort, sock) -> do
  serverStarted <- newEmptyMVar
  let tlss = Warp.tlsSettingsMemory (cs someCert) (cs somePrivKey)
  let defs = Warp.defaultSettings {Warp.settingsPort = sPort, Warp.settingsBeforeMainLoop = putMVar serverStarted ()}
  buf <- newChan
  srv <- async . liftIO . Warp.runTLSSocket tlss defs sock $ mkApp buf
  srvMVar <- UnliftIO.Timeout.timeout 5_000_000 (takeMVar serverStarted)
  case srvMVar of
    Just () -> go sPort buf `finally` cancel srv
    Nothing -> error . show =<< poll srv

-- | LegalHold service.  Just fake the API, do not maintain any internal state.
lhMockApp :: Chan (Wai.Request, LBS.ByteString) -> Wai.Application
lhMockApp ch req cont = do
  reqBody <- Wai.strictRequestBody req
  writeChan ch (req, reqBody)
  case (cs <$> pathInfo req, cs $ requestMethod req, cs @_ @String <$> getRequestHeader "Authorization" req) of
    (["legalhold", "status"], "GET", _) -> cont respondOk
    (_, _, Nothing) -> cont missingAuth
    (["legalhold", "initiate"], "POST", Just _) -> cont initiateResp
    (["legalhold", "confirm"], "POST", Just _) -> cont respondOk
    (["legalhold", "remove"], "POST", Just _) -> cont respondOk
    _ -> cont respondBad
  where
    initiateResp :: Wai.Response
    initiateResp =
      responseLBS status200 [(hContentType, cs "application/json")] . encode . Data.Aeson.object $
        [ "prekeys" .= drop 3 somePrekeysRendered,
          "last_prekey" .= (someLastPrekeysRendered !! 2)
        ]

    respondOk :: Wai.Response
    respondOk = responseLBS status200 mempty mempty

    respondBad :: Wai.Response
    respondBad = responseLBS status404 mempty mempty

    missingAuth :: Wai.Response
    missingAuth = responseLBS status400 mempty (cs "no authorization header")

    getRequestHeader :: String -> Wai.Request -> Maybe ByteString
    getRequestHeader name = lookup (fromString name) . requestHeaders

mkLegalHoldSettings :: Warp.Port -> Value
mkLegalHoldSettings lhPort =
  object
    [ "base_url" .= ("https://localhost:" <> show lhPort <> "/legalhold"),
      "public_key" .= somePubKey,
      "auth_token" .= "tok"
    ]
