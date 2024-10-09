module Testlib.MockIntegrationService
  ( withMockServer,
    lhMockAppWithPrekeys,
    lhMockApp,
    lhMockAppV,
    mkLegalHoldSettings,
    CreateMock (..),
    LiftedApplication,
    MockServerSettings (..),
    LhApiVersion (..),
  )
where

import Control.Monad.Catch
import Control.Monad.Reader
import qualified Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Streaming.Network
import Data.String.Conversions (cs)
import Network.HTTP.Types
import Network.Socket
import qualified Network.Socket as Socket
import Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.Warp.Internal as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Testlib.Prelude hiding (IntegrationConfig (integrationTestHostName))
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Async
import UnliftIO.Chan
import UnliftIO.MVar
import UnliftIO.Timeout (timeout)

withFreePortAnyAddr :: (MonadMask m, MonadIO m) => ((Warp.Port, Socket) -> m a) -> m a
withFreePortAnyAddr = bracket openFreePortAnyAddr (liftIO . Socket.close . snd)

openFreePortAnyAddr :: (MonadIO m) => m (Warp.Port, Socket)
openFreePortAnyAddr = liftIO $ bindRandomPortTCP (fromString "*6")

type LiftedApplication = Request -> (Wai.Response -> App ResponseReceived) -> App ResponseReceived

type Host = String

-- | The channel exists to facilitate out of http comms between the test and the
-- service. Could be used for recording (request, response) pairs.
withMockServer ::
  (HasCallStack) =>
  -- | the mock server settings
  MockServerSettings ->
  -- | The certificate and key pair
  (Chan e -> LiftedApplication) ->
  -- | the test
  ((Host, Warp.Port) -> Chan e -> App a) ->
  App a
withMockServer settings mkApp go = withFreePortAnyAddr \(sPort, sock) -> do
  serverStarted <- newEmptyMVar
  host <- asks integrationTestHostName
  let tlss = Warp.tlsSettingsMemory (cs settings.certificate) (cs settings.privateKey)
  let defs = Warp.defaultSettings {Warp.settingsPort = sPort, Warp.settingsBeforeMainLoop = putMVar serverStarted ()}
  buf <- newChan
  srv <- async $ withRunInIO \inIO -> do
    Warp.runTLSSocket tlss defs sock \req respond -> do
      inIO $ mkApp buf req (liftIO . respond)
  srvMVar <- UnliftIO.Timeout.timeout 5_000_000 (takeMVar serverStarted)
  case srvMVar of
    Just () -> go (host, sPort) buf `finally` cancel srv
    Nothing -> error . show =<< poll srv

lhMockApp :: Chan (Wai.Request, LBS.ByteString) -> LiftedApplication
lhMockApp = lhMockAppWithPrekeys V0 def

lhMockAppV :: LhApiVersion -> Chan (Wai.Request, LBS.ByteString) -> LiftedApplication
lhMockAppV v = lhMockAppWithPrekeys v def

data MockServerSettings = MkMockServerSettings
  { -- | the certificate the mock service uses
    certificate :: String,
    -- | the private key the mock service uses
    privateKey :: String,
    -- | the public key the mock service uses
    publicKey :: String
  }

instance Default MockServerSettings where
  def =
    MkMockServerSettings
      { certificate = mockServerCert,
        privateKey = mockServerPrivKey,
        publicKey = mockServerPubKey
      }

data CreateMock f = MkCreateMock
  { -- | how to obtain the next last prekey of a mock app
    nextLastPrey :: f Value,
    -- | how to obtain some prekeys of a mock app
    somePrekeys :: f [Value]
  }

instance (App ~ f) => Default (CreateMock f) where
  def =
    MkCreateMock
      { nextLastPrey = getLastPrekey,
        somePrekeys = replicateM 3 getPrekey
      }

data LhApiVersion = V0 | V1
  deriving (Show, Generic)

-- | LegalHold service.  Just fake the API, do not maintain any internal state.
lhMockAppWithPrekeys ::
  LhApiVersion -> CreateMock App -> Chan (Wai.Request, LBS.ByteString) -> LiftedApplication
lhMockAppWithPrekeys version mks ch req cont = withRunInIO \inIO -> do
  reqBody <- Wai.strictRequestBody req
  writeChan ch (req, reqBody)
  inIO do
    case version of
      V0 ->
        case (cs <$> pathInfo req, cs $ requestMethod req, cs @_ @String <$> getRequestHeader "Authorization" req) of
          (["legalhold", "status"], "GET", _) -> cont respondOk
          (_, _, Nothing) -> cont missingAuth
          (["legalhold", "initiate"], "POST", Just _) -> do
            (nextLastPrekey, threePrekeys) <- getPreyKeys
            cont (initiateResp nextLastPrekey threePrekeys)
          (["legalhold", "confirm"], "POST", Just _) -> cont respondOk
          (["legalhold", "remove"], "POST", Just _) -> cont respondOk
          _ -> cont respondBad
      V1 ->
        case (cs <$> pathInfo req, cs $ requestMethod req, cs @_ @String <$> getRequestHeader "Authorization" req) of
          (["legalhold", "status"], "GET", _) -> cont respondOk
          (["legalhold", "api-version"], "GET", _) -> cont apiVersionResp
          (_, _, Nothing) -> cont missingAuth
          (["legalhold", "initiate"], "POST", Just _) -> do
            (nextLastPrekey, threePrekeys) <- getPreyKeys
            cont (initiateResp nextLastPrekey threePrekeys)
          (["legalhold", "confirm"], "POST", Just _) -> cont respondOk
          (["legalhold", "remove"], "POST", Just _) -> cont respondOk
          (["legalhold", "v1", "initiate"], "POST", Just _) -> do
            (nextLastPrekey, threePrekeys) <- getPreyKeys
            cont (initiateResp nextLastPrekey threePrekeys)
          (["legalhold", "v1", "confirm"], "POST", Just _) -> cont respondOk
          (["legalhold", "v1", "remove"], "POST", Just _) -> cont respondOk
          _ -> cont respondBad
  where
    getPreyKeys :: App (Value, [Value])
    getPreyKeys = (,) <$> mks.nextLastPrey <*> mks.somePrekeys

    initiateResp :: Value -> [Value] -> Wai.Response
    initiateResp npk pks =
      responseLBS status200 [(hContentType, cs "application/json")]
        . encode
        . Data.Aeson.object
        $ [ "prekeys" .= pks,
            "last_prekey" .= npk
          ]

    apiVersionResp :: Wai.Response
    apiVersionResp =
      responseLBS status200 [(hContentType, cs "application/json")]
        . encode
        . Data.Aeson.object
        $ [ "supported" .= ([0, 1] :: [Int])
          ]

    respondOk :: Wai.Response
    respondOk = responseLBS status200 mempty mempty

    respondBad :: Wai.Response
    respondBad = responseLBS status404 mempty mempty

    missingAuth :: Wai.Response
    missingAuth = responseLBS status400 mempty (cs "no authorization header")

    getRequestHeader :: String -> Wai.Request -> Maybe ByteString
    getRequestHeader name = lookup (fromString name) . requestHeaders

mkLegalHoldSettings :: (String, Warp.Port) -> Value
mkLegalHoldSettings (botHost, lhPort) =
  object
    [ "base_url" .= ("https://" <> botHost <> ":" <> show lhPort <> "/legalhold"),
      "public_key" .= mockServerPubKey,
      "auth_token" .= "tok"
    ]

mockServerPubKey :: String
mockServerPubKey =
  "-----BEGIN PUBLIC KEY-----\n\
  \MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0\n\
  \G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH\n\
  \WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV\n\
  \VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS\n\
  \bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8\n\
  \7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la\n\
  \nQIDAQAB\n\
  \-----END PUBLIC KEY-----"

mockServerPrivKey :: String
mockServerPrivKey =
  "-----BEGIN RSA PRIVATE KEY-----\n\
  \MIIEpAIBAAKCAQEAu+Kg/PHHU3atXrUbKnw0G06FliXcNt3lMwl2os5twEDcPPFw\n\
  \/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPHWvUBdiLfGrZqJO223DB6D8K2Su/o\n\
  \dmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKVVPOaOzgtAB21XKRiQ4ermqgi3/nj\n\
  \r03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiSbUKr/BeArYRcjzr/h5m1In6fG/if\n\
  \9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg87X883H+LA/d6X5CTiPv1VMxXdBUi\n\
  \GPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7lanQIDAQABAoIBAQC0doVy7zgpLsBv\n\
  \Sz0AnbPe1pjxEwRlntRbJSfSULySALqJvs5s4adSVGUBHX3z/LousAP1SRpCppuU\n\
  \8wrLBFgjQVlaAzyQB84EEl+lNtrG8Jrvd2es9R/4sJDkqy50+yuPN5wnzWPFIjhg\n\
  \3jP5CHDu29y0LMzsY5yjkzDe9B0bueXEZVU+guRjhpwHHKOFeAr9J9bugFUwgeAr\n\
  \jF0TztzFAb0fsUNPiQAho1J5PyjSVgItaPfAPv/p30ROG+rz+Rd5NSSvBC5F+yOo\n\
  \azb84zzwCg/knAfIz7SOMRrmBh2qhGZFZ8gXdq65UaYv+cpT/qo28mpAT2vOkyeD\n\
  \aPZp0ysBAoGBAOQROoDipe/5BTHBcXYuUE1qa4RIj3wgql5I8igXr4K6ppYBmaOg\n\
  \DL2rrnqD86chv0P4l/XOomKFwYhVGXtqRkeYnk6mQXwNVkgqcGbY5PSNyMg5+ekq\n\
  \jSOOPHGzzTWKzYuUDUpB/Lf6jbTv8fq2GYW3ZYiqQ/xiugOvglZrTE7NAoGBANLl\n\
  \irjByfxAWGhzCrDx0x5MBpsetadI9wUA8u1BDdymsRg73FDn3z7NipVUAMDXMGVj\n\
  \lqbCRlHESO2yP4GaPEA4FM+MbTZSuhAYV+SY07mEPLHF64/nJas83Zp91r5rhaqJ\n\
  \L9rWCl3KJ5OUnr3YizCnHIW72FxjwtpjxHJLupsRAoGAGIbhy8qUHeKh9F/hW9xP\n\
  \NoQjW+6Rv7+jktA1eqpRbbW1BJzXcQldVWiJMxPNuEOg1iZ98SlvvTi1P3wnaWZc\n\
  \eIapP7wRfs3QYaJuxCC/Pq2g0ieqALFazGAXkALOJtvujvw1Ea9XBlIjuzmyxEuh\n\
  \Iwg+Gxx0g0f6yTquwax4YGECgYEAnpAK3qKFNO1ECzQDo8oNy0ep59MNDPtlDhQK\n\
  \katJus5xdCD9oq7TQKrVOTTxZAvmzTQ1PqfuqueDVYOhD9Zg2n/P1cRlEGTek99Z\n\
  \pfvppB/yak6+r3FA9yBKFS/r1zuMQg3nNweav62QV/tz5pT7AdeDMGFtaPlwtTYx\n\
  \qyWY5aECgYBPySbPccNj+xxQzxcti2y/UXjC04RgOA/Hm1D0exa0vBqS9uxlOdG8\n\
  \F47rKenpBrslvdfTVsCDB1xyP2ebWVzp6EqMycw6OLPxgo3fBfZ4pi6P+rByh0Cc\n\
  \Lhfh+ET0CPnKCxtop3lUrn4ZvqchS0j3J+M0pDuqoWF5hfKxFhkEIw==\n\
  \-----END RSA PRIVATE KEY-----"

mockServerCert :: String
mockServerCert =
  "-----BEGIN CERTIFICATE-----\n\
  \MIIDdjCCAl4CCQCm0AiwERR/qjANBgkqhkiG9w0BAQsFADB9MQswCQYDVQQGEwJE\n\
  \RTEPMA0GA1UECAwGQmVybGluMQ8wDQYDVQQHDAZCZXJsaW4xGDAWBgNVBAoMD1dp\n\
  \cmUgU3dpc3MgR21iSDERMA8GA1UEAwwId2lyZS5jb20xHzAdBgkqhkiG9w0BCQEW\n\
  \EGJhY2tlbmRAd2lyZS5jb20wHhcNMTYwODA0MTMxNDQyWhcNMzYwNzMwMTMxNDQy\n\
  \WjB9MQswCQYDVQQGEwJERTEPMA0GA1UECAwGQmVybGluMQ8wDQYDVQQHDAZCZXJs\n\
  \aW4xGDAWBgNVBAoMD1dpcmUgU3dpc3MgR21iSDERMA8GA1UEAwwId2lyZS5jb20x\n\
  \HzAdBgkqhkiG9w0BCQEWEGJhY2tlbmRAd2lyZS5jb20wggEiMA0GCSqGSIb3DQEB\n\
  \AQUAA4IBDwAwggEKAoIBAQC74qD88cdTdq1etRsqfDQbToWWJdw23eUzCXaizm3A\n\
  \QNw88XD994aIArKbGn7smpkOux5LkP1Mcatb45BEg8da9QF2It8atmok7bbcMHoP\n\
  \wrZK7+h2aeNknbPbeuFegQCtOmW74OD0r5zYtV5dMpVU85o7OC0AHbVcpGJDh6ua\n\
  \qCLf+eOvTetfKr+o2S413q01yD4cB8bF8a+8JJgF+JJtQqv8F4CthFyPOv+HmbUi\n\
  \fp8b+J/0YQjqbx3EdP0ltjnfCKSyjDLpqMK6qyQgWDztfzzcf4sD93pfkJOI+/VU\n\
  \zFd0FSIY+4L0hP/oI1DX8sW3Q/ftrHnz4sZiVoWjuVqdAgMBAAEwDQYJKoZIhvcN\n\
  \AQELBQADggEBAEuwlHElIGR56KVC1dJiw238mDGjMfQzSP76Wi4zWS6/zZwJUuog\n\
  \BkC+vacfju8UAMvL+vdqkjOVUHor84/2wuq0qn91AjOITD7tRAZB+XLXxsikKv/v\n\
  \OXE3A/lCiNi882NegPyXAfFPp/71CIiTQZps1eQkAvhD5t5WiFYPESxDlvEJrHFY\n\
  \XP4+pp8fL8YPS7iZNIq+z+P8yVIw+B/Hs0ht7wFIYN0xACbU8m9+Rs08JMoT16c+\n\
  \hZMuK3BWD3fzkQVfW0yMwz6fWRXB483ZmekGkgndOTDoJQMdJXZxHpI3t2FcxQYj\n\
  \T45GXxRd18neXtuYa/OoAw9UQFDN5XfXN0g=\n\
  \-----END CERTIFICATE-----"
