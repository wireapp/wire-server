{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.APISpec where

import Bilge
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Data.String.Conversions
import Lens.Micro
import SAML2.WebSSO as SAML
import Spar.API ()
import Spar.Types
import Spar.Options as Opts
import Test.Hspec
import Util.Options
import URI.ByteString.QQ

import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import qualified Text.XML.DSig as SAML


-- TODO: what else needs to be tested?


mkspec :: Opts -> IO Spec
mkspec opts = do
  mgr :: Manager <- newManager defaultManagerSettings
  let brigreq :: (Request -> Request)
      brigreq = Bilge.host (opts ^. to Opts.brig . epHost . to cs)
              . Bilge.port (opts ^. to Opts.brig . epPort)
      sparreq :: (Request -> Request)
      sparreq = Bilge.host (opts ^. to Opts.saml . SAML.cfgSPHost . to cs)
              . Bilge.port (opts ^. to Opts.saml . SAML.cfgSPPort . to fromIntegral)

      shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a) => Http a -> (a -> Bool) -> Expectation
      shouldRespondWith action proper = liftIO (runHttpT mgr action) >>= \resp -> resp `shouldSatisfy` proper

      ping :: (Request -> Request) -> Http ()
      ping req = void . get $ req . path "/i/status" . expect2xx

  pure $ do
    describe "status, metainfo" $ do
      it "brig /i/status" $ do
        ping brigreq `shouldRespondWith` (== ())

      it "spar /i/status" $ do
        ping sparreq `shouldRespondWith` (== ())

      it "metainfo" $ do
        get (sparreq . path "/sso/metainfo" . expect2xx)
          `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                [ "md:SPSSODescriptor"
                                , "validUntil"
                                , "WantAssertionsSigned=\"true\""
                                ])

    describe "/sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          let uuid = cs $ UUID.toText UUID.nil
          get (sparreq . path ("/sso/initiate-login/" <> uuid))
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP" $ do
        it "responds with request" $ do
          idp <- createTestIdP mgr sparreq
          get (sparreq . path ("/sso/initiate-login/" <> idp) . expect2xx)
            `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                  [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
                                  , "<body onload=\"document.forms[0].submit()\">"
                                  , "<input name=\"SAMLRequest\" type=\"hidden\" "
                                  ])

    describe "/sso/finalize-login" $ do
      context "access denied" $ do
        it "responds with 'forbidden'" $ do
          pending

      context "access granted" $ do
        it "responds with redirect to app" $ do
          pending

      context "response does not match any request" $ do
        it "rejects" $ do
          pending

      context "response contains assertions that have been offered before" $ do
        it "rejects" $ do
          pending


    describe "GET /sso/identity-providers/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          pending

      context "known IdP" $ do
        it "responds with IdP" $ do
          pending

    describe "DELETE /sso/identity-providers/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          pending

      context "known IdP" $ do
        it "remove the IdP and responds with 'NoContent'" $ do
          pending

    describe "POST /sso/identity-providers/:idp" $ do
      context "invalid or unresponsive metainfo url" $ do
        it "rejects" $ do
          pending

      context "invalid metainfo content" $ do
        it "rejects" $ do
          pending

      context "invalid metainfo signature" $ do
        it "rejects" $ do
          pending

      context "invalid or unresponsive login request url" $ do
        it "rejects" $ do
          pending

      context "pubkey in IdPConfig does not match the one provided in metainfo url" $ do
        it "rejects" $ do
          pending

      context "everything in order" $ do
        it "responds with 2xx" $ do
          pending

        it "makes IdP available for POST /sso/identity-providers/" $ do
          pending


----------------------------------------------------------------------

createTestIdP :: Manager -> (Request -> Request) -> IO SBS
createTestIdP mgr sparreq = cs . UUID.toText . fromIdPId <$> runHttpT mgr (createTestIdP' sparreq)

createTestIdP' :: (MonadIO m, MonadHttp m) => (Request -> Request) -> m IdPId
createTestIdP' sparreq = do
  let new = NewIdP
        { _nidpMetadata        = [uri|http://idp.net/meta|]
        , _nidpIssuer          = Issuer [uri|http://idp.net/|]
        , _nidpRequestUri      = [uri|http://idp.net/sso/request|]
        , _nidpPublicKey       = either (error . show) id $ SAML.parseKeyInfo "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"
        }
  resp :: Bilge.Response (Maybe LBS)
    <- post $ sparreq . path "/sso/identity-providers/" . json new . expect2xx

  pure undefined
