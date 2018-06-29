{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.APISpec where

import Bilge
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.QQ as Aeson
import Data.Either (isRight)
import Data.EitherR (fmapL)
import Data.Id
import Data.List (isInfixOf)
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Lens.Micro
import SAML2.WebSSO as SAML
import Spar.API ()
import Spar.Types
import Test.Hspec
import URI.ByteString.QQ
import Util

import qualified Data.X509 as X509
import qualified Text.XML.DSig as SAML


-- TODO: what else needs to be tested, beyond the pending tests listed here?


spec :: IntegrationConfig -> Spec
spec opts = beforeAll (mkEnv opts) $ do
    describe "status, metainfo" $ do
      it "brig /i/status" $ \env -> (`runReaderT` env) $ do
        ping (env ^. teBrig) `shouldRespondWith` (== ())

      it "spar /i/status" $ \env -> (`runReaderT` env) $ do
        ping (env ^. teSpar) `shouldRespondWith` (== ())

      it "metainfo" $ \env -> (`runReaderT` env) $ do
        get ((env ^. teSpar) . path "/sso/metainfo" . expect2xx)
          `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                [ "md:SPSSODescriptor"
                                , "validUntil"
                                , "WantAssertionsSigned=\"true\""
                                ])

    describe "/sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          let uuid = cs $ UUID.toText UUID.nil
          get ((env ^. teSpar) . path ("/sso/initiate-login/" <> uuid))
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP" $ do
        it "responds with request" $ \env -> (`runReaderT` env) $ do
          (_, _, cs . UUID.toText . fromIdPId -> idp) <- createTestIdP
          get ((env ^. teSpar) . path ("/sso/initiate-login/" <> idp) . expect2xx)
            `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                  [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
                                  , "<body onload=\"document.forms[0].submit()\">"
                                  , "<input name=\"SAMLRequest\" type=\"hidden\" "
                                  ])

    describe "/sso/finalize-login" $ do  -- TODO: either use workingIdP or mock one locally.  the
                                         -- latter is faster to run, but we need the former anyway,
                                         -- so we might as well rely on that.
      context "access denied" $ do
        it "responds with 'forbidden'" $ \_ -> do
          pending

      context "access granted" $ do
        it "responds with redirect to app" $ \_ -> do
          pending

        context "unknown user" $ do
          it "creates the user" $ \_ -> do
            pending

      context "unknown IdP" $ do
        it "rejects" $ \_ -> do
          pending

      context "bad AuthnRequest" $ do
        it "rejects" $ \_ -> do
          pending

      context "response does not match any request" $ do
        it "rejects" $ \_ -> do
          pending

      context "response contains assertions that have been offered before" $ do
        it "rejects" $ \_ -> do
          pending


    describe "GET /sso/identity-providers/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          callIdpGet' ((env ^. teSpar)) Nothing (IdPId UUID.nil)
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP, but no zuser" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          (_, _, idp) <- createTestIdP
          callIdpGet' ((env ^. teSpar)) Nothing idp
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP that does not belong to user" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createUserWithTeam ((env ^. teBrig)) (env ^. teGalley)
          (_, _, idp) <- createTestIdP
          callIdpGet' ((env ^. teSpar)) (Just uid) idp
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP" $ do
        it "responds with 2xx and IdP" $ \env -> (`runReaderT` env) $ do
          (uid, _, idp) <- createTestIdP
          callIdpGet' ((env ^. teSpar)) (Just uid) idp
            `shouldRespondWith` (\resp -> statusCode resp < 300 && isRight (responseJSON @IdP resp))

    describe "DELETE /sso/identity-providers/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          callIdpDelete' ((env ^. teSpar)) Nothing (IdPId UUID.nil)
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP, but no zuser" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          (_, _, idp) <- createTestIdP
          callIdpDelete' ((env ^. teSpar)) Nothing idp
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP that does not belong to user" $ do
        it "responds with 'not found'" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          (_, _, idp) <- createTestIdP
          callIdpDelete' ((env ^. teSpar)) (Just uid) idp
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP" $ do
        it "responds with 2xx and removes IdP" $ \env -> (`runReaderT` env) $ do
          (uid, _, idp) <- createTestIdP
          callIdpDelete' ((env ^. teSpar)) (Just uid) idp
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' ((env ^. teSpar)) (Just uid) idp
            `shouldRespondWith` ((>= 400) . statusCode)

    describe "POST /sso/identity-providers/:idp" $ do
      let check :: (Int -> Bool) -> Value -> ResponseLBS -> Bool
          check statusIs msg resp = statusIs (statusCode resp) && responseJSON resp == Right msg

      context "no zuser" $ do
        it "responds with 'forbidden' and a helpful message" $ \env -> (`runReaderT` env) $ do
          callIdpCreate' ((env ^. teSpar)) Nothing (env ^. teNewIdp)
            `shouldRespondWith` check (>= 400) [aesonQQ|{"error":"no auth token"}|]

      context "zuser has no team" $ do
        it "responds with 'forbidden' and a helpful message" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createRandomPhoneUser ((env ^. teBrig))
          callIdpCreate' ((env ^. teSpar)) (Just uid) (env ^. teNewIdp)
            `shouldRespondWith` check (>= 400) [aesonQQ|{"error":"you need to be team admin to create an IdP"}|]

      context "zuser is a team member, not a team admin" $ do
        it "responds with 'forbidden' and a helpful message" $ \_ -> do
          pending

      context "invalid or unresponsive metainfo url" $ do
        it "rejects" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createUserWithTeam ((env ^. teBrig)) (env ^. teGalley)
          callIdpCreate' ((env ^. teSpar)) (Just uid) ((env ^. teNewIdp) & nidpMetadata .~ [uri|http://www.example.com/|])
            `shouldRespondWith` check (>= 400) [aesonQQ|{"error":"invalid or unresponsive metainfo URL"}|]

      context "invalid metainfo content" $ do
        it "rejects" $ \_ -> do
          pending

      context "invalid metainfo signature" $ do
        it "rejects" $ \_ -> do
          pending

      context "invalid or unresponsive login request url" $ do
        it "rejects" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createUserWithTeam ((env ^. teBrig)) (env ^. teGalley)
          callIdpCreate' ((env ^. teSpar)) (Just uid) ((env ^. teNewIdp) & nidpRequestUri .~ [uri|http://www.example.com/|])
            `shouldRespondWith` check (>= 400) [aesonQQ|{"error":"invalid or unresponsive request URL"}|]

      context "pubkey in IdPConfig does not match the one provided in metainfo url" $ do
        let differentPubKey :: X509.SignedCertificate
            differentPubKey = _
        it "rejects" $ \env -> (`runReaderT` env) $ do
          (uid, _) <- call $ createUserWithTeam ((env ^. teBrig)) (env ^. teGalley)
          callIdpCreate' ((env ^. teSpar)) (Just uid) ((env ^. teNewIdp) & nidpPublicKey .~ _)
            `shouldRespondWith` check (>= 400) [aesonQQ|{"error":"public keys in request body and metainfo do not match"}|]

      context "everything in order" $ do
        it "responds with 2xx" $ \_ -> do
          pending

        it "makes IdP available for GET /sso/identity-providers/" $ \_ -> do
          pending


----------------------------------------------------------------------

shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a)
                  => Http a -> (a -> Bool) -> ReaderT TestEnv IO ()
shouldRespondWith action proper = do
  resp <- call action
  liftIO $ resp `shouldSatisfy` proper

-- I tried this, but i don't  think it's worth the learning effort.  Perhaps it'll be helpful as a comment here.  :-)
-- envit :: Example (r -> m a) => String -> ReaderT r m a -> SpecWith (Arg (r -> m a))
-- envit msg action = it msg $ \env -> action `runReaderT` env

call :: Http a -> ReaderT TestEnv IO a
call req = ask >>= \env -> liftIO $ runHttpT (env ^. teMgr) req

ping :: (Request -> Request) -> Http ()
ping req = void . get $ req . path "/i/status" . expect2xx


createTestIdP :: (HasCallStack, MonadReader TestEnv m, MonadIO m) => m (UserId, TeamId, IdPId)
createTestIdP = do
  env <- ask
  liftIO . runHttpT (env ^. teMgr) $ do
    (uid, tid) <- createUserWithTeam ((env ^. teBrig)) (env ^. teGalley)
    (uid, tid,) . (^. idpId) <$> callIdpCreate ((env ^. teSpar)) (Just uid) sampleIdP

sampleIdP :: NewIdP
sampleIdP = NewIdP
  { _nidpMetadata        = [uri|http://idp.net/meta|]
  , _nidpIssuer          = Issuer [uri|http://idp.net/|]
  , _nidpRequestUri      = [uri|http://idp.net/sso/request|]
  , _nidpPublicKey       = either (error . show) id $ SAML.parseKeyInfo "<KeyInfo xmlns=\"http://www.w3.org/2000/09/xmldsig#\"><X509Data><X509Certificate>MIIDBTCCAe2gAwIBAgIQev76BWqjWZxChmKkGqoAfDANBgkqhkiG9w0BAQsFADAtMSswKQYDVQQDEyJhY2NvdW50cy5hY2Nlc3Njb250cm9sLndpbmRvd3MubmV0MB4XDTE4MDIxODAwMDAwMFoXDTIwMDIxOTAwMDAwMFowLTErMCkGA1UEAxMiYWNjb3VudHMuYWNjZXNzY29udHJvbC53aW5kb3dzLm5ldDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAMgmGiRfLh6Fdi99XI2VA3XKHStWNRLEy5Aw/gxFxchnh2kPdk/bejFOs2swcx7yUWqxujjCNRsLBcWfaKUlTnrkY7i9x9noZlMrijgJy/Lk+HH5HX24PQCDf+twjnHHxZ9G6/8VLM2e5ZBeZm+t7M3vhuumEHG3UwloLF6cUeuPdW+exnOB1U1fHBIFOG8ns4SSIoq6zw5rdt0CSI6+l7b1DEjVvPLtJF+zyjlJ1Qp7NgBvAwdiPiRMU4l8IRVbuSVKoKYJoyJ4L3eXsjczoBSTJ6VjV2mygz96DC70MY3avccFrk7tCEC6ZlMRBfY1XPLyldT7tsR3EuzjecSa1M8CAwEAAaMhMB8wHQYDVR0OBBYEFIks1srixjpSLXeiR8zES5cTY6fBMA0GCSqGSIb3DQEBCwUAA4IBAQCKthfK4C31DMuDyQZVS3F7+4Evld3hjiwqu2uGDK+qFZas/D/eDunxsFpiwqC01RIMFFN8yvmMjHphLHiBHWxcBTS+tm7AhmAvWMdxO5lzJLS+UWAyPF5ICROe8Mu9iNJiO5JlCo0Wpui9RbB1C81Xhax1gWHK245ESL6k7YWvyMYWrGqr1NuQcNS0B/AIT1Nsj1WY7efMJQOmnMHkPUTWryVZlthijYyd7P2Gz6rY5a81DAFqhDNJl2pGIAE6HWtSzeUEh3jCsHEkoglKfm4VrGJEuXcALmfCMbdfTvtu4rlsaP2hQad+MG/KJFlenoTK34EMHeBPDCpqNDz8UVNk</X509Certificate></X509Data></KeyInfo>"
  }


-- TODO: do we want to implement these with servant-client?  if not, are there better idioms for
-- handling the various errors?

-- TODO: move this to /lib/bilge?
responseJSON :: FromJSON a => ResponseLBS -> Either String a
responseJSON = fmapL show . Aeson.eitherDecode <=< maybe (Left "no body") pure . responseBody

callIdpGet :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> IdPId -> m IdP
callIdpGet sparreq_ muid idpid = do
  resp <- callIdpGet' (sparreq_ . expect2xx) muid idpid
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpGet' :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> IdPId -> m ResponseLBS
callIdpGet' sparreq_ muid idpid = do
  get $ sparreq_ . maybe id zUser muid . path ("/sso/identity-providers/" <> cs (idPIdToST idpid))

callIdpCreate :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> NewIdP -> m IdP
callIdpCreate sparreq_ muid newidp = do
  resp <- callIdpCreate' (sparreq_ . expect2xx) muid newidp
  either (liftIO . throwIO . ErrorCall . show) pure
    $ responseJSON @IdP resp

callIdpCreate' :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> NewIdP -> m ResponseLBS
callIdpCreate' sparreq_ muid newidp = do
  post $ sparreq_ . maybe id zUser muid . path "/sso/identity-providers/" . json newidp

callIdpDelete :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> IdPId -> m ()
callIdpDelete sparreq_ muid idpid = void $ callIdpDelete' (sparreq_ . expect2xx) muid idpid

callIdpDelete' :: (MonadIO m, MonadHttp m) => Spar -> Maybe UserId -> IdPId -> m ResponseLBS
callIdpDelete' sparreq_ muid idpid = do
  delete $ sparreq_ . maybe id zUser muid . path ("/sso/identity-providers/" <> cs (idPIdToST idpid))
