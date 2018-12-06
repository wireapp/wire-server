{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.AppSpec (spec) where

import Imports
import Bilge
import Control.Exception
import Control.Lens
import Data.Id
import Data.String.Conversions
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import SAML2.Util ((-/))
import SAML2.WebSSO as SAML
import Spar.Orphans ()
import Spar.Types (IdP)
import URI.ByteString as URI
import URI.ByteString.QQ (uri)
import Util
import Web.Cookie

import qualified Data.ByteString.Builder as Builder
import qualified Data.List as List
import qualified SAML2.WebSSO.Test.Credentials as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import qualified Servant
import qualified Spar.App as Spar
import qualified Text.XML as XML


spec :: SpecWith TestEnv
spec = describe "accessVerdict" $ do
      context "web" $ do
        context "invalid idp" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            pending

        context "denied" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            (_, _, idp) <- createTestIdP
            (_, outcome, _, _) <- requestAccessVerdict idp False mkAuthnReqWeb
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 200
              Servant.errReasonPhrase outcome `shouldBe` "forbidden"
              ('1', cs @LBS @String (Servant.errBody outcome))
                `shouldSatisfy` (("<title>wire:sso:error:forbidden</title>" `List.isInfixOf`) . snd)
              ('2', XML.parseLBS XML.def $ Servant.errBody outcome)
                `shouldSatisfy` (isRight . snd)

        context "granted" $ do
          it "responds with status 200 and a valid html page with constant expected title." $ do
            (_, _, idp) <- createTestIdP
            (_, outcome, _, _) <- requestAccessVerdict idp True mkAuthnReqWeb
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 200
              Servant.errReasonPhrase outcome `shouldBe` "success"
              ('1', cs @LBS @String (Servant.errBody outcome))
                `shouldSatisfy` (("<title>wire:sso:success</title>" `List.isInfixOf`) . snd)
              ('2', XML.parseLBS XML.def (Servant.errBody outcome))
                `shouldSatisfy` (isRight . snd)
              ('3', List.lookup "Set-Cookie" . Servant.errHeaders $ outcome)
                `shouldSatisfy` (isJust . snd)

      context "mobile" $ do
        context "invalid idp" $ do
          it "responds with status 303 with appropriate details." $ do
            pending

        context "denied" $ do
          it "responds with status 303 with appropriate details." $ do
            (_, _, idp) <- createTestIdP
            (_uid, outcome, loc, qry) <- requestAccessVerdict idp False mkAuthnReqMobile
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 303
              Servant.errReasonPhrase outcome `shouldBe` "forbidden"
              Servant.errBody outcome `shouldBe` "[\"No Bearer SubjectConfirmation\",\"no AuthnStatement\"]"
              uriScheme loc `shouldBe` (URI.Scheme "wire")
              List.lookup "userid" qry `shouldBe` Nothing
              List.lookup "cookie" qry `shouldBe` Nothing
              List.lookup "label"  qry `shouldBe` Just "forbidden"

        context "granted" $ do
          it "responds with status 303 with appropriate details." $ do
            (_, _, idp) <- createTestIdP
            (uid, outcome, loc, qry) <- requestAccessVerdict idp True mkAuthnReqMobile
            liftIO $ do
              Servant.errHTTPCode outcome `shouldBe` 303
              Servant.errReasonPhrase outcome `shouldBe` "success"
              Servant.errBody outcome `shouldBe` mempty
              uriScheme loc `shouldBe` (URI.Scheme "wire")
              List.lookup "label"  qry `shouldBe` Nothing
              List.lookup "userid" qry `shouldBe` (Just . cs . show $ uid)
              List.lookup "cookie" qry `shouldNotBe` Nothing
              List.lookup "cookie" qry `shouldNotBe` Just "$cookie"
                  -- cookie variable should be substituted with value.  see
                  -- 'mkVerdictGrantedFormatMobile', 'mkVerdictDeniedFormatMobile'
              let Just (ckies :: SBS) = List.lookup "cookie" qry
                  cky :: SetCookie = parseSetCookie ckies
              setCookieName cky `shouldBe` "zuid"
              ('s', setCookieSecure cky) `shouldBe` ('s', False)  -- we're in integration test mode, no https here!
              ('h', setCookieHttpOnly cky) `shouldBe` ('h', True)


mkAuthnReqWeb :: SAML.IdPId -> TestSpar ResponseLBS
mkAuthnReqWeb idpid = do
  env <- ask
  call $ get ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid) . expect2xx)

mkAuthnReqMobile :: SAML.IdPId -> TestSpar ResponseLBS
mkAuthnReqMobile idpid = do
  env <- ask
  let succurl   = [uri|wire://login-granted/?cookie=$cookie&userid=$userid|]
      errurl    = [uri|wire://login-denied/?label=$label|]
      mk        = Builder.toLazyByteString . urlEncode [] . serializeURIRef'
      arQueries = cs $ "success_redirect=" <> mk succurl <> "&error_redirect=" <> mk errurl
      arPath    = cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid <> "?" <> arQueries
  call $ get ((env ^. teSpar) . path arPath . expect2xx)

requestAccessVerdict :: HasCallStack
                     => IdP
                     -> Bool                                   -- ^ is the verdict granted?
                     -> (SAML.IdPId -> TestSpar ResponseLBS)   -- ^ raw authnreq
                     -> TestSpar ( UserId
                                 , SAML.ResponseVerdict
                                 , URI                         -- ^ location header
                                 , [(SBS, SBS)]                -- ^ query params
                                 )
requestAccessVerdict idp isGranted mkAuthnReq = do
  uid <- nextWireId
  subject <- SAML.opaqueNameID . UUID.toText <$> liftIO UUID.nextRandom
  let uref    = SAML.UserRef tenant subject
      idpid   = idp ^. SAML.idpId
      tenant  = idp ^. SAML.idpMetadata . SAML.edIssuer
  runSpar $ Spar.insertUser uref uid
  authnreq :: SAML.FormRedirect SAML.AuthnRequest <- do
    raw <- mkAuthnReq idpid
    bdy <- maybe (error "authreq") pure $ responseBody raw
    either (error . show) pure $ Servant.mimeUnrender (Servant.Proxy @SAML.HTML) bdy
  spmeta <- getTestSPMetadata
  authnresp <- do
    let mk :: SAML.FormRedirect SAML.AuthnRequest -> TestSpar SAML.AuthnResponse
        mk (SAML.FormRedirect _ req) = do
          SAML.SignedAuthnResponse (XML.Document _ el _) <- runSimpleSP $
            SAML.mkAuthnResponseWithSubj subject SAML.sampleIdPPrivkey idp spmeta req True
          either (liftIO . throwIO . ErrorCall . show) pure $ SAML.parse [XML.NodeElement el]
    mk authnreq
  let verdict = if isGranted
        then SAML.AccessGranted uref
        else SAML.AccessDenied [DeniedNoBearerConfSubj, DeniedNoAuthnStatement]
  outcome <- runSpar $ Spar.verdictHandler Nothing authnresp verdict
  let loc :: URI.URI
      loc = maybe (error "no location") (either error id . SAML.parseURI' . cs)
          . List.lookup "Location" . Servant.errHeaders
          $ outcome
      qry :: [(SBS, SBS)]
      qry = queryPairs $ uriQuery loc
  pure (uid, outcome, loc, qry)
