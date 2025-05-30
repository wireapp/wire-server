{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.SAML2.WebSSO.APISpec
  ( spec,
  )
where

import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Base64.Lazy qualified as EL (decodeLenient, encode)
import Data.Either
import Data.EitherR
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.String.Conversions
import Data.UUID
import Data.Yaml qualified as Yaml
import Network.Wai.Test
import SAML2.Util
import SAML2.WebSSO
import SAML2.WebSSO.API.Example (RequestStore)
import SAML2.WebSSO.Test.MockResponse
import SAML2.WebSSO.Test.Util
import Servant
import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Test.Hspec.Wai.Matcher
import Text.XML as XML
import URI.ByteString.QQ

spec :: Spec
spec = describe "API" $ do
  describe "base64 encoding" $ do
    describe "compatible with /usr/bin/env base64" $ do
      let check :: LBS -> Spec
          check input = it (show input) $ do
            o <- base64ours (cs input)
            t <- base64theirs (cs input)
            chomp o `shouldBe` chomp t
          chomp = reverse . dropWhile (== '\n') . reverse . cs
      check ""
      check "..."
      check "foiy0t019061.........|||"
      check (cs $ replicate 1000 '_')
    it "works with proper %0a newlines" $ do
      let encoded = "MTIzN\nDUK\n"
      EL.decodeLenient encoded `shouldBe` "12345\n"
    it "works with MSDOS and %0d%0a newlines" $ do
      let encoded = "MTIzN\r\nDUK\r\n"
      EL.decodeLenient encoded `shouldBe` "12345\n"
    it "works with just plain broken input" $ do
      -- there is no strong reason why we would want this test to pass or fail; it is just here to
      -- document the current behavior.  see also the comment in 'parseAuthnResponseBody'.
      let encoded = "MTI##zN@@DUK??"
      EL.decodeLenient encoded `shouldBe` "12345\n"
  describe "MimeRender HTML FormRedirect" $ do
    it "fake roundtrip-0" $ do
      let -- source: [2/3.5.8]
          have =
            "<samlp:LogoutRequest xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\""
              <> "    ID=\"d2b7c388cec36fa7c39c28fd298644a8\" IssueInstant=\"2004-01-21T19:00:49Z\" Version=\"2.0\">"
              <> "    <Issuer>https://IdentityProvider.com/SAML</Issuer>"
              <> "    <NameID Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent\">005a06e0-ad82-110d-a556-004005b13a2b</NameID>"
              <> "    <samlp:SessionIndex>1</samlp:SessionIndex>"
              <> "</samlp:LogoutRequest>"
          Right want =
            parseText def $
              "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                <> "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\""
                <> " \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
                <> "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\">"
                <> "<body onload=\"document.forms[0].submit()\">"
                <> "<noscript>"
                <> "<p>"
                <> "<strong>Note:</strong>Since your browser does not support JavaScript,"
                <> " you must press the Continue button once to proceed."
                <> "</p>"
                <> "</noscript>"
                <> "<form action=\"https://ServiceProvider.com/SAML/SLO/Browser/%25%25\""
                <> " method=\"post\" accept-charset=\"utf-8\">"
                <> "<input type=\"hidden\" name=\"SAMLRequest\""
                <> " value=\"PHNhbWxwOkxvZ291dFJlcXVlc3QgSUQ9ImQyYjdjMzg4Y2VjMzZmYTdjMzljMjhmZDI5ODY0NGE4IiBJc3N1ZUluc3RhbnQ9IjIwMDQtMDEtMjFUMTk6MDA6NDlaIiBWZXJzaW9uPSIyLjAiIHhtbG5zOnNhbWxwPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6cHJvdG9jb2wiPiAgICA8SXNzdWVyIHhtbG5zPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6YXNzZXJ0aW9uIj5odHRwczovL0lkZW50aXR5UHJvdmlkZXIuY29tL1NBTUw8L0lzc3Vlcj4gICAgPE5hbWVJRCBGb3JtYXQ9InVybjpvYXNpczpuYW1lczp0YzpTQU1MOjIuMDpuYW1laWQtZm9ybWF0OnBlcnNpc3RlbnQiIHhtbG5zPSJ1cm46b2FzaXM6bmFtZXM6dGM6U0FNTDoyLjA6YXNzZXJ0aW9uIj4wMDVhMDZlMC1hZDgyLTExMGQtYTU1Ni0wMDQwMDViMTNhMmI8L05hbWVJRD4gICAgPHNhbWxwOlNlc3Npb25JbmRleD4xPC9zYW1scDpTZXNzaW9uSW5kZXg+PC9zYW1scDpMb2dvdXRSZXF1ZXN0Pg==\"/>"
                <> "<noscript>"
                <> "<input type=\"submit\" value=\"Continue\"/>"
                <> "</noscript>"
                <> "</form>"
                <> "</body>"
                <> "</html>"
          Right (SomeSAMLRequest -> doc) = XML.parseText XML.def have
          spuri = [uri|https://ServiceProvider.com/SAML/SLO/Browser/%%|]
      Right want `shouldBe` (fmapL show . parseText def . cs $ mimeRender (Proxy @HTML) (FormRedirect spuri doc))

  describe "simpleVerifyAuthnResponse" $ do
    let check :: Bool -> Maybe Bool -> Bool -> Spec
        check goodsig mgoodkey expectOutcome =
          it (show expectOutcome) $ do
            let respfile =
                  if goodsig
                    then "microsoft-authnresponse-2.xml"
                    else "microsoft-authnresponse-2-badsig.xml"
            resp :: LBS <-
              cs <$> readSampleIO respfile
            midpcfg :: Maybe (IdPConfig_, SampleIdP) <-
              case mgoodkey of
                Nothing -> pure Nothing
                Just goodkey -> do
                  let cfgfile =
                        if goodkey
                          then "microsoft-idp-config.yaml"
                          else "microsoft-idp-config-badkey.yaml"
                  either (error . show) (pure . Just . (,SampleIdP undefined undefined undefined undefined)) . Yaml.decodeEither' . cs =<< readSampleIO cfgfile
            let run :: TestSP a -> IO a
                run action = do
                  ctx <- mkTestCtxSimple
                  modifyMVar_ ctx (pure . (ctxIdPs .~ maybeToList midpcfg))
                  ioFromTestSP ctx action
                missuer = (^. _1 . idpMetadata . edIssuer) <$> midpcfg
                go :: TestSP Assertion
                go = do
                  creds <- issuerToCreds missuer Nothing
                  simpleVerifyAuthnResponse creds resp >>= \case a :| [] -> pure a
            if expectOutcome
              then fmap _assID (run go) `shouldReturn` ID "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6"
              else run go `shouldThrow` anyException
    context "good signature" $ do
      context "known key" $ check True (Just True) True
      context "bad key" $ check True (Just False) False
      context "unknown key" $ check True Nothing False
    context "bad signature" $ do
      context "known key" $ check False (Just True) False
      context "bad key" $ check False (Just False) False
      context "unknown key" $ check False Nothing False
    describe "counter-examples" $ do
      it "1" $ do
        rawResp <- readSampleIO "authnresponse-3.xml"
        xmlResp <- either (error . show) pure $ decode @_ @AuthnResponse (cs rawResp)
        let respId = fromID $ xmlResp ^. rspID
        respId `shouldBe` "_5ae4ec37-8e91-4f15-9170-655941b42b8e"
  describe "cookies" $ do
    let rndtrip =
          parseUrlPiece @Cky
            . cs
            . snd
            . cookieToHeader
    it "roundtrip-1" $ do
      ctx <- mkTestCtxSimple
      c1 <- ioFromTestSP ctx $ toggleCookie @CookieName "/" Nothing
      rndtrip c1 `shouldBe` Left "missing cookie value"
    it "roundtrip-2" $ do
      ctx <- mkTestCtxSimple
      c2 <- ioFromTestSP ctx $ toggleCookie @CookieName "/" (Just ("nick", defReqTTL))
      rndtrip c2 `shouldBe` Right c2
  describe "meta" . withapp (Proxy @APIMeta') (meta "toy-sp" defSPIssuer defResponseURI defContactPersons) mkTestCtxSimple $ do
    it "responds with 200 and an 'SPSSODescriptor'" . runtest' $ do
      get "/meta"
        `shouldRespondWith` 200 {matchBody = bodyContains "OrganizationName xml:lang=\"EN\">toy-sp"}
  describe "authreq" $ do
    context "invalid uuid" . withapp (Proxy @APIAuthReq') (authreq' defSPIssuer) mkTestCtxSimple $ do
      it "responds with 400" . runtest' $ do
        get "/authreq/broken-uuid" `shouldRespondWith` 400
    context "unknown idp" . withapp (Proxy @APIAuthReq') (authreq' defSPIssuer) mkTestCtxSimple $ do
      it "responds with 404" . runtest' $ do
        get "/authreq/6bf0dfb0-754f-11e8-b71d-00163e5e6c14" `shouldRespondWith` 404
    context "known idp" . withapp (Proxy @APIAuthReq') (authreq' defSPIssuer) mkTestCtxWithIdP $ do
      it "responds with 200" . runtest $ \ctxv -> do
        ctx <- liftIO $ readMVar ctxv
        let idpid = testIdPConfig ^. idpId . to (cs . idPIdToST)
            [(testIdPConfig, _)] = ctx ^. ctxIdPs
        get ("/authreq/" <> idpid) `shouldRespondWith` 200
      it "responds with a body that contains the IdPs response URL" . runtest $ \ctxv -> do
        ctx <- liftIO $ readMVar ctxv
        let idpid = testIdPConfig ^. idpId . to (cs . idPIdToST)
            [(testIdPConfig, _)] = ctx ^. ctxIdPs
        get ("/authreq/" <> idpid)
          `shouldRespondWith` 200
            { matchBody = bodyContains . cs . renderURI $ testIdPConfig ^. idpMetadata . edRequestURI
            }
  describe "authresp" $ do
    let -- Create an AuthnRequest in the SP, then call 'mkAuthnResponse' to make an 'AuthnResponse'
        -- in the IdP, then post the 'AuthnResponse' to the appropriate SP end-point.  @spmeta@ is
        -- needed for making the 'AuthnResponse'.
        postTestAuthnResp :: (HasCallStack) => CtxV -> Bool -> Bool -> WaiSession st SResponse
        postTestAuthnResp ctxv badIdP badTimeStamp = do
          aresp <- liftIO . ioFromTestSP ctxv $ do
            (testIdPConfig, SampleIdP _ privkey _ _) <- do
              idpctx <- liftIO $ makeTestIdPConfig
              unless badIdP $ do
                liftIO $ modifyMVar_ ctxv (pure . (ctxIdPs %~ (idpctx :)))
              pure idpctx
            spmeta :: SPMetadata <- mkTestSPMetadata
            spiss <- defSPIssuer
            authnreq :: AuthnRequest <- createAuthnRequest 3600 spiss (testIdPConfig ^. idpMetadata . edIssuer)
            fromSignedAuthnResponse
              <$> (if badTimeStamp then timeTravel 1800 else id) (mkAuthnResponse privkey testIdPConfig spmeta (Just authnreq) True)
          postHtmlForm "/authresp" [("SAMLResponse", cs . EL.encode . renderLBS def $ aresp)]

    let testAuthRespApp :: IO CtxV -> SpecWith (CtxV, Application) -> Spec
        testAuthRespApp =
          withapp
            (Proxy @APIAuthResp')
            (authresp' Nothing defSPIssuer defResponseURI (HandleVerdictRedirect (simpleOnSuccess SubjectFoldCase)))

    let testAuthnRespWithCtx ::
          String ->
          (ID AuthnRequest -> Issuer -> Time -> RequestStore) ->
          (WaiSession st SResponse -> WaiExpectation ()) ->
          SpecWith (CtxV, Application)
        testAuthnRespWithCtx name createRequestStore checkPostCondition =
          it name . runtest $ \ctxv -> do
            -- we inline `postTestAuthnResp` here to avoid making it more branchy.
            (aresp, authnreq, (idpConfig, sampleIdP), timestamp) <- liftIO . ioFromTestSP ctxv $ do
              idpEntry@(testIdPConfig, SampleIdP _ privkey _ _) <- liftIO makeTestIdPConfig
              spmeta :: SPMetadata <- mkTestSPMetadata
              spiss <- defSPIssuer
              authnreq :: AuthnRequest <- createAuthnRequest 3600 spiss (testIdPConfig ^. idpMetadata . edIssuer)
              aresp <- fromSignedAuthnResponse <$> mkAuthnResponse privkey testIdPConfig spmeta (Just authnreq) True
              timestamp <- getNow
              pure (aresp, authnreq, idpEntry, timestamp)

            let iss = idpConfig ^. idpMetadata . edIssuer
                loadCtx :: Ctx -> Ctx
                loadCtx =
                  (ctxIdPs .~ [(idpConfig, sampleIdP)])
                    . (ctxRequestStore .~ createRequestStore (authnreq ^. rqID) iss timestamp)
            liftIO $ modifyMVar_ ctxv $ pure . loadCtx
            postHtmlForm "/authresp" [("SAMLResponse", cs . EL.encode . renderLBS def $ aresp)]
              & checkPostCondition

    context "unknown idp" . testAuthRespApp mkTestCtxSimple $ do
      it "responds with 404" . runtest $ \ctx -> do
        postTestAuthnResp ctx True False
          `shouldRespondWith` 404 {matchBody = bodyContains "Unknown IdP"}
    context "known idp, bad timestamp" . testAuthRespApp mkTestCtxWithIdP $ do
      it "responds with 403" . runtest $ \ctx -> do
        postTestAuthnResp ctx False True
          `shouldRespondWith` 403 {matchBody = bodyContains "IssueInstant"}

    context "known idp, good timestamp" . testAuthRespApp mkTestCtxWithIdP $ do
      testAuthnRespWithCtx
        "responds with 303 if authnrequest is stored in SP"
        (\reqId issuer timestamp -> Map.singleton reqId (issuer, 10 `addTime` timestamp))
        (`shouldRespondWith` 303)

      testAuthnRespWithCtx
        "no authn req matching response is stored on sp"
        (\_reqId _issuer _timestamp -> Map.empty)
        (`shouldRespondWith` 403 {matchBody = bodyContains "bad InResponseTo attribute(s)"})

      testAuthnRespWithCtx
        "idp issuer stored with authn req on sp doesn't match the one in resp"
        ( \reqId _issuer timestamp ->
            Map.singleton reqId ((Issuer [uri|https://anything.example/|]), 10 `addTime` timestamp)
        )
        (`shouldRespondWith` 403 {matchBody = bodyContains "mismatching Issuers"})

  describe "mkAuthnResponse (this is testing the test helpers)" $ do
    it "Produces output that decodes into 'AuthnResponse'" $ do
      ctx <- mkTestCtxWithIdP
      spmeta <- ioFromTestSP ctx mkTestSPMetadata
      (testIdPConfig, SampleIdP _ privcert _ _) <- makeTestIdPConfig
      Right authnreq :: Either SomeException AuthnRequest <- do
        let idpiss = testIdPConfig ^. idpMetadata . edIssuer
        try . ioFromTestSP ctx $ do
          spiss <- defSPIssuer
          createAuthnRequest 3600 spiss idpiss
      SignedAuthnResponse authnrespDoc <-
        ioFromTestSP ctx $ mkAuthnResponse privcert testIdPConfig spmeta (Just authnreq) True
      parseFromDocument @AuthnResponse authnrespDoc `shouldSatisfy` isRight
    let check :: (HasCallStack) => Bool -> (Either SomeException () -> Bool) -> IO ()
        check certIsGood expectation = do
          testIdPConfig@(_, SampleIdP _ privkey _ goodCert) <- makeTestIdPConfig
          (_, SampleIdP _ _ _ badCert) <- makeTestIdPConfig
          let idpcfg = testIdPConfig & _1 . idpMetadata . edCertAuthnResponse .~ (cert :| [])
              cert = if certIsGood then goodCert else badCert
          ctx <- mkTestCtxSimple
          modifyMVar_ ctx $ pure . (ctxIdPs .~ [idpcfg])
          spmeta <- ioFromTestSP ctx mkTestSPMetadata
          let idpissuer :: Issuer = idpcfg ^. _1 . idpMetadata . edIssuer
          result :: Either SomeException () <- try . ioFromTestSP ctx $ do
            spissuer :: Issuer <- defSPIssuer
            authnreq <- createAuthnRequest 3600 spissuer idpissuer
            SignedAuthnResponse authnrespDoc <-
              liftIO . ioFromTestSP ctx $ mkAuthnResponse privkey (idpcfg ^. _1) spmeta (Just authnreq) True
            let authnrespLBS = renderLBS def authnrespDoc
            creds <- issuerToCreds (Just idpissuer) Nothing
            void $ simpleVerifyAuthnResponse creds authnrespLBS
          result `shouldSatisfy` expectation
    it "Produces output that passes 'simpleVerifyAuthnResponse'" $ do
      check True isRight
    it "Produces output that is rejected by 'simpleVerifyAuthnResponse' if the signature is wrong" $ do
      check False isLeft

  describe "IdPMetadata parsing" $ do
    let parseSample :: FilePath -> IO (Either String IdPMetadata)
        parseSample samplePath = decode <$> readSampleIO samplePath

    it "fails with helpful error message if HTTP-POST is missing (eg., if only HTTP-Redirect is provided)" $ do
      res <- parseSample "post-missing.xml"
      res `shouldBe` Left "Couldnt find any matches for: \"Binding\" attribute with value \"urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST\""

    it "succeeds with HTTP-Post (not all caps)" $ do
      res <- parseSample "authnresponse-case-insensitive.xml"
      res `shouldSatisfy` isRight

  describe "AuthnResponse parsing" $ do
    let parseSample :: FilePath -> IO (Either String AuthnResponse)
        parseSample samplePath = decode <$> readSampleIO samplePath
    it "works" $ do
      res <- parseSample "authnresponse-1.xml"
      res `shouldSatisfy` isRight

  describe "vendor compatibility tests" $ do
    vendorCompatibility "okta.com" [uri|https://staging-nginz-https.zinfra.io/sso/finalize-login|]
    -- https://developer.okta.com/signup/

    vendorCompatibility "azure.microsoft.com" [uri|https://zb2.zerobuzz.net:60443/authresp|]
    -- https://azure.microsoft.com/en-us/

    vendorCompatibility "centrify.com" [uri|https://prod-nginz-https.wire.com/sso/finalize-login|]

  -- TODO:
  --  * onelogin
  --  * jives [https://community.jivesoftware.com/docs/DOC-240217#jive_content_id_IdP_Metadata]

  describe "simpleVerifyAuthnResponse, second attempt" $ do
    let check :: FilePath -> FilePath -> Expectation
        check metaFile respFile = do
          resp :: LBS <- readSampleIOLBS respFile
          assertions <- liftIO $ do
            idpCfg :: IdPConfig_ <- do
              raw :: LBS <- cs <$> readSampleIO metaFile
              idpMeta <- either (error . show) pure (mimeUnrender @_ @IdPMetadata (Proxy @XML) raw)
              pure $ IdPConfig (IdPId nil) idpMeta ()
            ctx <- mkTestCtxSimple
            modifyMVar_ ctx (pure . (ctxIdPs .~ [(idpCfg, SampleIdP undefined undefined undefined undefined)]))
            ioFromTestSP ctx $ do
              let issuer = idpCfg ^. idpMetadata . edIssuer
              creds <- issuerToCreds (Just issuer) Nothing
              simpleVerifyAuthnResponse creds resp

          length assertions `shouldBe` 1

    it "utf8 characters in authentication response are parsed correctly (not as Char8)" $ do
      check "microsoft-azure-utf8-issue-metadata.xml" "microsoft-azure-utf8-issue-authentication-request.xml"

    it "utf8 encoding, another test with keycloak" $ do
      check "keycloak-idp-metadata.xml" "keycloak-response.xml"
