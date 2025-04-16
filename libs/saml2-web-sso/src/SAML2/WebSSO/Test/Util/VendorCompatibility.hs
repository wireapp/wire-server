{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module SAML2.WebSSO.Test.Util.VendorCompatibility
  ( vendorCompatibility,
  )
where

import Control.Concurrent.MVar
import Control.Lens
import Control.Monad
import Data.ByteString.Base64.Lazy qualified as EL (encode)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String.Conversions
import Data.UUID qualified as UUID
import Network.HTTP.Types.Status (statusCode)
import Network.Wai.Test
import SAML2.WebSSO
import SAML2.WebSSO.Test.Util.Misc
import SAML2.WebSSO.Test.Util.TestSP
import SAML2.WebSSO.Test.Util.Types
import Servant
import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Text.Show.Pretty (ppShow)
import URI.ByteString as URI

testAuthRespApp :: (HasCallStack) => URI.URI -> SpecWith (CtxV, Application) -> Spec
testAuthRespApp ssoURI =
  withapp
    (Proxy @("sso" :> APIAuthResp'))
    (authresp' Nothing spissuer respuri (HandleVerdictRedirect (simpleOnSuccess SubjectFoldCase)))
    mkTestCtxSimple
  where
    spissuer = Issuer <$> respuri
    respuri = pure ssoURI

vendorCompatibility :: (HasCallStack) => FilePath -> URI.URI -> Spec
vendorCompatibility filePath ssoURI = testAuthRespApp ssoURI $ do
  let filePathMeta = "vendors/" <> filePath <> "-metadata.xml"
      filePathResp = "vendors/" <> filePath <> "-authnresp.xml"

  it filePath . runtest $ \ctx -> do
    idpmeta :: IdPMetadata <- readSampleIO filePathMeta >>= either (error . show) pure . decode
    liftIO $ length (show idpmeta) `shouldNotBe` 0

    checkAuthResp <- liftIO $ doesSampleExistIO filePathResp
    if not checkAuthResp
      then liftIO $ do
        putStrLn $ "*** no response for filePath (" <> filePathResp <> ") [skipping]"
      else do
        authnrespRaw :: LT <- readSampleIO filePathResp
        authnresp :: AuthnResponse <- either (error . show) pure $ decode authnrespRaw
        let idpcfg = IdPConfig {..}
              where
                _idpId = IdPId UUID.nil
                _idpMetadata = idpmeta
                _idpExtraInfo = ()
            sampleidp :: SampleIdP
            sampleidp = SampleIdP idpmeta (error "no private credentials available") undefined undefined

        let -- NB: reqstore, new are taken from the unsigned AuthnResponse header.  the test still
            -- makes perfect sense given the information is available in the header.  if it is
            -- not, just dig into the assertions and take the information from there.
            -- authnresp inResponseTo, with comfortable end of life.
            reqstore :: Map.Map (ID AuthnRequest) (Issuer, Time)
            reqstore = Map.singleton (fromJust $ authnresp ^. rspInRespTo) (idpIssuer, timeInALongTime)

            -- The issuer we expect in the SAML response (IdP -> SP)
            idpIssuer = idpcfg ^. idpMetadata . edIssuer
            -- 1 second after authnresp IssueInstant
            now :: Time
            now = addTime 1 $ authnresp ^. rspIssueInstant

        liftIO . modifyMVar_ ctx $ \ctx' ->
          pure $
            ctx'
              & ctxIdPs .~ [(idpcfg, sampleidp)]
              -- & ctxConfig . cfgSPAppURI .~ _
              -- (the SPAppURI default is a incorrect, but that should not invalidate the test)
              & ctxConfig . cfgDomainConfigs . _Left . cfgSPSsoURI .~ ssoURI
              & ctxRequestStore .~ reqstore
              & ctxNow .~ now
        verdict :: SResponse <-
          -- it is essential to not use @encode authnresp@ here, as that has no signature!
          postHtmlForm
            "/sso/authresp"
            [("SAMLResponse", cs . EL.encode . cs $ authnrespRaw)]
        when (statusCode (simpleStatus verdict) /= 303) . liftIO $ do
          putStrLn $ ppShow verdict
        liftIO $ statusCode (simpleStatus verdict) `shouldBe` 303
