{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-unused-imports #-}

module Test.SAML2.WebSSO.XML.ExamplesSpec
  ( spec,
  )
where

import Control.Exception
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import qualified Data.ByteString.Base64.Lazy as EL (decodeLenient)
import qualified Data.CaseInsensitive as CI
import Data.Either
import qualified Data.List as List
import Data.List.NonEmpty as NL
import qualified Data.Map as Map
import Data.String.Conversions
import SAML2.Util
import SAML2.WebSSO
import SAML2.WebSSO.Test.Lenses
import SAML2.WebSSO.Test.MockResponse
import SAML2.WebSSO.Test.Util
import qualified Samples
import System.Environment (setEnv)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Text.Show.Pretty (ppShow)
import Text.XML
import Text.XML.DSig as DSig
import URI.ByteString

spec :: Spec
spec = describe "XML serialization" $ do
  describe "unit tests" $ do
    it "Time seconds have no more than 7 decimal digits" $ do
      --  (or else azure/AD will choke on it with a very useless error message)
      renderTime (unsafeReadTime "2013-03-18T03:28:54.1839884Z")
        `shouldBe` renderTime (unsafeReadTime "2013-03-18T03:28:54.18398841817Z")
      let decimalses = dot <$> List.inits "1839884181781"
            where
              dot "" = ""
              dot s = '.' : s
      forM_ decimalses $ \decimals -> do
        let bad = "2013-03-18T03:28:54" <> decimals <> "Z"
            good = "2013-03-18T03:28:54" <> List.take 8 decimals <> "Z"
        renderTime (unsafeReadTime good) `shouldBe` renderTime (unsafeReadTime bad)
    roundtrip 0 (readSampleIO "microsoft-authnrequest-1.xml") Samples.microsoft_authnrequest_1
  -- roundtrip 1 (readSample "microsoft-authnresponse-0.xml") Samples.microsoft_authnresponse_0
  -- roundtrip 2 (readSample "microsoft-authnresponse-1.xml") Samples.microsoft_authnresponse_1
  -- roundtrip 3 (readSample "microsoft-authnresponse-2.xml") Samples.microsoft_authnresponse_2
  -- roundtrip 4 (readSample "microsoft-meta-2.xml") Samples.microsoft_meta_2
  -- roundtrip 5 (readSample "onelogin-request-1.xml") Samples.onelogin_request_1
  -- roundtrip 6 (readSample "onelogin-response-1.xml") (undefined :: AuthnResponse)
  -- roundtrip 7 (readSample "onelogin-response-2.xml") (undefined :: AuthnResponse)
  -- roundtrip 8 (readSample "onelogin-response-3.xml") (undefined :: AuthnResponse)

  describe "AuthnRequest" $ do
    it "works" $ do
      let req =
            AuthnRequest
              { _rqID = mkID "_233f9cee-b6bc-11e8-87ff-97a7b126bf5a",
                _rqIssueInstant = unsafeReadTime "2013-03-18T07:33:56Z",
                _rqIssuer = iss,
                _rqNameIDPolicy = Nothing
              }
          iss = Issuer $ unsafeParseURI "http://wire.com"
      decodeElem @Issuer @(Either String) (encodeElem iss) `shouldBe` Right iss
      decodeElem @AuthnRequest @(Either String) (encodeElem req) `shouldBe` Right req
  describe "AuthnResponse with tricky Subject elements" $ do
    -- https://developer.okta.com/blog/2018/02/27/a-breakdown-of-the-new-saml-authentication-bypass-vulnerability
    -- We were not affected, but it's always good to have tests.  'mkAuthnResponseWithRawSubj'
    -- deletes all 'SubjectConfirmation' children, but that is irrelevant for what we're testing
    -- here.

    let check :: (HasCallStack) => String -> [Node] -> Maybe ST -> Spec
        check msg nameId expectedsubj = it msg $ do
          ctx :: CtxV <- mkTestCtxSimple
          spmeta :: SPMetadata <- ioFromTestSP ctx mkTestSPMetadata
          (testIdPConfig, SampleIdP _ sampleIdPPrivkey _ _) <- makeTestIdPConfig
          authnreq :: AuthnRequest <- ioFromTestSP ctx $ createAuthnRequest 3600 defSPIssuer
          SignedAuthnResponse doc <-
            ioFromTestSP ctx $
              mkAuthnResponseWithRawSubj nameId sampleIdPPrivkey testIdPConfig spmeta authnreq True
          let parsed :: Either String AuthnResponse = parseFromDocument @AuthnResponse doc
          case expectedsubj of
            Nothing -> do
              parsed `shouldSatisfy` isLeft
            Just subj -> do
              parsed `shouldSatisfy` isRight
              let Right (subjid :: NameID) = parsed <&> (^. assertionL . assContents . sasSubject . subjectID)
              (CI.original <$> shortShowNameID subjid) `shouldBe` Just subj

        mknid :: [Node] -> Node
        mknid = NodeElement . Element "{urn:oasis:names:tc:SAML:2.0:assertion}NameID" mempty

    check
      "good"
      [mknid [NodeContent "xkcd"]]
      (Just "xkcd")
    check
      "NameID with fragmented contents"
      [mknid [NodeContent "we", NodeContent "f"]]
      (Just "wef")
    check
      "no NameID"
      []
      Nothing
    check
      "empty NameID"
      [mknid mempty]
      Nothing
    check
      "NameID with comments in contents"
      [mknid [NodeContent "wef", NodeComment "phlaa", NodeContent "fie"]]
      Nothing
    check
      "NameID with whitespace in contents"
      [mknid [NodeContent "  we f  "]]
      (Just "  we f  ")
    check
      "NameID with fragmented, whitespacy contents"
      [mknid [NodeContent "  we  ", NodeContent "  f  "]]
      (Just "  we    f  ")
