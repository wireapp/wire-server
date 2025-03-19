{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.Text.XML.DSigSpec
  ( spec,
  )
where

import Control.Monad (replicateM_, (>=>))
import Data.Either
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.String.Conversions
import qualified Data.UUID as UUID
import qualified Data.X509 as X509
import SAML2.WebSSO.Test.Util
import qualified Samples
import Test.Hspec
import Text.Hamlet.XML (xml)
import Text.XML
import Text.XML.DSig

spec :: Spec
spec = describe "xml:dsig" $ do
  describe "parseKeyInfo" $ do
    it "works(1)" $ do
      keyinfo <- readSampleIO "microsoft-idp-keyinfo.xml"
      let want = Samples.microsoftIdpKeyinfo
          Right (SignCreds _ (SignKeyRSA have)) = certToCreds =<< parseKeyInfo True keyinfo
      have `shouldBe` want
    it "works(2)" $ do
      keyinfo <- readSampleIO "okta-keyinfo-1.xml"
      (certToCreds =<< parseKeyInfo True keyinfo) `shouldSatisfy` isRight
    it "works against mkSignCredsWithCert" $ do
      (_privcreds, creds, cert) <- mkSignCredsWithCert Nothing 192
      verifySelfSignature cert `shouldBe` Right ()
      certToCreds cert `shouldBe` Right creds
  describe "parseKeyInfo / renderKeyInfo roundtrip" $ do
    let check :: (HasCallStack) => Int -> Expectation
        check size = do
          (_, _, x :: X509.SignedCertificate) <- mkSignCredsWithCert Nothing size
          let y :: LT = renderKeyInfo x
          let z :: X509.SignedCertificate = either error id $ parseKeyInfo True y
          x `shouldBe` z
    it "works (96 bytes)" $ replicateM_ 10 (check 96)
    it "works (128 bytes)" $ check 128
    it "works (256 bytes)" $ check 256
    it "works (512 bytes)" $ check 512
  describe "verify" $ do
    it "works" $ do
      Right keyinfo <- (parseKeyInfo True >=> certToCreds) <$> readSampleIO "microsoft-idp-keyinfo.xml"
      raw <- cs <$> readSampleIO "microsoft-authnresponse-2.xml"
      verify (keyinfo :| []) raw "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6" `shouldBe` Right ()
    it "works with more than one key" $ do
      SampleIdP _ _ cert _ <- makeSampleIdPMetadata
      Right keyinfo <- (parseKeyInfo True >=> certToCreds) <$> readSampleIO "microsoft-idp-keyinfo.xml"
      raw <- cs <$> readSampleIO "microsoft-authnresponse-2.xml"
      verify (keyinfo :| [cert]) raw "_c79c3ec8-1c26-4752-9443-1f76eb7d5dd6" `shouldBe` Right ()
