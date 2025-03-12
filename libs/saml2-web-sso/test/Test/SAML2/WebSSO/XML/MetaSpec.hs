{-# LANGUAGE OverloadedStrings #-}

module Test.SAML2.WebSSO.XML.MetaSpec
  ( spec,
  )
where

import Control.Lens
import Data.EitherR
import Data.String.Conversions
import SAML2.WebSSO
import SAML2.WebSSO.Test.Util
import Test.Hspec
import Text.XML
import URI.ByteString.QQ

spec :: Spec
spec = do
  describe "spDesc" $ do
    it "does not smoke" $ do
      testCtx1 <- mkTestCtxSimple
      have <-
        ioFromTestSP testCtx1 $
          mkSPMetadata
            "drnick"
            [uri|http://example.com/|]
            [uri|http://example.com/sso/login|]
            [fallbackContact]
      let want = testSPMetadata (have ^. spID)
      have `shouldBe` want
  describe "spMeta" $ do
    it "does not smoke" $ do
      let given = testSPMetadata $ mkID "_e3a565aa-1392-4446-a4d6-3771453808f0"
          want = renderToDocument given
      have :: Either String Document <- fmapL show . parseText def . cs <$> readSampleIO "our-spssodescriptor.xml"
      have `shouldBe` Right want

testSPMetadata :: ID SPMetadata -> SPMetadata
testSPMetadata mid =
  SPMetadata
    { _spID = mid,
      _spValidUntil = fromTime $ addTime (60 * 60 * 24 * 365) timeNow,
      _spCacheDuration = 2592000,
      _spOrgName = mkXmlText "drnick",
      _spOrgDisplayName = mkXmlText "drnick",
      _spOrgURL = [uri|http://example.com/|],
      _spResponseURL = [uri|http://example.com/sso/login|],
      _spContacts = [fallbackContact]
    }
