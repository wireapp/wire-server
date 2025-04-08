{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.SAML2.UtilSpec
  ( spec,
  )
where

import SAML2.Util
import SAML2.WebSSO.Test.Arbitrary ()
import Test.Hspec
import URI.ByteString.QQ

-- | @uriSegments "/one/two" == uriSegments "one/two/" == uriSegments "///one//two///" == ["one", "two"]@
spec :: Spec
spec = do
  describe "(-/)" $ do
    it "a)" $ "one" -/ "two" `shouldBe` "/one/two"
    it "b)" $ "/one" -/ "two" `shouldBe` "/one/two"
    it "c)" $ "/one/" -/ "two" `shouldBe` "/one/two"
    it "d)" $ "/one" -/ "/two" `shouldBe` "/one/two"
    it "e)" $ "/one/" -/ "/two" `shouldBe` "/one/two"
    it "f)" $ "/one/" -/ "/two/" `shouldBe` "/one/two"
    it "g)" $ "///one///" -/ "///two///" `shouldBe` "/one/two"
  describe "(=/)" $ do
    it "a)" $ [uri|http://a.b/ef|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "b)" $ [uri|http://a.b/ef/|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "c)" $ [uri|http://a.b/ef|] =/ "/gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "d)" $ [uri|http://a.b/ef/|] =/ "/gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "e)" $ [uri|http://a.b/ef////|] =/ "gh" `shouldBe` [uri|http://a.b/ef/gh|]
    it "f)" $ [uri|http://a.b/ef|] =/ "//////gh" `shouldBe` [uri|http://a.b/ef/gh|]
