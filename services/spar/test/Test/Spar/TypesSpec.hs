{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.TypesSpec where

import Data.Id
import Data.UUID
import Imports
import Spar.Types
import Test.Hspec
import URI.ByteString
import URI.ByteString.QQ
import Web.Cookie

spec :: Spec
spec = do
  describe "mkVerdictGrantedFormatMobile" $ do
    it "1" $ do
      mkVerdictGrantedFormatMobile [uri|wire://granted/$cookie/$userid|] def (Id nil)
        `shouldBe` Right [uri|wire://granted/name=value/00000000-0000-0000-0000-000000000000|]
    it "2" $ do
      mkVerdictGrantedFormatMobile ([uri|http://$cookie:1039/granted|] :: URI) def (Id nil)
        `shouldBe` Right [uri|http://name=value:1039/granted|]
  describe "mkVerdictDeniedFormatMobile" $ do
    it "1" $ do
      mkVerdictDeniedFormatMobile [uri|wire://$label|] "forbidden"
        `shouldBe` Right [uri|wire://forbidden|]
    it "2" $ do
      mkVerdictDeniedFormatMobile [uri|http://bad/?label=$label|] "forbidden"
        `shouldBe` Right [uri|http://bad/?label=forbidden|]
