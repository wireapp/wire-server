{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.MiscSpec where

import Data.Aeson (eitherDecode', encode)
import Test.Hspec (Spec, describe, it, shouldBe)
import Web.Scim.Server.Mock (Id (..))
import Web.Scim.Test.Util ((<//>))

-- | These tests are also doctests in the prod code,
-- [but](https://github.com/zinfra/backend-issues/issues/1549).
spec :: Spec
spec = do
  describe "(<//>)" $ do
    it "works" $ do
      "a" <//> "b" `shouldBe` "a/b"
      "a" <//> "/b" `shouldBe` "a/b"
      "a/" <//> "b" `shouldBe` "a/b"
      "a/" <//> "/b" `shouldBe` "a/b"
  describe "Id" $ do
    it "works" $ do
      (eitherDecode' . encode) (Id 3) `shouldBe` Right Id {unId = 3}
