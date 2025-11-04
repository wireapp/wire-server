{-# OPTIONS_GHC -Wno-orphans #-}

module Wire.ErrorSpec (spec) where

import Data.Aeson
import Data.ByteString.UTF8 qualified as UTF8BS
import Data.Text.Lazy qualified as LT
import Imports
import Network.HTTP.Types
import Network.Wai.Utilities.Error qualified as Wai
import Servant
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.Error

spec :: Spec
spec = describe "httpErrorToServerError, serverErrorToHttpError" do
  prop "serverErrorToHttpError == httpErrorToServerError^(-1) (kinda)" $ \waiError ->
    let httpError = StdError waiError
     in serverErrorToHttpError (httpErrorToServerError httpError) === httpError

  prop "httpErrorToServerError == serverErrorToHttpError^(-1)" $ \serverError ->
    httpErrorToServerError (serverErrorToHttpError serverError) === serverError

  prop "servant error not containing a wai error in its body" $ \serverError_ ->
    let serverError = serverError_ {errBody = "<html>...</html>"}
     in httpErrorToServerError (serverErrorToHttpError serverError) === serverError

instance Arbitrary ServerError where
  -- headers are lost in translation
  arbitrary =
    ServerError
      <$> (abs <$> arbitrary)
      <*> arbitrary
      <*> (encode <$> (arbitrary @Wai.Error))
      <*> pure []
  shrink _ = []

instance Arbitrary Wai.Error where
  arbitrary =
    Wai.mkError
      <$> (Status <$> (abs <$> arbitrary) <*> (UTF8BS.fromString <$> arbitrary))
      <*> (LT.pack <$> arbitrary)
      <*> (LT.pack <$> arbitrary)
  shrink _ = []
