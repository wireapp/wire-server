{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Aeson (encode, eitherDecode)
import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Proxy (Proxy(Proxy))
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Spar.Types (IdPMetadataInfo(IdPMetadataValue), SSOSettings)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Control.Lens as Lens


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)

  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty

  it "roundtrip: IdPMetadataInfo" . property $ \(val :: IdPMetadataInfo) -> do
    let withoutRaw (IdPMetadataValue _ x) = x
    (withoutRaw <$> (eitherDecode . encode) val) `shouldBe` Right (withoutRaw val)

  describe "SSOSettings JSON instance" $ do
    it "always has and requires the field default_sso_code" $
      property $ \(ssoSettings :: SSOSettings) -> do
        let object = Aeson.toJSON ssoSettings
        let objectWithoutKey = Lens.over Aeson._Object (HM.delete "default_sso_key") $ object
        (HM.lookup "sso_default_code" =<< Lens.preview Aeson._Object object)
          `shouldSatisfy` isJust
        Aeson.parseMaybe (Aeson.parseJSON @SSOSettings) objectWithoutKey
          `shouldSatisfy` isNothing
