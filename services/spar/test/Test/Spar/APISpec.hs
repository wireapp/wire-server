{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Proxy (Proxy(Proxy))
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Spar.Types (IdPMetadataInfo(IdPMetadataValue), SsoSettings)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.QuickCheck (property)

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Lens as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)

  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty

  it "roundtrip: IdPMetadataInfo" . property $ \(val :: IdPMetadataInfo) -> do
    let withoutRaw (IdPMetadataValue _ x) = x
    (withoutRaw <$> (Aeson.eitherDecode . Aeson.encode) val) `shouldBe` Right (withoutRaw val)

  describe "SsoSettings JSON instance" $ do
    it "always has and requires the field default_sso_code" $
      property $ \(ssoSettings :: SsoSettings) -> do
        let object = Aeson.toJSON ssoSettings
        let objectWithoutKey = Lens.over Aeson._Object (HM.delete "default_sso_code") $ object
        (HM.lookup "default_sso_code" =<< Lens.preview Aeson._Object object)
          `shouldSatisfy` isJust
        Aeson.parseMaybe (Aeson.parseJSON @SsoSettings) objectWithoutKey
          `shouldSatisfy` isNothing
