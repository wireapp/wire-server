{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Aeson (ToJSON, encode, eitherDecode)
import Data.Metrics.Servant (routesToPaths)
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Proxy (Proxy(Proxy))
import Data.Swagger.Schema (ToSchema)
import Data.Swagger.Schema.Validation (validateToJSON)
import Servant (JSON)
import Servant.Swagger.Internal.Test (props)
import Servant.Swagger.Internal.TypeLevel.API (Remove)
import Servant.Swagger.TypeLevel (BodyTypes)
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Spar.API (OutsideWorldAPI)
import Spar.Types (IdPMetadataInfo)
import Test.Hspec (Spec, it, shouldBe)
import Test.QuickCheck (property)


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM', or because they
  -- are removed here explicitly.
  props
    (Proxy @[ToJSON, ToSchema])
    (null . validateToJSON)
    (Proxy @(Remove IdPMetadataInfo (BodyTypes JSON OutsideWorldAPI)))

  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty

  it "roundtrip: IdPMetadataInfo" . property $ \(val :: IdPMetadataInfo) -> do
    (eitherDecode . encode) val `shouldBe` Right val
