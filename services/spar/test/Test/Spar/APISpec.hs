{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Metrics.Test (pathsConsistencyCheck)
import Data.Metrics.Servant (routesToPaths)
import Data.Proxy
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Test.Hspec


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)
  it "api consistency" $ do
    pathsConsistencyCheck (routesToPaths @API.API) `shouldBe` mempty
