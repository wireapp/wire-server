{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Data.Proxy
import Arbitrary ()
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Test.Hspec

spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)
