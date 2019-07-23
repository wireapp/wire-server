{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Data.Metrics.Test (servantApiConsistency)
import Data.Proxy
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import Test.Hspec

import qualified Test.Tasty


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)
  it "..." $ Test.Tasty.defaultMain (servantApiConsistency (Proxy @API.API))
