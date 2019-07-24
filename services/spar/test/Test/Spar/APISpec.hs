{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Spar.APISpec where

import Imports

import Arbitrary ()
import Control.Exception
import Data.Metrics.Test (servantApiConsistency)
import Data.Proxy
import Servant.Swagger (validateEveryToJSON)
import Spar.API as API
import System.Exit
import Test.Hspec

import qualified Test.Tasty


spec :: Spec
spec = do
  -- Note: SCIM types are not validated because their content-type is 'SCIM'.
  validateEveryToJSON (Proxy @API.OutsideWorldAPI)
  tastyToHspec (servantApiConsistency (Proxy @API.API))


tastyToHspec :: Test.Tasty.TestTree -> Spec
tastyToHspec tasty = it "tasty" $ do
  Test.Tasty.defaultMain tasty `catch` (`shouldBe` ExitSuccess)
