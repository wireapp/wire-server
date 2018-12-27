{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Imports
import System.Environment
import Test.Hspec
import Util

import qualified Test.Spar.APISpec
import qualified Test.Spar.AppSpec
import qualified Test.Spar.DataSpec
import qualified Test.Spar.Intra.BrigSpec
import qualified Test.Spar.SCIMSpec


main :: IO ()
main = do
  env <- mkEnvFromOptions
  withArgs [] . hspec . beforeAll (pure env) . afterAll destroyEnv $ mkspec

mkspec :: SpecWith TestEnv
mkspec = do
    describe "Test.Spar.API" Test.Spar.APISpec.spec
    describe "Test.Spar.App" Test.Spar.AppSpec.spec
    describe "Test.Spar.Data" Test.Spar.DataSpec.spec
    describe "Test.Spar.Intra.Brig" Test.Spar.Intra.BrigSpec.spec
    describe "Test.Spar.SCIM" Test.Spar.SCIMSpec.spec
