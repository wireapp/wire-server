-- | It would be nice to use hspec-discover, which even has support for
-- <https://hspec.github.io/hspec-discover.html#using-a-custom-main-function custom main functions>.
--
-- This is trickier than expected, though: hspec-discover expects the modules to export @spec
-- :: Spec@, but we would need that to be @spec :: SpecWith TestEnv@. See
-- <https://github.com/hspec/hspec/issues/404>.
--
-- On the other hand, we cannot easily 'mkEnvFromOptions' inside each module, since it
-- requires @-s@, @-i@ command line modules, which will make 'hspec' choke. Related, but not
-- the solution: https://github.com/hspec/hspec/pull/397.
module Main where

import Imports
import System.Environment (withArgs)
import Test.Hspec
import Util

import qualified Test.LoggingSpec
import qualified Test.MetricsSpec
import qualified Test.Spar.APISpec
import qualified Test.Spar.AppSpec
import qualified Test.Spar.DataSpec
import qualified Test.Spar.Intra.BrigSpec
import qualified Test.Spar.Scim.AuthSpec
import qualified Test.Spar.Scim.UserSpec


main :: IO ()
main = do
  (wireArgs, hspecArgs) <- partitionArgs <$> getArgs
  env <- withArgs wireArgs mkEnvFromOptions
  withArgs (hspecArgs <> ["-m", "XXXX"]). hspec . beforeAll (pure env) . afterAll destroyEnv $ mkspec

-- HACK: here be dragons because there are wire specific flags but Hspec also expects flags
partitionArgs :: [String] -> ([String], [String])
partitionArgs = go [] []
  where
    go wireArgs hspecArgs ("-s" : x : xs) = go (wireArgs <> ["-s", x]) hspecArgs xs
    go wireArgs hspecArgs ("-i" : x : xs) = go (wireArgs <> ["-i", x]) hspecArgs xs
    go wireArgs hspecArgs (x : xs)        = go wireArgs (hspecArgs <> [x]) xs
    go wireArgs hspecArgs []              = (wireArgs, hspecArgs)

mkspec :: SpecWith TestEnv
mkspec = do
    describe "Logging" Test.LoggingSpec.spec
    describe "Metrics" Test.MetricsSpec.spec
    describe "Spar.API" Test.Spar.APISpec.spec
    describe "Spar.App" Test.Spar.AppSpec.spec
    describe "Spar.Data" Test.Spar.DataSpec.spec
    describe "Spar.Intra.Brig" Test.Spar.Intra.BrigSpec.spec
    describe "Spar.Scim.Auth" Test.Spar.Scim.AuthSpec.spec
    describe "Spar.Scim.User" Test.Spar.Scim.UserSpec.spec
