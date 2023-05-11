{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Brig.API.Federation (tests) where

import Brig.API.Federation (getFederationStatus)
import Brig.App (AppT (AppT), Env)
import Control.Error (ExceptT, runExceptT)
import Data.Default (Default (..))
import Data.Domain (Domain (Domain))
import Imports
import Polysemy (Final, runFinal)
import Polysemy.Input (Input, runInputConst)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)
import Wire.API.Federation.API.Brig

tests :: TestTree
tests =
  testGroup
    "Federation - get federation status"
    [ testCase "both empty" $ testFederationStatus [] [] Connected,
      testCase "federating domains empty" $ testFederationStatus [] [Domain "b.com"] NotConnected,
      testCase "domains to check empty" $ testFederationStatus [Domain "b.com"] [] Connected,
      testCase "not federating with own domain" $ testFederationStatus [] [Domain "a.com"] NotConnected,
      testCase "federating with subset of domains to check" $ testFederationStatus [Domain "b.com"] [Domain "b.com", Domain "c.com"] NotConnected,
      testCase "federating with all domains to check" $ testFederationStatus [Domain "b.com", Domain "c.com"] [Domain "b.com", Domain "c.com"] Connected,
      testCase "federating with superset of domains to check" $ testFederationStatus [Domain "b.com", Domain "c.com"] [Domain "b.com"] Connected
    ]

testFederationStatus :: [Domain] -> [Domain] -> FederationStatus -> Assertion
testFederationStatus federatingDomains domainsToCheck expectedStatus = do
  fedStatusOrError <- run federatingDomains $ getFederationStatus (Domain "a.com") (DomainList domainsToCheck)
  let fedStatus = fromRight (error "an error occurred") fedStatusOrError
  assertEqual "FederationStatus" fedStatus.status expectedStatus

run :: [Domain] -> ExceptT e (AppT '[Input [Domain], Final IO]) a -> IO (Either e a)
run domains = runFinal' . runExceptT
  where
    runFinal' :: AppT '[Input [Domain], Final IO] (Either e a) -> IO (Either e a)
    runFinal' (AppT ma) = runFinal . runInputConst domains $ runReaderT ma def

instance Default Env where
  -- We don't need the Env and can get away with this dues to Haskell's laziness.
  def = undefined
