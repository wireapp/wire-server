{-# OPTIONS_GHC -fprint-potential-instances #-}

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

module TestHelpers where

import Control.Lens (view)
import Control.Monad.Catch (MonadMask)
import Control.Retry
import Data.Domain (Domain)
import Data.Qualified
import Galley.Options (optSettings, setFederationDomain)
import Imports
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (Assertion, testCase)
import TestSetup

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
      setup <- s
      void . flip runReaderT setup . runTestM $ h

viewFederationDomain :: TestM Domain
viewFederationDomain = view (tsGConf . optSettings . setFederationDomain)

qualifyLocal :: a -> TestM (Local a)
qualifyLocal x = do
  domain <- viewFederationDomain
  pure $ toLocalUnsafe domain x

eventually :: (MonadIO m, MonadMask m) => m a -> m a
eventually = recoverAll (limitRetries 3 <> exponentialBackoff 100000) . const
