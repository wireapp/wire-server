{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

import API.SQS
import Control.Lens (view)
import qualified Galley.Aws as Aws
import Imports
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import TestSetup

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    assertClean :: TestM ()
    assertClean = do
      awsEnv <- fromJust <$> view tsAwsEnv
      msgs <- liftIO $ Aws.execute awsEnv readAllUntilEmpty
      liftIO $
        assertBool
          ( "Found "
              <> show (length msgs)
              <> " messages left on queue:\n"
              <> show msgs
          )
          (null msgs)
    runTest :: Assertion
    runTest = do
      setup <- s
      void . flip runReaderT setup . runTestM $ h >> assertClean
