{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

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
