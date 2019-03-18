{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
  ( test
  , manager
  , galley
  , brig
  , cannon
  , awsEnv
  , maxConvSize
  , Galley
  , Brig
  , Cannon
  , TestM(..)
  , TestSetup(..)
  ) where

import Imports
import Test.Tasty          (TestName, TestTree)
import Test.Tasty.HUnit    (Assertion, testCase)
import Control.Lens        ((^.), makeLenses)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Bilge (Http, HttpT(..), Manager, MonadHttp, Request, runHttpT)

import qualified Galley.Aws          as Aws

type Galley      = Request -> Request
type Brig        = Request -> Request
type Cannon      = Request -> Request

newtype TestM a =
  TestM { runTestM :: ReaderT TestSetup (HttpT IO) a
        }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TestSetup
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadMask
             , MonadHttp
             , MonadUnliftIO
             )

data TestSetup = TestSetup
  { _manager         :: Manager
  , _galley          :: Galley
  , _brig            :: Brig
  , _cannon          :: Cannon
  , _awsEnv          :: Maybe Aws.Env
  , _maxConvSize     :: Word16
  }

makeLenses ''TestSetup


test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
        setup <- s
        void . runHttpT (setup ^. manager) . flip runReaderT setup . runTestM $ h

