{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
  ( test
  , TestSignature
  , TestSetup
  , manager
  , galley
  , brig
  , cannon
  , awsEnv
  , maxConvSize
  , Galley
  , Brig
  , Cannon
  , ResponseLBS
  , TestM
  ) where

import Imports
import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import Control.Monad.Catch

import Bilge (Request, HttpT(..), Http, Manager, Response, runHttpT, MonadHttp)
import qualified Galley.Aws             as Aws

type Galley      = Request -> Request
type Brig        = Request -> Request
type Cannon      = Request -> Request
type ResponseLBS = Response (Maybe LByteString)


type TestSignature a = Galley -> Brig -> Cannon -> Maybe Aws.Env -> Http a

newtype TestM a = TestM {runTestM :: ReaderT TestSetup (HttpT IO) a}
    deriving (Functor, Applicative, Monad, MonadReader TestSetup, MonadIO, MonadCatch
            , MonadThrow, MonadMask,  MonadHttp, MonadUnliftIO)

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

