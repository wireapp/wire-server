{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
    ( test
    , tsManager
    , tsGalley
    , tsBrig
    , tsCannon
    , tsAwsEnv
    , tsMaxConvSize
    , TestM(..)
    , TestSetup(..)
    ) where

import Imports
import Test.Tasty          (TestName, TestTree)
import Test.Tasty.HUnit    (Assertion, testCase)
import Control.Lens        (makeLenses, view)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Bilge (Manager, MonadHttp(..), Request, handleRequestWithManager)

import qualified Galley.Aws          as Aws

type GalleyR      = Request -> Request
type BrigR        = Request -> Request
type CannonR      = Request -> Request

data TestSetup = TestSetup
    { _tsManager     :: Manager
    , _tsGalley      :: GalleyR
    , _tsBrig        :: BrigR
    , _tsCannon      :: CannonR
    , _tsAwsEnv      :: Maybe Aws.Env
    , _tsMaxConvSize :: Word16
    }

makeLenses ''TestSetup

newtype TestM a =
  TestM { runTestM :: ReaderT TestSetup IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader TestSetup
             , MonadIO
             , MonadCatch
             , MonadThrow
             , MonadMask
             , MonadUnliftIO
             )

instance MonadHttp TestM where
    handleRequestWithCont req cont = do
        m <- view tsManager
        handleRequestWithManager m req cont

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test s n h = testCase n runTest
  where
    runTest :: Assertion
    runTest = do
        setup <- s
        void . flip runReaderT setup . runTestM $ h
