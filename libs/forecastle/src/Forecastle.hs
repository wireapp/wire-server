{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Forecastle
    ( tManager
    , tGalley
    , tBrig
    , tCannon
    , tAwsEnv
    , test
    , testGroup
    , TestM'
    , TestM(..)
    , TestSetup(..)
    , GalleyR(..)
    , BrigR(..)
    , CannonR(..)
    , module Test.Hspec
    ) where

import Imports
import Bilge
import Control.Monad.Catch

import Data.Generics.Product

import qualified Network.AWS as AWS
import Control.Lens

import Test.Hspec

newtype GalleyR = GalleyR { runGalleyR :: Request -> Request }
newtype BrigR = BrigR { runBrigR :: Request -> Request }
newtype CannonR = CannonR { runCannonR :: Request -> Request }

type TestM' a = TestM TestSetup a
newtype TestM e a =
  TestM { runTestM :: ReaderT e (HttpT IO) a
        } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader e
        , MonadIO
        , MonadCatch
        , MonadThrow
        , MonadMask
        , MonadHttp
        , MonadUnliftIO
        )

data TestSetup = TestSetup
    { tsManager     :: Manager
    , tsGalley      :: GalleyR
    , tsBrig        :: BrigR
    , tsCannon      :: CannonR
    , tsAwsEnv      :: Maybe AWS.Env
    } deriving (Generic)

tManager :: HasType Manager e => Lens' e Manager
tManager = typed @Manager

tGalley :: HasType GalleyR e => Lens' e (Request -> Request)
tGalley = typed @GalleyR . coerced

tBrig :: HasType BrigR e => Lens' e (Request -> Request)
tBrig = typed @BrigR . coerced

tCannon :: HasType CannonR e => Lens' e (Request -> Request)
tCannon = typed @CannonR . coerced

tAwsEnv :: HasType (Maybe AWS.Env) e => Traversal' e AWS.Env
tAwsEnv = typed @(Maybe AWS.Env) . _Just

type TestDescription = String
test :: HasType Manager e => IO e -> TestDescription -> TestM e a -> Spec
test s desc h = specify desc runTest
  where
    runTest = do
        setup <- s
        void . runHttpT (setup ^. tManager) . flip runReaderT setup . runTestM $ h

testGroup :: TestDescription -> [Spec] -> Spec
testGroup desc specs = describe desc (sequence_ specs)
