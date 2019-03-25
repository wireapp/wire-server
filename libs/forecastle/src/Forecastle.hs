{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Forecastle
    ( tManager
    , tGalley
    , tBrig
    , tCannon
    , tCargoHold
    , tAwsEnv
    , test
    , testGroup
    , withEnv
    , TestM'
    , TestM(..)
    , TestSetup(..)
    , GalleyR(..)
    , CargoHoldR(..)
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
newtype CargoHoldR = CargoHoldR { runCargoHoldR :: Request -> Request }

type TestM' a = TestM TestSetup a
newtype TestM e a =
  TestM { runTestM :: ReaderT e IO a
        } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadReader e
        , MonadIO
        , MonadCatch
        , MonadThrow
        , MonadMask
        , MonadUnliftIO
        )

instance (HasType Manager e) => MonadHttp (TestM e) where
    getManager = view tManager

data TestSetup = TestSetup
    { tsManager     :: Manager
    , tsGalley      :: GalleyR
    , tsBrig        :: BrigR
    , tsCannon      :: CannonR
    , tsCargoHoldR  :: CargoHoldR
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

tCargoHold :: HasType CargoHoldR e => Lens' e (Request -> Request)
tCargoHold = typed @CargoHoldR . coerced

tAwsEnv :: HasType (Maybe AWS.Env) e => Traversal' e AWS.Env
tAwsEnv = typed @(Maybe AWS.Env) . _Just

type TestDescription = String
test :: HasType Manager e => IO e -> TestDescription -> TestM e a -> Spec
test setupEnv desc h = before setupEnv $ specify desc t
  where
    t s = do
        void . flip runReaderT s . runTestM $ h

testGroup :: TestDescription -> [SpecWith a] -> SpecWith a
testGroup desc specs = describe desc (sequence_ specs)

-- | Can be used with 'it'; e.g. it "does something with env" . withEnv $ myTestM
withEnv :: TestM e () -> e -> IO ()
withEnv t testEnv =
    void . flip runReaderT testEnv . runTestM $ t
