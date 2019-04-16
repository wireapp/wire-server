{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PolyKinds #-}

module Forecastle
    ( test
    , testGroup
    , withEnv

    , tManager
    , tGalley
    , tBrig
    , tCannon
    , tCargoHold
    , tAwsEnv

    , TestM'
    , TestM(..)
    , GalleyR(..)
    , CargoHoldR(..)
    , BrigR(..)
    , CannonR(..)

    , ContainsTypes
    , module Test.Hspec
    ) where

import Imports
import Bilge
import Control.Monad.Catch
import Data.Kind

import Data.Generics.Product

import qualified Network.AWS as AWS
import Control.Lens

import Test.Hspec

type family ContainsTypes e (ts :: [Type]) = (constraints :: Constraint) where
  ContainsTypes e '[] = ()
  ContainsTypes e (t:ts) = (HasType t e, ContainsTypes e ts)

newtype GalleyR = GalleyR { runGalleyR :: Request -> Request }
newtype BrigR = BrigR { runBrigR :: Request -> Request }
newtype CannonR = CannonR { runCannonR :: Request -> Request }
newtype CargoHoldR = CargoHoldR { runCargoHoldR :: Request -> Request }

type TestM' e a = TestM e a
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
    getManager = tManager

tManager :: HasType Manager e => TestM e Manager
tManager = view (typed @Manager)

tGalley :: ContainsTypes e '[GalleyR] => TestM e (Request -> Request)
tGalley = view (typed @GalleyR . coerced)

tBrig :: HasType BrigR e => TestM e (Request -> Request)
tBrig = view (typed @BrigR . coerced)

tCannon :: HasType CannonR e => TestM e (Request -> Request)
tCannon = view (typed @CannonR . coerced)

tCargoHold :: HasType CargoHoldR e => TestM e (Request -> Request)
tCargoHold = view (typed @CargoHoldR . coerced)

tAwsEnv :: HasType (Maybe AWS.Env) e => TestM e (Maybe AWS.Env)
tAwsEnv = preview (typed @(Maybe AWS.Env) . _Just)

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
