{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE PolyKinds #-}

module Forecastle
    ( runTestM
    , runTestM_

    , tManager
    , tGalley
    , tBrig
    , tCannon
    , tCargoHold

    , TestM(..)
    , GalleyR(..)
    , CargoHoldR(..)
    , BrigR(..)
    , CannonR(..)

    , ContainsTypes
    ) where

import Imports
import Bilge
import Control.Monad.Catch
import Data.Kind

import Data.Generics.Product

-- import qualified Network.AWS as AWS
import Control.Lens

-- | 'ContainsTypes' builds a constraint which asserts that the given type 'e' is a record
-- containing each of the provided types.
type family ContainsTypes e (ts :: [Type]) = (constraints :: Constraint) where
  ContainsTypes e '[] = ()
  ContainsTypes e (t:ts) = (HasType t e, ContainsTypes e ts)

newtype GalleyR = GalleyR { runGalleyR :: Request -> Request }
newtype BrigR = BrigR { runBrigR :: Request -> Request }
newtype CannonR = CannonR { runCannonR :: Request -> Request }
newtype CargoHoldR = CargoHoldR { runCargoHoldR :: Request -> Request }

newtype TestM e a =
  TestM { runTestM' :: ReaderT e IO a
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

instance (ContainsTypes e '[Manager]) => MonadHttp (TestM e) where
    getManager = tManager

tManager :: ContainsTypes e '[Manager] => TestM e Manager
tManager = view (typed @Manager)

tGalley :: ContainsTypes e '[GalleyR] => TestM e (Request -> Request)
tGalley = view (typed @GalleyR . coerced)

tBrig :: ContainsTypes e '[BrigR] => TestM e (Request -> Request)
tBrig = view (typed @BrigR . coerced)

tCannon :: ContainsTypes e '[CannonR] => TestM e (Request -> Request)
tCannon = view (typed @CannonR . coerced)

tCargoHold :: ContainsTypes e '[CargoHoldR] => TestM e (Request -> Request)
tCargoHold = view (typed @CargoHoldR . coerced)

-- | Run a test in IO
runTestM :: TestM e a -> e -> IO a
runTestM t testEnv = flip runReaderT testEnv . runTestM' $ t

runTestM_ :: TestM e a -> e -> IO ()
runTestM_ t testEnv = void $ runTestM t testEnv
