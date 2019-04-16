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
--
-- The following says that @action@ can run in any TestM parameterized by an
-- environment which is a Record containing each of a 'GalleyR', 'BrigR', and 'Manager'.
-- > action :: ContainsTypes e '[GalleyR, BrigR, Manager] => TestM e a
--
type family ContainsTypes e (ts :: [Type]) = (constraints :: Constraint) where
  -- No requirements left; return the empty constraint
  ContainsTypes e '[] = ()
  -- Require that e has type 't' inside; then recurse on remaining constraints
  ContainsTypes e (t:ts) = (HasType t e, ContainsTypes e ts)

newtype GalleyR = GalleyR { runGalleyR :: Request -> Request }
newtype BrigR = BrigR { runBrigR :: Request -> Request }
newtype CannonR = CannonR { runCannonR :: Request -> Request }
newtype CargoHoldR = CargoHoldR { runCargoHoldR :: Request -> Request }

-- | The test monad; parameterized by the environment 'e'
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

-- | TestM has MonadHttp if it's environment has a 'Manager' in it.
instance (ContainsTypes e '[Manager]) => MonadHttp (TestM e) where
    getManager = tManager

-- | Retrieve the 'Manager' from test dependencies
tManager :: ContainsTypes e '[Manager] => TestM e Manager
tManager = view (typed @Manager)

-- | Retrieve 'GalleyR' from test dependencies
tGalley :: ContainsTypes e '[GalleyR] => TestM e (Request -> Request)
tGalley = view (typed @GalleyR . coerced)

-- | Retrieve 'BrigR' from test dependencies
tBrig :: ContainsTypes e '[BrigR] => TestM e (Request -> Request)
tBrig = view (typed @BrigR . coerced)

-- | Retrieve 'CannonR' from test dependencies
tCannon :: ContainsTypes e '[CannonR] => TestM e (Request -> Request)
tCannon = view (typed @CannonR . coerced)

-- | Retrieve 'CargoHoldR' from test dependencies
tCargoHold :: ContainsTypes e '[CargoHoldR] => TestM e (Request -> Request)
tCargoHold = view (typed @CargoHoldR . coerced)

-- | Run a test in IO
runTestM :: TestM e a -> e -> IO a
runTestM t testEnv = flip runReaderT testEnv . runTestM' $ t

runTestM_ :: TestM e a -> e -> IO ()
runTestM_ t testEnv = void $ runTestM t testEnv
