{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
    ( tsManager
    , tsGalley
    , tsBrig
    , tsCannon
    , tsAwsEnv
    , tsMaxConvSize
    , TestM(..)
    , TestSetup(..)
    ) where

import Imports
import Bilge (Manager)
import Forecastle
import Control.Lens (makeLenses)

import qualified Galley.Aws          as Aws

data TestSetup = TestSetup
    { _tsManager     :: Manager
    , _tsGalley      :: GalleyR
    , _tsBrig        :: BrigR
    , _tsCannon      :: CannonR
    , _tsAwsEnv      :: Maybe Aws.Env
    , _tsMaxConvSize :: Word16
    } deriving Generic

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
    getManager = view tsManager
