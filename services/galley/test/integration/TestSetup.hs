{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}
module TestSetup
    ( tsAwsEnv
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
    { tsManager     :: Manager
    , tsGalley      :: GalleyR
    , tsBrig        :: BrigR
    , tsCannon      :: CannonR
    , _tsAwsEnv      :: Maybe Aws.Env
    , _tsMaxConvSize :: Word16
    } deriving Generic

makeLenses ''TestSetup
