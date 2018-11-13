{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | All general-purpose orphan instances used in Brig.
module Brig.Orphans () where

import Data.Aeson
import GHC.Generics
import System.Logger as Logger

deriving instance Generic Logger.Level
instance FromJSON Logger.Level
instance ToJSON Logger.Level
