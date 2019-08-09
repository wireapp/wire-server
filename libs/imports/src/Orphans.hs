{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for non-Wire-specific types and classes.
module Orphans () where

import Data.Aeson
import GHC.Generics
import System.Logger.Class as Logger

deriving instance Generic Logger.Level
instance FromJSON Logger.Level
instance ToJSON Logger.Level
