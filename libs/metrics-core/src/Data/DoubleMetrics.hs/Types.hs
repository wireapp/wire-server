{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Metrics.Types where

import Imports
import Data.Hashable

newtype Path =
    Path { _path :: Text
         } deriving stock (Eq, Show)
           deriving newtype (Hashable, Semigroup)

path :: Text -> Path
path = Path

