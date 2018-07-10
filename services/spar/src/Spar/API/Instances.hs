{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Instances where

import Servant
import qualified Data.Id as Brig

instance FromHttpApiData (Brig.Id Brig.U) where
  parseUrlPiece = fmap Brig.Id . parseUrlPiece
