{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Instances where

import Servant
import Data.Id

instance FromHttpApiData UserId where
  parseUrlPiece = fmap Id . parseUrlPiece
