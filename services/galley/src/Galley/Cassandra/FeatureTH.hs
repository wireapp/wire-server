{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Galley.Cassandra.FeatureTH where

import Imports
import Language.Haskell.TH
import Wire.API.Team.Feature

featureCases :: ExpQ -> Q Exp
featureCases rhsQ = do
  rhs <- rhsQ
  TyConI (DataD _ _ _ _ constructors _) <- reify ''FeatureSingleton
  pure $
    LamCaseE
      [ Match (ConP c [] []) (NormalB rhs) []
        | GadtC [c] _ _ <- constructors
      ]
