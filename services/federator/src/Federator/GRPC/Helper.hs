{-# LANGUAGE TemplateHaskell #-}

module Federator.GRPC.Helper where

import Imports
import Language.Haskell.TH.Syntax (Dec, Q, addDependentFile)

recompileUponProtoChanges :: Q [Dec]
recompileUponProtoChanges = do
  addDependentFile "federator.proto"
  pure []
