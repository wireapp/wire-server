{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Federation.GRPC.Helper where

import Imports
import Language.Haskell.TH.Syntax (Dec, Q, addDependentFile)

protoFile :: FilePath
protoFile = "proto/federator.proto"

routerProtoFile :: FilePath
routerProtoFile = "proto/router.proto"

recompileUponProtoChanges :: Q [Dec]
recompileUponProtoChanges = do
  addDependentFile protoFile
  pure []

recompileRouterUponProtoChanges :: Q [Dec]
recompileRouterUponProtoChanges = do
  addDependentFile routerProtoFile
  pure []
