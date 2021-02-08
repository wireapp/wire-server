{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Federation.GRPC.Helper where

import Imports
import Language.Haskell.TH.Syntax (Dec, Q, addDependentFile)

routerProtoFile :: FilePath
routerProtoFile = "proto/router.proto"

recompileRouterUponProtoChanges :: Q [Dec]
recompileRouterUponProtoChanges = do
  addDependentFile routerProtoFile
  pure []
