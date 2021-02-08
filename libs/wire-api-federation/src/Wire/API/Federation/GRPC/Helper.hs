{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Wire.API.Federation.GRPC.Helper where

import Imports
import Language.Haskell.TH.Syntax (Dec, Q, addDependentFile)

routerProtoFile :: FilePath
#if __GHCIDE__
routerProtoFile = "libs/wire-api-federation/proto/router.proto"
#else
routerProtoFile = "proto/router.proto"
#endif

recompileRouterUponProtoChanges :: Q [Dec]
recompileRouterUponProtoChanges = do
  addDependentFile routerProtoFile
  pure []
