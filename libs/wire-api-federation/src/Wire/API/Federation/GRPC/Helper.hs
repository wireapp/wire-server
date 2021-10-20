{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Federation.GRPC.Helper where

import Imports
import Language.Haskell.TH.Syntax (Dec, Q, addDependentFile)

routerProtoFile :: FilePath
#if __GHCIDE__
routerProtoFile = "libs/wire-api-federation/proto/router.proto"
#elif WIRE_GHCI
-- Similar to __GHCIDE__ this fixes a compilation issue with ghci and ghcid.
-- There doesn't seem to be cpp variable to signify GHCI, so use -DWIRE_GHCI
routerProtoFile = "libs/wire-api-federation/proto/router.proto"
#else
routerProtoFile = "proto/router.proto"
#endif

recompileRouterUponProtoChanges :: Q [Dec]
recompileRouterUponProtoChanges = do
  addDependentFile routerProtoFile
  pure []
