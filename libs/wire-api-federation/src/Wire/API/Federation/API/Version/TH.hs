-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.Federation.API.Version.TH (genVersions) where

import Imports
import Language.Haskell.TH

genVersions :: Name -> Q [Dec]
genVersions ty =
  reify ty >>= \case
    TyConI (DataD _ name _ _ cs _) -> do
      pcs <- traverse promoteConstructor cs
      pure $
        [ TySynD
            (mkName ("Supported" <> nameBase name <> "s"))
            []
            (foldr pcons PromotedNilT pcs)
        ]
    _ -> fail $ "Unsupported version type: " <> nameBase ty

promoteConstructor :: Con -> Q Type
promoteConstructor (NormalC name []) = pure $ PromotedT name
promoteConstructor _ = fail "Unsupported constructor"

pcons :: Type -> Type -> Type
pcons x y = PromotedConsT `AppT` x `AppT` y
