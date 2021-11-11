-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.Effects.GundeckAccess
  ( -- * Gundeck access effect
    GundeckAccess (..),
    push,
    push1,
  )
where

import qualified Galley.Intra.Push as G
import Imports
import Polysemy

data GundeckAccess m a where
  Push :: Foldable f => f G.Push -> GundeckAccess m ()

makeSem ''GundeckAccess

-- | Asynchronously send a single push, chunking it into multiple
-- requests if there are more than 128 recipients.
push1 :: Member GundeckAccess r => G.Push -> Sem r ()
push1 x = push [x]
