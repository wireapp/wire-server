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

module Galley.Effects
  ( GalleyEffects,
    Concurrency,
    Intra,
    BrigAccess,
    GundeckAccess,
    ExternalAccess,
    FederatorAccess,
    SparAccess,
    BotAccess,
    FireAndForget,

    -- * Polysemy re-exports
    Member,
    Members,
  )
where

import Polysemy

data Concurrency m a

data Intra m a

data BrigAccess m a

data GundeckAccess m a

data ExternalAccess m a

data FederatorAccess m a

data SparAccess m a

data BotAccess m a

data FireAndForget m a

type GalleyEffects =
  '[ Intra,
     BrigAccess,
     GundeckAccess,
     SparAccess,
     ExternalAccess,
     FederatorAccess,
     BotAccess,
     FireAndForget
   ]
