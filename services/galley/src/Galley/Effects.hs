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
  ( -- * Effects needed in Galley
    GalleyEffects1,

    -- * Internal services
    Intra,
    interpretIntra,

    -- * Brig
    BrigAccess,
    interpretBrig,

    -- * Federator
    FederatorAccess,
    interpretFederator,

    -- * Spar
    SparAccess,
    interpretSpar,

    -- * Gundeck
    GundeckAccess,
    interpretGundeck,

    -- * External services
    ExternalAccess,
    interpretExternal,

    -- * Bot API
    BotAccess,
    interpretBot,

    -- * Fire-and-forget async
    FireAndForget,
    interpretFireAndForget,

    -- * Polysemy re-exports
    Member,
    Members,
  )
where

import Galley.Effects.FireAndForget
import Imports
import Polysemy

data Intra m a

interpretIntra :: Sem (Intra ': r) a -> Sem r a
interpretIntra = interpret $ \case

data BrigAccess m a

interpretBrig :: Sem (BrigAccess ': r) a -> Sem r a
interpretBrig = interpret $ \case

data GundeckAccess m a

interpretGundeck :: Sem (GundeckAccess ': r) a -> Sem r a
interpretGundeck = interpret $ \case

data ExternalAccess m a

interpretExternal :: Sem (ExternalAccess ': r) a -> Sem r a
interpretExternal = interpret $ \case

data FederatorAccess m a

interpretFederator :: Sem (FederatorAccess ': r) a -> Sem r a
interpretFederator = interpret $ \case

data SparAccess m a

interpretSpar :: Sem (SparAccess ': r) a -> Sem r a
interpretSpar = interpret $ \case

data BotAccess m a

interpretBot :: Sem (BotAccess ': r) a -> Sem r a
interpretBot = interpret $ \case

-- All the possible high-level effects.
type GalleyEffects1 =
  '[ BrigAccess,
     GundeckAccess,
     SparAccess,
     ExternalAccess,
     FederatorAccess,
     BotAccess,
     Intra,
     FireAndForget
   ]
