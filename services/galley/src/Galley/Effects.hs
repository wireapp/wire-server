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

module Galley.Effects
  ( -- * Effects needed in Galley
    GalleyEffects1,

    -- * Effects to access the Intra API
    BotAccess,
    BrigAccess,
    FederatorAccess,
    GundeckAccess,
    SparAccess,

    -- * External services
    ExternalAccess,

    -- * Fire-and-forget async
    FireAndForget,

    -- * Store effects
    ClientStore,
    CodeStore,
    ConversationStore,
    CustomBackendStore,
    LegalHoldStore,
    MemberStore,
    ProposalStore,
    SearchVisibilityStore,
    ServiceStore,
    SubConversationStore,
    TeamFeatureStore,
    TeamMemberStore,
    TeamNotificationStore,
    TeamStore,

    -- * Paging effects
    ListItems,

    -- * Other effects
    Queue,
    WaiRoutes,

    -- * Polysemy re-exports
    Member,
    Members,
  )
where

import Data.Id
import Data.Qualified
import Data.Time.Clock
import Galley.Cassandra.TeamFeatures (Cassandra)
import Galley.Effects.BotAccess
import Galley.Effects.BrigAccess
import Galley.Effects.ClientStore
import Galley.Effects.CodeStore
import Galley.Effects.ConversationStore
import Galley.Effects.CustomBackendStore
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.FireAndForget
import Galley.Effects.GundeckAccess
import Galley.Effects.LegalHoldStore
import Galley.Effects.ListItems
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.Queue
import Galley.Effects.SearchVisibilityStore
import Galley.Effects.ServiceStore
import Galley.Effects.SparAccess
import Galley.Effects.SubConversationStore
import Galley.Effects.SubConversationSupply
import Galley.Effects.TeamFeatureStore
import Galley.Effects.TeamMemberStore
import Galley.Effects.TeamNotificationStore
import Galley.Effects.TeamStore
import Galley.Effects.WaiRoutes
import Galley.Env
import Galley.Options
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Error
import Wire.Sem.Paging.Cassandra
import Wire.Sem.Random

-- All the possible high-level effects.
type GalleyEffects1 =
  '[ BrigAccess,
     SparAccess,
     GundeckAccess,
     ExternalAccess,
     FederatorAccess,
     BotAccess,
     FireAndForget,
     ClientStore,
     CodeStore,
     ProposalStore,
     ConversationStore,
     SubConversationStore,
     SubConversationSupply,
     Random,
     CustomBackendStore,
     LegalHoldStore,
     MemberStore,
     SearchVisibilityStore,
     ServiceStore,
     TeamFeatureStore Cassandra,
     TeamNotificationStore,
     TeamStore,
     TeamMemberStore InternalPaging,
     TeamMemberStore CassandraPaging,
     ListItems CassandraPaging ConvId,
     ListItems CassandraPaging (Remote ConvId),
     ListItems LegacyPaging ConvId,
     ListItems LegacyPaging TeamId,
     ListItems InternalPaging TeamId,
     Input (Local ()),
     Input Opts,
     WaiRoutes,
     Input UTCTime,
     Queue DeleteItem,
     TinyLog,
     Error DynError
   ]
