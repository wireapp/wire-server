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
    BrigAPIAccess,
    FederatorAccess,
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
    Random,
    TeamFeatureStore,
    TeamMemberStore,
    TeamNotificationStore,
    TeamStore,

    -- * Paging effects
    ListItems,

    -- * Other effects
    Queue,

    -- * Polysemy re-exports
    Member,
    Members,

    -- * Queueing effects
    BackendNotificationQueueAccess,
  )
where

import Data.Id
import Data.Qualified
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Effects.ClientStore
import Galley.Effects.CodeStore
import Galley.Effects.ConversationStore
import Galley.Effects.CustomBackendStore
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.FireAndForget
import Galley.Effects.LegalHoldStore
import Galley.Effects.ListItems
import Galley.Effects.MemberStore
import Galley.Effects.ProposalStore
import Galley.Effects.Queue
import Galley.Effects.SearchVisibilityStore
import Galley.Effects.ServiceStore
import Galley.Effects.SparAccess
import Galley.Effects.SubConversationStore
import Galley.Effects.TeamFeatureStore
import Galley.Effects.TeamMemberStore
import Galley.Effects.TeamNotificationStore
import Galley.Effects.TeamStore
import Galley.Env
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Wire.API.Error
import Wire.API.Team.Feature
import Wire.BrigAPIAccess
import Wire.GundeckAPIAccess
import Wire.HashPassword
import Wire.NotificationSubsystem
import Wire.RateLimit
import Wire.Rpc
import Wire.Sem.Now
import Wire.Sem.Paging.Cassandra
import Wire.Sem.Random
import Wire.TeamCollaboratorsStore (TeamCollaboratorsStore)
import Wire.TeamCollaboratorsSubsystem (TeamCollaboratorsSubsystem)
import Wire.TeamSubsystem (TeamSubsystem)

-- All the possible high-level effects.
type GalleyEffects1 =
  '[ ExternalAccess,
     BrigAPIAccess,
     SparAccess,
     TeamCollaboratorsSubsystem,
     NotificationSubsystem,
     TeamSubsystem,
     GundeckAPIAccess,
     Rpc,
     FederatorAccess,
     BackendNotificationQueueAccess,
     FireAndForget,
     TeamCollaboratorsStore,
     ClientStore,
     CodeStore,
     ProposalStore,
     ConversationStore,
     SubConversationStore,
     RateLimit,
     HashPassword,
     Random,
     CustomBackendStore,
     LegalHoldStore,
     MemberStore,
     SearchVisibilityStore,
     ServiceStore,
     TeamNotificationStore,
     TeamStore,
     TeamFeatureStore,
     TeamMemberStore InternalPaging,
     TeamMemberStore CassandraPaging,
     ListItems CassandraPaging ConvId,
     ListItems CassandraPaging (Remote ConvId),
     ListItems LegacyPaging ConvId,
     ListItems LegacyPaging TeamId,
     ListItems InternalPaging TeamId,
     Input AllTeamFeatures,
     Input (Maybe [TeamId], FeatureDefaults LegalholdConfig),
     Input (Local ()),
     Input Opts,
     Now,
     Queue DeleteItem,
     TinyLog,
     Error DynError,
     Error RateLimitExceeded
   ]
