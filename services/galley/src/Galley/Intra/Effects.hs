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

module Galley.Intra.Effects
  ( interpretBrigAccess,
    interpretSparAccess,
    interpretBotAccess,
    interpretGundeckAccess,
    interpretDefederationNotifications,
  )
where

import Cassandra (ClientState, Consistency (LocalQuorum), Page (hasMore, nextPage, result), paginate, paramsP)
import Control.Lens ((.~))
import Data.Id (ProviderId, ServiceId, UserId)
import Data.Range (Range (fromRange))
import Galley.API.Error
import Galley.API.Util (localBotsAndUsers)
import Galley.Cassandra.Conversation.Members (toMember)
import Galley.Cassandra.Queries (MemberStatus, selectAllMembers)
import Galley.Cassandra.Store (embedClient)
import Galley.Effects.BotAccess (BotAccess (..))
import Galley.Effects.BrigAccess (BrigAccess (..))
import Galley.Effects.DefederationNotifications (DefederationNotifications (..))
import Galley.Effects.ExternalAccess (ExternalAccess, deliverAsync)
import Galley.Effects.GundeckAccess (GundeckAccess (..), push1)
import Galley.Effects.SparAccess (SparAccess (..))
import Galley.Env
import Galley.Intra.Client
import Galley.Intra.Push qualified as Intra
import Galley.Intra.Push.Internal qualified as G
import Galley.Intra.Spar
import Galley.Intra.Team
import Galley.Intra.User
import Galley.Monad
import Galley.Types.Conversations.Members (LocalMember)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import UnliftIO qualified
import Wire.API.Conversation (MutedStatus)
import Wire.API.Conversation.Role (RoleName)
import Wire.API.Event.Federation qualified as Federation
import Wire.API.Team.Member (ListType (ListComplete))

interpretBrigAccess ::
  ( Member (Embed IO) r,
    Member (Error InternalError) r,
    Member P.TinyLog r,
    Member (Input Env) r
  ) =>
  Sem (BrigAccess ': r) a ->
  Sem r a
interpretBrigAccess = interpret $ \case
  GetConnectionsUnqualified uids muids mrel ->
    embedApp $ getConnectionsUnqualified uids muids mrel
  GetConnectionsUnqualifiedBidi uids1 uids2 mrel1 mrel2 ->
    embedApp $
      UnliftIO.concurrently
        (getConnectionsUnqualified uids1 (Just uids2) mrel1)
        (getConnectionsUnqualified uids2 (Just uids1) mrel2)
  GetConnections uids mquids mrel ->
    embedApp $
      getConnections uids mquids mrel
  PutConnectionInternal uc -> embedApp $ putConnectionInternal uc
  ReauthUser uid reauth -> embedApp $ reAuthUser uid reauth
  LookupActivatedUsers uids -> embedApp $ lookupActivatedUsers uids
  GetUsers uids -> embedApp $ getUsers uids
  DeleteUser uid -> embedApp $ deleteUser uid
  GetContactList uid -> embedApp $ getContactList uid
  GetRichInfoMultiUser uids -> embedApp $ getRichInfoMultiUser uids
  GetSize tid -> embedApp $ getSize tid
  LookupClients uids -> embedApp $ lookupClients uids
  LookupClientsFull uids -> embedApp $ lookupClientsFull uids
  NotifyClientsAboutLegalHoldRequest self other pk ->
    embedApp $ notifyClientsAboutLegalHoldRequest self other pk
  GetLegalHoldAuthToken uid mpwd -> getLegalHoldAuthToken uid mpwd
  AddLegalHoldClientToUserEither uid conn pks lpk ->
    embedApp $ addLegalHoldClientToUser uid conn pks lpk
  RemoveLegalHoldClientFromUser uid ->
    embedApp $ removeLegalHoldClientFromUser uid
  GetAccountConferenceCallingConfigClient uid ->
    embedApp $ getAccountConferenceCallingConfigClient uid
  GetLocalMLSClients qusr ss -> embedApp $ getLocalMLSClients qusr ss
  UpdateSearchVisibilityInbound status ->
    embedApp $ updateSearchVisibilityInbound status

interpretSparAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (SparAccess ': r) a ->
  Sem r a
interpretSparAccess = interpret $ \case
  DeleteTeam tid -> embedApp $ deleteTeam tid
  LookupScimUserInfos uids -> embedApp $ lookupScimUserInfos uids

interpretBotAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (BotAccess ': r) a ->
  Sem r a
interpretBotAccess = interpret $ \case
  DeleteBot cid bid -> embedApp $ deleteBot cid bid

interpretGundeckAccess ::
  ( Member (Embed IO) r,
    Member (Input Env) r
  ) =>
  Sem (GundeckAccess ': r) a ->
  Sem r a
interpretGundeckAccess = interpret $ \case
  Push ps -> embedApp $ G.push ps
  PushSlowly ps -> embedApp $ G.pushSlowly ps

interpretDefederationNotifications ::
  forall r a.
  ( Member (Embed IO) r,
    Member (Input Env) r,
    Member (Input ClientState) r,
    Member GundeckAccess r,
    Member ExternalAccess r
  ) =>
  Sem (DefederationNotifications ': r) a ->
  Sem r a
interpretDefederationNotifications = interpret $ \case
  SendDefederationNotifications domain ->
    getPage
      >>= void . sendNotificationPage (Federation.FederationDelete domain)
  SendOnConnectionRemovedNotifications domainA domainB ->
    getPage
      >>= void . sendNotificationPage (Federation.FederationConnectionRemoved (domainA, domainB))
  where
    getPage :: Sem r (Page PageType)
    getPage = do
      maxPage <- inputs (fromRange . currentFanoutLimit . _options) -- This is based on the limits in removeIfLargeFanout
      embedClient $ paginate selectAllMembers (paramsP LocalQuorum () maxPage)
    pushEvents :: Federation.Event -> [LocalMember] -> Sem r ()
    pushEvents eventData results = do
      let (bots, mems) = localBotsAndUsers results
          recipients = Intra.recipient <$> mems
          event = Intra.FederationEvent eventData
      for_ (Intra.newPush ListComplete Nothing event recipients) $ \p -> do
        -- Futurework: Transient or not?
        -- RouteAny is used as it will wake up mobile clients
        -- and notify them of the changes to federation state.
        push1 $ p & Intra.pushRoute .~ Intra.RouteAny
      deliverAsync (bots `zip` repeat (G.pushEventJson event))
    sendNotificationPage :: Federation.Event -> Page PageType -> Sem r ()
    sendNotificationPage eventData page = do
      let res = result page
          mems = mapMaybe toMember res
      pushEvents eventData mems
      when (hasMore page) $ do
        page' <- embedClient $ nextPage page
        sendNotificationPage eventData page'

type PageType =
  ( UserId,
    Maybe ServiceId,
    Maybe ProviderId,
    Maybe MemberStatus,
    Maybe MutedStatus,
    Maybe Text,
    Maybe Bool,
    Maybe Text,
    Maybe Bool,
    Maybe Text,
    Maybe RoleName
  )
