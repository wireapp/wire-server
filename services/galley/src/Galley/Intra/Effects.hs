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
import Data.Set qualified as Set
import Galley.API.Error
import Galley.API.Util (localBotsAndUsers)
import Galley.Cassandra.Conversation.Members (toMember)
import Galley.Cassandra.Queries (MemberStatus, selectAllMembers)
import Galley.Cassandra.Store (embedClient)
import Galley.Effects.BotAccess (BotAccess (..))
import Galley.Effects.BrigAccess (BrigAccess (..))
import Galley.Effects.DefederationNotifications (DefederationNotifications (..))
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
import Wire.API.Team.Member (HardTruncationLimit, ListType (ListComplete))

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
  GetClientByKeyPackageRef ref ->
    embedApp $ getClientByKeyPackageRef ref
  GetLocalMLSClients qusr ss -> embedApp $ getLocalMLSClients qusr ss
  AddKeyPackageRef ref qusr cl qcnv ->
    embedApp $
      addKeyPackageRef ref qusr cl qcnv
  ValidateAndAddKeyPackageRef nkp ->
    embedApp $
      validateAndAddKeyPackageRef nkp
  UpdateKeyPackageRef update ->
    embedApp $
      updateKeyPackageRef update
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

-- FUTUREWORK:
-- This functions uses an in-memory set for tracking UserIds that we have already
-- sent notifications to. This set will only grow throughout the lifttime of this
-- function, and may cause memory & performance problems with millions of users.
-- How we are tracking which users have already been sent 0, 1, or 2 defederation
-- messages should be rethought to be more fault tollerant, e.g. this method doesn't
-- handle the server crashing and restarting.
interpretDefederationNotifications ::
  forall r a.
  ( Member (Embed IO) r,
    Member (Input ClientState) r,
    Member GundeckAccess r
  ) =>
  Sem (DefederationNotifications ': r) a ->
  Sem r a
interpretDefederationNotifications = interpret $ \case
  SendDefederationNotifications maxPage domain ->
    getPage maxPage
      >>= void . sendNotificationPage mempty (Federation.FederationDelete domain)
  SendOnConnectionRemovedNotifications maxPage domainA domainB ->
    getPage maxPage
      >>= void . sendNotificationPage mempty (Federation.FederationConnectionRemoved (domainA, domainB))
  where
    getPage :: (Range 1 HardTruncationLimit Int32) -> Sem r (Page PageType)
    getPage maxPage = do
      -- This is based on the limits in removeIfLargeFanout
      -- selectAllMembers will return duplicate members when they are in more than one chat
      -- however we need the full row to build out the bot members to send notifications
      -- to them. We have to do the duplicate filtering here.
      embedClient $ paginate selectAllMembers (paramsP LocalQuorum () (fromRange maxPage))
    pushEvents :: Set UserId -> Federation.Event -> [LocalMember] -> Sem r (Set UserId)
    pushEvents seenRecipients eventData results = do
      -- FUTUREWORK: we are ignoring bots for now, but we might need to add them in the future
      -- when bots are compatible with federation
      let mems = snd $ localBotsAndUsers results
          recipients = Intra.recipient <$> mems
          event = Intra.FederationEvent eventData
          filteredRecipients =
            -- Deduplicate by UserId the page of recipients that we are working on
            nubBy (\a b -> a._recipientUserId == b._recipientUserId)
            -- Sort the remaining recipients by their IDs
            $
              sortBy (\a b -> a._recipientUserId `compare` b._recipientUserId)
              -- Filter out any recipient that we have already seen in a previous page
              $
                filter (\r -> r._recipientUserId `notElem` seenRecipients) recipients
      for_ (Intra.newPush ListComplete Nothing event filteredRecipients) $ \p -> do
        -- Futurework: Transient or not?
        -- RouteAny is used as it will wake up mobile clients
        -- and notify them of the changes to federation state.
        push1 $ p & Intra.pushRoute .~ Intra.RouteAny
      -- Add the users to the set of users we've sent messages to.
      pure $ seenRecipients <> Set.fromList ((._recipientUserId) <$> filteredRecipients)
    sendNotificationPage :: Set UserId -> Federation.Event -> Page PageType -> Sem r ()
    sendNotificationPage seenRecipients eventData page = do
      let res = result page
          mems = mapMaybe toMember res
      seenRecipients' <- pushEvents seenRecipients eventData mems
      when (hasMore page) $ do
        page' <- embedClient $ nextPage page
        sendNotificationPage seenRecipients' eventData page'

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
