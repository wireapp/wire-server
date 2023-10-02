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

module Galley.API.MLS.Propagate where

import Control.Comonad
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Galley.API.MLS.Types
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.BackendNotificationQueueAccess
import Galley.Types.Conversations.Members
import Imports
import Network.AMQP qualified as Q
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog hiding (trace)
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Message

-- | Propagate a message.
-- The message will not be propagated to the sender client if provided. This is
-- a requirement from Core Crypto and the clients.
propagateMessage ::
  ( Member BackendNotificationQueueAccess r,
    Member ExternalAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Maybe ClientId ->
  Local ConvOrSubConv ->
  Maybe ConnId ->
  RawMLS Message ->
  ClientMap ->
  Sem r ()
propagateMessage qusr mSenderClient lConvOrSub con msg cm = do
  now <- input @UTCTime
  let mlsConv = (.conv) <$> lConvOrSub
      lmems = mcLocalMembers . tUnqualified $ mlsConv
      rmems = mcRemoteMembers . tUnqualified $ mlsConv
      botMap = Map.fromList $ do
        m <- lmems
        b <- maybeToList $ newBotMember m
        pure (lmId m, b)
      mm = defMessageMetadata
  let qt =
        tUntagged lConvOrSub <&> \case
          Conv c -> (mcId c, Nothing)
          SubConv c s -> (mcId c, Just (scSubConvId s))
      qcnv = fst <$> qt
      sconv = snd (qUnqualified qt)
      e = Event qcnv sconv qusr now $ EdMLSMessage msg.raw

  runMessagePush lConvOrSub (Just qcnv) $
    newMessagePush botMap con mm (lmems >>= localMemberMLSClients mlsConv) e

  -- send to remotes
  (>>= either (logRemoteNotificationError @"on-mls-message-sent") (const (pure ())))
    . enqueueNotificationsConcurrently Q.Persistent (map remoteMemberQualify rmems)
    $ \rs ->
      fedQueueClient @'Galley @"on-mls-message-sent" $
        RemoteMLSMessage
          { time = now,
            sender = qusr,
            metadata = mm,
            conversation = qUnqualified qcnv,
            subConversation = sconv,
            recipients = tUnqualified rs >>= remoteMemberMLSClients,
            message = Base64ByteString msg.raw
          }
  where
    cmWithoutSender = maybe cm (flip cmRemoveClient cm . mkClientIdentity qusr) mSenderClient
    localMemberMLSClients :: Local x -> LocalMember -> [(UserId, ClientId)]
    localMemberMLSClients loc lm =
      let localUserQId = tUntagged (qualifyAs loc localUserId)
          localUserId = lmId lm
       in map
            (\(c, _) -> (localUserId, c))
            (Map.assocs (Map.findWithDefault mempty localUserQId cmWithoutSender))

    remoteMemberMLSClients :: RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients rm =
      let remoteUserQId = tUntagged (rmId rm)
          remoteUserId = qUnqualified remoteUserQId
       in map
            (\(c, _) -> (remoteUserId, c))
            (Map.assocs (Map.findWithDefault mempty remoteUserQId cmWithoutSender))
