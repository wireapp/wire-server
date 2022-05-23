{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

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

module Galley.API.Push
  ( -- * Message pushes
    MessagePush (..),
    MessageType (..),
    newBotPush,

    -- * Executing message pushes
    LocalMemberMap,
    MessagePushEffects,
    newMessagePush,
    runMessagePush,

    -- * Singleton definitions
    NormalMessageSym0,
    BroadcastSym0,
  )
where

import Control.Lens (set)
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import Data.Semigroup.Generic
import Data.Singletons.TH
import Galley.Data.Services
import Galley.Effects.ExternalAccess
import Galley.Effects.GundeckAccess hiding (Push)
import Galley.Intra.Push
import Galley.Types.Conversations.Members
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Polysemy
import Polysemy.TinyLog
import qualified System.Logger.Class as Log
import Wire.API.Event.Conversation
import Wire.API.Message

data MessageType = NormalMessage | Broadcast

$(genSingletons [''MessageType])

data family MessagePush (t :: MessageType)

data instance MessagePush 'NormalMessage = NormalMessagePush
  { userPushes :: [Push],
    botPushes :: [(BotMember, Event)]
  }
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (MessagePush 'NormalMessage)

data instance MessagePush 'Broadcast = BroadcastPush
  {broadcastPushes :: [Push]}
  deriving stock (Generic)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (MessagePush 'Broadcast)

newUserPush :: forall t. SingI t => Push -> MessagePush t
newUserPush p = withSing @t $ \case
  SNormalMessage -> NormalMessagePush {userPushes = pure p, botPushes = mempty}
  SBroadcast -> BroadcastPush (pure p)

newBotPush :: BotMember -> Event -> MessagePush 'NormalMessage
newBotPush b e = NormalMessagePush {userPushes = mempty, botPushes = pure (b, e)}

type family LocalMemberMap (t :: MessageType) = (m :: *) | m -> t where
  LocalMemberMap 'NormalMessage = Map UserId LocalMember
  LocalMemberMap 'Broadcast = ()

type family MessagePushEffects (t :: MessageType) :: [Effect]

type instance MessagePushEffects 'NormalMessage = '[ExternalAccess, GundeckAccess, TinyLog]

type instance MessagePushEffects 'Broadcast = '[GundeckAccess]

newMessagePush ::
  forall t x.
  SingI t =>
  Local x ->
  LocalMemberMap t ->
  Maybe ConnId ->
  MessageMetadata ->
  (UserId, ClientId) ->
  Event ->
  MessagePush t
newMessagePush loc members mconn mm (user, client) e = withSing @t $ \case
  SNormalMessage -> case newBotMember =<< Map.lookup user members of
    Just bm -> newBotPush bm e
    Nothing -> fold $ newUserMessagePush loc mconn mm user client e
  SBroadcast -> fold $ newUserMessagePush loc mconn mm user client e

runMessagePush ::
  forall t x r.
  SingI t =>
  Members (MessagePushEffects t) r =>
  Local x ->
  Maybe (Qualified ConvId) ->
  MessagePush t ->
  Sem r ()
runMessagePush loc mqcnv mp = withSing @t $ \case
  SNormalMessage -> do
    push (userPushes mp)
    pushToBots (botPushes mp)
  SBroadcast -> push (broadcastPushes mp)
  where
    pushToBots ::
      Members '[ExternalAccess, TinyLog] r =>
      [(BotMember, Event)] ->
      Sem r ()
    pushToBots pushes = for_ mqcnv $ \qcnv ->
      if tDomain loc /= qDomain qcnv
        then unless (null pushes) $ do
          warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show qcnv)
        else deliverAndDeleteAsync (qUnqualified qcnv) pushes

newUserMessagePush ::
  SingI t =>
  Local x ->
  Maybe ConnId ->
  MessageMetadata ->
  UserId ->
  ClientId ->
  Event ->
  Maybe (MessagePush t)
newUserMessagePush loc mconn mm user cli e =
  fmap newUserPush $
    newConversationEventPush e (qualifyAs loc [user])
      <&> set pushConn mconn
        . set pushNativePriority (mmNativePriority mm)
        . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
        . set pushTransient (mmTransient mm)
        . set (pushRecipients . traverse . recipientClients) (RecipientClientsSome (pure cli))
