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

    -- * Executing message pushes
    BotMap,
    newMessagePush,
    runMessagePush,
  )
where

import Control.Lens (set)
import Data.Id
import qualified Data.Map as Map
import Data.Qualified
import Data.Semigroup.Generic
import Galley.Data.Services
import Galley.Effects.ExternalAccess
import Galley.Effects.GundeckAccess hiding (Push)
import Galley.Intra.Push
import Gundeck.Types.Push.V2 (RecipientClients (..))
import Imports
import Polysemy
import Polysemy.TinyLog
import qualified System.Logger.Class as Log
import Wire.API.Event.Conversation
import Wire.API.Message

data MessagePush = NormalMessagePush
  { userPushes :: [Push],
    botPushes :: [(BotMember, Event)]
  }
  deriving stock (Generic, Show)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid MessagePush

newUserPush :: Push -> MessagePush
newUserPush p = NormalMessagePush {userPushes = pure p, botPushes = mempty}

type BotMap = Map UserId BotMember

newMessagePush ::
  forall x.
  Local x ->
  BotMap ->
  Maybe ConnId ->
  MessageMetadata ->
  (UserId, ClientId) ->
  Event ->
  MessagePush
newMessagePush loc bots mconn mm (user, client) e =
  case Map.lookup user bots of
    Just bm -> NormalMessagePush {userPushes = mempty, botPushes = pure (bm, e)}
    Nothing -> fold $ newUserMessagePush loc mconn mm user client e

runMessagePush ::
  forall x r.
  ( Member ExternalAccess r,
    Member GundeckAccess r,
    Member TinyLog r
  ) =>
  Local x ->
  Maybe (Qualified ConvId) ->
  MessagePush ->
  Sem r ()
runMessagePush loc mqcnv mp = do
  push (userPushes mp)
  pushToBots (botPushes mp)
  where
    pushToBots [] = pure ()
    pushToBots pushes = for_ mqcnv $ \qcnv ->
      if tDomain loc /= qDomain qcnv
        then unless (null pushes) $ do
          warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show qcnv)
        else deliverAndDeleteAsync (qUnqualified qcnv) pushes

newUserMessagePush ::
  Local x ->
  Maybe ConnId ->
  MessageMetadata ->
  UserId ->
  ClientId ->
  Event ->
  Maybe MessagePush
newUserMessagePush loc mconn mm user cli e =
  fmap newUserPush $
    newConversationEventPush e (qualifyAs loc [user])
      <&> set pushConn mconn
        . set pushNativePriority (mmNativePriority mm)
        . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
        . set pushTransient (mmTransient mm)
        . set (pushRecipients . traverse . recipientClients) (RecipientClientsSome (pure cli))
