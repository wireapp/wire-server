{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import Data.Json.Util
import Data.List1 qualified as List1
import Data.Map qualified as Map
import Data.Qualified
import Galley.Data.Services
import Galley.Effects.ExternalAccess
import Imports
import Polysemy
import Polysemy.TinyLog
import System.Logger.Class qualified as Log
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Push.V2 (RecipientClients (RecipientClientsSome), Route (..))
import Wire.NotificationSubsystem

data MessagePush
  = MessagePush (Maybe ConnId) MessageMetadata [Recipient] [BotMember] Event

type BotMap = Map UserId BotMember

class ToRecipient a where
  toRecipient :: a -> Recipient

instance ToRecipient (UserId, ClientId) where
  toRecipient (u, c) = Recipient u (RecipientClientsSome (List1.singleton c))

instance ToRecipient Recipient where
  toRecipient = id

newMessagePush ::
  (ToRecipient r) =>
  BotMap ->
  Maybe ConnId ->
  MessageMetadata ->
  [r] ->
  Event ->
  MessagePush
newMessagePush botMap mconn mm userOrBots event =
  let toPair r = case Map.lookup (recipientUserId r) botMap of
        Just botMember -> ([], [botMember])
        Nothing -> ([r], [])
      (recipients, botMembers) = foldMap (toPair . toRecipient) userOrBots
   in MessagePush mconn mm recipients botMembers event

runMessagePush ::
  forall x r.
  ( Member ExternalAccess r,
    Member TinyLog r,
    Member NotificationSubsystem r
  ) =>
  Local x ->
  Maybe (Qualified ConvId) ->
  MessagePush ->
  Sem r ()
runMessagePush loc mqcnv mp@(MessagePush _ _ _ botMembers event) = do
  pushNotifications $ maybeToList $ toPush mp
  for_ mqcnv $ \qcnv ->
    if tDomain loc /= qDomain qcnv
      then unless (null botMembers) $ do
        warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show qcnv)
      else deliverAndDeleteAsync (qUnqualified qcnv) (map (,event) botMembers)

toPush :: MessagePush -> Maybe Push
toPush (MessagePush mconn mm rs _ event) =
  let usr = qUnqualified (evtFrom event)
   in newPush (Just usr) (toJSONObject event) rs
        <&> set pushConn mconn
        . set pushNativePriority (mmNativePriority mm)
        . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
        . set pushTransient (mmTransient mm)
