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
import qualified Data.List1 as List1
import qualified Data.Map as Map
import Data.Qualified
import Galley.Data.Services
import Galley.Effects.ExternalAccess
import Galley.Effects.GundeckAccess hiding (Push)
import Galley.Intra.Push
import Galley.Intra.Push.Internal hiding (push)
import Gundeck.Types.Push (RecipientClients (RecipientClientsSome))
import Imports
import Polysemy
import Polysemy.TinyLog
import qualified System.Logger.Class as Log
import Wire.API.Event.Conversation
import Wire.API.Message
import Wire.API.Team.Member

data MessagePush
  = MessagePush (Maybe ConnId) MessageMetadata [Recipient] [BotMember] Event

type BotMap = Map UserId BotMember

newMessagePush ::
  BotMap ->
  Maybe ConnId ->
  MessageMetadata ->
  [(UserId, ClientId)] ->
  Event ->
  MessagePush
newMessagePush botMap mconn mm userOrBots event =
  let (recipients, botMembers) =
        foldMap
          ( \(u, c) ->
              case Map.lookup u botMap of
                Just botMember -> ([], [botMember])
                Nothing -> ([Recipient u (RecipientClientsSome (List1.singleton c))], [])
          )
          userOrBots
   in MessagePush mconn mm recipients botMembers event

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
runMessagePush loc mqcnv mp@(MessagePush _ _ _ botMembers event) = do
  push (toPush mp)
  for_ mqcnv $ \qcnv ->
    if tDomain loc /= qDomain qcnv
      then unless (null botMembers) $ do
        warn $ Log.msg ("Ignoring messages for local bots in a remote conversation" :: ByteString) . Log.field "conversation" (show qcnv)
      else deliverAndDeleteAsync (qUnqualified qcnv) (map (,event) botMembers)

toPush :: MessagePush -> Maybe Push
toPush (MessagePush mconn mm userRecipients _ event) =
  let usr = qUnqualified (evtFrom event)
   in newPush ListComplete (Just usr) (ConvEvent event) userRecipients
        <&> set pushConn mconn
          . set pushNativePriority (mmNativePriority mm)
          . set pushRoute (bool RouteDirect RouteAny (mmNativePush mm))
          . set pushTransient (mmTransient mm)
