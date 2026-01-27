{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

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

module Galley.API.Create where

import Data.Id
import Data.Qualified
import Galley.API.Mapping
import Galley.Types.Error
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation (CreateGroupConversation (..), CreateGroupOwnConversation (..), NewConv, NewOne2OneConv)
import Wire.API.Conversation qualified as Public
import Wire.API.Event.Conversation (Connect)
import Wire.API.Routes.Public.Galley.Conversation
import Wire.API.Routes.Public.Util (ResponseForExistedCreated (..))
import Wire.ConversationSubsystem qualified as ConversationSubsystem

----------------------------------------------------------------------------
-- API Handlers

createGroupConversationUpToV3 ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createGroupConversationUpToV3 lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  Created <$> conversationViewV9 lusr dbConv

createGroupOwnConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversationResponseV9
createGroupOwnConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  conv <- conversationViewV9 lusr dbConv
  pure . GroupConversationCreatedV9 $ CreateGroupOwnConversation conv mempty

createGroupConversation ::
  (Member ConversationSubsystem.ConversationSubsystem r) =>
  Local UserId ->
  Maybe ConnId ->
  NewConv ->
  Sem r CreateGroupConversation
createGroupConversation lusr conn newConv = do
  dbConv <- ConversationSubsystem.createGroupConversation lusr conn newConv
  pure $
    CreateGroupConversation
      { conversation = conversationView (qualifyAs lusr ()) (Just lusr) dbConv,
        failedToAdd = mempty
      }

createProteusSelfConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Sem r (ConversationResponse Public.OwnConversation)
createProteusSelfConversation lusr = do
  (c, created) <- ConversationSubsystem.createProteusSelfConversation lusr
  if created
    then Created <$> conversationViewV9 lusr c
    else Existed <$> conversationViewV9 lusr c

createOne2OneConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  ConnId ->
  NewOne2OneConv ->
  Sem r (ConversationResponse Public.OwnConversation)
createOne2OneConversation lusr zcon j = do
  (c, created) <- ConversationSubsystem.createOne2OneConversation lusr zcon j
  if created
    then Created <$> conversationViewV9 lusr c
    else Existed <$> conversationViewV9 lusr c

----------------------------------------------------------------------------
-- Helpers

createConnectConversation ::
  ( Member ConversationSubsystem.ConversationSubsystem r,
    Member (Error InternalError) r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  Maybe ConnId ->
  Connect ->
  Sem r (ConversationResponse Public.OwnConversation)
createConnectConversation lusr conn j = do
  c <- ConversationSubsystem.createConnectConversation lusr conn j
  Existed <$> conversationViewV9 lusr c
