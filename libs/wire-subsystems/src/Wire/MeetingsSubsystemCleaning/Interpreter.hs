{-# LANGUAGE DuplicateRecordFields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MeetingsSubsystemCleaning.Interpreter where

import Data.Qualified (qUnqualified)
import Data.Time.Clock (UTCTime)
import Imports
import Polysemy
import Wire.API.Conversation (GroupConvType (MeetingConversation), cnvmGroupConvType)
import Wire.API.Meeting (Meeting (..))
import Wire.ConversationStore qualified as ConvStore
import Wire.MeetingsStore qualified as Store
import Wire.MeetingsSubsystemCleaning
import Wire.StoredConversation (StoredConversation (..))

interpretMeetingsSubsystemCleaning ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  InterpreterFor MeetingsSubsystemCleaning r
interpretMeetingsSubsystemCleaning = interpret $ \case
  CleanupOldMeetings cutoffTime batchSize ->
    cleanupOldMeetingsImpl cutoffTime batchSize

cleanupOldMeetingsImpl ::
  ( Member Store.MeetingsStore r,
    Member ConvStore.ConversationStore r
  ) =>
  UTCTime ->
  Int ->
  Sem r Int64
cleanupOldMeetingsImpl cutoffTime batchSize = do
  -- 1. Fetch old meetings
  oldMeetings <- Store.getOldMeetings cutoffTime batchSize

  if null oldMeetings
    then pure 0
    else do
      -- 2. Extract meeting IDs and conversation IDs
      let meetingIds = map (\Meeting {id = mid} -> mid) oldMeetings
          convIds = map (\Meeting {conversationId = cid} -> cid) oldMeetings

      -- 3. Delete meetings from database
      deletedCount <- Store.deleteMeetingBatch meetingIds

      -- 4. Delete associated conversations if they are meeting conversations
      -- We need to check if conversation has GroupConvType = MeetingConversation
      for_ (zip oldMeetings convIds) $ \(meeting, qConvId) -> do
        let convId = qUnqualified qConvId
        maybeConv <- ConvStore.getConversation convId
        case maybeConv of
          Just conv
            | conv.metadata.cnvmGroupConvType == Just MeetingConversation,
              conv.id_ == convId,
              meeting.conversationId == qConvId ->
                ConvStore.deleteConversation convId
          _ -> pure ()

      pure deletedCount
