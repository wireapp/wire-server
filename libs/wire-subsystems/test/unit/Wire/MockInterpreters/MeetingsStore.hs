-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.MockInterpreters.MeetingsStore where

import Data.Id
import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.MeetingsStore
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random

inMemoryMeetingsStoreInterpreter ::
  (Member (State (Map MeetingId StoredMeeting)) r, Member Now r, Member Random r) =>
  InterpreterFor MeetingsStore r
inMemoryMeetingsStoreInterpreter = interpret $ \case
  CreateMeeting title creator startTime endTime recurrence conversationId invitedEmails trial -> do
    mid <- Random.newId
    now <- Now.get
    let sm =
          StoredMeeting
            { id = mid,
              title = title,
              creator = creator,
              startTime = startTime,
              endTime = endTime,
              recurrence = recurrence,
              conversationId = conversationId,
              invitedEmails = invitedEmails,
              trial = trial,
              createdAt = now,
              updatedAt = now
            }
    modify (Map.insert mid sm)
    pure sm
  GetMeeting mid -> gets (Map.lookup mid)
  UpdateMeeting mid title startTime endTime recurrence -> do
    sm <- gets (Map.lookup mid)
    case sm of
      Nothing -> pure Nothing
      Just meeting -> do
        now <- Now.get
        let updatedMeeting =
              meeting
                { title = fromMaybe (meeting.title) title,
                  startTime = fromMaybe meeting.startTime startTime,
                  endTime = fromMaybe meeting.endTime endTime,
                  recurrence = fromMaybe meeting.recurrence recurrence,
                  updatedAt = now
                }
        modify (Map.insert mid updatedMeeting) >> pure (Just updatedMeeting)
  DeleteMeeting mid -> modify (Map.delete mid)
