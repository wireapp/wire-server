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

module Wire.CodeStore where

import Data.Code
import Data.Domain (Domain)
import Data.Id
import Data.Misc
import Imports
import Polysemy
import Wire.API.Password
import Wire.CodeStore.Code

-- | Result of 'BackfillMeetingCode'.
data BackfillCodeResult
  = -- | No code existed; a fresh one was inserted.
    BackfilledCode Code
  | -- | A live code row already exists (another replica won the race or
    -- the meeting was already backfilled); it was left untouched.
    BackfillCodePresent
  | -- | The backing store cannot hold meeting codes (Cassandra-only mode).
    BackfillCodeStoreUnsupported
  deriving (Eq, Show)

data CodeStore m a where
  CreateCode :: Code -> Maybe Password -> CodeStore m ()
  GetCode :: Key -> CodeStore m (Maybe (Code, Maybe Password))
  DeleteConversationCode :: ConvId -> CodeStore m ()
  DeleteMeetingCode :: MeetingId -> CodeStore m ()
  -- | Generate and insert a join code for a meeting, storing the (hashed)
  -- password if given. Returns 'Nothing' when the backing store cannot hold
  -- meeting codes (Cassandra-only mode); API callers fail with
  -- @LinkUnavailable@.
  CreateMeetingCode :: MeetingId -> Timeout -> Maybe Password -> CodeStore m (Maybe Code)
  -- | Atomic insert-if-absent of a meeting's join code on the
  -- meeting-id-derived key, with no password. Concurrent galley replicas
  -- race on this op during backfill: the loser observes the winner's code
  -- ('BackfillCodePresent') instead of replacing it, so an already-served
  -- link is never invalidated. 'BackfillCodeStoreUnsupported' when the
  -- backing store cannot hold meeting codes (Cassandra-only mode).
  BackfillMeetingCode :: MeetingId -> Timeout -> CodeStore m BackfillCodeResult
  -- | Atomically rotate a meeting's join code (upsert of the stable-key row)
  -- and revoke the meeting conversation's guest code, in a single
  -- transaction. Splitting these into two writes would let a crash between
  -- them leave the old guest link usable even though the meeting link has
  -- already been replaced, so refresh must not compose them externally.
  -- Returns 'Nothing' when the backing store cannot hold meeting codes
  -- (Cassandra-only mode); refresh is an explicit user action and fails with
  -- @LinkUnavailable@ in that case.
  RotateMeetingCode :: MeetingId -> ConvId -> Timeout -> Maybe Password -> CodeStore m (Maybe Code)
  -- | Look up a meeting's join code by meeting id, together with its (hashed)
  -- password if one is set, so a future join endpoint can enforce it.
  -- 'Nothing' when no code
  -- row exists (legacy meeting, or code creation failed) or the backing store
  -- cannot hold meeting codes (Cassandra-only mode).
  GetMeetingCode :: MeetingId -> CodeStore m (Maybe (Code, Maybe Password))
  -- | Look up a join code by its stable code key (the key part of a join
  -- link), together with its (hashed) password if one is set. 'Nothing'
  -- when no live code row exists for the key or the backing store cannot
  -- hold meeting codes (Cassandra-only mode); note a found row may belong
  -- to a conversation code, so callers must check the code's referent.
  GetMeetingCodeByKey :: Key -> CodeStore m (Maybe (Code, Maybe Password))
  -- | Look up join codes for many meetings in a single query, together with
  -- each code's (hashed) password if one is set. Meetings without a code
  -- (legacy, expired, or a backing store that cannot hold meeting codes)
  -- are absent from the result map.
  GetMeetingCodes :: [MeetingId] -> CodeStore m (Map MeetingId (Code, Maybe Password))
  MakeKey :: CodeReferent -> CodeStore m Key
  GenerateCode :: CodeReferent -> Timeout -> CodeStore m Code
  GetConversationCodeURI :: Maybe Domain -> CodeStore m (Maybe HttpsUrl)

makeSem ''CodeStore
