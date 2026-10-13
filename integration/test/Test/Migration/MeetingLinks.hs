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

-- | WPB-29361: the application-level meeting join-code backfill gives every
-- meeting without a live join code one (without touching existing codes), so
-- that every meeting can render its mandatory @link@.
module Test.Migration.MeetingLinks where

import API.Galley
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import SetupHelpers
import Test.Meetings (defaultMeetingJson, getMeetingIdAndDomain, linkKeyAndCode)
import Testlib.PG (execSql)
import Testlib.Prelude
import Text.Regex.TDFA ((=~))
import UnliftIO.Concurrent (threadDelay)

testMeetingJoinCodeBackfill :: (HasCallStack) => App ()
testMeetingJoinCodeBackfill = do
  (owner, _tid, [bob]) <- createTeam OwnDomain 2
  now <- liftIO getCurrentTime
  let newMeeting = defaultMeetingJson "Backfill Meeting" (addUTCTime 3600 now) (addUTCTime 7200 now) []

  -- (1) Meeting A is created through the API and has a code and a link.
  meetingA <- postMeetings owner newMeeting >>= getJSON 201
  (meetingAId, domain) <- getMeetingIdAndDomain meetingA
  linkA <- meetingA %. "link" >>= asString

  -- (2) Plant a legacy meeting row B without a conversation_codes row (and
  -- therefore without a link); it shares A's conversation so joins resolve.
  meetingBId <- T.unpack . UUID.toText <$> liftIO UUIDV4.nextRandom
  creatorId <- owner %. "id" >>= asString
  convAId <- meetingA %. "conversation" %. "qualified_id" %. "id" >>= asString
  execSql
    . T.pack
    $ "INSERT INTO meetings (id, title, creator, start_time, end_time, tzid, mtype,"
    <> " conversation_id, invited_emails, trial, created_at, updated_at)"
    <> " VALUES ('"
    <> meetingBId
    <> "', 'Legacy Backfill Meeting', '"
    <> creatorId
    <> "',"
    <> " now() + interval '1 hour', now() + interval '2 hours', 'Europe/Berlin', 'scheduled',"
    <> " '"
    <> convAId
    <> "', '{}', false, now(), now())"

  -- (3) Poll the list endpoint (which 500s with link-unavailable while B has
  -- no code) until B gains a link; A's link must stay byte-identical.
  startNow <- liftIO getCurrentTime
  let deadline = addUTCTime 120 startNow
  let pollLink mid = do
        nowX <- liftIO getCurrentTime
        if nowX > deadline
          then assertFailure ("meeting " <> mid <> " did not gain a link within 120s")
          else do
            listResp <- getMeetingsList owner
            if listResp.status == 500
              then liftIO (threadDelay 5_000_000) >> pollLink mid
              else do
                assertSuccess listResp
                meetings <- listResp.json & asList
                ms <- findMeetingById mid meetings
                case ms of
                  Nothing -> liftIO (threadDelay 5_000_000) >> pollLink mid
                  Just m -> m %. "link" >>= asString
  linkB <- pollLink meetingBId
  let expected = "meeting-join/\\?key=[A-Za-z0-9_-]{20}&code=[A-Za-z0-9_-]{6,20}&domain=" <> domain
  assertBool ("unexpected backfilled meeting link: " <> linkB) (linkB =~ expected)

  fetchedA <- getMeeting owner domain meetingAId >>= getJSON 200
  fetchedLinkA <- fetchedA %. "link" >>= asString
  assertBool "backfill must not touch an existing meeting's link" (fetchedLinkA == linkA)

  -- (4) B's key/code resolve via code-check and join.
  let (keyB, codeB) = linkKeyAndCode linkB
  getMeetingCodeCheck bob domain keyB codeB Nothing >>= assertStatus 200
  postMeetingJoin bob domain keyB codeB Nothing >>= assertStatus 200

  -- (5) Idempotence: after another backfill pass, B's link (its code was
  -- created in the previous pass) is byte-identical, and A's is untouched.
  liftIO $ threadDelay 65_000_000
  linkB' <- pollLink meetingBId
  assertBool "backfill must not rotate an existing meeting's link" (linkB' == linkB)
  fetchedA' <- getMeeting owner domain meetingAId >>= getJSON 200
  fetchedLinkA' <- fetchedA' %. "link" >>= asString
  assertBool "backfill must not touch an existing meeting's link" (fetchedLinkA' == linkA)

findMeetingById :: (HasCallStack) => String -> [Value] -> App (Maybe Value)
findMeetingById mid meetings = do
  ids <- mapM (\m -> m %. "qualified_id" %. "id" >>= asString) meetings
  pure (fmap fst (find ((== mid) . snd) (zip meetings ids)))
