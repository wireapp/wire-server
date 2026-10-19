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

-- | WPB-29361: the one-off meeting join-code backfill migration gives every
-- meeting without a live join code one (without touching existing codes), so
-- that every meeting can render its mandatory @link@.
module Test.Migration.MeetingLinks where

import API.Galley
import qualified Crypto.Hash as Crypto
import Crypto.Hash.Algorithms (SHA256 (SHA256))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time.Clock
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import SetupHelpers
import Test.Meetings (defaultMeetingJson, getMeetingIdAndDomain, linkKeyAndCode)
import Testlib.PG (execSql)
import Testlib.Prelude
import Text.Regex.TDFA ((=~))

-- | Mirror of
-- libs/wire-subsystems/postgres-migrations/20261019090000-meeting-join-code-backfill.sql;
-- the integration image has no repo checkout, so the statement is duplicated
-- here.
backfillSql :: String
backfillSql =
  "INSERT INTO conversation_codes (key, conversation, password, value, expires_at, target)\n\
  \SELECT\n\
  \  k.key,\n\
  \  m.id,\n\
  \  NULL,\n\
  \  translate(encode(substring(decode(replace(gen_random_uuid()::text, '-', ''), 'hex') FROM 1 FOR 15), 'base64'), '+/', '-_'),\n\
  \  now() + make_interval(secs => 2147483647),\n\
  \  'meeting'\n\
  \FROM meetings m\n\
  \CROSS JOIN LATERAL (\n\
  \  SELECT translate(encode(substring(sha256(('meeting:' || m.id::text)::bytea) FROM 1 FOR 15), 'base64'), '+/', '-_') AS key\n\
  \) k\n\
  \WHERE NOT EXISTS (\n\
  \  SELECT 1 FROM conversation_codes live\n\
  \  WHERE live.key = k.key AND live.expires_at > now()\n\
  \)\n\
  \ON CONFLICT (key) DO UPDATE\n\
  \SET conversation = EXCLUDED.conversation,\n\
  \    password = EXCLUDED.password,\n\
  \    value = EXCLUDED.value,\n\
  \    expires_at = EXCLUDED.expires_at,\n\
  \    target = EXCLUDED.target\n\
  \WHERE conversation_codes.expires_at <= now();"

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

  -- (2b) Plant a legacy meeting C that already has an *expired*
  -- conversation_codes row under its (deterministic) key. Lookups filter
  -- expired rows, so C has no live link; the backfill must REPLACE the
  -- stale row (ON CONFLICT ... WHERE expires_at <= now()) instead of
  -- treating the key as occupied forever.
  meetingCId <- T.unpack . UUID.toText <$> liftIO UUIDV4.nextRandom
  execSql
    . T.pack
    $ "INSERT INTO meetings (id, title, creator, start_time, end_time, tzid, mtype,"
    <> " conversation_id, invited_emails, trial, created_at, updated_at)"
    <> " VALUES ('"
    <> meetingCId
    <> "', 'Expired Backfill Meeting', '"
    <> creatorId
    <> "',"
    <> " now() + interval '1 hour', now() + interval '2 hours', 'Europe/Berlin', 'scheduled',"
    <> " '"
    <> convAId
    <> "', '{}', false, now(), now())"
  execSql
    . T.pack
    $ "INSERT INTO conversation_codes (key, conversation, password, value, expires_at, target)"
    <> " VALUES ('"
    <> meetingJoinCodeKey meetingCId
    <> "', '"
    <> convAId
    <> "', NULL, 'stale-expired-value', now() - interval '1 hour', 'meeting')"

  -- (3) Run the one-off backfill migration (first pass); B gains a link and
  -- A's link stays byte-identical.
  execSql (T.pack backfillSql)
  linkB <- fetchLink owner meetingBId
  let expected = "meeting-join/\\?key=[A-Za-z0-9_-]{20}&code=[A-Za-z0-9_-]{6,20}&domain=" <> domain
  assertBool ("unexpected backfilled meeting link: " <> linkB) (linkB =~ expected)

  fetchedA <- getMeeting owner domain meetingAId >>= getJSON 200
  fetchedLinkA <- fetchedA %. "link" >>= asString
  assertBool "backfill must not touch an existing meeting's link" (fetchedLinkA == linkA)

  -- (4) B's key/code resolve via code-check and join.
  let (keyB, codeB) = linkKeyAndCode linkB
  getMeetingCodeCheck bob domain keyB codeB Nothing >>= assertStatus 200
  postMeetingJoin bob domain keyB codeB Nothing >>= assertStatus 200

  -- (4b) C's expired code row was replaced: C gained a link whose key is the
  -- same deterministic key, but whose code is a fresh value.
  linkC <- fetchLink owner meetingCId
  assertBool ("unexpected backfilled meeting link: " <> linkC) (linkC =~ expected)
  let (keyC, codeC) = linkKeyAndCode linkC
  assertBool "backfill must reuse the meeting's stable key" (keyC == meetingJoinCodeKey meetingCId)
  assertBool "backfill must replace the expired code value" (codeC /= "stale-expired-value")
  getMeetingCodeCheck bob domain keyC codeC Nothing >>= assertStatus 200
  postMeetingJoin bob domain keyC codeC Nothing >>= assertStatus 200

  -- (5) Idempotence: a second pass must change nothing — B's and C's links
  -- (their codes were created/replaced in the first pass) are byte-identical,
  -- and A's is untouched.
  execSql (T.pack backfillSql)
  linkB' <- fetchLink owner meetingBId
  assertBool "backfill must not rotate an existing meeting's link" (linkB' == linkB)
  linkC' <- fetchLink owner meetingCId
  assertBool "backfill must not rotate a replaced code" (linkC' == linkC)
  fetchedA' <- getMeeting owner domain meetingAId >>= getJSON 200
  fetchedLinkA' <- fetchedA' %. "link" >>= asString
  assertBool "backfill must not touch an existing meeting's link" (fetchedLinkA' == linkA)

-- | List the owner's meetings and return the link of the given meeting.
fetchLink :: (HasCallStack) => Value -> String -> App String
fetchLink owner mid = do
  listResp <- getMeetingsList owner
  assertSuccess listResp
  meetings <- listResp.json & asList
  ms <- findMeetingById mid meetings
  case ms of
    Nothing -> assertFailure ("meeting " <> mid <> " missing from meetings list")
    Just m -> m %. "link" >>= asString

findMeetingById :: (HasCallStack) => String -> [Value] -> App (Maybe Value)
findMeetingById mid meetings = do
  ids <- mapM (\m -> m %. "qualified_id" %. "id" >>= asString) meetings
  pure (fmap fst (find ((== mid) . snd) (zip meetings ids)))

-- | The deterministic meeting join-code key, mirroring
-- 'Wire.CodeStore.Code.mkKeyId' (base64url of the first 15 bytes of
-- SHA256("meeting:" <> meetingId)).
meetingJoinCodeKey :: (HasCallStack) => String -> String
meetingJoinCodeKey mid =
  T.unpack
    . decodeUtf8
    . B64URL.encode
    . BS.take 15
    . BA.convert
    $ Crypto.hashWith SHA256 (BSC.append (BSC.pack "meeting:") (encodeUtf8 (T.pack mid)))
