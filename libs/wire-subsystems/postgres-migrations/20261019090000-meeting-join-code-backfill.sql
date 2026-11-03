-- WPB-29361: one-off data migration giving every existing meeting a live
-- join code (and therefore a canonical join link). Meetings created after
-- the mandatory-link feature always receive a code in the creation path
-- (Wire.MeetingsSubsystem.Interpreter.createMeeting), so this runs once per
-- database; the migrations schema prevents re-running it.

-- Mirrors the runtime insert-if-absent semantics this migration replaces:
-- a live row (expires_at > now()) is never touched, so an already-served
-- join link is never invalidated; only an expired row is replaced.
-- key mirrors Wire.CodeStore.Code.mkKeyId: unpadded base64url of the first
-- 15 bytes of SHA256("meeting:" <> canonical textual meeting id).
-- expires_at mirrors meetingCodeTimeout (max Int32 seconds).
-- sha256() and gen_random_uuid() are Postgres built-ins; no pgcrypto needed.
-- (The UUID-derived value carries ~114 random bits vs 120 in
-- Wire.CodeStore.Code.generate, which draws 15 fresh random bytes.)
INSERT INTO conversation_codes (key, conversation, password, value, expires_at, target)
SELECT
  k.key,
  m.id,
  NULL,
  translate(encode(substring(decode(replace(gen_random_uuid()::text, '-', ''), 'hex') FROM 1 FOR 15), 'base64'), '+/', '-_'),
  now() + make_interval(secs => 2147483647),
  'meeting'
FROM meetings m
CROSS JOIN LATERAL (
  SELECT translate(encode(substring(sha256(('meeting:' || m.id::text)::bytea) FROM 1 FOR 15), 'base64'), '+/', '-_') AS key
) k
WHERE NOT EXISTS (
  SELECT 1 FROM conversation_codes live
  WHERE live.key = k.key AND live.expires_at > now()
)
ON CONFLICT (key) DO UPDATE
SET conversation = EXCLUDED.conversation,
    password = EXCLUDED.password,
    value = EXCLUDED.value,
    expires_at = EXCLUDED.expires_at,
    target = EXCLUDED.target
WHERE conversation_codes.expires_at <= now();
