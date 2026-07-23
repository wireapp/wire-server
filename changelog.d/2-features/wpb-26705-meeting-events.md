* Added meeting lifecycle events: `meeting.create`, `meeting.update`, and
  `meeting.delete` (WPB-26705). These websocket notifications are pushed to all
  local members of the meeting's conversation on every successful create, update,
  and delete operation. Each payload carries the event `type`, the meeting's
  qualified ID in the top-level `qualified_id` field, the
  `qualified_conversation`, `qualified_from`, `via`, `time`, and optional `team`.
  Meeting events use a dedicated event envelope (not the conversation event
  envelope).
