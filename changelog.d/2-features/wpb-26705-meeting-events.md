* Added meeting lifecycle events: `meeting.create`, `meeting.update`, and
  `meeting.delete` (WPB-26705). These websocket notifications are pushed to all
  local members of the meeting's conversation on every successful create, update,
  and delete operation. The events use the standard conversation event envelope:
  each payload contains the event `type`, the meeting's qualified ID in the
  `data.qualified_id` field, the `qualified_conversation`, `qualified_from`,
  `via`, `time`, and optional `team`.
