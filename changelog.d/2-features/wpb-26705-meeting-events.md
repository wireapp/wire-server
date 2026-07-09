* Added meeting lifecycle events: `meeting.create`, `meeting.update`, and
  `meeting.delete` (WPB-26705). These websocket notifications are pushed to all
  local members of the meeting's conversation on every successful create, update,
  and delete operation. Each event payload contains the event `type`, `time`, and
  the `qualified_id` of the affected meeting.
