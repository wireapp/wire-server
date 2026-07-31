Added `meeting.member-add` websocket event (WPB-27620). When a user becomes a
member of an MLS meeting conversation, a `meeting.member-add` lifecycle event is
pushed to the newly-added local members, alongside the existing `meeting.create`,
`meeting.update`, and `meeting.delete` events. The payload uses the same meeting
event structure as the other meeting lifecycle events.
