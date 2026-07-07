* Deprecate the `meetingsPremium` feature flag (WPB-26771). It no longer drives
  the meeting `trial` status — team meetings are always non-trial, and only
  personal (non-team) users create trial meetings. The `MeetingsPremiumConfig`
  type now defaults to enabled/locked and carries a `DEPRECATED` pragma; the
  flag, its data type and its endpoints are kept for API compatibility and
  scheduled for removal. The Helm override and the `galley.integration.yaml`
  entry for `meetingsPremium` have been removed.
