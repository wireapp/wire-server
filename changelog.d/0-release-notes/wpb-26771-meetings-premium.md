* The `meetingsPremium` team feature flag is **deprecated** (WPB-26771). It no
  longer affects meeting behaviour: team meetings are always non-trial
  regardless of its value. Its default is now **enabled and locked**, and the
  Helm configuration override for `meetingsPremium` has been removed from
  `charts/wire-server`. The flag's data type and its public/internal HTTP
  endpoints are retained for backward compatibility but have no behavioural
  effect; any Helm overrides for `meetingsPremium` are now ignored and can be
  removed. The flag is scheduled for removal in a future release.
