* The `meetingsPremium` team feature flag is **deprecated** (WPB-26771). It no
  longer affects meeting behaviour: team meetings are always non-trial
  regardless of its value. Its default is now **enabled and locked**, and the
  Helm configuration override for `meetingsPremium` has been removed from
  `charts/wire-server`. The flag's data type and its public/internal HTTP
  endpoints are retained for backward compatibility but have no behavioural
  effect; any Helm overrides for `meetingsPremium` are now ignored and can be
  removed. The public/internal HTTP endpoints now return 404 at API version v17
  and remain available through v16; the flag type remains deprecated. The aggregate `GET /feature-configs` and `GET /teams/:tid/features` endpoints continue to include `meetingsPremium` at all API versions, including v17.
