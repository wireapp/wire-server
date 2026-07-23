The `meetingsPremium` team feature endpoints are deprecated and return 404 for
clients on API version v17: the public `GET`/`PUT /teams/:tid/features/meetingsPremium`
and the internal legacy lock `PUT /i/teams/:tid/features/meetingsPremium/(un)?locked`.
They remain available through v16. The flag has had no behavioural effect since
WPB-26771 (team meetings are always non-trial). The aggregate endpoints
`GET /feature-configs` and `GET /teams/:tid/features` are unaffected and continue
to include `meetingsPremium` at all API versions: the aggregate feature list is
version-agnostic, like other version-gated features such as MLS. (WPB-26771)
