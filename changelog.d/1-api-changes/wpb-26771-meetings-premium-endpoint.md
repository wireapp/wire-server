The `meetingsPremium` team feature endpoints are deprecated and return 404 for
clients on API version v17: the public `GET`/`PUT /teams/:tid/features/meetingsPremium`
and the internal legacy lock `PUT /i/teams/:tid/features/meetingsPremium/(un)?locked`.
They remain available through v16. The flag has had no behavioural effect since
WPB-26771 (team meetings are always non-trial). (WPB-26771)
