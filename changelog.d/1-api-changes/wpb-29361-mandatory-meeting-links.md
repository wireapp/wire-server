Meetings now always expose a `link` field on the V19 API shape: every meeting is guaranteed a canonical join link derived from a live join code in the PostgreSQL code store.

Refreshing a meeting link reports `has_password` according to the password supplied with the refresh request.
