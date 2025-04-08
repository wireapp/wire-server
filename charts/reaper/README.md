Reaper
------

This pod is useful in the following scenario: You run wire-server alongside a single
redis-ephemeral (part of databases-ephemeral). If you have a different setup for redis,
do not use this chart.

Due to the nature of pods and their ephemerality, there might be situations where a
redis-ephemeral pod is restarted. In such cases, wire clients will have stale
connections (they will have an active websocket connection, but gundeck (responsible for
sending messages) will be unaware of this (as the record of who is connected where is
gone with a redis-ephemeral restart). So these stale clients will not receive any
messages. Here, this reaper will check that the `redis-ephemeral` pod is older than any
other `cannon`; if that is not the case, it kills the `cannon`s forcing clients to
reconnect.

