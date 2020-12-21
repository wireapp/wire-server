reaper
------

Due to the nature of pods and their ephemerality, there might be situations where a redis pod is restarted.
In such cases, clients will have stale connections and will not receive any messages; this reaper will check that the `redis-ephemeral` pod is older than any other `cannon`; if that is not the case, it kills the `cannon`s forcing clients to reconnect.
