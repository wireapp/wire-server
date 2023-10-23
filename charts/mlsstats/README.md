# mlsstats

The kubernetes cronjob resource will spawn a new `mlsstats-XXXXXX` pod every day. Logs for the pod can be gathered with `kubectl log`.

## Important note

This cron job is _not_ meant for general use! It can leak data about one team to other teams.
