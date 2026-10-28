# fan-in-perf (WPB-26288)

Load generator for the notification fan-in store proposed in the RFC [RFC:
Notification
fan-in](https://wearezeta.atlassian.net/wiki/spaces/PAD/pages/2774597689/2026-04-14+RFC+Notification+fan-in).
It drives `FanInPerf.NotificationsStore` against
PostgreSQL and measures write throughput, read throughput, delivery lag, ack
cost and notification loss.
