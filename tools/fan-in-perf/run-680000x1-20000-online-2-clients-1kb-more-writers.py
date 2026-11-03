#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) like run-680000x1-20000-online-2-clients-1kb.py
(--targets user:680000x1, only the first 20,000 users online with 2 clients each: 40,000
readers, woken by pushes; 1 KB payload), but with more writers and fewer reader connections
in B-D, because the writers are the limit there. Pg_partman (partitioned) and plain tables,
seed 42, markdown report. E and F (plain only) split B and C: first `produce`, then
`consume-only[-ack]` on the rows it left, for the raw consumption speed.

  run-680000x1-20000-online-2-clients-1kb-more-writers.py [OUT.md] [--only A,E,...] [--quick] [--payload-bytes B] [--db CONNSTR]
  (OUT.md default: fan-in-680000x1-20000-online-2-clients-1kb-more-writers-results.md; --payload-bytes default: 1024)

DESTRUCTIVE: drops and recreates the fan-in tables and pg_partman in the
database (default fan_in_perf; created if missing). --only E,F: the latter only,
--quick: all durations divided by 10, for a smoke test. Settings were tuned on
the author's machine (22 CPUs, 62 GiB) before, for run-2000x1.py (2,000 users, 1
client per user, 512 B payload); B-D differ, see below.
The tool's output of every run goes to the log directory.
Shares its code with run-2000x1.py (drivers-lib.py).
"""

import importlib
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
lib = importlib.import_module("drivers-lib")

CFG = lib.Config(
    script="run-680000x1-20000-online-2-clients-1kb-more-writers.py",
    targets="user:680000x1",
    # the first 20,000 pool users are online; both clients of a user read its user stream
    reader_args=["--online-users", 20000, "--clients-per-user", 2],
    payload_bytes=1024,
    wake_on_push=True,  # no idle polls: readers fetch only after a push to one of their streams
    title="`user:680000x1`, 680,000 users, 20,000 online x 2 clients",
    clients="40,000 clients",
    load_text="680,000 users, the first 20,000 online with 2 clients each; every push writes 1 notification to 1 of the 680,000 user streams (plus its `last_user_notifications` upsert) in one transaction, read by both clients if the user is online (about 1 of 34 pushes), so cons/s = about 2 x push/s / 34.",
    scope_text="Only the `user` stream kind is exercised (no client/team/epoch streams, fan-out 2 for online users, 0 for the others); not comparable to runs with a mixed target set.",
    default_out="fan-in-680000x1-20000-online-2-clients-1kb-more-writers-results.md",
    logdir="/tmp/fan-in-680000x1-20000-online-2-clients-1kb-more-writers",
    # A, E, F: tuned for run-2000x1.py (2,000 users, 1 client per user, 512 B), taken over.
    # B-D: the readers are woken by pushes and see only about 1/34 of them, so the writers are
    # the limit: twice the writers of run-2000x1.py (B 64, C/D 32) and a quarter of its reader
    # connections (16 instead of 64). A guess, not tuned.
    w_a=64, w_b=64, w_c=32, pool_b=16, pool_c=16,
    # E, F: produce (w_split writers) for split_produce s, then consume-only. Long enough
    # for the consume phase to last well past split_warmup (max rate ignores it).
    # w_split: more writers than A: in 300 s runs push/s decays with 64 (WAL-bound), not with 128.
    w_split=128, split_produce=300, split_warmup=2, split_timeout=1200,
    # With 64, the readers sometimes idled for about 5 s (while all connections were idle,
    # cause not found), which dominates the average; 16, 32 and 48 never did in repeated runs.
    pool_split=32,
    tuning_text="A, E, F: taken over from run-2000x1.py (2,000 users, 1 client per user, 512 B payload), not retuned. B-D: twice the writers and a quarter of the reader connections of run-2000x1.py, because the readers are woken by pushes and see only a small share of them; a guess, not tuned.",
)

if __name__ == "__main__":
    lib.main(CFG, __doc__)
