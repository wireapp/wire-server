#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) with --targets user:2000x1 and
2,000 clients on pg_partman (partitioned) and plain tables, seed 42, and writes
a markdown report for the Jira ticket. E and F (plain only) split B and C: first
`produce`, then `consume-only[-ack]` on the rows it left, for the raw consumption
speed.

  run-2000x1.py [OUT.md] [--only A,E,...] [--quick] [--payload-bytes B] [--db CONNSTR]
  (OUT.md default: fan-in-2000x1-results.md)

DESTRUCTIVE: drops and recreates the fan-in tables and pg_partman in the
database (default fan_in_perf; created if missing). Takes about
40 minutes for A-D plus about 10 for E and F (--only E,F: the latter only,
--quick: all durations divided by 10, for a smoke test). Settings were tuned on
the author's machine (22 CPUs, 62 GiB) before.
The tool's output of every run goes to the log directory (/tmp/fan-in-2000x1, with --payload-bytes B: -pB). Checks of the results against
the database and Prometheus: sanity-helpers.py.
"""

import importlib
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
lib = importlib.import_module("drivers-lib")

CFG = lib.Config(
    script="run-2000x1.py",
    targets="user:2000x1",
    # Not passed to the tool: the targets' 2000 pool users are all online, 1 client each (2000 readers).
    reader_args=[],
    title="`user:2000x1`, 2,000 clients",
    clients="2,000 clients",
    load_text="2,000 users with one client each; every push writes 1 notification to 1 user stream (plus its `last_user_notifications` upsert) in one transaction, read by 1 client, so cons/s = push/s.",
    scope_text="Only the `user` stream kind is exercised (no client/team/epoch streams, fan-out 1); not comparable to runs with a mixed target set.",
    default_out="fan-in-2000x1-results.md",
    logdir="/tmp/fan-in-2000x1",
    # Tuned beforehand on plain (45 s runs, repeated): A max push/s; B, C highest rate
    # whose readers kept up (backlog at writer stop < 1 s, no loss) in every run.
    w_a=64, w_b=32, w_c=16, pool_b=64, pool_c=64,
    # E, F: produce (w_split writers) for split_produce s, then consume-only. Long enough
    # for the consume phase to last well past split_warmup (max rate ignores it).
    # w_split: more writers than A: in 300 s runs push/s decays with 64 (WAL-bound), not with 128.
    w_split=128, split_produce=300, split_warmup=2, split_timeout=1200,
    # With 64, the readers sometimes idled for about 5 s (while all connections were idle,
    # cause not found), which dominates the average; 16, 32 and 48 never did in repeated runs.
    pool_split=32,
    tuning_text="highest rate in sweeps beforehand at which readers keep up.",
)

if __name__ == "__main__":
    lib.main(CFG, __doc__)
