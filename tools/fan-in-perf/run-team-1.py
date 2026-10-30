#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) with --targets team:1 (2,000
online users with 2 clients each, all reading the one team stream: 4,000
readers, every notification is delivered 4,000 times) on pg_partman
(partitioned) and plain tables, seed 42, and writes a markdown report. E and F
(plain only) split B and C: first `produce`, then `consume-only[-ack]` on the
rows it left, for the raw consumption speed.

  run-team-1.py [OUT.md] [--only A,E,...] [--quick] [--payload-bytes B] [--db CONNSTR]
  (OUT.md default: fan-in-team-1-results.md)

DESTRUCTIVE: drops and recreates the fan-in tables and pg_partman in the
database (default fan_in_perf; created if missing). --only E,F: the latter only,
--quick: all durations divided by 10, for a smoke test. Settings were tuned on
the author's machine (22 CPUs, 62 GiB) before.
The tool's output of every run goes to the log directory. Shares its code with
run-2000x1.py (drivers-lib.py).
"""

import importlib
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
lib = importlib.import_module("drivers-lib")

CFG = lib.Config(
    script="run-team-1.py",
    targets="team:1",
    # no user pool in the targets: the online users are synthetic
    reader_args=["--online-users", 2000, "--clients-per-user", 2],
    title="`team:1`, 2,000 users x 2 clients",
    clients="4,000 clients",
    load_text="2,000 online users with 2 clients each, all in one team; every push writes 1 notification to the team stream in one transaction, read by all 4,000 clients, so cons/s = 4,000 x push/s.",
    scope_text="Only the `team` stream kind is exercised (one team, no user/client/epoch streams, fan-out 4,000); not comparable to runs with a mixed target set.",
    default_out="fan-in-team-1-results.md",
    logdir="/tmp/fan-in-team-1",
    # Tuned on plain (30-40 s runs, repeated; the run-to-run variance is high). All writers hit
    # one stream and one last_user_notifications row: 2 writers give the best push/s (about
    # 3.6k/s; more writers contend, 3 or 8 sometimes fall to 0.6k/s).
    # B, C: the readers (4,000, every row delivered 4,000 times) are the limit, not the
    # database: the tool is at about 875 % CPU, backends idle. They take at most about
    # 800k deliveries/s (B) and 40k acks/s (C), i.e. about 200 and 10 push/s, but one writer
    # already pushes 600+/s: no setting keeps up, readers fall behind (backlog, `missing`)
    # in every run. 1 writer is the least overload; the pool sizes are the best of 16/32/64.
    w_a=2, w_b=1, w_c=1, pool_b=16, pool_c=32,
    # E, F: one second of produce leaves about 600-3,500 rows (bimodal), i.e. 2-14M deliveries:
    # consume-only takes about 15-40 s (E, about 900k/s) and 1-6 min (F, about 40k acks/s), as
    # long as the 2000x1 runs (20 s, 290 s). Longer would take hours in F (hot-row contention
    # aside, a 300 s produce leaves about 1M rows = 4G acks). Pool 32: best for F, E is
    # faster with 16 but 32 is within 15 %.
    w_split=2, split_produce=1, split_warmup=2, split_timeout=1200, pool_split=32,
    tuning_text="A: best push/s in sweeps beforehand. B-D: the readers (tool-bound) cannot keep up with even one writer, the writer count is the lowest possible, so backlog and `missing` are expected; pools: best of sweeps. E, F: pool best of sweeps.",
)

if __name__ == "__main__":
    lib.main(CFG, __doc__)
