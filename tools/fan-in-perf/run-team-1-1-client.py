#!/usr/bin/env python3
"""Runs fan-in-perf experiments A-D (WPB-26288) with --targets team:1 (2,000
online users with 1 client each, all reading the one team stream: 2,000
readers, every notification is delivered 2,000 times) on pg_partman
(partitioned) and plain tables, seed 42, and writes a markdown report. E and F
(plain only) split B and C: first `produce`, then `consume-only[-ack]` on the
rows it left, for the raw consumption speed.

  run-team-1-1-client.py [OUT.md] [--only A,E,...] [--quick] [--payload-bytes B] [--db CONNSTR]
  (OUT.md default: fan-in-team-1-1-client-results.md)

DESTRUCTIVE: drops and recreates the fan-in tables and pg_partman in the
database (default fan_in_perf; created if missing). --only E,F: the latter only,
--quick: all durations divided by 10, for a smoke test. Settings were tuned on
the author's machine (22 CPUs, 62 GiB) before, for run-team-1.py (2 clients per
user), and taken over unchanged.
The tool's output of every run goes to the log directory. Shares its code with
run-2000x1.py and run-team-1.py (drivers-lib.py).
"""

import importlib
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
lib = importlib.import_module("drivers-lib")

CFG = lib.Config(
    script="run-team-1-1-client.py",
    targets="team:1",
    # no user pool in the targets: the online users are synthetic
    reader_args=["--online-users", 2000, "--clients-per-user", 1],
    title="`team:1`, 2,000 users x 1 client",
    clients="2,000 clients",
    load_text="2,000 online users with 1 client each, all in one team; every push writes 1 notification to the team stream in one transaction, read by all 2,000 clients, so cons/s = 2,000 x push/s.",
    scope_text="Only the `team` stream kind is exercised (one team, no user/client/epoch streams, fan-out 2,000); not comparable to runs with a mixed target set.",
    default_out="fan-in-team-1-1-client-results.md",
    logdir="/tmp/fan-in-team-1-1-client",
    # All values below: tuned for run-team-1.py (2 clients per user, 4,000 readers) and taken
    # over unchanged, not retuned for 1 client.
    # Tuned on plain (30-40 s runs, repeated; the run-to-run variance is high). All writers hit
    # one stream and one last_user_notifications row: 2 writers give the best push/s (about
    # 3.6k/s; more writers contend, 3 or 8 sometimes fall to 0.6k/s).
    # B, C: the readers are the limit, not the database: the tool is at about 875 % CPU,
    # backends idle. With 4,000 readers they take at most about 800k deliveries/s (B) and 40k
    # acks/s (C), but one writer already pushes 600+/s: no setting keeps up, readers fall
    # behind (backlog, `missing`) in every run. 1 writer is the least overload; the pool
    # sizes are the best of 16/32/64.
    w_a=2, w_b=1, w_c=1, pool_b=16, pool_c=32,
    # E, F: one second of produce leaves about 600-3,500 rows (bimodal): consume-only takes
    # a short time in E and minutes in F. Pool 32: best for F, E is faster with 16 but 32 is
    # within 15 %.
    w_split=2, split_produce=1, split_warmup=2, split_timeout=1200, pool_split=32,
    tuning_text="Taken over from run-team-1.py (2 clients per user), not retuned. A: best push/s in sweeps beforehand. B-D: the readers (tool-bound) probably cannot keep up with even one writer, the writer count is the lowest possible, so backlog and `missing` are possible; pools: best of sweeps. E, F: pool best of sweeps.",
)

if __name__ == "__main__":
    lib.main(CFG, __doc__)
